// Copyright 2020, Brian Swetland <swetland@frotz.net>
// Licensed under the Apache License, Version 2.0.

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdint.h>
#include <strings.h>
#include <string.h>

#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#define FNMAXARGS 8

typedef uint32_t u32;
typedef int32_t i32;

typedef enum {
	tEOF, tEOL,
	tDOT, tCOMMA, tCOLON, tSEMI, tBANG, tOBRACK, tCBRACK,
	tOPAREN, tCPAREN, tOBRACE, tCBRACE, tASSIGN,
	tPLUS, tMINUS, tSTAR, tSLASH, tAMP, tPIPE, tCARET,
	tAND, tOR, tEQ, tGT, tLT, tGE, tLE, tNE,
	tINCR, tDECR,
	tVAR, tSTRUCT, tFUNC, tRETURN, tIF, tELSE,
	tWHILE, tFOR, tBREAK, tSWITCH, tCASE,
	tNAME, tNUMBER, tSTRING,
	NUMTOKENS,
} token_t;

char *tnames[] = {
	"<EOF>", "<EOL>", 
	".", ",", ":", ";", "!", "[", "]",
	"(", ")","{", "}", "=",
	"+", "-", "*", "/", "&", "|", "^",
	"&&", "||", "==", ">", "<", ">=", "<=", "!=",
	"++", "--",
	"var", "struct", "func", "return", "if", "else",
	"while", "for", "break", "switch", "case", 
	"<NAME>", "<NUMBER>", "<STRING>",
};


typedef struct StringRec* String;
typedef struct SymbolRec* Symbol;
typedef struct ScopeRec* Scope;
typedef struct TypeRec* Type;
typedef struct FuncRec* Func;
typedef struct CtxRec* Ctx;

typedef struct StringRec StringRec;
typedef struct SymbolRec SymbolRec;
typedef struct ScopeRec ScopeRec;
typedef struct TypeRec TypeRec;
typedef struct FuncRec FuncRec;
typedef struct CtxRec CtxRec;

struct StringRec {
	String next;
	u32 len;
	char text[0];
};

#define TF_INTEGER 0x01
#define TF_SIGNED  0x02
#define TF_VOID    0x04

struct TypeRec {
	Type next;
	String name;
	u32 flags;
	u32 width;
};

struct CtxRec {
	const char* source;    // entire source file
	const char* sptr;      // tokenizer source pointer
	const char* line;      // start of most recent line
	const char* filename;  // filename of active source
	unsigned linenumber;   // line number of most recent line

	token_t tok;           // most recent token
	unsigned num;
	char tmp[256];         // used for tNAME, tTYPE, tNUMBER, tSTRING;

	String strtab;         // TODO: hashtable
	Type typetab;       // TODO: hashtable
	Symbol symtab;      // TODO: hashtable, globals
	Scope scope;

	Type type_void;
	Type type_i32;
	Type type_u32;
};

struct ScopeRec {
	Scope next;
	Symbol first;
};

#define SF_REGISTER   0x01
#define SF_FRAMEREL   0x02
#define SF_GLOBAL     0x04 // global variable
#define SF_FUNC       0x08 // function 
#define SF_DEFINED    0x10 // defined, not just declared (function)

struct SymbolRec {
	Symbol next;
	String name;
	Type type;
	u32 flags;
	i32 posn;
	i32 regno;
	Func func;
};

struct FuncRec {
	ScopeRec scope;
	u32 pcount;
	Type type;      // return type
	SymbolRec param[0];
};

String mkstring(Ctx ctx, const char* text, unsigned len) {
	String str;

	for (str = ctx->strtab; str != NULL; str = str->next) {
		if ((str->len == len) && (memcmp(text, str->text, len) == 0)) {
			return str;
		}
	}

	str = malloc(sizeof(StringRec) + len + 1);
	str->len = len;
	memcpy(str->text, text, len);
	str->text[len] = 0;
	str->next = ctx->strtab;
	ctx->strtab = str;

	return str;
}

Type mktype(Ctx ctx, const char* text, unsigned len, unsigned flags, unsigned width) {
	String str = mkstring(ctx, text, len);
	Type type = malloc(sizeof(TypeRec));
	type->name = str;
	type->width = width;
	type->next = ctx->typetab;
	ctx->typetab = type;
	return type;
}

void init_ctx(Ctx ctx) {
	memset(ctx, 0, sizeof(CtxRec));

	// install built-in plain types
	ctx->type_void = mktype(ctx, "void", 4, TF_VOID, 0);
	ctx->type_i32 = mktype(ctx, "i32",  3, TF_INTEGER | TF_SIGNED, 4);
	ctx->type_u32 = mktype(ctx, "u32",  3, TF_SIGNED, 4);
}

void error(Ctx ctx, const char *fmt, ...) {
	va_list ap;

	unsigned len = 0;
	const char *s = ctx->line;
	while (len < 255) {
		if ((*s < ' ') && (*s != 9)) break;
		s++;
		len++;
	}

	fprintf(stderr,"%s:%d: ", ctx->filename, ctx->linenumber);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	
	fprintf(stderr,"\n%.*s\n", len, ctx->line);
	exit(1);
}

void load(Ctx ctx, const char* filename) {
	ctx->filename = filename;
	ctx->linenumber = 0;

	int fd;
	struct stat s;
	char* data;

	if ((fd = open(filename, O_RDONLY)) < 0)
		error(ctx, "cannot open file");
	if (fstat(fd, &s) < 0)
		error(ctx, "cannot stat file");
	if ((data = malloc(s.st_size + 1)) == NULL)
		error(ctx, "cannot allocate memory");
	if (read(fd, data, s.st_size) != s.st_size)
		error(ctx, "cannot read file");
	close(fd);
	data[s.st_size] = 0;

	ctx->source = data;
	ctx->sptr = data;
	ctx->linenumber = 1;
}

int unhex(unsigned ch) {
	switch (ch) {
	case '0' ... '9': return ch - '0';
	case 'a' ... 'f': return ch - 'a' + 10;
	case 'A' ... 'F': return ch - 'A' + 10;
	default: return -1;
	}
}

token_t next_string(Ctx ctx, const char* s) {
	unsigned ch, len = 0;
	for (;;) {
		switch ((ch = *s++)) {
		case 0: error(ctx, "unterminated string");
		case '"': goto done;
		case '\\':
			switch ((ch = *s++)) {
			case '0': error(ctx, "unterminated string");
			case 'n': ch = 10; break;
			case 't': ch = 9; break;
		  	case '"': ch = '"'; break;
			case 'x': {
				int x0 = unhex(s[0]);
				int x1 = unhex(s[1]);
				//TODO: if error() is ever non-fatal, this may leave
				//sptr past end of input
				if ((x0 < 0) || (x1 < 0)) error(ctx, "invalid hex escape");
				ch = (x0 << 4) | x1;
				s += 2;
				break;
			}
			default: error(ctx, "invalid string escape 0x%02x", ch);
			}
			break;
		default:
			break;
		}
		if (len == 255) error(ctx, "string constant too long");
		ctx->tmp[len++] = ch;
	}
done:
	ctx->tmp[len] = 0;
	ctx->sptr = s;
	return tSTRING;
}

token_t next_num(Ctx ctx, u32 n, const char* str, size_t len) {
	if (len > 255) error(ctx, "number too large");
	memcpy(ctx->tmp, str, len);
	ctx->tmp[len] = 0;
	ctx->num = n;
	ctx->sptr += len;
	return ctx->tok = tNUMBER;
}

int streq(const char* s1, unsigned l1, const char* s2, unsigned l2) {
	return (l1 == l2) && (!memcmp(s1, s2, l1));
}

token_t next_word(Ctx ctx, const char* str, size_t len) {
	if (len > 255) error(ctx, "word too large");
	memcpy(ctx->tmp, str, len);
	ctx->tmp[len] = 0;
	ctx->num = 0;
	ctx->sptr += len;
	switch (len) {
	case 2:
		if (streq(str, len, "if", 2)) return ctx->tok = tIF;
		break;
	case 3:
		if (streq(str, len, "for", 3)) return ctx->tok = tFOR;
		if (streq(str, len, "var", 3)) return ctx->tok = tVAR;
		break;
	case 4:
		if (streq(str, len, "case", 4)) return ctx->tok = tCASE;
		if (streq(str, len, "func", 4)) return ctx->tok = tFUNC;
		if (streq(str, len, "else", 4)) return ctx->tok = tELSE;
		break;
	case 5:
		if (streq(str, len, "break", 5)) return ctx->tok = tBREAK;
		if (streq(str, len, "while", 5)) return ctx->tok = tWHILE;
		break;
	case 6:
		if (streq(str, len, "switch", 6)) return ctx->tok = tSWITCH;
		if (streq(str, len, "struct", 6)) return ctx->tok = tSTRUCT;
		if (streq(str, len, "return", 6)) return ctx->tok = tRETURN;
		break;
	}
	return ctx->tok = tNAME;
}

#define TOKEN(t) { ctx->sptr++; return ctx->tok = t; }
#define TOKEN2(t) { ctx->sptr+=2; return ctx->tok = t; }

token_t _next(Ctx ctx, int misc) {
	for (;;) {
		const char* s = ctx->sptr;

		switch (*s) {
		case 0:
			return ctx->tok = tEOF;
		case '\n':
			ctx->linenumber++;
			ctx->sptr++;
			ctx->line = ctx->sptr;
			if (misc) return ctx->tok = tEOL;
			continue;
		case ' ':
		case '\t':
		case '\r':
			ctx->sptr++;
			continue;
		case '0':
			if (s[1] == 'x') {
				u32 n = 0;
				s++;
				for (;;) {
					s++;
					int x = unhex(*s);
					if (x < 0) return next_num(ctx, n, ctx->sptr, s - ctx->sptr);
					n = (n << 4) | x;
				}
			}
			if (s[1] == 'b') {
				u32 n = 0;
				s += 2;
				while ((*s == '1') || (*s == '0')) {
					n = (n << 1) | (*s - '0');
					s++;
				}
				return next_num(ctx, n, ctx->sptr, s - ctx->sptr);
			}
		case '1' ... '9': {
			u32 n = 0;
			for (;;) {
				switch (*s) {
				case '0' ... '9':
					n = (n * 10) + (*s - '0');
					break;
				default:
					return next_num(ctx, n, ctx->sptr, s - ctx->sptr);
				}
				s++;
			}
			}
		case 'a' ... 'z':
		case 'A' ... 'Z':
		case '_':
			for (;;) {
				s++;
				switch (*s) {
				case '0' ... '9':
				case 'a' ... 'z':
				case 'A' ... 'Z':
				case '_':
					break;
				default:
					return next_word(ctx, ctx->sptr, s - ctx->sptr);
				}
			}
		case '.': TOKEN(tDOT);
		case ',': TOKEN(tCOMMA);
		case ':': TOKEN(tCOLON);
		case ';': TOKEN(tSEMI);
		case '[': TOKEN(tOBRACK);
		case ']': TOKEN(tCBRACK);
		case '{': TOKEN(tOBRACE);
		case '}': TOKEN(tCBRACE);
		case '(': TOKEN(tOPAREN);
		case ')': TOKEN(tCPAREN);
		case '+': if (s[1] == '+') TOKEN2(tINCR) else TOKEN(tPLUS);
		case '-': if (s[1] == '-') TOKEN2(tDECR) else TOKEN(tMINUS);
		case '*': TOKEN(tSTAR);
		case '^': TOKEN(tCARET);
		case '=': if (s[1] == '=') TOKEN2(tEQ) else TOKEN(tASSIGN);
		case '&': if (s[1] == '&') TOKEN2(tAND) else TOKEN(tAMP);
		case '|': if (s[1] == '|') TOKEN2(tOR) else TOKEN(tPIPE);
		case '>': if (s[1] == '=') TOKEN2(tGE) else TOKEN(tGT);
		case '<': if (s[1] == '=') TOKEN2(tLE) else TOKEN(tLT);
		case '!': if (s[1] == '=') TOKEN2(tNE) else TOKEN(tBANG);
		case '/':
			if (s[1] == '/') {
				while ((*s != '\n') && (*s != 0)) s++;
				ctx->sptr = s;
				continue;
			} else {
				TOKEN(tSLASH);
			}
		case '"': return next_string(ctx, ctx->sptr + 1);
		default:
			error(ctx, "unknown character '%c' (0x%02x)\n",
			      ((*s > ' ') && (*s < 128)) ? *s : '.', *s);
		}
	}
}

token_t next(Ctx ctx, int misc) {
	return (ctx->tok = _next(ctx, misc));
}

void printstr(const char* s) {
	unsigned ch;
	printf("\"");
	while ((ch = *s++) != 0) {
		if ((ch < ' ') || (ch > '~')) {
			switch (ch) {
			case 9: printf("\\t"); break;
			case 10: printf("\\n"); break;
			default: printf("\\x%02x", ch); break;
			}
		} else {
			switch (ch) {
			case '"': printf("\\\""); break;
			case '\\': printf("\\\\"); break;
			default: printf("%c", ch); break;
			}
		}
	}
	printf("\"");
}

void print(Ctx ctx) {
	switch (ctx->tok) {
	case tNUMBER: printf("#%u ", ctx->num); break;
	case tNAME:   printf("@%s ", ctx->tmp); break;
	case tEOL:    printf("\n"); break;
	case tSTRING: printstr(ctx->tmp); break;
	default:      printf("%s ", tnames[ctx->tok]); break;
	}
}

void expected(Ctx ctx, const char* what) {
	error(ctx, "expected %s, found %s", what, tnames[ctx->tok]);
}

void expect(Ctx ctx, token_t tok) {
	if (ctx->tok != tok) {
		error(ctx, "expected %s, found %s", tnames[tok], tnames[ctx->tok]);
	}
}

void require(Ctx ctx, token_t tok) {
	expect(ctx, tok);
	next(ctx, 0);
}

String parse_name(Ctx ctx, const char* what) {
	if (ctx->tok != tNAME) {
		error(ctx, "expected %s, found %s", what, tnames[ctx->tok]);
	}
	String str = mkstring(ctx, ctx->tmp, strlen(ctx->tmp));
	next(ctx, 0);
	return str;
}

Type parse_type(Ctx ctx) {
	String tname = parse_name(ctx, "type name");
	for (Type type = ctx->typetab; type != NULL; type = type->next) {
		if (type->name == tname) {
			return type;
		}
	}
	error(ctx, "unknown type name '%s'", tname->text);
	return NULL;
}

void parse_function_body(Ctx ctx) {
	error(ctx, "unsupported");
}

void parse_function(Ctx ctx) {
	SymbolRec param[FNMAXARGS];
	unsigned n = 0;
	String fname = parse_name(ctx, "funcion name");
	Type ftype = ctx->type_void;

	require(ctx, tOPAREN);

	// process parameters
	if (ctx->tok != tCPAREN) {
		for (;;) {
			if (n == FNMAXARGS) {
				error(ctx, "too many parameters (%d)", FNMAXARGS);
			}

			String name = parse_name(ctx, "parameter name");
			Type type = parse_type(ctx);

			for (unsigned i = 0; i < n; i++) {
				if (param[i].name == name) {
					error(ctx, "duplicate parameter name '%s'", name->text);
				}
			}

			param[n].name = name;
			param[n].type = type;
			param[n].flags = SF_FRAMEREL;
			param[n].posn = -4 * (n + 1);
			param[n].regno = 0;
			param[n].next = NULL;
			n++;

			if (ctx->tok != tCOMMA) {
				break;
			}
			next(ctx, 0);
		}
	}

	require(ctx, tCPAREN);

	if ((ctx->tok != tSEMI) && (ctx->tok != tOBRACE)) {
		ftype = parse_type(ctx);
	}

	int isdef = 0;
	if (ctx->tok == tSEMI) {
		// declaration
		next(ctx, 0);
	} else if (ctx->tok == tOBRACE) {
		// definition
		next(ctx, 0);
		isdef = 1;
	} else {
		expected(ctx, "semi or open brace");
	}

	// Look for an existing declaration or definintion of this function
	// and if it exists, ensure that we are in argeement with it
	Symbol sym;
	for (sym = ctx->symtab; sym != NULL; sym = sym->next) {
		if (sym->name == fname) {
			if (!(sym->flags & SF_FUNC)) {
				error(ctx, "redefining variable as function '%s'", fname->text);
			}
			if (!isdef) {
				error(ctx, "redeclared function '%s'", fname->text);
			}
			if (sym->flags & SF_DEFINED) {
				error(ctx, "redefined function '%s'", fname->text);
			}
			int bad = 0;
			if (n != sym->func->pcount) {
				bad = 1;
			} else if (ftype != sym->func->type) {
				bad = 1;
			} else {
				for (unsigned i = 0; i < n; i++) {
					if (sym->func->param[i].type != param[i].type) {
						bad = 1;
						break;
					}
				}
			}
			if (bad) {
				error(ctx, "function declaration/definition mismatch for '%s'", fname->text);
			}
			break;
		}
	}

	// if there was no existing record of this function, create one now
	if (sym == NULL) {
		Func func = malloc(sizeof(FuncRec) + sizeof(SymbolRec) * n);
		func->scope.next = NULL;
		func->scope.first = NULL;
		func->type = ftype;
		func->pcount = n;
		memcpy(func->param, param, sizeof(SymbolRec) * n);
		
		sym = malloc(sizeof(SymbolRec));
		sym->name = fname;
		sym->type = NULL;
		sym->flags = SF_FUNC;
		sym->posn = 0;
		sym->regno = 0;
		sym->func = func;

		sym->next = ctx->symtab;
		ctx->symtab = sym;
	}

	// handle definition if it is one
	if (isdef) {
		sym->flags |= SF_DEFINED;
		parse_function_body(ctx);
	}
}

void parse_global_var(Ctx ctx) {
	error(ctx, "unsupported");
}

void parse_program(Ctx ctx) {
	next(ctx, 0);
	for (;;) {
		switch (ctx->tok) {
		case tFUNC:
			next(ctx, 0);
			parse_function(ctx);
			break;
		case tVAR:
			next(ctx, 0);
			parse_global_var(ctx);
			break;
		case tEOF:
			break;
		default:
			expected(ctx, "func or var");
		}
	}
}

int main(int argc, char **argv) {
	const char *outname = "out.hex";

	CtxRec ctx;
	init_ctx(&ctx);
	
	if (argc < 2) {
		ctx.filename = "<commandline>";
		error(&ctx, "no file specified");
	}

	ctx.filename = argv[1];
	if (argc == 3)
		outname = argv[2];

	load(&ctx, argv[1]);
	ctx.line = ctx.sptr;
	ctx.linenumber = 1;

#if 0
	do {
		next(&ctx, 1);
		print(&ctx);
	} while (ctx.tok != tEOF);
	printf("\n");
#else
	parse_program(&ctx);
#endif

	return 0;
}
