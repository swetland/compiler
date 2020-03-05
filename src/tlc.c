// Copyright 2020, Brian Swetland <swetland@frotz.net>
// Licensed under the Apache License, Version 2.0.

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdbool.h>
#include <strings.h>
#include <string.h>

#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#define FNMAXARGS 8

#define nil 0

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
	tWHILE, tFOR, tBREAK, tCONTINUE, tSWITCH, tCASE,
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
typedef struct ObjectRec* Object;
typedef struct TypeRec* Type;
typedef struct ItemRec* Item;
typedef struct CtxRec* Ctx;

typedef struct StringRec StringRec;
typedef struct ObjectRec ObjectRec;
typedef struct TypeRec TypeRec;
typedef struct ItemRec ItemRec;
typedef struct CtxRec CtxRec;

struct StringRec {
	String next;
	u32 len;
	char text[0];
};

// ------------------------------------------------------------------

struct ObjectRec {
	u32 kind;
	u32 flags;
	u32 value;
	Object next;  // link in list
	Object first; // list of...
	Type type;
	String name;
};

// Object Kind IDs
enum {
	oConst,
	oVar,
	oParam,
	oField,
	oType,
};

// Object Flags
#define ofReadOnly 1
#define ofPublic   2 
#define ofDefined  4

// ------------------------------------------------------------------

struct TypeRec {
	u32 kind;
	Object obj;   // if we're non-anonymous
	Object first; // list of Params or Fields
	Type base;    // Pointer-to, Func-return, or Array-elem
	u32 len;      // of Array
	u32 size;     // of Type in Memory
};

// Type Kind IDs
enum {
	tVoid,
	tByte,
	tBool,
	tInt32,
	tNil,
	tString,
	tPointer,
	tArray,
	rRecord,
	tFunc,
};

// ------------------------------------------------------------------

struct ItemRec {
	u32 kind;
	u32 flags;
	Type type;
	u32 r;
	u32 a;
	u32 b;
};

// Item Kind IDs
enum {           // r       a         b
	iConst,  // -       value     -
	iVar,    // base    offset
	iParam,  // -       offset0   offset1
	iReg,    // regno
	iRegInd, // regno   offset
	iCond,   // ccode   f-chain   t-chain
};

// Item Flags
#define ifReadOnly 1

// ------------------------------------------------------------------

struct CtxRec {
	const char* source;    // entire source file
	const char* sptr;      // tokenizer source pointer
	const char* line;      // start of most recent line
	const char* filename;  // filename of active source
	u32 linenumber;        // line number of most recent line
	u32 flags;

	token_t tok;           // most recent token
	u32 num;
	char tmp[256];         // used for tNAME, tTYPE, tNUMBER, tSTRING;

	String strtab;         // TODO: hashtable
	Object typetab;        // TODO: hashtable
	Object symtab;         // TODO: hashtable, globals, functions
	Object scope;

	Type type_void;
	Type type_byte;
	Type type_bool;
	Type type_int32;
	Type type_nil;
	Type type_string;
};


String mkstring(Ctx ctx, const char* text, u32 len) {
	String str = ctx->strtab;
	while (str != nil) {
		if ((str->len == len) && (memcmp(text, str->text, len) == 0)) {
			return str;
		}
		str = str->next;
	}

	str = malloc(sizeof(StringRec) + len + 1);
	str->len = len;
	memcpy(str->text, text, len);
	str->text[len] = 0;
	str->next = ctx->strtab;
	ctx->strtab = str;

	return str;
}

Type mktype(Ctx ctx, const char* text, u32 len, u32 kind, u32 size) {
	String str = mkstring(ctx, text, len);
	Type type = malloc(sizeof(TypeRec));
	Object obj = malloc(sizeof(ObjectRec));

	type->kind = kind;
	type->obj = obj;
	type->first = nil;
	type->base = nil;
	type->len = 0;
	type->size = size;

	obj->kind = oType;
	obj->flags = 0;
	obj->value = 0;
	obj->next = nil;
	obj->first = nil;
	obj->type = type;
	obj->name = str;

	obj->next = ctx->typetab;
	ctx->typetab = obj;

	return type;
}

void init_ctx(Ctx ctx) {
	memset(ctx, 0, sizeof(CtxRec));

	// install built-in basic types
	ctx->type_void    = mktype(ctx, "void", 4, tVoid, 0);
	ctx->type_byte    = mktype(ctx, "byte", 4, tByte, 1);
	ctx->type_bool    = mktype(ctx, "bool", 4, tBool, 1);
	ctx->type_int32   = mktype(ctx, "i32",  3, tInt32, 4);
	ctx->type_nil     = mktype(ctx, "nil",  3, tNil, 4);
	ctx->type_string  = mktype(ctx, "str",  3, tString, 8);
}

bool sametype(Type a, Type b) {
	if (a->kind != b->kind) {
		return false;
	}
	if (a->base != b->base) {
		return false;
	}
	if (a->len != b->len) {
		return false;
	}
	Object a1 = a->first;
	Object b1 = b->first;
	while ((a1 != nil) && (b1 != nil)) {
		// check that parameters and fields match
		if (!sametype(a1->type, b1->type)) {
			return false;
		}
	}
	if ((a1 != nil) || (b1 != nil)) {
		// mismatched number of parameters or fields
		return false;
	}
	return true;
}

void error(Ctx ctx, const char *fmt, ...) {
	va_list ap;

	u32 len = 0;
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

int unhex(u32 ch) {
	switch (ch) {
	case '0' ... '9': return ch - '0';
	case 'a' ... 'f': return ch - 'a' + 10;
	case 'A' ... 'F': return ch - 'A' + 10;
	default: return -1;
	}
}

token_t next_string(Ctx ctx, const char* s) {
	u32 ch, len = 0;
	while (true) {
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

int streq(const char* s1, u32 l1, const char* s2, u32 l2) {
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
	case 8:
		if (streq(str, len, "continue", 8)) return ctx->tok = tCONTINUE;
		break;
	}
	return ctx->tok = tNAME;
}

#define TOKEN(t) { ctx->sptr++; return ctx->tok = t; }
#define TOKEN2(t) { ctx->sptr+=2; return ctx->tok = t; }

token_t _next(Ctx ctx) {
	while (true) {
		const char* s = ctx->sptr;

		switch (*s) {
		case 0:
			return ctx->tok = tEOF;
		case '\n':
			ctx->linenumber++;
			ctx->sptr++;
			ctx->line = ctx->sptr;
			if (ctx->flags & 1) return ctx->tok = tEOL;
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

token_t next(Ctx ctx) {
	return (ctx->tok = _next(ctx));
}

void printstr(const char* s) {
	u32 ch;
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
	next(ctx);
}

String parse_name(Ctx ctx, const char* what) {
	if (ctx->tok != tNAME) {
		error(ctx, "expected %s, found %s", what, tnames[ctx->tok]);
	}
	String str = mkstring(ctx, ctx->tmp, strlen(ctx->tmp));
	next(ctx);
	return str;
}

Type parse_type(Ctx ctx) {
	String tname = parse_name(ctx, "type name");
	Object obj = ctx->typetab;
	while (obj != nil) {
		if (obj->name == tname) {
			return obj->type;
		}
		obj = obj->next;
	}
	error(ctx, "unknown type name '%s'", tname->text);
	return nil;
}

void parse_function_body(Ctx ctx) {
	error(ctx, "unsupported");
}

Object parse_param(Ctx ctx, String fname, u32 n, Object first, Object last) {
	if (n == FNMAXARGS) {
		error(ctx, "too many parameters (%d) for '%s'", FNMAXARGS, fname->text);
	}
	Object param = malloc(sizeof(ObjectRec));
	param->kind = oParam;
	param->flags = 0;
	param->value = n;
	param->next = nil;
	param->first = nil;
	param->name = parse_name(ctx, "parameter name");
	param->type = parse_type(ctx);

	Object obj = first;
	while (obj != nil) {
		if (obj->name == param->name) {
			error(ctx, "duplicate parameter name '%s'", fname->text);
		}
		obj = obj->next;
	}

	if (last != nil) {
		last->next = param;
	}
	return param;
}

void parse_function(Ctx ctx) {
	Object first = nil;
	Object last = nil;
	u32 n = 0;
	String fname = parse_name(ctx, "funcion name");
	Type ftype = ctx->type_void;

	require(ctx, tOPAREN);

	// process parameters
	if (ctx->tok != tCPAREN) {
		first = parse_param(ctx, fname, n, nil, nil);
		last = first;
		n++;
		while (ctx->tok == tCOMMA) {
			next(ctx);
			last = parse_param(ctx, fname, n, first, last);
			n++;
		}
	}

	require(ctx, tCPAREN);

	if ((ctx->tok != tSEMI) && (ctx->tok != tOBRACE)) {
		ftype = parse_type(ctx);
	}

	int isdef = 0;
	if (ctx->tok == tSEMI) {
		// declaration
		next(ctx);
	} else if (ctx->tok == tOBRACE) {
		// definition
		next(ctx);
		isdef = 1;
	} else {
		expected(ctx, "semi or open brace");
	}

	// Look for an existing declaration or definintion of this function
	// and if it exists, ensure that we are in argeement with it
	Object obj = ctx->symtab;
	while (obj != nil) {
		if (obj->name == fname) {
			if (obj->type->kind != tFunc) {
				error(ctx, "redefining '%s' as function", fname->text);
			}
			if (!isdef) {
				error(ctx, "redeclared function '%s'", fname->text);
			}
			if (obj->flags & ofDefined) {
				error(ctx, "redefined function '%s'", fname->text);
			}
			if (ftype != obj->type->base) {
				error(ctx, "function definition mismatch for '%s' (return type)", fname->text);
			}
			Object pa = first;
			Object pb = obj->type->first;
			u32 i = 1;
			while ((pa != nil) && (pb != nil)) {
				if (!sametype(pa->type, pb->type)) {
					error(ctx, "function definition mismatch for '%s' (parameter #%u)", fname->text, i);
				}
				pa = pa->next;
				pb = pb->next;
			}
			if ((pa != nil) || (pb != nil)) {
				error(ctx, "function definition mismatch for '%s' (parameter count mismatch)", fname->text);
			}
			break;
		}
		obj = obj->next;
	}

	// if there was no existing record of this function, create one now
	if (obj == nil) {
		Type type = malloc(sizeof(TypeRec));
		obj = malloc(sizeof(ObjectRec));

		type->kind = tFunc;
		type->obj = obj;
		type->first = first;
		type->base = ftype;
		type->len = 0;
		type->size = 0;

		obj->kind = oType; //??
		obj->flags = 0;
		obj->value = 0;
		obj->next = nil;
		obj->first = first;
		obj->type = type;
		obj->name = fname;

		obj->next = ctx->symtab;
		ctx->symtab = obj;
	}

	// handle definition if it is one
	if (isdef) {
		obj->flags |= ofDefined;
		parse_function_body(ctx);
	}
}

void parse_global_var(Ctx ctx) {
	error(ctx, "unsupported");
}

void parse_program(Ctx ctx) {
	next(ctx);
	for (;;) {
		switch (ctx->tok) {
		case tFUNC:
			next(ctx);
			parse_function(ctx);
			break;
		case tVAR:
			next(ctx);
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
	ctx->flags |= 1;
	do {
		next(&ctx);
		print(&ctx);
	} while (ctx.tok != tEOF);
	printf("\n");
#else
	parse_program(&ctx);
#endif

	return 0;
}
