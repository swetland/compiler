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
typedef uint8_t u8;

// token classes (tok >> 8)
enum {
	tcNONE = 0, tcRELOP = 1, tcADDOP = 2, tcMULOP = 3,
};

typedef enum {
	tEOF = 0x000, tEOL = 0x001,
	// RelOps (do not reorder)
	tEQ = 0x102, tNE = 0x103, tLT = 0x104, tLE = 0x105, tGT = 0x106, tGE = 0x107,
	// AddOps (do not reorder)
	tPLUS = 0x208, tMINUS = 0x209, tPIPE = 0x20A, tCARET = 0x20B,
	// MulOps (do not reorder)
	tSTAR = 0x30C, tSLASH = 0x30D, tPERCENT = 0x30E, tAMP = 0x30F,
	tANDNOT = 0x310, tLEFT = 0x311, tRIGHT = 0x312,
	// UnaryOps
	tNOT = 0x013,
	// LogicalOps
        tAND = 0x014, tOR = 0x015, tBANG = 0x016,
	// Brackets, Braces, Parens
	tOBRACK = 0x017, tCBRACK = 0x018, tOPAREN = 0x019, tCPAREN = 0x01A,
	tOBRACE = 0x01B, tCBRACE = 0x01C,
	// Various Punctuation
	tSEMI = 0x01D, tCOLON = 0x1E, tDOT = 0x01F, tCOMMA = 0x020,
	tINC = 0x021, tDEC = 0x022, tASSIGN = 0x023,
	// Keywords
	tVAR = 0x024, tSTRUCT = 0x025, tFUNC = 0x026, tRETURN = 0x027,
	tIF = 0x028, tELSE = 0x029, tWHILE = 0x02A, tFOR = 0x02B,
	tBREAK = 0x02C, tCONTINUE = 0x02D, tSWITCH = 0x02E, tCASE = 0x02F,
	// Special Constants
	tTRUE = 0x030, tFALSE = 0x031, tNIL = 0x032,
	// Idenitfiers, Numbers, Strings
	tNAME = 0x033, tNUMBER = 0x034, tSTRING = 0x035,
} token_t;

char *tnames[] = {
	"<EOF>", "<EOL>", 
	"==", "!=", "<", "<=", ">", ">=",
	"+", "-", "|", "^",
	"*", "/", "%", "&",
	"&~", "<<", ">>",
	"~",
	"&&", "||", "!",
	"[", "]", "(", ")",
	"{", "}",
	";", ":", ".", ",",
	"++", "--", "=",
	"var", "struct", "func", "return",
	"if", "else", "while", "for",
	"break", "continue", "switch", "case",
	"true", "false", "nil",
	"<NAME>", "<NUMBER>", "<STRING>",
};

// encodings for ops in Items
enum { rEQ, rNE, rLT, rLE, rGT, rGE }; // RelOps
enum { aADD, aSUB, aIOR, aXOR }; // AddOps
enum { mMUL, mDIV, mMOD, mAND, mANN, mLSL, mLSR }; // MulOps

u8 invert_relop_tab[6] = { rNE, rEQ, rGE, rGT, rLE, rLT };

typedef struct StringRec* String;
typedef struct ObjectRec* Object;
typedef struct ScopeRec* Scope;
typedef struct FixupRec* Fixup;
typedef struct TypeRec* Type;
typedef struct ItemRec* Item;
typedef struct CtxRec* Ctx;

typedef struct StringRec StringRec;
typedef struct ObjectRec ObjectRec;
typedef struct ScopeRec ScopeRec;
typedef struct FixupRec FixupRec;
typedef struct TypeRec TypeRec;
typedef struct ItemRec ItemRec;
typedef struct CtxRec CtxRec;

struct StringRec {
	String next;
	u32 len;
	char text[0];
};

struct ScopeRec {
	u32 kind;
	Scope next;    // next in scope stack
	Object first;  // first object in this scope
	u32 level;     // height in stack (0 == globals, ...)
	Fixup fixups;
};

struct FixupRec {
	Fixup next;
	u32 pc;
};

// Scope Kind IDs
enum {
	sBlock,
	sFunc,
	sLoop,
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
	Fixup fixups; // forward func refs
};

// Object Kind IDs
enum {           // value
	oConst,  // const value
	oGlobal, // global offset
	oVar,    // frame offset
	oParam,  // param slot
	oField,  // record offset
	oType,   // type-desc-ptr
	oFunc,   // address
	oScope,  // scope depth
};

// Object Flags
#define ofReadOnly 1
#define ofPublic   2 
#define ofDefined  4
#define ofBuiltin  8 // for builtin functions

// ------------------------------------------------------------------

struct TypeRec {
	u32 kind;
	Object obj;   // if we're non-anonymous
	Object first; // list of Params or Fields
	Type base;    // Pointer-to, Func-return, or Array-elem
	u32 len;      // of Array, num of Params
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
	iComp,   // relop   regno-a   regno-b
	iFunc,
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
	Scope scope;           // scope stack
	ScopeRec global;
	Object fn;             // function being compiled if non-nil

	Type type_void;
	Type type_byte;
	Type type_bool;
	Type type_int32;
	Type type_nil;
	Type type_string;

	u32 code[8192];
	u32 pc;

	u32 regbits;

	u32 xref[8192];
};

#define cfVisibleEOL 1
#define cfAbortOnError 2

CtxRec ctx;


// generate function prologue and epilogue
void gen_prologue(Object fn);
void gen_epilogue(Object fn);

// return from function, consumes x
void gen_return(Item x);

// generate a binary op, consumes y, transforms x
void gen_add_op(u32 op, Item x, Item y);
void gen_mul_op(u32 op, Item x, Item y);
void gen_rel_op(u32 op, Item x, Item y);

// generate unary op, transforms x
void gen_unary_op(u32 op, Item x);

// stores val into var, consuming both
void gen_store(Item val, Item var);

// sets up call param #n, consuming val
void gen_param(u32 n, Item val);

// call func, consumes parameters and func
void gen_call(Item func);

// generate a forward conditional branch
// consumes x which must be Bool or Cond
// returns address to fixup
u32 gen_branch_cond(Item x, bool sense);

// generate a backward branch to addr
void gen_branch_back(u32 addr);

// generate an unconditional forward branch
// returns address for later fixup
u32 gen_branch_fwd();

// patches provided list of branch fixups to branch to
// the address where we will emit the next instruction
void fixup_branches_fwd(Fixup list);

// patches a single branch at addr to branch to the
// address where we will emit the next instruction
void fixup_branch_fwd(u32 addr);

String mkstring(const char* text, u32 len) {
	String str = ctx.strtab;
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
	str->next = ctx.strtab;
	ctx.strtab = str;

	return str;
}

Type make_type(const char* text, u32 len, u32 kind, u32 size) {
	String str = mkstring(text, len);
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

	obj->next = ctx.typetab;
	ctx.typetab = obj;

	return type;
}

enum {
	biPrintHex32,
};

void make_builtin(const char* name, u32 id, Type p0, Type p1, Type rtn);

void init_ctx() {
	memset(&ctx, 0, sizeof(ctx));

	// install built-in basic types
	ctx.type_void    = make_type("void", 4, tVoid, 0);
	ctx.type_byte    = make_type("byte", 4, tByte, 1);
	ctx.type_bool    = make_type("bool", 4, tBool, 1);
	ctx.type_int32   = make_type("i32",  3, tInt32, 4);
	ctx.type_nil     = make_type("nil",  3, tNil, 4);
	ctx.type_string  = make_type("str",  3, tString, 8);

	ctx.scope = &(ctx.global);
	ctx.line = "";

	make_builtin("_hexout_", biPrintHex32, ctx.type_int32, nil, ctx.type_void);
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

void error(const char *fmt, ...) {
	va_list ap;

	u32 len = 0;
	const char *s = ctx.line;
	while (len < 255) {
		if ((*s < ' ') && (*s != 9)) break;
		s++;
		len++;
	}

	fprintf(stderr,"%s:%d: ", ctx.filename, ctx.linenumber);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	
	fprintf(stderr,"\n%.*s\n", len, ctx.line);
	if (ctx.flags & cfAbortOnError) {
		abort();
	} else {
		exit(1);
	}
}

void load(const char* filename) {
	ctx.filename = filename;
	ctx.linenumber = 0;

	int fd;
	struct stat s;
	char* data;

	if ((fd = open(filename, O_RDONLY)) < 0)
		error("cannot open file");
	if (fstat(fd, &s) < 0)
		error("cannot stat file");
	if ((data = malloc(s.st_size + 1)) == NULL)
		error("cannot allocate memory");
	if (read(fd, data, s.st_size) != s.st_size)
		error("cannot read file");
	close(fd);
	data[s.st_size] = 0;

	ctx.source = data;
	ctx.sptr = data;
	ctx.linenumber = 1;
}

int unhex(u32 ch) {
	switch (ch) {
	case '0' ... '9': return ch - '0';
	case 'a' ... 'f': return ch - 'a' + 10;
	case 'A' ... 'F': return ch - 'A' + 10;
	default: return -1;
	}
}

token_t next_string(const char* s) {
	u32 ch, len = 0;
	while (true) {
		switch ((ch = *s++)) {
		case 0: error("unterminated string");
		case '"': goto done;
		case '\\':
			switch ((ch = *s++)) {
			case '0': error("unterminated string");
			case 'n': ch = 10; break;
			case 't': ch = 9; break;
		  	case '"': ch = '"'; break;
			case 'x': {
				int x0 = unhex(s[0]);
				int x1 = unhex(s[1]);
				//TODO: if error() is ever non-fatal, this may leave
				//sptr past end of input
				if ((x0 < 0) || (x1 < 0)) error("invalid hex escape");
				ch = (x0 << 4) | x1;
				s += 2;
				break;
			}
			default: error("invalid string escape 0x%02x", ch);
			}
			break;
		default:
			break;
		}
		if (len == 255) error("string constant too long");
		ctx.tmp[len++] = ch;
	}
done:
	ctx.tmp[len] = 0;
	ctx.sptr = s;
	return tSTRING;
}

token_t next_num(u32 n, const char* str, size_t len) {
	if (len > 255) error("number too large");
	memcpy(ctx.tmp, str, len);
	ctx.tmp[len] = 0;
	ctx.num = n;
	ctx.sptr += len;
	return ctx.tok = tNUMBER;
}

int streq(const char* s1, u32 l1, const char* s2, u32 l2) {
	return (l1 == l2) && (!memcmp(s1, s2, l1));
}

token_t next_word(const char* str, size_t len) {
	if (len > 255) error("word too large");
	memcpy(ctx.tmp, str, len);
	ctx.tmp[len] = 0;
	ctx.num = 0;
	ctx.sptr += len;
	switch (len) {
	case 2:
		if (streq(str, len, "if", 2)) return ctx.tok = tIF;
		break;
	case 3:
		if (streq(str, len, "for", 3)) return ctx.tok = tFOR;
		if (streq(str, len, "var", 3)) return ctx.tok = tVAR;
		if (streq(str, len, "nil", 3)) return ctx.tok = tNIL;
		break;
	case 4:
		if (streq(str, len, "case", 4)) return ctx.tok = tCASE;
		if (streq(str, len, "func", 4)) return ctx.tok = tFUNC;
		if (streq(str, len, "else", 4)) return ctx.tok = tELSE;
		if (streq(str, len, "true", 4)) return ctx.tok = tTRUE;
		break;
	case 5:
		if (streq(str, len, "break", 5)) return ctx.tok = tBREAK;
		if (streq(str, len, "while", 5)) return ctx.tok = tWHILE;
		if (streq(str, len, "false", 5)) return ctx.tok = tFALSE;
		break;
	case 6:
		if (streq(str, len, "switch", 6)) return ctx.tok = tSWITCH;
		if (streq(str, len, "struct", 6)) return ctx.tok = tSTRUCT;
		if (streq(str, len, "return", 6)) return ctx.tok = tRETURN;
		break;
	case 8:
		if (streq(str, len, "continue", 8)) return ctx.tok = tCONTINUE;
		break;
	}
	return ctx.tok = tNAME;
}

#define TOKEN(t) { ctx.sptr++; return ctx.tok = t; }
#define TOKEN2(t) { ctx.sptr+=2; return ctx.tok = t; }

token_t _next() {
	while (true) {
		const char* s = ctx.sptr;

		switch (*s) {
		case 0:
			return ctx.tok = tEOF;
		case '\n':
			ctx.linenumber++;
			ctx.sptr++;
			ctx.line = ctx.sptr;
			ctx.xref[ctx.pc / 4] = ctx.linenumber;
			if (ctx.flags & cfVisibleEOL) return ctx.tok = tEOL;
			continue;
		case ' ':
		case '\t':
		case '\r':
			ctx.sptr++;
			continue;
		case '0':
			if (s[1] == 'x') {
				u32 n = 0;
				s++;
				for (;;) {
					s++;
					int x = unhex(*s);
					if (x < 0) return next_num(n, ctx.sptr, s - ctx.sptr);
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
				return next_num(n, ctx.sptr, s - ctx.sptr);
			}
		case '1' ... '9': {
			u32 n = 0;
			for (;;) {
				switch (*s) {
				case '0' ... '9':
					n = (n * 10) + (*s - '0');
					break;
				default:
					return next_num(n, ctx.sptr, s - ctx.sptr);
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
					return next_word(ctx.sptr, s - ctx.sptr);
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
		case '+': if (s[1] == '+') TOKEN2(tINC) else TOKEN(tPLUS);
		case '-': if (s[1] == '-') TOKEN2(tDEC) else TOKEN(tMINUS);
		case '*': TOKEN(tSTAR);
		case '%': TOKEN(tPERCENT);
		case '^': TOKEN(tCARET);
		case '~': TOKEN(tNOT);
		case '=': if (s[1] == '=') TOKEN2(tEQ) else TOKEN(tASSIGN);
		case '&':
			if (s[1] == '&') TOKEN2(tAND)
			if (s[1] == '~') TOKEN2(tANDNOT)
			else TOKEN(tAMP);
		case '|': if (s[1] == '|') TOKEN2(tOR) else TOKEN(tPIPE);
		case '>':
			if (s[1] == '=') TOKEN2(tGE)
			else if(s[1] == '>') TOKEN2(tRIGHT)
			else TOKEN(tGT);
		case '<':
			if (s[1] == '=') TOKEN2(tLE)
			else if (s[1] == '<') TOKEN2(tLEFT)
			else TOKEN(tLT);
		case '!': if (s[1] == '=') TOKEN2(tNE) else TOKEN(tBANG);
		case '/':
			if (s[1] == '/') {
				while ((*s != '\n') && (*s != 0)) s++;
				ctx.sptr = s;
				continue;
			} else {
				TOKEN(tSLASH);
			}
		case '"': return next_string(ctx.sptr + 1);
		default:
			error("unknown character '%c' (0x%02x)\n",
			      ((*s > ' ') && (*s < 128)) ? *s : '.', *s);
		}
	}
}

token_t next() {
	return (ctx.tok = _next());
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

void print() {
	switch (ctx.tok) {
	case tNUMBER: printf("#%u ", ctx.num); break;
	case tNAME:   printf("@%s ", ctx.tmp); break;
	case tEOL:    printf("\n"); break;
	case tSTRING: printstr(ctx.tmp); break;
	default:      printf("%s ", tnames[ctx.tok & 0x7F]); break;
	}
}

void expected(const char* what) {
	error("expected %s, found %s", what, tnames[ctx.tok & 0x7F]);
}

void expect(token_t tok) {
	if (ctx.tok != tok) {
		error("expected %s, found %s", tnames[tok], tnames[ctx.tok & 0x7F]);
	}
}

void require(token_t tok) {
	expect(tok);
	next();
}

// Look for an identifier in the scope stack
Object find(String str) {
	Scope scope = ctx.scope;
	while (scope != nil) {
		Object obj = scope->first;
		while (obj != nil) {
			if (obj->name == str) {
				return obj;
			}
			obj = obj->next;
		}
		scope = scope->next;
	}
	return nil;
}

void make_global(Object obj) {
	obj->next = ctx.global.first;
	ctx.global.first = obj;
}

// push a scope on the scope stack
// if obj is non-nil, it is the first of a list of items in that scope
Scope push_scope(u32 kind, Object obj) {
	Scope scope = malloc(sizeof(ScopeRec));
	scope->kind = kind;
	scope->next = ctx.scope;
	scope->first = obj;
	scope->level = ctx.scope->level + 1;
	scope->fixups = nil;

	ctx.scope = scope;
	return scope;
	// XXX lazy scopes
}

void pop_scope() {
	if (ctx.scope->level == 0) {
		error("cannot pop the global scope");
	}
	fixup_branches_fwd(ctx.scope->fixups);
	// XXX delete?
	ctx.scope = ctx.scope->next;
}

// find the first surrounding scope of a specified kind
Scope find_scope(u32 scope_kind) {
	Scope scope = ctx.scope;
	while (scope != nil) {
		if (scope->kind == scope_kind) {
			return scope;
		}
		scope = scope->next;
	}
	return nil;
}

// add a fixup for the last-emitted instruction
// to the scope (used for return and break)
void add_scope_fixup(Scope scope) {
	Fixup fixup = malloc(sizeof(FixupRec));
	fixup->next = scope->fixups;
	fixup->pc = ctx.pc - 4;
	scope->fixups = fixup;
}

// add a fixup for the last-emitted instruction
// to the object (used for forward func refs)
void add_object_fixup(Object obj) {
	Fixup fixup = malloc(sizeof(FixupRec));
	fixup->next = obj->fixups;
	fixup->pc = ctx.pc - 4;
	obj->fixups = fixup;
}

void setitem(Item itm, u32 kind, Type type, u32 r, u32 a, u32 b) {
	itm->kind = kind;
	itm->flags = 0;
	itm->type = type;
	itm->r = r;
	itm->a = a;
	itm->b = b;
}

u32 invert_relop(u32 op) {
	if (op > 5) { abort(); }
	return invert_relop_tab[op];
}
// ================================================================

void parse_expr(Item x);

void parse_operand(Item x) {
	if (ctx.tok == tNUMBER) {
		setitem(x, iConst, ctx.type_int32, 0, ctx.num, 0);
	} else if (ctx.tok == tSTRING) {
		error("unsupported string const");
	} else if (ctx.tok == tTRUE) {
		setitem(x, iConst, ctx.type_bool, 0, 1, 0);
	} else if (ctx.tok == tFALSE) {
		setitem(x, iConst, ctx.type_bool, 0, 0, 0);
	} else if (ctx.tok == tNIL) {
		setitem(x, iConst, ctx.type_nil, 0, 0, 0);
	} else if (ctx.tok == tOPAREN) {
		next();
		parse_expr(x);
		require(tCPAREN);
		return;
	} else if (ctx.tok == tNAME) {
		String str = mkstring(ctx.tmp, strlen(ctx.tmp));
		Object obj = find(str);
		if (obj == nil) {
			error("unknown identifier '%s'", str->text);
		}
		if (obj->kind == oParam) {
			setitem(x, iParam, obj->type, 0, obj->value, 0);
		} else if (obj->kind == oFunc) {
			setitem(x, iFunc, obj->type, 0, 0, 0);
		} else {
			error("unsupported identifier");
		}
	}
	next();
}

void parse_primary_expr(Item x) {
	parse_operand(x);
	while (true) {
		if (ctx.tok == tOPAREN) {
			next();
			if (x->kind != iFunc) {
				error("cannot call non-function");
			}
			// TODO ptr-to-func
			u32 n = 0;
			Object param = x->type->first;
			while (param != nil) {
				if (n != 0) {
					require(tCOMMA);
				}
				ItemRec y;
				parse_expr(&y);
				gen_param(n, &y);
				param = param->next;
				n++;
			}
			require(tCPAREN);
			gen_call(x);
		} else if (ctx.tok == tDOT) {
			error("unsupported field deref");
		} else if (ctx.tok == tOBRACK) {
			error("unsupported array deref");
		} else {
			break;
		}
	}
}

void parse_unary_expr(Item x) {
	if (ctx.tok == tPLUS) {
		next();
		parse_unary_expr(x);
	} else if ((ctx.tok == tMINUS) || (ctx.tok == tBANG) || (ctx.tok == tNOT)) {
		u32 op = ctx.tok;
		next();
		parse_unary_expr(x);
		if ((x->kind == iComp) && (op == tBANG)) {
			x->r = invert_relop(x->r);
		} else {
			gen_unary_op(op, x);
		}
	} else if (ctx.tok == tAMP) {
		next();
		error("deref unsupported");
	} else {
		parse_primary_expr(x);
	}
}

void parse_mul_expr(Item x) {
	parse_unary_expr(x);
	while ((ctx.tok >> 8) == tcMULOP) {
		u32 mulop = ctx.tok - tSTAR;
		next();
		ItemRec y;
		parse_unary_expr(&y);
		gen_mul_op(mulop, x, &y);
	}
}

void parse_add_expr(Item x) {
	parse_mul_expr(x);
	while ((ctx.tok >> 8) == tcADDOP) {
		u32 addop = ctx.tok - tPLUS;
		next();
		ItemRec y;
		parse_mul_expr(&y);
		gen_add_op(addop, x, &y);
	}
}

void parse_rel_expr(Item x) {
	parse_add_expr(x);
	if ((ctx.tok >> 8) == tcRELOP) {
		u32 relop = ctx.tok - tEQ;
		next();
		ItemRec y;
		parse_add_expr(&y);
		gen_rel_op(relop, x, &y);
	}
}

void parse_and_expr(Item x) {
	parse_rel_expr(x);
	while (ctx.tok == tAND) {
		next();
		ItemRec y;
		parse_rel_expr(&y);
		error("unsupported and op");
	}
}

void parse_expr(Item x) {
	parse_and_expr(x);
	while (ctx.tok == tOR) {
		next();
		ItemRec y;
		parse_and_expr(&y);
		error("unsupported or op");
	}
}

String parse_name(const char* what) {
	if (ctx.tok != tNAME) {
		error("expected %s, found %s", what, tnames[ctx.tok & 0x7F]);
	}
	String str = mkstring(ctx.tmp, strlen(ctx.tmp));
	next();
	return str;
}

Type parse_type() {
	String tname = parse_name("type name");
	Object obj = ctx.typetab;
	while (obj != nil) {
		if (obj->name == tname) {
			return obj->type;
		}
		obj = obj->next;
	}
	error("unknown type name '%s'", tname->text);
	return nil;
}

void parse_block();

void parse_while() {
	ItemRec x;
	u32 l0_loop = ctx.pc; // for backward branch

	parse_expr(&x);
	u32 l1_br_false = gen_branch_cond(&x, false);

	require(tOBRACE);
	push_scope(sLoop, nil);
	parse_block();
	gen_branch_back(l0_loop);
	pop_scope();

	fixup_branch_fwd(l1_br_false);
}

void parse_if() {
	Scope outer = push_scope(sBlock, nil);

	ItemRec x;
	parse_expr(&x);
	// branch over "if" code
	u32 l0_br_false = gen_branch_cond(&x, false);

	// generate "if" code
	require(tOBRACE);
	push_scope(sBlock, nil);
	parse_block();
	pop_scope();

	// branch past "else" code
	gen_branch_fwd();
	add_scope_fixup(outer);

	while (ctx.tok == tELSE) {
		next();
		fixup_branch_fwd(l0_br_false);
		if (ctx.tok == tIF) {
			next();
			parse_expr(&x);
			// branch over "if" code
			l0_br_false = gen_branch_cond(&x, false);

			// generate "if else" code
			require(tOBRACE);
			push_scope(sBlock, nil);
			parse_block();
			pop_scope();

			// branch past "else" code
			gen_branch_fwd();
			add_scope_fixup(outer);
		} else {
			// generate "else" code
			require(tOBRACE);
			push_scope(sBlock, nil);
			parse_block();
			pop_scope();

			// close outer scope
			pop_scope();
			return; // no further fixups needed
		}
	}
	fixup_branch_fwd(l0_br_false);

	// close outer scope
	pop_scope();
}

void parse_return() {
	ItemRec x;
	if (ctx.tok == tSEMI) {
		if (ctx.fn->type->base != ctx.type_void) {
			error("function requires return type");
		}
		next();
		x.type = ctx.type_void;
	} else {
		parse_expr(&x);
		if (!sametype(ctx.fn->type->base, x.type)) {
			error("return types do not match");
		}
		require(tSEMI);
	}
	gen_return(&x);
}

void parse_break() {
	// XXX break-to-labeled-loop support
	require(tSEMI);
	gen_branch_fwd();
	Scope scope = find_scope(sLoop);
	if (scope == nil) {
		error("break must be used from inside a looping construct");
	}
	add_scope_fixup(scope);
}

void parse_block() {
	while (true) {
		if (ctx.tok == tCBRACE) {
			next();
			break;
		} else if (ctx.tok == tRETURN) {
			next();
			parse_return();
		} else if (ctx.tok == tBREAK) {
			next();
			parse_break();
		} else if (ctx.tok == tWHILE) {
			next();
			parse_while();
		} else if (ctx.tok == tIF) {
			next();
			parse_if();
		} else if (ctx.tok == tSEMI) {
			next();
			// empty statement
		} else {
			ItemRec x;
			parse_expr(&x);
			if (ctx.tok == tASSIGN) {
				next();
				ItemRec y;
				parse_expr(&y);
				gen_store(&y, &x);
			} else if ((ctx.tok == tINC) || (ctx.tok == tDEC)) {
				ItemRec y;
				setitem(&y, iConst, ctx.type_int32, 0, 1, 0);
				next();
			}
			require(tSEMI);
		}
	}
}

void parse_function_body(Object fn) {
	ctx.fn = fn;
	gen_prologue(fn);
	push_scope(sFunc, fn->first); // scope for parameters
	push_scope(sBlock, nil);      // top scope for function body
	parse_block();
	pop_scope();
	pop_scope();
	gen_epilogue(fn);
}


Object parse_param(String fname, u32 n, Object first, Object last) {
	if (n == FNMAXARGS) {
		error("too many parameters (%d) for '%s'", FNMAXARGS, fname->text);
	}
	Object param = malloc(sizeof(ObjectRec));
	param->kind = oParam;
	param->flags = 0;
	param->value = n;
	param->next = nil;
	param->first = nil;
	param->name = parse_name("parameter name");
	param->type = parse_type();
	param->fixups = nil;

	Object obj = first;
	while (obj != nil) {
		if (obj->name == param->name) {
			error("duplicate parameter name '%s'", fname->text);
		}
		obj = obj->next;
	}

	if (last != nil) {
		last->next = param;
	}
	return param;
}

void make_builtin(const char* name, u32 id, Type p0, Type p1, Type rtn) {
	Type type = malloc(sizeof(TypeRec));
	Object obj = malloc(sizeof(ObjectRec));

	type->kind = tFunc;
	type->obj = obj;
	type->first = nil;
	type->base = rtn;
	type->len = 0;
	type->size = 0;

	obj->kind = oFunc;
	obj->flags = ofBuiltin;
	obj->value = id;
	obj->next = nil;
	obj->first = nil;
	obj->type = type;
	obj->name = mkstring(name, strlen(name));
	obj->fixups = nil;

	if (p0 != nil) {
		Object param = malloc(sizeof(ObjectRec));
		obj->first = param;
		type->first = param;
		param->kind = oParam;
		param->flags = 0;
		param->value = 0;
		param->next = nil;
		param->first = nil;
		param->name = mkstring("a", 1);
		param->type = p0;
		param->fixups = nil;
		type->len = 1;
		if (p1 != nil) {
			param->next = malloc(sizeof(ObjectRec));
			param = param->next;
			param->kind = oParam;
			param->flags = 0;
			param->value = 1;
			param->next = nil;
			param->first = nil;
			param->name = mkstring("b", 1);
			param->type = p1;
			param->fixups = nil;
			type->len = 2;
		}
	}
	make_global(obj);
}

void parse_function() {
	Object first = nil;
	Object last = nil;
	u32 n = 0;
	String fname = parse_name("funcion name");
	Type ftype = ctx.type_void;

	require(tOPAREN);

	// process parameters
	if (ctx.tok != tCPAREN) {
		first = parse_param(fname, n, nil, nil);
		last = first;
		n++;
		while (ctx.tok == tCOMMA) {
			next();
			last = parse_param(fname, n, first, last);
			n++;
		}
	}
	require(tCPAREN);

	if ((ctx.tok != tSEMI) && (ctx.tok != tOBRACE)) {
		ftype = parse_type();
	}

	int isdef = 0;
	if (ctx.tok == tSEMI) {
		// declaration
		next();
	} else if (ctx.tok == tOBRACE) {
		// definition
		next();
		isdef = 1;
	} else {
		expected("semi or open brace");
	}

	// Look for an existing declaration or definintion of this function
	Object obj = find(fname);
	if (obj != nil) {
		// such a named identifier exists
		// check to see if we are in agreement with it
		if (obj->kind != oFunc) {
			error("redefining '%s' as function", fname->text);
		}
		if (isdef && (obj->flags & ofDefined)) {
			error("redefined function '%s'", fname->text);
		}
		if (ftype != obj->type->base) {
			error("func '%s' return type differs from decl", fname->text);
		}
		if (obj->type->len != n) {
			error("func '%s' parameter count differs from decl", fname->text);
		}
		Object pa = first;
		Object pb = obj->type->first;
		u32 i = 1;
		while ((pa != nil) && (pb != nil)) {
			if (!sametype(pa->type, pb->type)) {
				error("func '%s' param %u differs from decl", fname->text, i);
			}
			pa = pa->next;
			pb = pb->next;
		}
	} else {
		// if there was no existing record of this function, create one now
		Type type = malloc(sizeof(TypeRec));
		obj = malloc(sizeof(ObjectRec));

		type->kind = tFunc;
		type->obj = obj;
		type->first = first;
		type->base = ftype;
		type->len = n;
		type->size = 0;

		obj->kind = oFunc;
		obj->flags = 0;
		obj->value = 0;
		obj->next = nil;
		obj->first = first;
		obj->type = type;
		obj->name = fname;
		obj->fixups = nil;

		make_global(obj);
	}

	// handle definition if it is one
	if (isdef) {
		// patch any forward references
		fixup_branches_fwd(obj->fixups);

		// mark as defined and save entry address
		obj->flags |= ofDefined;
		obj->value = ctx.pc;
		parse_function_body(obj);
	}
}

void parse_global_var() {
	error("unsupported");
}

void parse_program() {
	next();
	for (;;) {
		switch (ctx.tok) {
		case tFUNC:
			next();
			parse_function();
			break;
		case tVAR:
			next();
			parse_global_var();
			break;
		case tEOF:
			return;
		default:
			expected("func or var");
		}
	}
}

// ================================================================
u32 get_reg_tmp() {
	u32 n = 8;
	while (n < 12) {
		if (!(ctx.regbits & (1 << n))) {
			ctx.regbits |= (1 << n);
			//printf("GET REG %u\n", n);
			return n;
		}
		n++;
	}
	error("cannot allocate register");
	return 0;
}

void put_reg(u32 r) {
	//printf("PUT REG %u\n", r);
	if (r < 8) {
		// currently we don't strictly track r0..r7
		// they are used for function calls and returns
		return;
	}
	if (!(ctx.regbits & (1 << r))) {
		error("freeing non-allocated register %u\n", r);
	}
	ctx.regbits = ctx.regbits & (~(1 << r));
}

void emit(u32 ins) {
	ctx.code[ctx.pc / 4] = ins;
	ctx.pc = ctx.pc + 4;
}

enum {
	R0 = 0, R1 = 1, R2 = 2, R3 = 3, R4 = 4, R5 = 5, R6 = 6, R7 = 7,
	R8 = 9, R9 = 9, R10 = 10, R11 = 11, MT = 12, SB = 13, SP = 14, LR = 15,
	TMP = 16
};
enum {
	MOV = 0x0000, LSL = 0x0001, ASR = 0x0002, ROR = 0x0003,
	AND = 0x0004, ANN = 0x0005, IOR = 0x0006, XOR = 0x0007,
	ADD = 0x0008, SUB = 0x0009, MUL = 0x000A, DIV = 0x000B,
	FAD = 0x000C, FSB = 0x000D, FML = 0x000E, FDV = 0x000F,
	ADC = 0x2008, SBC = 0x2009, UML = 0x200A,
	MHI = 0x2000,
	MOV_H = 0x2000,
	MOV_CC = 0x3000,
	MOD = 0x001B, // fake op for plumbing (DIV+MOV_H)
};

void emit_op(u32 op, u32 a, u32 b, u32 c) {
	emit((op << 16) | (a << 24) | (b << 20) | c);
}
void emit_opi(u32 op, u32 a, u32 b, u32 n) {
	emit(((0x4000 | op) << 16) | (a << 24) | (b << 20) | (n & 0xffff));
}

// mov (load immediate) using the appropriate one or two
// instruction form for the immediate argument
void emit_mov(u32 a, u32 n) {
	u32 m = n >> 16;
	if (m == 0) {
		emit_opi(MOV, a, 0, n);
	} else if (m == 0xFFFF) {
		emit_opi(MOV | 0x1000, a, 0, n);
	} else {
		emit_opi(MHI, a, 0, m);
		if ((n & 0xFFFF) != 0) {
			emit_opi(IOR, a, a, n);
		}
	}
}

// immediate op, using a temporary register and register op if the
// immediate argument does not fit the single instruction form
void emit_opi_n(u32 op, u32 a, u32 b, u32 n) {
	u32 m = n >> 16;
	if (m == 0) {
		emit_opi(op, a, b, n);
	} else if (m == 0xFFFF) {
		emit_opi(op | 0x1000, a, b, n);
	} else {
		u32 t0 = get_reg_tmp();
		emit_opi(MHI, t0, 0, m);
		if ((n & 0xFFFF) != 0) {
			emit_opi(IOR, t0, t0, n);
		}
		emit_op(op, a, b, t0);
		put_reg(t0);
	}
}

enum {
	LDW = 8, LDB = 9, STW = 10, STB = 11
};
void emit_mem(u32 op, u32 a, u32 b, u32 off) {
	emit((op << 28) | (a << 24) | (b << 20) | (off & 0xfffff));
}

enum {
	MI = 0, EQ = 1, CS = 2,  VS = 3,  LS = 4,  LT = 5,  LE = 6,  AL = 7,
	PL = 8, NE = 9, CC = 10, VC = 11, HI = 12, GE = 13, GT = 14, NV = 15,
	L = 0x10,
};
void emit_br(u32 op, u32 r) {
	emit(((0xC0 | op) << 24) | r);
}
void emit_bi(u32 op, u32 off) {
	emit(((0xE0 | op) << 24) | (off & 0xffffff));
}

u8 rel_op_to_cc_tab[6] = { EQ, NE, LT, LE, GT, GE };
u32 add_op_to_ins_tab[4] = { ADD, SUB, IOR, XOR };
u32 mul_op_to_ins_tab[7] = { MUL, DIV, MOD, AND, ANN, LSL, ASR };

// ================================================================

u32 rel_op_to_cc(u32 op) {
	if (op > 5) { abort(); }
	return rel_op_to_cc_tab[op];
}
u32 add_op_to_ins(u32 op) {
	if (op > 3) { abort(); }
	return add_op_to_ins_tab[op];
}
u32 mul_op_to_ins(u32 op) {
	if (op > 6) { abort(); };
	return mul_op_to_ins_tab[op];
}

// check to see if the last emitted instruction
// loaded a particular register and if so, patch
// it to load a different register
bool patch_last_load(u32 oldr, u32 newr) {
	u32 ins = ctx.code[(ctx.pc - 4) / 4];
	u32 n = ins >> 29;
	if ((n == 0b101) || (n == 0b110) || (n == 0b111)) {
		// store and branch instructions can be ignored
		return false;
	}
	if (((ins >> 24) & 15) != oldr) {
		return false;
	}
	ctx.code[(ctx.pc - 4) / 4] = (ins & 0xF0FFFFFF) | (newr << 24);
	return true;
}

// load the value of an item into a specific register
void gen_load_reg(Item x, u32 r) {
	if (x->kind == iReg) {
		if (x->r != r) {
			if (patch_last_load(x->r, r)) {
				put_reg(x->r);
			} else {
				emit_op(MOV, r, 0, x->r);
				put_reg(x->r);
			}
		}
	} else if (x->kind == iConst) {
		emit_mov(r, x->a);
	} else if (x->kind == iParam) {
		emit_mem(LDW, r, SP, 4 + x->a * 4);
	} else {
		error("gen_load failed");
	}
	x->kind = iReg;
	x->r = r;
}

// convert an item to value-in-register format
// if it's not already in that format
void gen_load(Item x) {
	if (x->kind != iReg) {
		gen_load_reg(x, get_reg_tmp());
	}
}

void gen_store(Item val, Item var) {
	gen_load(val);
	if (var->kind == iParam) {
		emit_mem(STW, val->r, SP, 4 + var->a * 4);
		put_reg(val->r);
	} else {
		error("gen_store: invalid target");
	}
}

u32 gen_branch_cond(Item x, bool sense) {
	u32 cc;
	if (x->kind == iComp) {
		if (sense == false) {
			x->r = invert_relop(x->r);
		}
		emit_op(SUB, x->a, x->a, x->b);
		put_reg(x->a);
		put_reg(x->b);
		cc = rel_op_to_cc(x->r);
	} else if (x->type == ctx.type_bool) {
		gen_load(x);
		emit_opi(SUB, x->r, x->r, 1);
		put_reg(x->r);
		if (sense) {
			cc = EQ;
		} else {
			cc = NE;
		}
	} else {
		error("conditional branch needs comparison or bool");
	}
	emit_bi(cc, 0);
	return ctx.pc - 4;
}

u32 gen_branch_fwd() {
	emit_bi(AL, 0);
	return ctx.pc - 4;
}

void gen_branch_back(u32 addr) {
	emit_bi(AL, (addr - ctx.pc - 4) >> 2);
}

void gen_return(Item x) {
	if (x->type != ctx.type_void) {
		gen_load_reg(x, R0);
	}
	emit_bi(AL, 0);
	add_scope_fixup(find_scope(sFunc));
}

void gen_param(u32 n, Item val) {
	if (n > 7) {
		error("gen_param - too many parameters");
	}
	gen_load_reg(val, n);
}

void gen_builtin(u32 id) {
	if (id == biPrintHex32) {
		emit_mov(1, 0xFFFF0000);    // MOV R1, IOBASE
		emit_mem(STW, 0, 1, 0x104); // SW R0, [R1, 0x104]
	} else {
		error("unknown builtin function");
	}
}

void gen_call(Item x) {
	if (x->type->obj->flags & ofBuiltin) {
		gen_builtin(x->type->obj->value);
	} else if (x->type->obj->flags & ofDefined) {
		u32 fnpc = x->type->obj->value;
		emit_bi(AL|L, (fnpc - ctx.pc - 4) >> 2);
	} else {
		emit_bi(AL|L, 0);
		add_object_fixup(x->type->obj);
	}
	// item becomes the return value
	x->type = x->type->base;
	if (x->type == ctx.type_void) {
		x->kind = iConst;
	} else {
		x->kind = iReg;
	}
	x->r = R0;
	x->a = 0;
	x->b = 0;
}

void gen_add_op(u32 op, Item x, Item y) {
	op = add_op_to_ins(op);
	if ((x->kind == iConst) && (y->kind == iConst)) {
		// XC = XC op YC
		if (op == ADD) {
			x->a = x->a + y->a;
		} else if (op == SUB) {
			x->a = x->a - y->a;
		} else if (op == IOR) {
			x->a = x->a | y->a;
		} else if (op == XOR) {
			x->a = x->a ^ y->a;
		}
	} else if (y->kind == iConst) {
		// XR = XR op YC
		gen_load(x);
		// for all of these, rhs==0 is no-op
		if (y->a != 0) {
			emit_opi_n(op, x->r, x->r, y->a);
		}
	} else {
		// XR = XR op YR
		gen_load(x);
		gen_load(y);
		emit_op(op, x->r, x->r, y->r);
		put_reg(y->r);
	}
}

void gen_mul_op(u32 op, Item x, Item y) {
	op = mul_op_to_ins(op);
	if ((x->kind == iConst) && (y->kind == iConst)) {
		// XC = XC op YC
		if (op == MUL) {
			x->a = x->a * y->a;
		} else if (op == DIV) {
			x->a = x->a / y->a;
		} else if (op == MOD) {
			x->a = x->a % y->a;
		} else if (op == LSL) {
			x->a = x->a << y->a;
		} else if (op == ASR) {
			x->a = x->a >> y->a;
		} else if (op == AND) {
			x->a = x->a & y->a;
		} else if (op == ANN) {
			x->a = x->a & (~y->a);
		}
	} else if (y->kind == iConst) {
		// XR = XR op YC
		gen_load(x);
		if (((op == DIV) || (op == MOD)) && (y->a == 0)) {
			error("divide by zero");
		} else if ((op == MUL) && (y->a == 1)) {
			return; // x * 1 = x
		} else if (((op == LSL) || (op == ASR)) && (y->a == 0)) {
			return; // shift-by-zero
		}
		if (op == MOD) {
			emit_opi_n(DIV, x->r, x->r, y->a);
			emit_op(MOV_H, x->r, 0, 0);
		} else {
			emit_opi_n(op, x->r, x->r, y->a);
		}
	} else {
		// TODO runtime div-by-zero check
		// XR = XR op YR
		gen_load(x);
		gen_load(y);
		if (op == MOD) {
			emit_op(op, x->r, x->r, y->r);
			emit_op(MOV_H, x->r, 0, 0);
		} else {
			emit_op(op, x->r, x->r, y->r);
		}
		put_reg(y->r);
	}
}

void gen_rel_op(u32 op, Item x, Item y) {
	gen_load(x);
	gen_load(y);
	x->kind = iComp;
	x->a = x->r;
	x->b = y->r;
	x->r = op;
}

void gen_unary_op(u32 op, Item x) {
	if (x->kind == iConst) {
		if (op == tMINUS) {
			x->a = - x->a;
		} else if (op == tBANG) {
			x->a = ! x->a;
		} else if (op == tNOT) {
			x->a = ~ x->a;
		}
		return;
	}
	gen_load(x);
	if (op == tBANG) {
		emit_opi_n(XOR, x->r, x->r, 1);
	} else if (op == tNOT) {
		emit_opi_n(XOR, x->r, x->r, 0xFFFFFFFF);
	} else if (op == tMINUS) {
		u32 t0 = get_reg_tmp();
		emit_mov(t0, 0);
		emit_op(SUB, x->r, t0, x->r);
		put_reg(t0);
	}
}

// patch branch instruction at addr to 
// branch to current pc
void fixup_branch_fwd(u32 addr) {
#if 0
	// disabled for now as this gets tripped up
	// by stuff like test/1030-flow-control.src (if (n==3) ...)
	if (addr == ctx.pc - 4) {
		// if the branch to be patched is the
		// instruction just previously emitted, we
		// can simply erase it
		ctx.pc -= 4;
		fprintf(stderr, "DELETED BRANCH @ 0x%x\n", ctx.pc);
	} else
#endif
       	{
		u32 off = (ctx.pc - addr - 4) >> 2;
		u32 ins = ctx.code[addr >> 2] & 0xFF000000;
		ctx.code[addr >> 2] = ins | (off & 0x00FFFFFF);
	}
}

void fixup_branches_fwd(Fixup fixup) {
	while (fixup != nil) {
		fixup_branch_fwd(fixup->pc);
		fixup = fixup->next;
	}
}

void gen_prologue(Object fn) {
	fn->value = ctx.pc;
	emit_opi(SUB, SP, SP, 4 + fn->type->len * 4);
	emit_mem(STW, LR, SP, 0);

	Object param = fn->first;
	u32 r = 0;
	while (param != nil) {
		emit_mem(STW, r, SP, (r + 1) * 4);
		r++;
		param = param->next;
	}
}

void gen_epilogue(Object fn) {
	emit_mem(LDW, LR, SP, 0);
	emit_opi(ADD, SP, SP, 4 + fn->type->len * 4);
	emit_br(AL, LR);
}


void gen_start() {
	// placeholder branch to init
	emit_bi(AL, 0);
}

void gen_end() {
	String str = mkstring("start", 5);
	Object obj = find(str);
	while (obj != nil) {
		if (obj->type->kind != tFunc) {
			error("'start' is not a function\n");
		}
		if (obj->first != nil) {
			error("'start' must have no parameters\n");
		}
		// patch branch at addr 0
		ctx.code[0] |= (obj->value - 4) >> 2;
		return;
	}
	error("no 'start' function\n");
}

void gen_write(const char* outname) {
	int fd = open(outname, O_CREAT | O_TRUNC | O_WRONLY, 0644);
	if (fd < 0) {
		error("cannot open '%s' to write", outname);
	}
	u32 n = 0;
	while (n < ctx.pc) {
		if (write(fd, ctx.code + (n/4), sizeof(u32)) != sizeof(u32)) {
			error("error writing '%s'", outname);
		}
		n += 4;
	}
	close(fd);
}

#include "risc5.h"

void gen_listing(const char* listfn, const char* srcfn) {
	FILE* fin = fopen(srcfn, "r");
	if (fin == NULL) {
		error("cannot re-read '%s'\n", srcfn);
	}
	FILE* fout = fopen(listfn, "w");
	if (fout == NULL) {
		error("cannot write '%s'\n", listfn);
	}
	u32 n = 0;
	u32 line = 1;
	char buf[1024];
	while (n < ctx.pc) {
		u32 ins = ctx.code[n/4];
		if ((line < ctx.xref[n/4]) && fin) {
			fprintf(fout, "\n");
			while (line < ctx.xref[n/4]) {
				if (fgets(buf, sizeof(buf), fin) == nil) {
					fin = nil;
					break;
				}
				u32 i = 0;
				while (buf[i] != 0) {
					if (buf[i] > ' ') {
						fprintf(fout,"%s", buf);
						break;
					}
					i++;
				}
				line++;
			}
			fprintf(fout, "\n");
		}
		risc5dis(n, ins, buf);
		fprintf(fout, "%08x: %08x  %s\n", n, ins, buf);
		n += 4;
	}
	fclose(fout);
	if (fin) {
		fclose(fin);
	}
}

// ================================================================

int main(int argc, char **argv) {
	const char *outname = "out.bin";
	const char *lstname = nil;
	const char *srcname = nil;

	init_ctx();
	ctx.filename = "<commandline>";

	while (argc > 1) {
		if (!strcmp(argv[1],"-o")) {
			if (argc < 2) {
				error("option -o requires argument");
			}
			outname = argv[2];
			argc--;
			argv++;
		} else if (!strcmp(argv[1], "-l")) {
			if (argc < 2) {
				error("option -l requires argument");
			}
			lstname = argv[2];
			argc--;
			argv++;
		} else if (!strcmp(argv[1], "-A")) {
			ctx.flags |= cfAbortOnError;
		} else if (argv[1][0] == '-') {
			error("unknown option: %s", argv[1]);
		} else {
			if (srcname != nil) {
				error("multiple source files disallowed");
			} else {
				srcname = argv[1];
			}
		}
		argc--;
		argv++;
	}

	if (srcname == nil) {
		error("no file specified");
	}
	ctx.filename = srcname;

	load(srcname);
	ctx.line = ctx.sptr;
	ctx.linenumber = 1;

#if 0
	ctx.flags |= 1;
	do {
		next();
		print();
	} while (ctx.tok != tEOF);
	printf("\n");
#else
	gen_start();
	parse_program();
	gen_end();
	gen_write(outname);
	if (lstname != nil) {
		gen_listing(lstname, ctx.filename);
	}
#endif

	return 0;
}
