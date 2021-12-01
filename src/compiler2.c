// Copyright 2020, Brian Swetland <swetland@frotz.net>
// Licensed under the Apache License, Version 2.0.

#if C
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

//#include "risc5.h"

// builtin types
#define nil 0
typedef uint32_t u32;
typedef int32_t i32;
typedef uint8_t u8;

// rewriter only handles single word types
typedef uint32_t token_t;
typedef uint32_t ast_t;
typedef uint32_t type_t;
typedef uint32_t symbol_t;
typedef uint32_t scope_t;
typedef char* str;
typedef char** args;
typedef uint32_t* u32ptr;

void error(const char *fmt, ...);
#endif

// ------------------------------------------------------------------
// structures

typedef struct StringRec StringRec;
typedef struct CtxRec CtxRec;
typedef struct AstRec AstRec;
typedef struct TypeRec TypeRec;
typedef struct SymbolRec SymbolRec;
typedef struct ScopeRec ScopeRec;

typedef struct StringRec* String;
typedef struct CtxRec* Ctx;
typedef struct AstRec* Ast;
typedef struct TypeRec* Type;
typedef struct SymbolRec* Symbol;
typedef struct ScopeRec* Scope;

struct StringRec {
	String next;
	u32 len;
	char text[0];
};

enum {
	AST_NAME,
	AST_U32,
	AST_STRING,
	AST_BINOP,    // EXPR EXPR
	AST_UNOP,     // EXPR
	AST_BLOCK,    // STMT*
	AST_ASSIGN,   // NAME EXPR
	AST_CALL,     // NAME EXPR*
	AST_WHILE,    // WHILE EXPR BLOCK
	AST_IF,       // IF EXPR BLOCKthen BLOCKelse
	AST_RETURN,   // EXPR
	AST_BREAK,
	AST_CONTINUE,
	AST_LOCAL,    // NAME EXPR
	AST_PROGRAM,  // (TYPEDEF | ENUMDEF | FUNC | GLOBAL)*
	AST_TYPEDEF,
	AST_ENUMDEF,  // FIELD*
	AST_FUNC,     // BLOCK PARAM*
	AST_GLOBAL,   // EXPR
	AST_FIELD,
	AST_DISCARD,
	AST_DEREF,
	AST_INDEX,
};

str ast_t_names[] = {
	"NAME", "U32", "STR", "BINOP", "UNOP", "BLOCK", "ASSIGN",
	"CALL", "WHILE", "IF", "RETURN", "BREAK", "CONTINUE",
	"LOCAL", "PROGRAM", "TYPEDEF", "ENUMDEF", "FUNCDEF",
	"GLOBAL", "FIELD", "DISCARD", "DEREF", "INDEX"
};

struct AstRec {
	ast_t kind;
	Ast child;
	Ast next;

	u32 ival;
	String name;
	Symbol sym;
	Type type;
};

enum {
	TYPE_VOID,
	TYPE_BOOL,
	TYPE_BYTE,
	TYPE_I32,
	TYPE_NIL,
	TYPE_POINTER,
	TYPE_ARRAY,
	TYPE_SLICE,
	TYPE_RECORD,
	TYPE_FUNC,
	TYPE_ENUM,
	TYPE_UNDEFINED,
};

str type_id_tab[TYPE_UNDEFINED + 1] = {
	"void", "bool", "byte", "i32", "nil", "*", "[]", "[]",
	"struct", "func", "enum", "undef"
};

struct TypeRec {
	type_t kind;
	Type base;      // pointer-to, func-return-type, array-of
	Symbol sym;     // if not anonymous
	Symbol first;   // list of params or fields
	u32 len;        // array, params
	u32 size;       // in bytes
};

enum {
	SYM_CONST,
	SYM_TYPE,
	SYM_FUNC,
	SYM_GLOBAL,
	SYM_LOCAL,
	SYM_PARAM,
	SYM_FIELD,
};

enum {
	SYM_IS_READ_ONLY = 1,
	SYM_IS_PUBLIC = 2,
	SYM_IS_DEFINED = 4,
	SYM_IS_BUILTIN = 8,
};

struct SymbolRec {
	symbol_t kind;
	Symbol next;
	u32 flags;
	String name;
	Type type;
	Symbol first; // list of?

	u32 value; // SYM_CONST
};

enum {
	SCOPE_GLOBAL, SCOPE_FUNC, SCOPE_BLOCK, SCOPE_LOOP,
};

struct ScopeRec {
	scope_t kind;
	Scope next;
	Symbol first;
	u32 level;
	u32 save_stack;
	// Fixup?
};

// ------------------------------------------------------------------
// compiler global context

struct CtxRec {
	const char* filename;  // filename of active source
	int fd;

	u8 iobuffer[1024];     // scanner file io buffer
	u32 ionext;
	u32 iolast;

	u32 linenumber;        // line number of most recent line
	u32 lineoffset;        // position of start of most recent line
	u32 byteoffset;        // position of the most recent character
	u32 flags;
	u32 cc;                // scanner: next character

	token_t tok;           // most recent token
	u32 num;               // used for tNUM
	char tmp[256];         // used for tIDN, tSTR;
	String ident;          // used for tIDN

	String strtab;         // TODO: hashtable
	Symbol typetab;        // TODO: hashtable
	Scope scope;           // scope stack

	Symbol fn;             // active function being parsed
	u32 spill_stack;       // where to spill temp regs (TODO: dynamic)
	u32 local_stack;       // total stack for all locals (params, vars, tmps)
	u32 alloc_stack;       // where to allocate next var (grows/shrinks as
	                       // we enter/exit scopes)
	ScopeRec global;

	String idn_if;
	String idn_for;
	String idn_var;
	String idn_nil;
	String idn_case;
	String idn_func;
	String idn_else;
	String idn_enum;
	String idn_true;
	String idn_type;
	String idn_break;
	String idn_while;
	String idn_false;
	String idn_switch;
	String idn_struct;
	String idn_return;
	String idn_continue;

	Type type_void;
	Type type_bool;
	Type type_byte;
	Type type_i32;
	Type type_nil;
	Type type_string;

	u32 pc;              // next ins to emit
	u32 gp;              // next global data to emit

	u32 code[8192];
	u32 data[8192];
};

CtxRec ctx;

// ------------------------------------------------------------------

String string_make(const char* text, u32 len) {
	// OPT obviously this wants to be a hash table
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

// ================================================================

Ast ast_make(ast_t kind, u32 ival, String name, Symbol sym, Type type) {
	Ast a = malloc(sizeof(AstRec));
	a->kind = kind;
	a->child = nil;
	a->next = nil;
	a->ival = ival;
	a->name = name;
	a->sym = sym;
	a->type = type;
	return a;
}

Ast ast_make_binop(u32 op, Ast left, Ast right) {
	Ast node = ast_make(AST_BINOP, op, nil, nil, nil);
	node->child = left;
	left->next = right;
	return node;
}

Ast ast_make_unop(u32 op, Ast child) {
	Ast node = ast_make(AST_UNOP, op, nil, nil, nil);
	node->child = child;
	return node;
}

Ast ast_make_simple(ast_t kind, u32 x) {
	return ast_make(kind, x, nil, nil, nil);
}

Ast ast_make_const(ast_t kind, u32 x, Type type) {
	return ast_make(kind, x, nil, nil, type);
}

Ast ast_make_name(String name) {
	return ast_make(AST_NAME, 0, name, nil, nil);
}

// ================================================================

Symbol symbol_make(symbol_t kind, u32 flags, String name, Type type, u32 value) {
	Symbol s = malloc(sizeof(SymbolRec));
	s->kind = kind;
	s->flags = flags;
	s->name = name;
	s->type = type;
	s->value = value;
	s->first = nil;
	return s;
}

void symbol_add_global(Symbol sym) {
	sym->next = ctx.global.first;
	ctx.global.first = sym;
}

// search the scope stack for a symbol
Symbol symbol_find(String str) {
	Scope scope = ctx.scope;
	while (scope != nil) {
		Symbol sym = scope->first;
		while (sym != nil) {
			if (sym->name == str) {
				return sym;
			}
			sym = sym->next;
		}
		scope = scope->next;
	}
	return nil;
}

// ================================================================

Type type_make(type_t kind, Type base, u32 len, u32 size) {
	Type t = malloc(sizeof(TypeRec));
	t->kind = kind;
	t->base = base;
	t->sym = nil;
	t->first = nil;
	t->len = len;
	t->size = size;
	return t;
}

void type_add(Type type, String name) {
	//fprintf(stderr,"type_add(%s, %s)\n",type_id_tab[type->kind],name->text);
	Symbol sym = symbol_make(SYM_TYPE, 0, name, type, 0);
	if (type->sym == nil) {
		// only set the the type's object if it is
		// not already set (otherwise aliases will
		// clobber the canonical type names -- yuck!)
		type->sym = sym;
	}
	sym->next = ctx.typetab;
	ctx.typetab = sym;
}

Type type_find(String name) {
	Symbol sym = ctx.typetab;
	while (sym != nil) {
		if (sym->name == name) {
			return sym->type;
		}
		sym = sym->next;
	}
	return nil;
}

Type type_setup(const char* text, u32 tlen, u32 kind, u32 size) {
	String name = string_make(text, tlen);
	Type type = type_make(kind, nil, 0, size);
	type_add(type, name);
	return type;
}

bool type_is_same(Type a, Type b) {
	if (a->kind != b->kind) {
		return false;
	}
	if (a->base != b->base) {
		return false;
	}
	if (a->len != b->len) {
		return false;
	}
	Symbol a1 = a->first;
	Symbol b1 = b->first;
	while ((a1 != nil) && (b1 != nil)) {
		// check that parameters and fields match
		if (!type_is_same(a1->type, b1->type)) {
			return false;
		}
	}
	if ((a1 != nil) || (b1 != nil)) {
		// mismatched number of parameters or fields
		return false;
	}
	return true;
}

void type_dump(Type type, bool use_short_name) {
	if (use_short_name && (type->sym != nil)) {
		printf("%s", type->sym->name->text);
	} else if (type->kind == TYPE_ARRAY) {
		printf("[%u]", type->len);
		type_dump(type->base, true);
	} else if (type->kind == TYPE_RECORD) {
		printf("struct {\n");
		Symbol field = type->first;
		while (field != nil) {
			printf("    %s ", field->name->text);
			type_dump(field->type, true);
			printf(", // off=%u, sz=%u\n", field->value, field->type->size);
			field = field->next;
		}
		printf("}");
	} else {
		printf("%s", type_id_tab[type->kind]);
		if ((type->kind == TYPE_POINTER) || (type->kind == TYPE_SLICE)) {
			type_dump(type->base, true);
		}
	}
}

// ================================================================

// push a new scope, adding list of symbols if provided
Scope scope_push(scope_t kind, Symbol list) {
	Scope scope = malloc(sizeof(ScopeRec));
	scope->kind = kind;
	scope->next = ctx.scope;
	scope->first = list;
	scope->level = ctx.scope->level + 1;
	// save current stack offset (next local var)
	scope->save_stack = ctx.alloc_stack;
	ctx.scope = scope;
	return scope;
}

void scope_add_symbol(Scope scope, Symbol sym) {
	sym->next = scope->first;
	scope->first = sym;
}

// pop a scope, return list of symbols
Symbol scope_pop() {
	if (ctx.scope->level == 0) {
		error("cannot pop the global scope");
	}

	// restore prev stack offset (next local var)
	ctx.alloc_stack = ctx.scope->save_stack;

	Symbol list = ctx.scope->first;
	// XXX dealloc
	ctx.scope = ctx.scope->next;
	return list;
}

// find the first surrounding scope of a specified kind
Scope scope_find(scope_t scope_kind) {
	Scope scope = ctx.scope;
	while (scope != nil) {
		if (scope->kind == scope_kind) {
			return scope;
		}
		scope = scope->next;
	}
	return nil;
}

enum {
	BI_PRINT_HEX_32,
	BI_PUT_C,
};

void builtin_make(const char* name, u32 id, Type p0, Type p1, Type rtn) {
	String fname = string_make(name, strlen(name));
	Type type = type_make(TYPE_FUNC, rtn, 0, 0);
	type->sym = symbol_make(SYM_FUNC, SYM_IS_DEFINED | SYM_IS_BUILTIN, fname, type, id);

	if (p0 != nil) {
		Symbol param = symbol_make(SYM_PARAM, 0, string_make("a", 1), p0, 0);
		type->sym->first = param;
		type->first = param;
		type->len = 1;
		if (p1 != nil) {
			param->next = symbol_make(SYM_PARAM, 0, string_make("b", 1), p1, 1);
			type->len = 2;
		}
	}
	symbol_add_global(type->sym);
}

// ================================================================

enum {
	cfVisibleEOL   = 1,
	cfAbortOnError = 2,
	cfTraceCodeGen = 3,
};

void ctx_init() {
	memset(&ctx, 0, sizeof(ctx));

	// pre-intern keywords
	ctx.idn_if       = string_make("if", 2);
	ctx.idn_for      = string_make("for", 3);
	ctx.idn_var      = string_make("var", 3);
	ctx.idn_nil      = string_make("nil", 3);
	ctx.idn_case     = string_make("case", 4);
	ctx.idn_func     = string_make("func", 4);
	ctx.idn_else     = string_make("else", 4);
	ctx.idn_enum     = string_make("enum", 4);
	ctx.idn_true     = string_make("true", 4);
	ctx.idn_type     = string_make("type", 4);
	ctx.idn_break    = string_make("break", 5);
	ctx.idn_while    = string_make("while", 5);
	ctx.idn_false    = string_make("false", 5);
	ctx.idn_switch   = string_make("switch", 6);
	ctx.idn_struct   = string_make("struct", 6);
	ctx.idn_return   = string_make("return", 6);
	ctx.idn_continue = string_make("continue", 8);

	// install built-in basic types
	ctx.type_void    = type_setup("void", 4, TYPE_VOID, 0);
	ctx.type_byte    = type_setup("byte", 4, TYPE_BYTE, 1);
	ctx.type_bool    = type_setup("bool", 4, TYPE_BOOL, 1);
	ctx.type_i32     = type_setup("i32",  3, TYPE_I32, 4);
	ctx.type_nil     = type_setup("nil",  3, TYPE_NIL, 4);
	ctx.type_string  = type_setup("str",  3, TYPE_SLICE, 8);
	ctx.type_string->base = ctx.type_byte;

	// magic builtin functions
	builtin_make("_hexout_", BI_PRINT_HEX_32, ctx.type_i32, nil, ctx.type_void);
	builtin_make("_putc_", BI_PUT_C, ctx.type_i32, nil, ctx.type_void);

	ctx.scope = &(ctx.global);
}

void dump_file_line(const char* fn, u32 offset);

#if C
void error(const char *fmt, ...)
#else
void error(const char *fmt)
#endif
{
	va_list ap;

	fprintf(stderr,"\n%s:%d: ", ctx.filename, ctx.linenumber);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);

	if (ctx.linenumber > 0) {
		// dump_file_line(ctx.filename, ctx.lineoffset);
	}
	fprintf(stderr, "\n");

	if (ctx.flags & cfAbortOnError) {
		abort();
	} else {
		exit(1);
	}
}

void ctx_open_source(const char* filename) {
	ctx.filename = filename;
	ctx.linenumber = 0;

	if (ctx.fd >= 0) {
		close(ctx.fd);
	}
	ctx.fd = open(filename, O_RDONLY);
	if (ctx.fd < 0) {
		error("cannot open file '%s'", filename);
	}
	ctx.ionext = 0;
	ctx.iolast = 0;
	ctx.linenumber = 1;
	ctx.lineoffset = 0;
	ctx.byteoffset = 0;
}

// ================================================================
// lexical scanner

// token classes (tok & tcMASK)
enum {
	tcRELOP = 0x08, tcADDOP = 0x10, tcMULOP = 0x18,
	tcAEQOP = 0x20, tcMEQOP = 0x28, tcMASK = 0xF8,
};

enum {
	// EndMarks, Braces, Brackets Parens
	tEOF, tEOL, tOBRACE, tCBRACE, tOBRACK, tCBRACK, tOPAREN, tCPAREN,
	// RelOps (do not reorder)
	tEQ, tNE, tLT, tLE, tGT, tGE, tx0E, tx0F,
	// AddOps (do not reorder)
	tPLUS, tMINUS, tPIPE, tCARET, tx14, tx15, tx16, tx17,
	// MulOps (do not reorder)
	tSTAR, tSLASH, tPERCENT, tAMP, tANDNOT, tLEFT, tRIGHT, tx1F,
	// AsnOps (do not reorder)
	tADDEQ, tSUBEQ, tOREQ, tXOREQ, tx24, tx25, tx26, tx27,
	tMULEQ, tDIVEQ, tMODEQ, tANDEQ, tANNEQ, tLSEQ, tRSEQ, t2F,
	// Various, UnaryNot, LogicalOps,
	tSEMI, tCOLON, tDOT, tCOMMA, tNOT, tAND, tOR, tBANG,
	tASSIGN, tINC, tDEC,
	// Keywords
	tTYPE, tFUNC, tSTRUCT, tVAR, tENUM,
	tIF, tELSE, tWHILE,
	tBREAK, tCONTINUE, tRETURN,
	tFOR, tSWITCH, tCASE,
	tTRUE, tFALSE, tNIL,
	tIDN, tNUM, tSTR,
	// used internal to the lexer but never returned
	tSPC, tINV, tDQT, tSQT, tMSC,
};

str tnames[] = {
	"<EOF>", "<EOL>", "{",  "}",  "[",   "]",   "(",   ")",
	"==",    "!=",    "<",  "<=", ">",   ">=",  "",    "",
	"+",     "-",     "|",  "^",  "",    "",    "",    "",
	"*",     "/",     "%",  "&",  "&~",  "<<",  ">>",  "",
	"+=",    "-=",    "|=", "^=", "",    "",    "",    "",
	"*=",    "/=",    "%=", "&=", "&~=", "<<=", ">>=", "",
	";",     ":",     ".",  ",",  "~",   "&&",  "||",  "!",
	"=",     "++",    "--",
	"type", "func", "struct", "var", "enum",
	"if", "else", "while",
	"break", "continue", "return",
	"for", "switch", "case",
	"true", "false", "nil",
	"<ID>", "<NUM>", "<STR>",
	"<SPC>", "<INV>", "<DQT>", "<SQT>", "<MSC>",
};

u8 lextab[256] = {
	tEOF, tINV, tINV, tINV, tINV, tINV, tINV, tINV,
	tINV, tSPC, tEOL, tSPC, tINV, tSPC, tINV, tINV,
	tINV, tINV, tINV, tINV, tINV, tINV, tINV, tINV,
	tINV, tINV, tINV, tINV, tINV, tINV, tINV, tINV,
	tSPC, tBANG, tDQT, tMSC, tMSC, tPERCENT, tAMP, tSQT,
	tOPAREN, tCPAREN, tSTAR, tPLUS, tCOMMA, tMINUS, tDOT, tSLASH,
	tNUM, tNUM, tNUM, tNUM, tNUM, tNUM, tNUM, tNUM,
	tNUM, tNUM, tCOLON, tSEMI, tLT, tASSIGN, tGT, tMSC,
	tMSC, tIDN, tIDN, tIDN, tIDN, tIDN, tIDN, tIDN,
	tIDN, tIDN, tIDN, tIDN, tIDN, tIDN, tIDN, tIDN,
	tIDN, tIDN, tIDN, tIDN, tIDN, tIDN, tIDN, tIDN,
	tIDN, tIDN, tIDN, tOBRACK, tMSC, tCBRACK, tCARET, tIDN,
	tMSC, tIDN, tIDN, tIDN, tIDN, tIDN, tIDN, tIDN,
	tIDN, tIDN, tIDN, tIDN, tIDN, tIDN, tIDN, tIDN,
	tIDN, tIDN, tIDN, tIDN, tIDN, tIDN, tIDN, tIDN,
	tIDN, tIDN, tIDN, tOBRACE, tPIPE, tCBRACE, tNOT, tINV,
	tINV, tINV, tINV, tINV, tINV, tINV, tINV, tINV,
	tINV, tINV, tINV, tINV, tINV, tINV, tINV, tINV,
	tINV, tINV, tINV, tINV, tINV, tINV, tINV, tINV,
	tINV, tINV, tINV, tINV, tINV, tINV, tINV, tINV,
	tINV, tINV, tINV, tINV, tINV, tINV, tINV, tINV,
	tINV, tINV, tINV, tINV, tINV, tINV, tINV, tINV,
	tINV, tINV, tINV, tINV, tINV, tINV, tINV, tINV,
	tINV, tINV, tINV, tINV, tINV, tINV, tINV, tINV,
	tINV, tINV, tINV, tINV, tINV, tINV, tINV, tINV,
	tINV, tINV, tINV, tINV, tINV, tINV, tINV, tINV,
	tINV, tINV, tINV, tINV, tINV, tINV, tINV, tINV,
	tINV, tINV, tINV, tINV, tINV, tINV, tINV, tINV,
	tINV, tINV, tINV, tINV, tINV, tINV, tINV, tINV,
	tINV, tINV, tINV, tINV, tINV, tINV, tINV, tINV,
	tINV, tINV, tINV, tINV, tINV, tINV, tINV, tINV,
	tINV, tINV, tINV, tINV, tINV, tINV, tINV, tINV,
};

i32 unhex(u32 ch) {
	if ((ch >= '0') && (ch <= '9')) {
		return ch - '0';
	}
	if ((ch >= 'a') && (ch <= 'f')) {
		return ch - 'a' + 10;
	}
	if ((ch >= 'A') && (ch <= 'F')) {
		return ch - 'A' + 10;
	}
	return -1;
}

u32 scan() {
	while (ctx.ionext == ctx.iolast) {
		if (ctx.fd < 0) {
			ctx.cc = 0;
			return ctx.cc;
		}
		int r = read(ctx.fd, ctx.iobuffer, sizeof(ctx.iobuffer));
		if (r <= 0) {
			ctx.fd = -1;
		} else {
			ctx.iolast = r;
			ctx.ionext = 0;
		}
	}
	ctx.cc = ctx.iobuffer[ctx.ionext];
	ctx.ionext++;
	ctx.byteoffset++;
	return ctx.cc;
}

u32 unescape(u32 n) {
	if (n == 'n') {
		return 10;
	} else if (n == 'r') {
		return 13;
	} else if (n == 't') {
		return 9;
	} else if (n == '"') {
		return '"';
	} else if (n == '\'') {
		return '\'';
	} else if (n == '\\') {
		return '\\';
	} else if (n == 'x') {
		int x0 = unhex(scan());
		int x1 = unhex(scan());
		if ((x0 < 0) || (x1 < 0)) {
			error("invalid hex escape");
		}
		return (x0 << 4) | x1;
	} else {
		error("invalid escape 0x%02x", n);
		return 0;
	}
}

token_t scan_string(u32 cc, u32 nc) {
	u32 n = 0;
	while (true) {
		if (nc == '"') {
			break;
		} else if (nc == 0) {
			error("unterminated string");
		} else if (nc == '\\') {
			ctx.tmp[n] = unescape(scan());
		} else {
			ctx.tmp[n] = nc;
		}
		nc = scan();
		n++;
		if (n == 255) {
			error("constant string too large");
		}
	}
	ctx.tmp[n] = 0;
	return tSTR;
}

token_t scan_keyword(u32 len) {
	ctx.tmp[len] = 0;
	String idn = string_make(ctx.tmp, len);
	ctx.ident = idn;

	if (len == 2) {
		if (idn == ctx.idn_if) { return tIF; };
	} else if (len == 3) {
		if (idn == ctx.idn_for) { return tFOR; }
		if (idn == ctx.idn_var) { return tVAR; }
		if (idn == ctx.idn_nil) { return tNIL; }
	} else if (len == 4) {
		if (idn == ctx.idn_case) { return tCASE; }
		if (idn == ctx.idn_func) { return tFUNC; }
		if (idn == ctx.idn_else) { return tELSE; }
		if (idn == ctx.idn_enum) { return tENUM; }
		if (idn == ctx.idn_true) { return tTRUE; }
		if (idn == ctx.idn_type) { return tTYPE; }
	} else if (len == 5) {
		if (idn == ctx.idn_break) { return tBREAK; }
		if (idn == ctx.idn_while) { return tWHILE; }
		if (idn == ctx.idn_false) { return tFALSE; }
	} else if (len == 6) {
		if (idn == ctx.idn_switch) { return tSWITCH; }
		if (idn == ctx.idn_struct) { return tSTRUCT; }
		if (idn == ctx.idn_return) { return tRETURN; }
	} else if (len == 8) {
		if (idn == ctx.idn_continue) { return tCONTINUE; }
	}
	return tIDN;
}

token_t scan_number(u32 cc, u32 nc) {
	u32 n = 1;
	u32 val = cc - '0';

	if ((cc == '0') && (nc == 'b')) { // binary
		nc = scan();
		while ((nc == '0') || (nc == '1')) {
			val = (val << 1) | (nc - '0');
			nc = scan();
			n++;
			if (n == 34) {
				error("binary constant too large");
			}
		}
	} else if ((cc == '0') && (nc == 'x')) { // hex
		nc = scan();
		while (true) {
			int tmp = unhex(nc);
			if (tmp == -1) {
				break;
			}
			val = (val << 4) | tmp;
			nc = scan();
			n++;
			if (n == 10) {
				error("hex constant too large");
			}
		}
	} else { // decimal
		while (lextab[nc] == tNUM) {
			u32 tmp = (val * 10) + (nc - '0');
			if (tmp <= val) {
				error("decimal constant too large");
			}
			val = tmp;
			nc = scan();
			n++;
		}
	}
	ctx.num = val;
	return tNUM;
}

token_t scan_ident(u32 cc, u32 nc) {
	ctx.tmp[0] = cc;
	u32 n = 1;

	while (true) {
		u32 tok = lextab[nc];
		if ((tok == tIDN) || (tok == tNUM)) {
			ctx.tmp[n] = nc;
			n++;
			if (n == 32) { error("identifier too large"); }
			nc = scan();
		} else {
			break;
		}
	}
	return scan_keyword(n);
}

token_t _next() {
	u8 nc = ctx.cc;
	while (true) {
		u8 cc = nc;
		nc = scan();
		u32 tok = lextab[cc];
		if (tok == tNUM) { // 0..9
			return scan_number(cc, nc);
		} else if (tok == tIDN) { // _ A..Z a..z
			return scan_ident(cc, nc);
		} else if (tok == tDQT) { // "
			return scan_string(cc, nc);
		} else if (tok == tSQT) { // '
			ctx.num = nc;
			if (nc == '\\') {
				ctx.num = unescape(scan());
			}
			nc = scan();
			if (nc != '\'') {
				error("unterminated character constant");
			}
			nc = scan();
			return tNUM;
		} else if (tok == tPLUS) {
			if (nc == '+') { tok = tINC; nc = scan(); }
		} else if (tok == tMINUS) {
			if (nc == '-') { tok = tDEC; nc = scan(); }
		} else if (tok == tAMP) {
			if (nc == '&') { tok = tAND; nc = scan(); }
			else if (nc == '~') { tok = tANDNOT; nc = scan(); }
		} else if (tok == tPIPE) {
			if (nc == '|') { tok = tOR; nc = scan(); }
		} else if (tok == tGT) {
			if (nc == '=') { tok = tGE; nc = scan(); }
			else if (nc == '>') { tok = tRIGHT; nc = scan(); }
		} else if (tok == tLT) {
			if (nc == '=') { tok = tLE; nc = scan(); }
			else if (nc == '<') { tok = tLEFT; nc = scan(); }
		} else if (tok == tASSIGN) {
			if (nc == '=') { tok = tEQ; nc = scan(); }
		} else if (tok == tBANG) {
			if (nc == '=') { tok = tNE; nc = scan(); }
		} else if (tok == tSLASH) {
			if (nc == '/') {
				// comment -- consume until EOL or EOF
				while ((nc != '\n') && (nc != 0)) {
					nc = scan();
				}
				continue;
			}
		} else if (tok == tEOL) {
			ctx.linenumber++;
			ctx.lineoffset = ctx.byteoffset;
			//ctx.xref[ctx.pc / 4] = ctx.linenumber;
			if (ctx.flags & cfVisibleEOL) {
				return tEOL;
			}
			continue;
		} else if (tok == tSPC) {
			continue;
		} else if ((tok == tMSC) || (tok == tINV)) {
			error("unknown character 0x%02x", cc);
		}

		// if we're an AddOp or MulOp, followed by an '='
		if (((tok & 0xF0) == 0x20) && (nc == '=')) {
			nc = scan();
			// transform us to a XEQ operation
			tok = tok + 0x10;
		}

		return tok;
	}
}

token_t next() {
	return (ctx.tok = _next());
}

void token_printstr(void) {
	u32 n = 0;
	printf("\"");
	while (n < 256) {
		u32 ch = ctx.tmp[n];
		if (ch == 0) {
			break;
		} else if ((ch < ' ') || (ch > '~')) {
			printf("\\x%02x", ch);
		} else if ((ch == '"') || (ch == '\\')) {
			printf("\\%c", ch);
		} else {
			printf("%c", ch);
		}
		n++;
	}
	printf("\"");
}

void token_print(void) {
	if (ctx.tok == tNUM) {
		printf("#%u ", ctx.num);
	} else if (ctx.tok == tIDN) {
		printf("@%s ", ctx.tmp);
	} else if (ctx.tok == tEOL) {
		printf("\n");
	} else if (ctx.tok == tSTR) {
		token_printstr();
	} else {
		printf("%s ", tnames[ctx.tok]);
	}
}

void expected(const char* what) {
	error("expected %s, found %s", what, tnames[ctx.tok]);
}

void expect(token_t tok) {
	if (ctx.tok != tok) {
		error("expected %s, found %s", tnames[tok], tnames[ctx.tok]);
	}
}

void require(token_t tok) {
	expect(tok);
	next();
}

// ================================================================

//TODO: handle overflow and div/mod-by-zero
i32 ast_get_const_i32(Ast node) {
	if (node->kind == AST_U32) {
		return node->ival;
	} else if (node->kind == AST_NAME) {
		if (node->sym->kind != SYM_CONST) {
			error("non-const symbol (%s) in constexpr\n", node->sym->name);
		}
		return node->sym->value;
	} else if (node->kind == AST_BINOP) {
		i32 left = ast_get_const_i32(node->child);
		i32 right = ast_get_const_i32(node->child->next);
		u32 op = node->ival;
		if (op == tPLUS) {
			return left + right;
		} else if (op == tMINUS) {
			return left - right;
		} else if (op == tSTAR) {
			return left * right;
		} else if (op == tSLASH) {
			return left / right;
		} else if (op == tPERCENT) {
			return left % right;
		} else if (op == tAMP) {
			return left & right;
		} else if (op == tPIPE) {
			return left | right;
		} else {
			error("unsupported BINOP %s\n", tnames[op]);
		}
	} else if (node->kind == AST_UNOP) {
		i32 left = ast_get_const_i32(node->child);
		u32 op = node->ival;
		if (op == tPLUS) {
			return left;
		} else if (op == tMINUS) {
			return -left;
		} else if (op == tBANG) {
			return !left;
		} else {
			error("unsupported UNOP %s\n", tnames[op]);
		}
	} else {
		error("non-const expr (%s)\n", ast_t_names[node->kind]);
	}
	return 0;
}

String parse_name(const char* what) {
	if (ctx.tok != tIDN) {
		error("expected %s, found %s %u", what, tnames[ctx.tok], ctx.tok);
	}
	String str = ctx.ident;
	next();
	return str;
}

Ast parse_expr();

Ast parse_operand() {
	Ast node = nil;
	if (ctx.tok == tNUM) {
		node = ast_make_const(AST_U32, ctx.num, ctx.type_i32);
	} else if (ctx.tok == tSTR) {
		error("<TODO> string const");
	} else if (ctx.tok == tTRUE) {
		node = ast_make_const(AST_U32, 1, ctx.type_bool);
	} else if (ctx.tok == tFALSE) {
		node = ast_make_const(AST_U32, 0, ctx.type_bool);
	} else if (ctx.tok == tNIL) {
		node = ast_make_const(AST_U32, 0, ctx.type_nil);
	} else if (ctx.tok == tOPAREN) {
		next();
		node = parse_expr();
		require(tCPAREN);
		return node;
	} else if (ctx.tok == tIDN) {
		// TODO: could happen in an AST pass to allow
		// for forward references
		Symbol sym = symbol_find(ctx.ident);
		if (sym == nil) {
			error("undefined identifier '%s'", ctx.ident->text);
		}
		node = ast_make_name(ctx.ident);
		node->sym = sym;
	} else {
		error("invalid expression");
	}
	next();
	return node;
}

Ast parse_primary_expr() {
	Ast node = parse_operand();
	while (true) {
		if (ctx.tok == tOPAREN) {
			next();
			//VALIDATE as func or ptr-to-func
			u32 n = 0;

			Ast call = ast_make_simple(AST_CALL, 0);
			call->child = node;

			while (ctx.tok != tCPAREN) {
				Ast param = parse_expr();
				node->next = param;
				node = param;
				if (ctx.tok != tCPAREN) {
					require(tCOMMA);
				}
				n++;
			}
			require(tCPAREN);
			call->ival = n;
			node = call;
		} else if (ctx.tok == tDOT) {
			next();
			String name = parse_name("field name");
			// VALIDATE appropriate name for type
			// VALIDATE type is ptr or struct
			Ast dot = ast_make_simple(AST_DEREF, 0);
			dot->child = node;
			dot->name = name;
			node = dot;
		} else if (ctx.tok == tOBRACK) {
			next();
			Ast index = ast_make_simple(AST_INDEX, 0);
			index->child = node;
			index->child->next = parse_expr();
			node = index;
			require(tCBRACK);
			//VALIDATE lhs is array, rhs is appropriate numeric
		} else {
			break;
		}
	}
	return node;
}

Ast parse_unary_expr() {
	u32 op = ctx.tok;
	if (op == tPLUS) {
		next();
		return parse_unary_expr();
	} else if ((op == tMINUS) || (op == tBANG) || (op == tNOT) || (op== tAMP)) {
		u32 op = ctx.tok;
		next();
		return ast_make_unop(op, parse_unary_expr());
	} else {
		return parse_primary_expr();
	}
}

Ast parse_mul_expr() {
	Ast node = parse_unary_expr();
	while ((ctx.tok & tcMASK) == tcMULOP) {
		u32 op = ctx.tok;
		next();
		node = ast_make_binop(op, node, parse_unary_expr());
	}
	return node;
}

Ast parse_add_expr() {
	Ast node = parse_mul_expr();
	while ((ctx.tok & tcMASK) == tcADDOP) {
		u32 op = ctx.tok;
		next();
		node = ast_make_binop(op, node, parse_mul_expr());
	}
	return node;
}

Ast parse_rel_expr() {
	Ast node = parse_add_expr();
	if ((ctx.tok & tcMASK) == tcRELOP) {
		u32 op = ctx.tok;
		next();
		node = ast_make_binop(op, node, parse_add_expr());
	}
	return node;
}

Ast parse_and_expr() {
	// XXX needs to handle short-circuit codegen etc
	Ast node = parse_rel_expr();
	if (ctx.tok == tAND) {
		while (ctx.tok == tAND) {
			next();
			node = ast_make_binop(tAND, node, parse_rel_expr());
		}
	}
	return node;
}

Ast parse_expr() {
	Ast node = parse_and_expr();
	if (ctx.tok == tOR) {
		while (ctx.tok == tOR) {
			next();
			node = ast_make_binop(tOR, node, parse_and_expr());
		}
	}
	return node;
}

// fwd_ref_ok indicates that an undefined typename
// may be treated as a forward reference.  This is
// only used for pointers (because their size does
// not depend on their target).
Type parse_type(bool fwd_ref_ok);

Type parse_struct_type() {
	Type rectype = type_make(TYPE_RECORD, nil, 0, 0);
	Symbol last = nil;
	require(tOBRACE);
	while (true) {
		if (ctx.tok == tCBRACE) {
			next();
			break;
		}
		String name = parse_name("field name");
		Type type = parse_type(false);
		Symbol field = symbol_make(SYM_FIELD, 0, name, type, rectype->size);

		// TODO sub-word packing
		rectype->size += (type->size + 3) & (~3);
		rectype->len++;

		// add field to record
		if (last == nil) {
			rectype->first = field;
		} else {
			last->next = field;
		}
		last = field;

		if (ctx.tok != tCBRACE) {
			require(tCOMMA);
		}
	}
	return rectype;
}

Type parse_array_type() {
	if (ctx.tok == tCBRACK) {
		next();
		return type_make(TYPE_SLICE, parse_type(false), 0, 8);
	} else {
		Ast expr = parse_expr();
		require(tCBRACK);
		i32 nelem = ast_get_const_i32(expr);
		if (nelem <= 0) {
			error("array size must be positive");
		}
		Type base = parse_type(false);
		u32 sz = nelem * base->size;
		if (sz < nelem) {
			error("array size overflow");
		}
		return type_make(TYPE_ARRAY, base, nelem, sz);
	}
}

Type parse_func_type() {
	error("<TODO> func type");
	return nil;
}

Type parse_type(bool fwd_ref_ok) {
	if (ctx.tok == tSTAR) { // pointer-to
		next();
		return type_make(TYPE_POINTER, parse_type(true), 0, 4);
	} else if (ctx.tok == tOBRACK) { // array-of
		next();
		return parse_array_type();
	} else if (ctx.tok == tFUNC) {
		next();
		return parse_func_type();
	} else if (ctx.tok == tSTRUCT) {
		next();
		return parse_struct_type();
	} else if (ctx.tok == tIDN) {
		String name = ctx.ident;
		next();
		Type type = type_find(name);
		if (type == nil) {
			if (fwd_ref_ok) {
				type = type_make(TYPE_UNDEFINED, nil, 0, 0);
				type_add(type, name);
			} else {
				error("undefined type '%s' not usable here", name->text);
			}
		}
		return type;
	} else {
		expected("type");
		return nil;
	}
}

Ast parse_block();

Ast parse_while() {
	Ast expr = parse_expr();
	require(tOBRACE);
	scope_push(SCOPE_LOOP, nil);
	Ast block = parse_block();
	block->sym = scope_pop();
	Ast node = ast_make_simple(AST_WHILE, 0);
	node->child = expr;
	expr->next = block;
	return node;
}

Ast parse_if() {
	// Scope outer = scope_push(SCOPE_BLOCK, nil);
	Ast expr = parse_expr();
	require(tOBRACE);
	scope_push(SCOPE_BLOCK, nil);
	Ast block = parse_block();
	block->sym = scope_pop();
	Ast ifnode = ast_make_simple(AST_IF, 0);
	ifnode->child = expr;
	expr->next = block;
	Ast last = block;
	while (ctx.tok == tELSE) {
		next();
		if (ctx.tok == tIF) {
			next();
			Ast expr = parse_expr();

			// generate "if else" code
			require(tOBRACE);
			scope_push(SCOPE_BLOCK, nil);
			Ast block = parse_block();
			block->sym = scope_pop();

			last->next = expr;
			expr->next = block;
			last = block;
		} else {
			// generate "else" code
			require(tOBRACE);
			scope_push(SCOPE_BLOCK, nil);
			Ast block = parse_block();
			block->sym = scope_pop();

			last = block;
			break;
		}
	}

	// close outer scope
	// scope_pop();
	return ifnode;
}

Ast parse_return() {
	Ast node = ast_make_simple(AST_RETURN, 0);
	if (ctx.tok == tSEMI) {
		//if (ctx.fn->type->base != ctx.type_void) {
		//	error("function requires return type");
		//}
		next();
		//x.type = ctx.type_void;
	} else {
		node->child = parse_expr();
		//if (!compatible_type(ctx.fn->type->base, x.type, &x)) {
		//	error("return types do not match");
		//}
		require(tSEMI);
	}
	return node;
}

Ast parse_break() {
	// XXX break-to-labeled-loop support
	Scope scope = scope_find(SCOPE_LOOP);
	if (scope == nil) {
		error("break must be used from inside a looping construct");
	}
	require(tSEMI);
	return ast_make_simple(AST_BREAK, 0);
}

Ast parse_continue() {
	// XXX continue-to-labeled-loop support
	Scope scope = scope_find(SCOPE_LOOP);
	if (scope == nil) {
		error("continue must be used from inside a looping construct");
	}
	require(tSEMI);
	return ast_make_simple(AST_CONTINUE, 0);
}

// unsafe write op
void STORE(u32 val, u32* ptr, u32 n, u32 sz) {
	if (sz == 4) {
		ptr[n >> 2] = val;
	} else if (sz == 1) {
		((u8*)ptr)[n] = val;
	}
}

u32 parse_array_init(Symbol var, u32ptr data, u32 dmax, u32 sz) {
	memset(data, 0, dmax);
	u32 n = 0;
	while (true) {
		if (ctx.tok == tCBRACE) {
			next();
			break;
		}
		if (n >= dmax) {
			error("initializer too large");
		}
		Ast expr = parse_expr();
		i32 v = ast_get_const_i32(expr);

		// VALIDATE type compat/fit
		STORE(v, data, n, sz);
		n += sz;
		if (ctx.tok != tCBRACE) {
			require(tCOMMA);
		}
	}
	return n;
}

void parse_struct_init(Symbol var, u32ptr data) {
	memset(data, 0, var->type->size);
	while (true) {
		if (ctx.tok == tCBRACE) {
			next();
			break;
		}
		String name = parse_name("field name");
		Symbol field = var->type->first;
		while (true) {
			if (field == nil) {
				error("structure has no '%s' field", name->text);
			}
			if (field->name == name) {
				break;
			}
			field = field->next;
		}
		require(tCOLON);
		Ast expr = parse_expr();
		i32 v = ast_get_const_i32(expr);
		// VALIDATE type compat/fit
		STORE(v, data, field->value, 4);
		if (ctx.tok != tCBRACE) {
			require(tCOMMA);
		}
	}
}

Ast parse_local_var() {
	String name = parse_name("variable name");
	// TODO: allow type inference
	Type type = parse_type(false);

	Symbol sym = symbol_make(SYM_LOCAL, 0, name, type, ctx.alloc_stack);
	scope_add_symbol(ctx.scope, sym);

	ctx.alloc_stack = ctx.alloc_stack + type->size;
	if (ctx.local_stack < ctx.alloc_stack) {
		ctx.local_stack = ctx.alloc_stack;
	}

	Ast node = ast_make_simple(AST_LOCAL, 0);
	node->name = name;
	node->type = type;
	node->sym = sym;

	if (ctx.tok == tASSIGN) {
		next();
		node->child = parse_expr();
	}
	require(tSEMI);

	return node;
}

Ast parse_global_var() {
	String name = parse_name("variable name");
	// TODO: allow type inference
	Type type = parse_type(false);

	Symbol sym = symbol_make(SYM_GLOBAL, 0, name, type, ctx.gp);
	symbol_add_global(sym);

	// advance global pointer and round to next workd
	ctx.gp = ctx.gp + type->size;
	ctx.gp = (ctx.gp + 3) & (~3);

	Ast node = ast_make_simple(AST_GLOBAL, 0);

	if (ctx.tok == tASSIGN) {
		next();
		u32* data = ctx.data + (sym->value >> 2);
		if (ctx.tok == tOBRACE) {
			next();
			if (type->kind == TYPE_ARRAY) {
				parse_array_init(sym, data, type->size, type->base->size);
			} else if (type->kind == TYPE_RECORD) {
				parse_struct_init(sym, data);
			} else {
				error("cannot initialize this way");
			}
		} else {
			Ast expr = parse_expr();
			i32 v = ast_get_const_i32(expr);
			node->child = expr;
			// VALIDATE compatible integer type
			ctx.data[sym->value >> 2] = v;
		}
	}
	require(tSEMI);
	node->name = name;
	node->type = type;
	node->sym = sym;
	return node;
}

Ast parse_expr_statement() {
	Ast node = nil;
	Ast left = parse_expr();
	Ast right = nil;
	if (ctx.tok == tASSIGN) {
		next();
		right = parse_expr();
		node = ast_make_binop(tASSIGN, left, right);
	} else if ((ctx.tok & tcMASK) == tcAEQOP) {
		u32 op = ctx.tok; // - tADDEQ;
		next();
		right = parse_expr();
		node = ast_make_binop(op, left, right);
	} else if ((ctx.tok & tcMASK) == tcMEQOP) {
		u32 op = ctx.tok; // - tMULEQ;
		next();
		right = parse_expr();
		node = ast_make_binop(op, left, right);
	} else if ((ctx.tok == tINC) || (ctx.tok == tDEC)) {
		node = ast_make_unop(ctx.tok, left);
		next();
	} else {
		node = ast_make_simple(AST_DISCARD, 0);
		node->child = left;
	}
	require(tSEMI);
	return node;
}

Ast parse_block() {
	Ast block = ast_make_simple(AST_BLOCK, 0);
	Ast last = nil;
	Ast node;
	while (true) {
		if (ctx.tok == tCBRACE) {
			next();
			break;
		} else if (ctx.tok == tRETURN) {
			next();
			node = parse_return();
		} else if (ctx.tok == tBREAK) {
			next();
			node = parse_break();
		} else if (ctx.tok == tCONTINUE) {
			next();
			node = parse_continue();
		} else if (ctx.tok == tWHILE) {
			next();
			node = parse_while();
		} else if (ctx.tok == tIF) {
			next();
			node = parse_if();
		} else if (ctx.tok == tVAR) {
			next();
			node = parse_local_var();
		} else if (ctx.tok == tSEMI) {
			next();
			// empty statement
			continue;
		} else {
			node = parse_expr_statement();
		}

		// append to block's list of statements
		if (last == nil) {
			block->child = node;
		} else {
			last->next = node;
		}
		last = node;
	}
	return block;
}

Ast parse_function_body(Symbol fn) {
	Ast node;
	ctx.fn = fn;
	ctx.local_stack = 0;
	ctx.alloc_stack = 0;
	scope_push(SCOPE_FUNC, fn->first); // scope for parameters
	scope_push(SCOPE_BLOCK, nil);      // top scope for function body
	node = parse_block();
	node->sym = scope_pop();
	scope_pop();
	ctx.fn = nil;
	return node;
}

Symbol parse_param(String fname, u32 n, Symbol first, Symbol last) {
	String pname = parse_name("parameter name");
	Type ptype = parse_type(false);
	Symbol param = symbol_make(SYM_PARAM, 0, pname, ptype, 4 + n * 4);

	Symbol sym = first;
	while (sym != nil) {
		if (sym->name == param->name) {
			error("duplicate parameter name '%s'", fname->text);
		}
		sym = sym->next;
	}

	if (last != nil) {
		last->next = param;
	}
	return param;
}

Ast parse_function() {
	Symbol first = nil;
	Symbol last = nil;
	u32 n = 0;
	String fname = parse_name("function name");
	Type rettype = ctx.type_void;

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
		rettype = parse_type(false);
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
	Symbol sym = symbol_find(fname);
	if (sym != nil) {
		// such a named identifier exists
		// check to see if we are in agreement with it
		if (sym->kind != SYM_FUNC) {
			error("redefining '%s' as function", fname->text);
		}
		if (isdef && (sym->flags & SYM_IS_DEFINED)) {
			error("redefined function '%s'", fname->text);
		}
		if (rettype != sym->type->base) {
			error("func '%s' return type differs from decl", fname->text);
		}
		if (sym->type->len != n) {
			error("func '%s' parameter count differs from decl", fname->text);
		}
		Symbol pa = first;
		Symbol pb = sym->type->first;
		u32 i = 1;
		while ((pa != nil) && (pb != nil)) {
			if (!type_is_same(pa->type, pb->type)) {
				error("func '%s' param %u differs from decl", fname->text, i);
			}
			pa = pa->next;
			pb = pb->next;
		}
	} else {
		// if there was no existing record of this function, create one now
		Type type = type_make(TYPE_FUNC, rettype, n, 0);
		type->first = first;
		sym = symbol_make(SYM_FUNC, 0, fname, type, 0);
		sym->first = first;
		type->sym = sym;
		symbol_add_global(sym);
	}

	Ast node = ast_make_simple(AST_FUNC, 0);
	node->name = fname;
	node->sym = sym;

	// handle definition if it is one
	if (isdef) {
		// patch any forward references
		//fixup_branches_fwd(obj->fixups);

		// mark as defined and save entry address
		sym->flags |= SYM_IS_DEFINED;
		//sym->value = ctx.pc;
		node->child = parse_function_body(sym);
	}

	return node;
}

Ast parse_type_def() {
	String name = parse_name("type name");
	Type type = parse_type(false);
	Type prev = type_find(name);
	if (prev == nil) {
		type_add(type, name);
	} else {
		if (prev->kind != TYPE_UNDEFINED) {
			error("cannot redefine type '%s'\n", name->text);
		}
		prev->kind = type->kind;
		prev->base = type->base;
		prev->first = type->first;
		prev->len = type->len;
		prev->size = type->size;
		prev->sym->type = type;
		// XXX discard type
		type = prev;
	}
	require(tSEMI);
	Ast node = ast_make_simple(AST_TYPEDEF, 0);
	node->name = name;
	node->type = type;
	return node;
}

Ast parse_enum_def() {
	Ast decl = ast_make_simple(AST_ENUMDEF, 0);
	Ast last = nil;

	require(tOBRACE);
	u32 val = 0;
	while (ctx.tok != tCBRACE) {
		String name = parse_name("enum tag name");
		Symbol sym = symbol_find(name);
		if (sym != nil) {
			error("cannot redefine %s as enum tag\n", name->text);
		}
		if (ctx.tok == tASSIGN) {
			next();
			Ast expr = parse_expr();
			val = ast_get_const_i32(expr);
			//XXX ensure const and set val
		}
		require(tCOMMA);
		Ast node = ast_make(AST_FIELD, val, name, sym, ctx.type_i32);
		if (last == nil) {
			decl->child = node;
		} else {
			last->next = node;
		}
		last = node;
		symbol_add_global(symbol_make(SYM_CONST, 0, name, ctx.type_i32, val));
		val++;
	}
	require(tCBRACE);
	require(tSEMI);
	return decl;
}

Ast parse_program() {
	Ast program = ast_make_simple(AST_PROGRAM, 0);
	Ast last = nil;
	Ast d = nil;
	next();
	for (;;) {
		if (ctx.tok == tENUM) {
			next();
			d = parse_enum_def();
		} else if (ctx.tok == tTYPE) {
			next();
			d = parse_type_def();
		} else if (ctx.tok == tFUNC) {
			next();
			d = parse_function();
		} else if (ctx.tok == tVAR) {
			next();
			d = parse_global_var();
		} else if (ctx.tok == tEOF) {
			return program;
		} else {
			expected("function, variable, or type definition");
		}
		if (last == nil) {
			program->child = d;
		} else {
			last->next = d;
		}
		last = d;
	}
}

void ast_dump_syms(Symbol sym, str tag, u32 indent) {
	while (sym != nil) {
		u32 i = 0;
		while (i < indent) { printf("  "); i++; }
		printf("%s '%s' ", tag, sym->name->text);
		type_dump(sym->type, true);
		printf("\n");
		sym = sym->next;
	}
}

void ast_dump_rtype(Symbol sym, u32 indent) {
	u32 i = 0;
	while (i < indent) { printf("  "); i++; }
	printf("Returns ");
	type_dump(sym->type->base, true);
	printf("\n");
}

void ast_dump(Ast node, u32 indent) {
	u32 i = 0;
	while (i < indent) { printf("  "); i++; }
	printf("%s ", ast_t_names[node->kind]);
	if (node->kind == AST_NAME) {
		printf("'%s'\n", node->name->text);
	} else if ((node->kind == AST_BINOP) || (node->kind == AST_UNOP)) {
		printf("%s\n", tnames[node->ival]);
	} else if (node->kind == AST_U32) {
		printf("0x%x\n", node->ival);
	} else if (node->kind == AST_TYPEDEF) {
		printf("'%s' ", node->name->text);
		type_dump(node->type, false);
		printf("\n");
	} else {
		if (node->name) {
			printf("'%s' ",node->name->text);
		}
		if (node->type) {
			type_dump(node->type, true);
			printf(" ");
			//printf("<type %p> ", node->type);
		}
		if (node->sym) {
			//printf("<sym@%p> ", node->sym);
		}
		if (node->ival) {
			printf("%u", node->ival);
		}
		printf("\n");
	}
	if (node->kind == AST_FUNC) {
		ast_dump_syms(node->sym->first, "Param", indent + 1);
		ast_dump_rtype(node->sym, indent + 1);
	}
	if (node->kind == AST_BLOCK) {
		ast_dump_syms(node->sym, "Local", indent + 1);
	}
	node = node->child;
	indent = indent + 1;
	while (node != nil) {
		ast_dump(node, indent);
		node = node->next;
	}
}

#if 0
void type_dump_all() {
	Symbol sym = ctx.typetab;

	while (sym != nil) {
		fprintf(stderr, "Symbol %p '%s'\n", sym, sym->name->text);
		fprintf(stderr, "  Type %p %s len=%u sz=%u\n",
		        sym->type, type_id_tab[sym->type->kind],
		        sym->type->len, sym->type->size);
		fprintf(stderr, "       sym=%p first=%p\n",
		        sym->type->sym, sym->type->first);
		sym = sym->next;
	}
}
#endif

// ================================================================

i32 main(int argc, args argv) {
	str outname = "out.bin";
	str lstname = nil;
	str srcname = nil;
	bool dump = false;
	bool scan_only = false;

	ctx_init();
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
		} else if (!strcmp(argv[1], "-p")) {
			dump = true;
		} else if (!strcmp(argv[1], "-v")) {
			ctx.flags |= cfTraceCodeGen;
		} else if (!strcmp(argv[1], "-s")) {
			scan_only = true;
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

	ctx_open_source(srcname);
	ctx.linenumber = 1;
	ctx.lineoffset = 0;
	// prime the lexer
	scan();

	if (scan_only) {
		ctx.flags |= 1;
		while (true) {
			next();
			token_print();
			if (ctx.tok == tEOF) {
				printf("\n");
				return 0;
			}
		}
	}

	Ast a = parse_program();

	ast_dump(a, 0);

	//type_dump_all();

	return 0;
}
