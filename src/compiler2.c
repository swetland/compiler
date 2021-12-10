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

#include "risc5.h"

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
typedef struct FixupRec FixupRec;

typedef struct StringRec* String;
typedef struct CtxRec* Ctx;
typedef struct AstRec* Ast;
typedef struct TypeRec* Type;
typedef struct SymbolRec* Symbol;
typedef struct ScopeRec* Scope;
typedef struct FixupRec* Fixup;

struct StringRec {
	String next;
	u32 len;
	char text[0];
};

enum {
// expression parts
	AST_SYMBOL,
	AST_CONST,
	AST_STRING,
	AST_BINOP,    // c0=EXPR c1=EXPR
	AST_UNOP,     // c0=EXPR
	AST_DEREF,    // c0=EXPR type: pointer-to-...
	AST_INDEX,    // c0=EXPR type: array-of-...  c1=EXPR index
	AST_FIELD,    // c0=EXPR type: struct        c1=SYMBOL field
	AST_ADDROF,   // c0=EXPR type: lvalue
// container of statements
	AST_BLOCK,    // c0=STMT
// statements (chained into a list by c2)
	AST_EXPR,     // c0=EXPR
	AST_CALL,     // c0=NAME c2=EXPR*
	AST_WHILE,    // c0=EXPR c1=BLOCK
	AST_IF,       // c0=IFELSE
	AST_RETURN,   // c0=EXPR
	AST_BREAK,
	AST_CONTINUE,
// sub-part of if
	AST_IFELSE,   // c0=EXPR c1=BLOCKthen c2=BLOCKelse|IFELSE
// program components (chained into a list by c2)
	AST_FUNC,     // c0=BLOCK
// top node
	AST_PROGRAM,  // c2=(TYPEDEF | ENUMDEF | FUNC | GLOBAL)*
};

str ast_kind[AST_PROGRAM + 1] = {
	"SYMBOL", "CONST", "STR", "BINOP", "UNOP",
	"DEREF", "INDEX", "FIELD", "ADDROF",
	"BLOCK", "EXPR", "CALL", "WHILE", "IF",
	"RETURN", "BREAK", "CONTINUE", "IFELSE",
	"FUNC", "PROGRAM",
};

struct AstRec {
	ast_t kind;
	Ast c0; // left
	Ast c1; // right
	Ast c2; // third or listhead (chained on c2 fields)

	u32 ival;
	String name;
	Symbol sym;
	Type type;

	u32 srcloc; // linenumber for now
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

str type_kind[TYPE_UNDEFINED + 1] = {
	"void", "bool", "byte", "i32", "nil", "*", "[]", "[]",
	"struct", "func", "enum", "undef"
};

struct TypeRec {
	type_t kind;
	Type base;      // pointer-to, func-return-type, array-of
	Symbol sym;     // if not anonymous
	Symbol first;   // list of params or fields
	u32 len;        // array elem count, param count
	u32 size;       // in bytes, local stack for funcs
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

str sym_kind[SYM_FIELD + 1] = {
	"CONST", "TYPE", "FUNC", "GLOBAL", "LOCAL", "PARAM", "FIELD",
};

enum {
	SYM_IS_READ_ONLY = 0x01,
	SYM_IS_PUBLIC =    0x02,
	SYM_IS_DEFINED =   0x04,
	SYM_IS_BUILTIN =   0x08,
	SYM_IS_PLACED =    0x10, // exists at addr in sym->value
};

struct SymbolRec {
	symbol_t kind;
	Symbol next;
	u32 flags;
	String name;
	Type type;
	Symbol first; // list of fields or params
	u32 value; // SYM_CONST
	Fixup fixups; // references from before placement
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


struct FixupRec {
	Fixup next;
	u32 addr;
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
	u32 xref[8192];
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
	a->c0 = nil;
	a->c1 = nil;
	a->c2 = nil;
	a->ival = ival;
	a->name = name;
	a->sym = sym;
	a->type = type;
	a->srcloc = ctx.linenumber;
	return a;
}

Ast ast_make_binop(u32 op, Ast left, Ast right) {
	Ast node = ast_make(AST_BINOP, op, nil, nil, nil);
	node->c0 = left;
	node->c1 = right;
	return node;
}

Ast ast_make_unop(u32 op, Ast child) {
	Ast node = ast_make(AST_UNOP, op, nil, nil, nil);
	node->c0 = child;
	return node;
}

Ast ast_make_simple(ast_t kind, u32 x) {
	return ast_make(kind, x, nil, nil, nil);
}

Ast ast_make_const(u32 value, Type type) {
	return ast_make(AST_CONST, value, nil, nil, type);
}

Ast ast_make_symbol(String name, Symbol sym) {
	return ast_make(AST_SYMBOL, 0, name, sym, sym->type);
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
	s->fixups = nil;
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

Type type_make_ptr(Type type) {
	return type_make(TYPE_POINTER, type, 4, 0);
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
	if (a == b) {
		return true;
	}
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

bool type_is_i32_shape(Type type) {
	if ((type->kind == TYPE_I32) ||
	    (type->kind == TYPE_BYTE) ||
	    (type->kind == TYPE_BOOL)) {
		return true;
	} else {
		return false;
	}
}

bool type_is_compatible(Type dst, Type src) {
	if (type_is_i32_shape(dst) && type_is_i32_shape(src)) {
		return true;
	}
	return type_is_same(dst, src);
	// check if deref of src is same as dst?
}

void type_dump(FILE* fp, Type type, bool use_short_name) {
	if (use_short_name && (type->sym != nil)) {
		fprintf(fp, "%s", type->sym->name->text);
	} else if (type->kind == TYPE_ARRAY) {
		fprintf(fp, "[%u]", type->len);
		type_dump(fp, type->base, true);
	} else if (type->kind == TYPE_RECORD) {
		fprintf(fp, "struct {\n");
		Symbol field = type->first;
		while (field != nil) {
			fprintf(fp, "    %s ", field->name->text);
			type_dump(fp, field->type, true);
			fprintf(fp, ", // off=%u, sz=%u\n", field->value, field->type->size);
			field = field->next;
		}
		fprintf(fp, "}");
	} else {
		fprintf(fp, "%s", type_kind[type->kind]);
		if ((type->kind == TYPE_POINTER) || (type->kind == TYPE_SLICE)) {
			type_dump(fp, type->base, true);
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
	BI_EXIT,
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

void fixup_add_list(Fixup list, u32 addr) {
	Fixup fixup = malloc(sizeof(FixupRec));
	fixup->next = list->next;
	fixup->addr = addr;
	list->next = fixup;
}

void fixup_add_sym(Symbol sym, u32 addr) {
	Fixup fixup = malloc(sizeof(FixupRec));
	fixup->next = sym->fixups;
	fixup->addr = addr;
	sym->fixups = fixup;
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
void dump_error_ctxt();

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

	dump_error_ctxt();

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

// used by ast graph printer
str txnames[] = {
	"<EOF>", "<EOL>", "{",  "}",  "[",   "]",   "(",   ")",
	"eq",    "ne",    "lt", "le", "gt",  "ge",  "",    "",
	"add",   "sub",   "or", "not","",    "",    "",    "",
	"mul",   "div",   "mod","and","ann", "lsl", "lsr", "",
	"add set", "sub set", "or set",  "not set", "", "", "", "",
	"mul set", "div set", "mod set", "and set", "ann set", "lsl set", "lsr set", "",
	";",  ":", "deref", ",", "bool not", "bool and", "bool or",  "bool not",
	"set", "inc", "dec",
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
	if (node->kind == AST_CONST) {
		return node->ival;
	} else if (node->kind == AST_SYMBOL) {
		if (node->sym->kind != SYM_CONST) {
			error("non-const symbol (%s) in constexpr\n", node->sym->name);
		}
		return node->sym->value;
	} else if (node->kind == AST_BINOP) {
		i32 left = ast_get_const_i32(node->c0);
		i32 right = ast_get_const_i32(node->c1);
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
			error("unsupported const BINOP %s\n", tnames[op]);
		}
	} else if (node->kind == AST_UNOP) {
		i32 left = ast_get_const_i32(node->c0);
		u32 op = node->ival;
		if (op == tPLUS) {
			return left;
		} else if (op == tMINUS) {
			return -left;
		} else if (op == tBANG) {
			return !left;
		} else {
			error("unsupported const UNOP %s\n", tnames[op]);
		}
	} else {
		error("non-const expr (%s)\n", ast_kind[node->kind]);
	}
	return 0;
}

void ast_type_compat(Ast node, Type type) {
	if (node->c0) {
		if (!type_is_compatible(type, node->c0->type)) {
			error("incompatible types");
		}
	}
	if (node->c1) {
		if (!type_is_compatible(type, node->c1->type)) {
			error("incompatible types");
		}
	}
	node->type = type;
}

Ast ast_make_deref(Ast child) {
	Ast node = ast_make_simple(AST_DEREF, 0);
	node->c0 = child;
	node->type = child->type->base;
	return node;
}

Ast ast_require_array_type(Ast node) {
	if (node->type->kind == TYPE_ARRAY) {
		return node;
	}
	if ((node->type->kind == TYPE_POINTER) &&
	    (node->type->base->kind == TYPE_ARRAY)) {
		return ast_make_deref(node);
	}
	error("expected an array");
	return nil;
}

Ast ast_require_struct_type(Ast node) {
	if (node->type->kind == TYPE_RECORD) {
		return node;
	}
	if ((node->type->kind == TYPE_POINTER) &&
	    (node->type->base->kind == TYPE_RECORD)) {
	    	return ast_make_deref(node);
	}
	error("expected a struct");
	return nil;
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
		node = ast_make_const(ctx.num, ctx.type_i32);
	} else if (ctx.tok == tSTR) {
		error("<TODO> string const");
	} else if (ctx.tok == tTRUE) {
		node = ast_make_const(1, ctx.type_bool);
	} else if (ctx.tok == tFALSE) {
		node = ast_make_const(0, ctx.type_bool);
	} else if (ctx.tok == tNIL) {
		node = ast_make_const(0, ctx.type_nil);
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
		node = ast_make_symbol(ctx.ident, sym);
	} else {
		error("invalid expression");
	}
	next();
	return node;
}

Symbol type_find_field(Type type, String name) {
	Symbol field = type->first;
	while (field != nil) {
		if (field->name == name) {
			return field;
		}
		field = field->next;
	}
	error("struct has no such field '%s'", name->text);
	return nil;
}

Ast parse_primary_expr() {
	Ast node = parse_operand();
	while (true) {
		if (ctx.tok == tOPAREN) {
			next();
			//VALIDATE as func or ptr-to-func
			if (node->kind != AST_SYMBOL) {
				error("cannot call '%s'", ast_kind[node->kind]);
			}
			if (node->sym->kind != SYM_FUNC) {
				error("cannot call non-function '%s'", node->sym->name->text);
			}
			u32 n = 0;

			Ast call = ast_make_simple(AST_CALL, 0);
			call->c0 = node;
			call->type = node->sym->type->base;
			Ast last = call;

			while (ctx.tok != tCPAREN) {
				// VALIDATE compatibility
				Ast param = parse_expr();
				last->c2 = param;
				last = param;
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
			node = ast_require_struct_type(node);
			String name = parse_name("field name");
			Ast dot = ast_make_simple(AST_FIELD, 0);
			dot->c0 = node;
			dot->c1 = ast_make_simple(AST_SYMBOL, 0);
			dot->c1->name = name;
			dot->c1->sym = type_find_field(node->type, name);
			dot->c1->type = dot->c1->sym->type;
			dot->type = dot->c1->type;
			node = dot;
		} else if (ctx.tok == tOBRACK) {
			next();
			node = ast_require_array_type(node);
			Ast index = ast_make_simple(AST_INDEX, 0);
			index->c0 = node;
			index->c1 = parse_expr();
			index->type = node->type->base;
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
	} else if ((op == tMINUS) || (op == tBANG) || (op == tNOT) || (op == tAMP)) {
		u32 op = ctx.tok;
		next();
		Ast node = ast_make_unop(op, parse_unary_expr());
		if (op == tAMP) {
			node->type = type_make_ptr(node->c0->type);
			node->kind = AST_ADDROF;
			node->ival = 0;
		} else if (op == tBANG) {
			ast_type_compat(node, ctx.type_bool);
		} else {
			ast_type_compat(node, ctx.type_i32);
		}
		return node;
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
		ast_type_compat(node, ctx.type_i32);
	}
	return node;
}

Ast parse_add_expr() {
	Ast node = parse_mul_expr();
	while ((ctx.tok & tcMASK) == tcADDOP) {
		u32 op = ctx.tok;
		next();
		node = ast_make_binop(op, node, parse_mul_expr());
		ast_type_compat(node, ctx.type_i32);
	}
	return node;
}

Ast parse_rel_expr() {
	Ast node = parse_add_expr();
	if ((ctx.tok & tcMASK) == tcRELOP) {
		u32 op = ctx.tok;
		next();
		node = ast_make_binop(op, node, parse_add_expr());
		ast_type_compat(node, ctx.type_bool);
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
			ast_type_compat(node, ctx.type_bool);
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
			ast_type_compat(node, ctx.type_bool);
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
	node->c0 = expr;
	node->c1 = block;
	return node;
}

Ast parse_if() {
	// if expr { block }
	Ast node = ast_make_simple(AST_IF, 0);
	Ast ifnode = ast_make_simple(AST_IFELSE, 0);
	node->c0 = ifnode;
	ifnode->c0 = parse_expr();
	require(tOBRACE);
	scope_push(SCOPE_BLOCK, nil);
	ifnode->c1 = parse_block();
	ifnode->c1->sym = scope_pop();
	while (ctx.tok == tELSE) {
		next();
		// ... else ...
		if (ctx.tok == tIF) {
			// ... if expr { block }
			next();
			Ast ifelse = ast_make_simple(AST_IFELSE, 0);
			ifelse->c0 = parse_expr();
			require(tOBRACE);
			scope_push(SCOPE_BLOCK, nil);
			ifelse->c1 = parse_block();
			ifelse->c1->sym = scope_pop();
			ifnode->c2 = ifelse;
			ifnode = ifelse;
		} else {
			// ... { block }
			require(tOBRACE);
			scope_push(SCOPE_BLOCK, nil);
			ifnode->c2 = parse_block();
			ifnode->c2->sym = scope_pop();
			break;
		}
	}
	return node;
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
		node->c0 = parse_expr();
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

	Ast node = nil;
	if (ctx.tok == tASSIGN) {
		next();
		node = ast_make_simple(AST_EXPR, 0);
		node->c0 = ast_make_binop(tASSIGN, ast_make_symbol(name, sym), parse_expr());
		node->c0->type = node->c0->c1->type;
	}
	require(tSEMI);

	return node;
}

void parse_global_var() {
	String name = parse_name("variable name");
	// TODO: allow type inference
	Type type = parse_type(false);

	Symbol sym = symbol_make(SYM_GLOBAL, 0, name, type, ctx.gp);
	symbol_add_global(sym);

	// advance global pointer and round to next workd
	ctx.gp = ctx.gp + type->size;
	ctx.gp = (ctx.gp + 3) & (~3);

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
			// DISCARD expr
			// VALIDATE compatible integer type
			ctx.data[sym->value >> 2] = v;
		}
	}
	require(tSEMI);
}

Ast parse_expr_statement() {
	u32 srcloc = ctx.linenumber;

	Ast node = nil;
	Ast left = parse_expr();
	Ast right = nil;
	if (ctx.tok == tASSIGN) {
		next();
		right = parse_expr();
		node = ast_make_binop(tASSIGN, left, right);
		node->type = node->c1->type;
	} else if ((ctx.tok & tcMASK) == tcAEQOP) {
		u32 op = ctx.tok; // - tADDEQ;
		next();
		right = parse_expr();
		node = ast_make_binop(op, left, right);
		// TODO: type prop, convert x op= y  to x = x op y
	} else if ((ctx.tok & tcMASK) == tcMEQOP) {
		u32 op = ctx.tok; // - tMULEQ;
		next();
		right = parse_expr();
		node = ast_make_binop(op, left, right);
		// TODO: type prop, convert x op= y  to x = x op y
	} else if ((ctx.tok == tINC) || (ctx.tok == tDEC)) {
		u32 op;
		if (ctx.tok == tINC) {
			op = tPLUS;
		} else {
			op = tMINUS;
		}
		next();
		right = ast_make_const(1, ctx.type_i32);
		right = ast_make_binop(op, left, right);
		right->type = ctx.type_i32;
		node = ast_make_binop(tASSIGN, left, right);
		node->type = ctx.type_i32;
		// TODO: check type?
	} else {
		node = left;
	}
	require(tSEMI);

	// wrap in a statement node
	Ast stmt = ast_make_simple(AST_EXPR, 0);
	stmt->c0 = node;
	stmt->srcloc = srcloc;

	return stmt;
}

Ast parse_block() {
	Ast block = ast_make_simple(AST_BLOCK, 0);
	Ast last = block;
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
		// some of these don't always return a node (like local var)
		if (node != nil) {
			last->c2 = node;
			last = node;
		}
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
	fn->type->size = ctx.local_stack;
	return node;
}

Symbol parse_param(String fname, u32 n, Symbol first, Symbol last) {
	String pname = parse_name("parameter name");
	Type ptype = parse_type(false);

	// arrays and structs are always passed as reference parameters
	if ((ptype->kind == TYPE_ARRAY) || (ptype->kind == TYPE_RECORD)) {
		ptype = type_make_ptr(ptype);
	}

	Symbol param = symbol_make(SYM_PARAM, 0, pname, ptype, /* 4 + */ n * 4);

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
		sym = symbol_make(SYM_FUNC, 0, fname, type, 0);
		sym->first = first;
		type->first = first;
		type->sym = sym;
		type->len = n; // parameter count
		symbol_add_global(sym);
	}

	// handle definition if it is one
	if (isdef) {
		Ast node = ast_make_simple(AST_FUNC, 0);
		node->name = fname;
		node->sym = sym;

		// mark as defined and save entry address
		sym->flags |= SYM_IS_DEFINED;
		node->c0 = parse_function_body(sym);
		return node;
	}

	return nil;
}

void parse_type_def() {
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
}

void parse_enum_def() {
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
			// typecheck? discard subtree
		}
		require(tCOMMA);
		symbol_add_global(symbol_make(SYM_CONST, 0, name, ctx.type_i32, val));
		val++;
	}
	require(tCBRACE);
	require(tSEMI);
}

Ast parse_program() {
	Ast program = ast_make_simple(AST_PROGRAM, 0);
	Ast last = program;
	next();
	for (;;) {
		if (ctx.tok == tENUM) {
			next();
			parse_enum_def();
		} else if (ctx.tok == tTYPE) {
			next();
			parse_type_def();
		} else if (ctx.tok == tFUNC) {
			next();
			Ast node = parse_function();
			if (node != nil) {
				last->c2 = node;
				last = node;
			}
		} else if (ctx.tok == tVAR) {
			next();
			parse_global_var();
		} else if (ctx.tok == tEOF) {
			return program;
		} else {
			expected("function, variable, or type definition");
		}
	}
}

void type_dump_compact(FILE* fp, Type type) {
	if (type->kind == TYPE_FUNC) {
		fprintf(fp, "fn(");
		Symbol param = type->first;
		while (param != nil) {
			type_dump_compact(fp, param->type);
			if (param->next != nil) {
				fprintf(fp, ", ");
			}
			param = param->next;
		}
		fprintf(fp, ") -> ");
		type_dump_compact(fp, type->base);
	} else if (type->sym != nil) {
		fprintf(fp, "%s", type->sym->name->text);
	} else if (type->kind == TYPE_ARRAY) {
		fprintf(fp, "[%u]", type->len);
		type_dump_compact(fp, type->base);
	} else if (type->kind == TYPE_RECORD) {
		fprintf(fp, "{...}");
	} else {
		fprintf(fp, "%s", type_kind[type->kind]);
		if ((type->kind == TYPE_POINTER) || (type->kind == TYPE_SLICE)) {
			type_dump_compact(fp, type->base);
		}
	}
}

void ast_dump_syms(FILE* fp, Symbol sym, str tag, u32 indent) {
	while (sym != nil) {
		u32 i = 0;
		while (i < indent) { fprintf(fp, "  "); i++; }
		fprintf(fp, "%s '%s' ", tag, sym->name->text);
		type_dump_compact(fp, sym->type);
		fprintf(fp, "\n");
		sym = sym->next;
	}
}

void ast_dump_rtype(FILE* fp, Symbol sym, u32 indent) {
	u32 i = 0;
	while (i < indent) { fprintf(fp, "  "); i++; }
	fprintf(fp, "Returns ");
	type_dump_compact(fp, sym->type->base);
	fprintf(fp, "\n");
}

int _ast_dump(FILE* fp, Ast node, u32 indent, bool dumplist, Ast mark) {
	u32 i = 0;
	i32 r = 0;

	if (mark == node) {
		while (i < indent) { fprintf(fp, ">>"); i++; }
	} else {
		while (i < indent) { fprintf(fp, "  "); i++; }
	}
	indent = indent + 1;

	fprintf(fp, "%s ", ast_kind[node->kind]);
	if (node->kind == AST_SYMBOL) {
		fprintf(fp, "'%s' ", node->name->text);
		if (node->type != nil) {
			type_dump_compact(fp, node->type);
		}
		fprintf(fp, "\n");
	} else if ((node->kind == AST_BINOP) || (node->kind == AST_UNOP)) {
		fprintf(fp, "%s ", tnames[node->ival]);
		if (node->type != nil) {
			type_dump_compact(fp, node->type);
		}
		fprintf(fp, "\n");
	} else if (node->kind == AST_CONST) {
		fprintf(fp, "0x%x ", node->ival);
		if (node->type != nil) {
			type_dump_compact(fp, node->type);
		}
		fprintf(fp, "\n");
	} else {
		if (node->name) {
			fprintf(fp,"'%s' ",node->name->text);
		}
		if (node->type) {
			type_dump_compact(fp, node->type);
			fprintf(fp, " ");
		}
		if (node->ival) {
			fprintf(fp,"%u", node->ival);
		}
		fprintf(fp, "\n");
	}
	if (node->kind == AST_FUNC) {
		ast_dump_syms(fp, node->sym->first, "Param", indent);
		ast_dump_rtype(fp, node->sym, indent);
	}
	if (node->kind == AST_BLOCK) {
		ast_dump_syms(fp, node->sym, "Local", indent);
	}
	if (mark == node) {
		return 1;
	}
	if (node->c0 != nil) {
		if (_ast_dump(fp, node->c0, indent, true, mark)) {
			return 1;
		}
	}
	if (node->c1 != nil) {
		if (_ast_dump(fp, node->c1, indent, true, mark)) {
			return 1;
		}
	}
	if (dumplist) {
		node = node->c2;
		while (node != nil) {
			if (_ast_dump(fp, node, indent, false, mark)) {
				return 1;
			}
			node = node->c2;
		}
	}
	return 0;
}

void ast_dump(FILE* fp, Ast node, Ast mark) {
	_ast_dump(fp, node, 0, true, mark);
}

void ast_dump_node(FILE* fp, Ast node, bool dump_c2) {
	fprintf(fp, "\"%p\" [ label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">\n", node);
	fprintf(fp, "<TR><TD PORT=\"p0\" COLSPAN=\"3\">%s", ast_kind[node->kind]);
	if ((node->kind == AST_BINOP) || (node->kind == AST_UNOP)) {
		fprintf(fp, " %s", txnames[node->ival]);
	} else if (node->kind == AST_CONST) {
		fprintf(fp, " 0x%x", node->ival);
	} else if (node->name != nil) {
		fprintf(fp, " %s", node->name->text);
	} else if (node->ival) {
		fprintf(fp, " %u", node->ival);
	}
	fprintf(fp, "</TD></TR>\n");
	if (node->type) {
		fprintf(fp, "<TR><TD COLSPAN=\"3\">");
		type_dump_compact(fp, node->type);
		fprintf(fp, "</TD></TR>");
	}
	fprintf(fp, "<TR><TD PORT=\"c0\"></TD>"
	        "<TD PORT=\"c1\"></TD>"
	        "<TD PORT=\"c2\"></TD></TR></TABLE>>; ];\n");

	if (node->c0 != nil) {
		ast_dump_node(fp, node->c0, true);
		fprintf(fp, "\"%p\":c0 -> \"%p\" [ ];\n", node, node->c0);
	}
	if (node->c1 != nil) {
		ast_dump_node(fp, node->c1, true);
		fprintf(fp, "\"%p\":c1 -> \"%p\" [ ];\n", node, node->c1);
	}
	if ((node->c2 != nil) && dump_c2) {
		Ast list = node->c2;
		Ast prev = node;
		while (list != nil) {
			ast_dump_node(fp, list, false);
			fprintf(fp, "\"%p\":c2 -> \"%p\":p0 [ ];\n", prev, list);
			list = list->c2;
			prev = prev->c2;
		}
		fprintf(fp, "{ rank=same");
		if (node->c0 != nil) {
			fprintf(fp, " \"%p\"", node->c0);
		}
		if (node->c1 != nil) {
			fprintf(fp, " \"%p\"", node->c1);
		}
		list = node->c2;
		while (list != nil) {
			fprintf(fp, " \"%p\"", list);
			list = list->c2;
		}
		fprintf(fp," }\n");
	}
}

void ast_dump_graph(FILE* fp, Ast node) {
	fprintf(fp,
"digraph g {\n"
"graph [ rankdir=TB; ];\n"
"node [ shape=plain; ];\n"
);
	ast_dump_node(fp, node, false);
	fprintf(fp, "}\n");
}

void ast_dump_graphs(Ast node) {
	node = node->c2;
	while (node != nil) {
		if (node->kind == AST_FUNC) {
			char tmp[256];
			sprintf(tmp, "%s.ast.dot", node->sym->name->text);
			FILE* fp;
			if ((fp = fopen(tmp, "w")) != nil) {
				ast_dump_graph(fp, node);
				fclose(fp);
			}
		}
		node = node->c2;
	}
}

#include "codegen-risc5-simple.c"

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

void binary_write(const char* outname) {
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
	n = 0;
	while (n < ctx.gp) {
		if (write(fd, ctx.data + (n/4), sizeof(u32)) != sizeof(u32)) {
			error("error writing '%s'", outname);
		}
		n += 4;
	}
	close(fd);
}

void listing_write(const char* listfn, const char* srcfn) {
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
#if 1
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
#endif
		risc5dis(n, ins, buf);
		fprintf(fout, "%08x: %08x  %s\n", n, ins, buf);
		n += 4;
	}
	n = 0;
	while (n < ctx.gp) {
		fprintf(fout, "%08x: %08x\n", ctx.pc + n, ctx.data[n >> 2]);
		n += 4;
	}
	fclose(fout);
	if (fin) {
		fclose(fin);
	}
}

// ================================================================

i32 main(int argc, args argv) {
	str outname = "out.bin";
	str lstname = nil;
	str srcname = nil;
	str astname = nil;
	bool dump = false;
	bool scan_only = false;
	bool dump_graphs = false;

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
		} else if (!strcmp(argv[1], "-a")) {
			if (argc < 2) {
				error("option -a requires argument");
			}
			astname = argv[2];
			argc--;
			argv++;
		} else if (!strcmp(argv[1], "-p")) {
			dump = true;
		} else if (!strcmp(argv[1], "-g")) {
			dump_graphs = true;
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
		printf(
"usage:    compiler [ <option> | <sourcefilename> ]*\n"
"\n"
"options:  -o <filename>    binary output (default 'out.bin')\n"
"          -l <filename>    listing output (default none)\n"
"          -a <filename>    dump AST tree\n"
"          -v               trace code generation\n"
"          -s               scan only\n"
"          -p               dump type context\n"
"          -A               abort on error\n");
		return 0;
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

	if (astname != nil) {
		FILE *fp;
		if ((fp = fopen(astname, "w")) != nil) {
			ast_dump(fp, a, nil);
			fclose(fp);
		}
	}

	if (dump_graphs) {
		ast_dump_graphs(a);
	}

	gen_risc5_simple(a);

	binary_write(outname);

	if (lstname != nil) {
		listing_write(lstname, ctx.filename);
	}

	//type_dump_all();

	return 0;
}
