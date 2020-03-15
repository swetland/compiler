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

#include "risc5.h"

#define FNMAXARGS 8

#define nil 0

typedef uint32_t u32;
typedef int32_t i32;
typedef uint8_t u8;

// token classes (tok & tcMASK)
enum {
	tcRELOP = 0x08, tcADDOP = 0x10, tcMULOP = 0x18,
	tcAEQOP = 0x20, tcMEQOP = 0x28, tcMASK = 0xF8
};

typedef enum {
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
	tTYPE, tFUNC, tSTRUCT, tVAR,
	tIF, tELSE, tWHILE,
	tBREAK, tCONTINUE, tRETURN,
	tFOR, tSWITCH, tCASE,
	tTRUE, tFALSE, tNIL,
	tIDN, tNUM, tSTR,
	// used internal to the lexer but never returned
	tSPC, tINV, tDQT, tSQT, tMSC,
} token_t;

char *tnames[] = {
	"<EOF>", "<EOL>", "{",  "}",  "[",   "]",   "(",   ")",
	"==",    "!=",    "<",  "<=", ">",   ">=",  "",    "",
	"+",     "-",     "|",  "^",  "",    "",    "",    "",
	"*",     "/",     "%",  "&",  "&~",  "<<",  ">>",  "",
	"+=",    "-=",    "|=", "^=", "",    "",    "",    "",
	"*=",    "/=",    "%=", "&=", "&~=", "<<=", ">>=", "",
	";",     ":",     ".",  ",",  "~",   "&&",  "||",  "!",
	"=",     "++",    "--",
	"type", "func", "struct", "var",
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
	Scope next;     // next in scope stack
	Object first;   // first object in this scope
	u32 level;      // height in stack (0 == globals, ...)
	u32 save_stack; // previous alloc_stack to restore on pop
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
	String name;
	Type type;
	Object first; // list of...
	Object next;  // link in list
	u32 flags;
	u32 value;
	Fixup fixups; // forward func refs
};

// Object Kind IDs
enum {           // value
	oConst,  // const value
	oGlobal, // static base offset
	oVar,    // frame offset
	oParam,  // frame offset
	oField,  // record offset
	oType,   // type-desc-ptr
	oFunc,   // address
};

// Object Flags
#define ofReadOnly 1
#define ofPublic   2 
#define ofDefined  4
#define ofBuiltin  8 // for builtin functions

// ------------------------------------------------------------------

struct TypeRec {
	u32 kind;
	Type base;    // Pointer-to, Func-return, or Array-elem
	Object obj;   // if we're non-anonymous
	Object first; // list of Params or Fields
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
	tPointer,
	tArray,
	tSlice,
	tRecord,
	tFunc,
	tUndefined,
};

const char* type_id_tab[] = {
	"void", "byte", "bool", "int32", "nil", "*", "[]", "[]",
	"struct", "func", "undef",
};
void print_type(Type t);

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
	iReg,    // regno
	iRegInd, // regno   offset
	iComp,   // relop   regno-a   regno-b
	iFunc,
};

const char* item_id_tab[] = { "Const", "Reg", "RegInd", "Comp", "Func" };
void print_item(Item x);


// Item Flags
#define ifReadOnly 1

// ------------------------------------------------------------------

struct CtxRec {
	const char* source;    // entire source file
	const char* sptr;      // tokenizer source pointer
	const char* filename;  // filename of active source
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
	Object typetab;        // TODO: hashtable
	Scope scope;           // scope stack
	ScopeRec global;

	u32 alloc_global;      // next available global offset

	Object fn;             // function being compiled if non-nil
	u32 spill_stack;       // where to spill temp regs (TODO: dynamic)
	u32 local_stack;       // total stack for all locals (params, vars, tmps)
	u32 alloc_stack;       // where to allocate next var (grows/shrinks as
	                       // we enter/exit scopes)

	Type type_void;
	Type type_byte;
	Type type_bool;
	Type type_int32;
	Type type_nil;
	Type type_string;

	String idn_if;
	String idn_for;
	String idn_var;
	String idn_nil;
	String idn_case;
	String idn_func;
	String idn_else;
	String idn_true;
	String idn_type;
	String idn_break;
	String idn_while;
	String idn_false;
	String idn_switch;
	String idn_struct;
	String idn_return;
	String idn_continue;

	u32 code[8192];
	u32 data[8192];
	u32 pc;

	// if nonzero, target for continue
	u32 pc_continue;

	// The latest pc that a forward branch has been
	// fixed up to.  cannot safely move code at or
	// before this address.
	u32 last_target_pc;

	u32 regbits;

	u32 xref[8192];
};

#define cfVisibleEOL 1
#define cfAbortOnError 2

CtxRec ctx;

bool TRACE_CODEGEN = false;

void gen_trace(const char* fn, Item x, Item y);
void gen_trace_n(const char* fn, u32 n, Item x, Item y);
void gen_trace_code(const char* msg, u32 pc);

// initialize x as appropriate for obj
// (which must be a local or global variable or function)
void gen_item_from_obj(Item x, Object obj);

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

// replace array item x with element item (at idx)
void gen_index(Item x, Item idx);

// consume ptr item, transforming into thing-pointed-at
void gen_deref_ptr(Item x);

// consume a memory-based item, transforming into an address (ptr)
void gen_get_ptr(Item x);

// release any registers or other resources held by this item
void gen_discard(Item val);

// sets up call param #n, consuming val
void gen_param(u32 n, Item val);
void gen_ref_param(u32 n, Item val);

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

String make_string(const char* text, u32 len) {
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

Object make_object(u32 kind, String name, Type type,
	Object first, u32 flags, u32 value) {
	Object obj = malloc(sizeof(ObjectRec));
	obj->kind = kind;
	obj->name = name;
	obj->type = type;
	obj->first = first;
	obj->next = nil;
	obj->flags = flags;
	obj->value = value;
	obj->fixups = nil;
	return obj;
}

Type make_type(u32 kind, Type base, Object obj,
	Object first, u32 len, u32 size) {
	Type type = malloc(sizeof(TypeRec));
	type->kind = kind;
	type->base = base;
	type->obj = obj;
	type->first = first;
	type->len = len;
	type->size = size;
	return type;
}

Object make_var(u32 kind, String name, Type type, u32 flags, u32 value) {
	Object var = malloc(sizeof(ObjectRec));
	var->kind = kind;
	var->name = name;
	var->type = type;
	var->first = nil;
	var->next = nil;
	var->flags = flags;
	var->value = value;
	var->fixups = nil;
	return var;
}

void set_item(Item itm, u32 kind, Type type, u32 r, u32 a, u32 b) {
	itm->kind = kind;
	itm->flags = 0;
	itm->type = type;
	itm->r = r;
	itm->a = a;
	itm->b = b;
}

void copy_item(Item src, Item dst) {
	dst->kind = src->kind;
	dst->flags = src->flags;
	dst->type = src->type;
	dst->r = src->r;
	dst->a = src->a;
	dst->b = src->b;
}

void add_type(Type type, String name) {
	Object obj = make_object(oType, name, type, nil, 0, 0);
	if (type->obj == nil) {
		// only set the the type's object if it is
		// not already set (otherwise aliases will
		// clobber the canonical type names -- yuck!)
		type->obj = obj;
	}
	obj->next = ctx.typetab;
	ctx.typetab = obj;
}

Type setup_type(const char* text, u32 tlen, u32 kind, u32 size) {
	String name = make_string(text, tlen);
	Type type = make_type(kind, nil, nil, nil, 0, size);
	add_type(type, name);
	return type;
}

enum {
	biPrintHex32,
	biPutC,
};

void make_builtin(const char* name, u32 id, Type p0, Type p1, Type rtn);

void init_ctx() {
	memset(&ctx, 0, sizeof(ctx));

	// install built-in basic types
	ctx.type_void    = setup_type("void", 4, tVoid, 0);
	ctx.type_byte    = setup_type("byte", 4, tByte, 1);
	ctx.type_bool    = setup_type("bool", 4, tBool, 1);
	ctx.type_int32   = setup_type("i32",  3, tInt32, 4);
	ctx.type_nil     = setup_type("nil",  3, tNil, 4);
	ctx.type_string  = setup_type("str",  3, tSlice, 8);
	ctx.type_string->base = ctx.type_byte;

	ctx.scope = &(ctx.global);

	make_builtin("_hexout_", biPrintHex32, ctx.type_int32, nil, ctx.type_void);
	make_builtin("_putc_", biPutC, ctx.type_int32, nil, ctx.type_void);

	// pre-intern keywords
	ctx.idn_if       = make_string("if", 2);
	ctx.idn_for      = make_string("for", 3);
	ctx.idn_var      = make_string("var", 3);
	ctx.idn_nil      = make_string("nil", 3);
	ctx.idn_case     = make_string("case", 4);
	ctx.idn_func     = make_string("func", 4);
	ctx.idn_else     = make_string("else", 4);
	ctx.idn_true     = make_string("true", 4);
	ctx.idn_type     = make_string("type", 4);
	ctx.idn_break    = make_string("break", 5);
	ctx.idn_while    = make_string("while", 5);
	ctx.idn_false    = make_string("false", 5);
	ctx.idn_switch   = make_string("switch", 6);
	ctx.idn_struct   = make_string("struct", 6);
	ctx.idn_return   = make_string("return", 6);
	ctx.idn_continue = make_string("continue", 8);

}

bool same_type(Type a, Type b) {
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
		if (!same_type(a1->type, b1->type)) {
			return false;
		}
	}
	if ((a1 != nil) || (b1 != nil)) {
		// mismatched number of parameters or fields
		return false;
	}
	return true;
}

// for assignments, etc
bool compatible_type(Type dst, Type src) {
	if (dst->kind == tInt32) {
		if (src->kind == tByte) {
			return true;
		}
	}
	return same_type(dst, src);
}

void dump_file_line(const char* fn, u32 offset);

void error(const char *fmt, ...) {
	va_list ap;

	fprintf(stderr,"%s:%d: ", ctx.filename, ctx.linenumber);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);

	if (ctx.linenumber > 0) {
		dump_file_line(ctx.filename, ctx.lineoffset);
	}
	fprintf(stderr, "\n");

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
	if (*ctx.sptr == 0) {
		ctx.cc = 0;
	} else {
		ctx.byteoffset++;
		ctx.cc = *ctx.sptr++;
	}
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
	String idn = make_string(ctx.tmp, len);
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
			ctx.xref[ctx.pc / 4] = ctx.linenumber;
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

void printstr() {
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

void print() {
	if (ctx.tok == tNUM) {
		printf("#%u ", ctx.num);
	} else if (ctx.tok == tIDN) {
		printf("@%s ", ctx.tmp);
	} else if (ctx.tok == tEOL) {
		printf("\n");
	} else if (ctx.tok == tSTR) {
		printstr();
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

	// save current stack offset (next local var)
	scope->save_stack = ctx.alloc_stack;

	ctx.scope = scope;
	return scope;
	// XXX lazy scopes
}

void pop_scope() {
	if (ctx.scope->level == 0) {
		error("cannot pop the global scope");
	}

	// restore prev stack offset (next local var)
	ctx.alloc_stack = ctx.scope->save_stack;

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

u32 invert_relop(u32 op) {
	if (op > 5) { abort(); }
	return invert_relop_tab[op];
}
// ================================================================

void parse_expr(Item x);

String parse_name(const char* what) {
	if (ctx.tok != tIDN) {
		error("expected %s, found %s", what, tnames[ctx.tok]);
	}
	String str = ctx.ident;
	next();
	return str;
}

void parse_operand(Item x) {
	if (ctx.tok == tNUM) {
		set_item(x, iConst, ctx.type_int32, 0, ctx.num, 0);
	} else if (ctx.tok == tSTR) {
		error("<TODO> string const");
	} else if (ctx.tok == tTRUE) {
		set_item(x, iConst, ctx.type_bool, 0, 1, 0);
	} else if (ctx.tok == tFALSE) {
		set_item(x, iConst, ctx.type_bool, 0, 0, 0);
	} else if (ctx.tok == tNIL) {
		set_item(x, iConst, ctx.type_nil, 0, 0, 0);
	} else if (ctx.tok == tOPAREN) {
		next();
		parse_expr(x);
		require(tCPAREN);
		return;
	} else if (ctx.tok == tIDN) {
		String str = ctx.ident;
		Object obj = find(str);
		if (obj == nil) {
			error("unknown identifier '%s'", str->text);
		}
		gen_item_from_obj(x, obj);
	}
	next();
}

void dereference(Item x, String name) {
	if (x->kind != iRegInd) {
		error("internal: cannot deref via item kind %u", x->kind);
	}

	if (x->type->kind == tRecord) {
		Object field = x->type->first;
		while (field != nil) {
			if (field->name == name) {
				x->type = field->type;
				x->a = x->a + field->value;
				return;
			}
			field = field->next;
		}
		error("field '%s' does not exist", name->text);
	} else {
		error("internal: cannot deref non-structs");
	}
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
				if (!compatible_type(param->type, y.type)) {
					error("incompatible type for parameter '%s'\n", param->name->text);
				}
				if (param->type->kind == tArray) {
					gen_ref_param(n, &y);
				} else {
					gen_param(n, &y);
				}
				param = param->next;
				n++;
			}
			require(tCPAREN);
			gen_call(x);
		} else if (ctx.tok == tDOT) {
			next();
			String name = parse_name("field name");
			if (x->type->kind == tRecord) {
				dereference(x, name);
			} else if ((x->type->kind == tPointer) &&
				(x->type->base->kind == tRecord)) {
				gen_deref_ptr(x);
				dereference(x, name);
			} else {
				error("can only use '.' with struct or pointer-to-struct");
			}
		} else if (ctx.tok == tOBRACK) {
			next();
			ItemRec y;
			parse_expr(&y);
			require(tCBRACK);
			if (x->type->kind != tArray) {
				error("can only use [] with array");
			}
			if (x->kind != iRegInd) {
				error("internal: cannot index via item kind %u", x->kind);
			}
			if (y.kind == iConst) {
				if (y.a >= x->type->len) {
					error("array index out of range");
				}
				x->a = x->a + y.a * x->type->base->size;
				x->type = x->type->base;
			} else {
				gen_index(x, &y);
			}
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
		parse_unary_expr(x);
		gen_get_ptr(x);
	} else {
		parse_primary_expr(x);
	}
}

void parse_mul_expr(Item x) {
	parse_unary_expr(x);
	while ((ctx.tok & tcMASK) == tcMULOP) {
		u32 mulop = ctx.tok - tSTAR;
		next();
		ItemRec y;
		parse_unary_expr(&y);
		gen_mul_op(mulop, x, &y);
	}
}

void parse_add_expr(Item x) {
	parse_mul_expr(x);
	while ((ctx.tok & tcMASK) == tcADDOP) {
		u32 addop = ctx.tok - tPLUS;
		next();
		ItemRec y;
		parse_mul_expr(&y);
		gen_add_op(addop, x, &y);
	}
}

void parse_rel_expr(Item x) {
	parse_add_expr(x);
	if ((ctx.tok & tcMASK) == tcRELOP) {
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
		error("<TODO> and op");
	}
}

void parse_expr(Item x) {
	parse_and_expr(x);
	while (ctx.tok == tOR) {
		next();
		ItemRec y;
		parse_and_expr(&y);
		error("<TODO> or op");
	}
}

Type find_type(String name) {
	Object obj = ctx.typetab;
	while (obj != nil) {
		if (obj->name == name) {
			return obj->type;
		}
		obj = obj->next;
	}
	return nil;
}

// fwd_ref_ok indicates that an undefined typename
// may be treated as a forward reference.  This is
// only used for pointers (size their size does not
// depend on their target).
Type parse_type(bool fwd_ref_ok);

Type parse_struct_type() {
	Type rectype = make_type(tRecord, nil, nil, nil, 0, 0);
	Object last = nil;
	require(tOBRACE);
	while (true) {
		if (ctx.tok == tCBRACE) {
			next();
			break;
		}
		String name = parse_name("field name");
		Type type = parse_type(false);
		Object field = make_object(oField, name, type, nil, 0, rectype->size);

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
		return make_type(tSlice, parse_type(false), nil, nil, 0, 8);
	} else {
		ItemRec x;
		parse_expr(&x);
		require(tCBRACK);
		if ((x.kind != iConst) || (x.type != ctx.type_int32)) {
			error("array size must be integer constant");
		}
		//XXX check for >0
		Type base = parse_type(false);
		u32 sz = x.a * base->size;
		if (sz < x.a) {
			error("array size overflow");
		}
		return make_type(tArray, base, nil, nil, x.a, sz);
	}
}

Type parse_func_type() {
	error("<TODO> func type");
	return nil;
}

Type parse_type(bool fwd_ref_ok) {
	if (ctx.tok == tSTAR) { // pointer-to
		next();
		return make_type(tPointer, parse_type(true), nil, nil, 0, 4);
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
		Type type = find_type(name);
		if (type == nil) {
			if (fwd_ref_ok) {
				type = make_type(tUndefined, nil, nil, nil, 0, 0);
				add_type(type, name);
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

void parse_block();

void parse_while() {
	ItemRec x;
	u32 save = ctx.pc_continue;
	ctx.pc_continue = ctx.pc; // for backward branch

	parse_expr(&x);
	u32 l1_br_false = gen_branch_cond(&x, false);

	require(tOBRACE);
	push_scope(sLoop, nil);
	parse_block();
	gen_branch_back(ctx.pc_continue);
	pop_scope();

	fixup_branch_fwd(l1_br_false);

	ctx.pc_continue = save;
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
		if (!compatible_type(ctx.fn->type->base, x.type)) {
			error("return types do not match");
		}
		require(tSEMI);
	}
	gen_return(&x);
}

void parse_break() {
	// XXX break-to-labeled-loop support
	gen_branch_fwd();
	Scope scope = find_scope(sLoop);
	if (scope == nil) {
		error("break must be used from inside a looping construct");
	}
	add_scope_fixup(scope);
	require(tSEMI);
}

void parse_continue() {
	// XXX continue-to-labeled-loop support
	if (ctx.pc_continue == 0) {
		error("continue must be used from inside a looping construct");
	}
	gen_branch_back(ctx.pc_continue);
	require(tSEMI);
}

void STORE(u32 val, u32* ptr, u32 n, u32 sz) {
	if (sz == 4) {
		ptr[n >> 2] = val;
	} else if (sz == 1) {
		((u8*)ptr)[n] = val;
	}
}

u32 parse_init_constexpr(Type type) {
	if (type->size > 4) {
		error("<TODO> larger init constexpr types");
	}
	ItemRec x;
	parse_expr(&x);
	if (x.kind != iConst) {
		error("non-constant initializer");
	}
	return x.a;
}

u32 parse_array_init(Object var, u32* data, u32 dmax, u32 sz) {
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
		u32 v = parse_init_constexpr(var->type->base);
		STORE(v, data, n, sz); 
		n += sz;
		if (ctx.tok != tCBRACE) {
			require(tCOMMA);
		}
	}
	return n;
}

void parse_struct_init(Object var, u32* data) {
	memset(data, 0, var->type->size);
	while (true) {
		if (ctx.tok == tCBRACE) {
			next();
			break;
		}
		String name = parse_name("field name");
		Object field = var->type->first;
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
		u32 v = parse_init_constexpr(field->type);
		STORE(v, data, field->value, 4);
		if (ctx.tok != tCBRACE) {
			require(tCOMMA);
		}
	}
}

void parse_local_var() {
	String name = parse_name("variable name");
	// TODO: allow type inference
	Type type = parse_type(false);

	Object lvar = make_var(oVar, name, type, 0, ctx.alloc_stack);
	lvar->next = ctx.scope->first;
	ctx.scope->first = lvar;

	ctx.alloc_stack = ctx.alloc_stack + type->size;
	if (ctx.local_stack < ctx.alloc_stack) {
		ctx.local_stack = ctx.alloc_stack;
	}

	if (ctx.tok == tASSIGN) {
		next();
		ItemRec x, y;
		parse_expr(&x);
		gen_item_from_obj(&y, lvar);
		gen_store(&x, &y);
	}
	require(tSEMI);
}

void parse_global_var() {
	String name = parse_name("variable name");
	// TODO: allow type inference
	Type type = parse_type(false);

	Object gvar = make_var(oGlobal, name, type, 0, ctx.alloc_global);
	gvar->next = ctx.scope->first;
	ctx.scope->first = gvar;
	ctx.alloc_global = ctx.alloc_global + type->size;

	if (ctx.tok == tASSIGN) {
		next();
		if (ctx.tok == tOBRACE) {
			next();
			u32* data = ctx.data + (gvar->value >> 2);
			if (type->kind == tArray) {
				parse_array_init(gvar, data, type->size, type->base->size);
			} else if (type->kind == tRecord) {
				parse_struct_init(gvar, data);
			} else {
				error("cannot initialize this way");
			}
		} else {
			ItemRec x;
			parse_expr(&x);
			if (x.kind != iConst) {
				error("non-constant global initializer");
			}
			//TODO: check type/size compat
			ctx.data[gvar->value >> 2] = x.a;
		}
	}
	require(tSEMI);
}

void parse_expr_statement() {
	ItemRec x;
	parse_expr(&x);
	if (ctx.tok == tASSIGN) {
		next();
		ItemRec y;
		parse_expr(&y);
		gen_store(&y, &x);
	} else if ((ctx.tok & tcMASK) == tcAEQOP) {
		u32 op = ctx.tok - tADDEQ;
		next();
		ItemRec y, z;
		parse_expr(&y);
		copy_item(&x, &z);
		gen_add_op(op, &x, &y);
		gen_store(&x, &z);
	} else if ((ctx.tok & tcMASK) == tcMEQOP) {
		u32 op = ctx.tok - tMULEQ;
		next();
		ItemRec y, z;
		parse_expr(&y);
		copy_item(&x, &z);
		gen_add_op(op, &x, &y);
		gen_store(&x, &z);
	} else if ((ctx.tok == tINC) || (ctx.tok == tDEC)) {
		ItemRec y, z;
		set_item(&y, iConst, ctx.type_int32, 0, 1, 0);
		copy_item(&x, &z);
		if (ctx.tok == tINC) {
			gen_add_op(aADD, &x, &y);
		} else {
			gen_add_op(aSUB, &x, &y);
		}
		gen_store(&x, &z);
		next();
	} else {
		gen_discard(&x);
	}
	require(tSEMI);
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
		} else if (ctx.tok == tCONTINUE) {
			next();
			parse_continue();
		} else if (ctx.tok == tWHILE) {
			next();
			parse_while();
		} else if (ctx.tok == tIF) {
			next();
			parse_if();
		} else if (ctx.tok == tVAR) {
			next();
			parse_local_var();
		} else if (ctx.tok == tSEMI) {
			next();
			// empty statement
		} else {
			parse_expr_statement();
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
	String pname = parse_name("parameter name");
	Type ptype = parse_type(false);
	Object param = make_var(oParam, pname, ptype, 0, 4 + n * 4);

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
	String fname = make_string(name, strlen(name));
	Type type = make_type(tFunc, rtn, nil, nil, 0, 0);
	type->obj = make_object(oFunc, fname, type, nil, ofBuiltin, id);

	if (p0 != nil) {
		Object param = make_var(oParam, make_string("a", 1), p0, 0, 0);
		type->obj->first = param;
		type->first = param;
		type->len = 1;
		if (p1 != nil) {
			param->next = make_var(oParam, make_string("b", 1), p1, 0, 1);
			type->len = 2;
		}
	}
	make_global(type->obj);
}

void parse_function() {
	Object first = nil;
	Object last = nil;
	u32 n = 0;
	String fname = parse_name("funcion name");
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
		if (rettype != obj->type->base) {
			error("func '%s' return type differs from decl", fname->text);
		}
		if (obj->type->len != n) {
			error("func '%s' parameter count differs from decl", fname->text);
		}
		Object pa = first;
		Object pb = obj->type->first;
		u32 i = 1;
		while ((pa != nil) && (pb != nil)) {
			if (!same_type(pa->type, pb->type)) {
				error("func '%s' param %u differs from decl", fname->text, i);
			}
			pa = pa->next;
			pb = pb->next;
		}
	} else {
		// if there was no existing record of this function, create one now
		Type type = make_type(tFunc, rettype, nil, first, n, 0);
		obj = make_object(oFunc, fname, type, first, 0, 0);
		type->obj = obj;
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

void parse_type_def() {
	String name = parse_name("type name");
	Type type = parse_type(false);
	Type prev = find_type(name);
	if (prev == nil) {
		add_type(type, name);
	} else {
		if (prev->kind != tUndefined) {
			error("cannot redefine type '%s'\n", name->text);
		}
		prev->kind = type->kind;
		prev->base = type->base;
		prev->first = type->first;
		prev->len = type->len;
		prev->size = type->size;
		prev->obj->type = type;
		// XXX discard type
	}
	require(tSEMI);
}

void parse_program() {
	next();
	for (;;) {
		if (ctx.tok == tFUNC) {
			next();
			parse_function();
		} else if (ctx.tok == tTYPE) {
			next();
			parse_type_def();
		} else if (ctx.tok == tVAR) {
			next();
			parse_global_var();
		} else if (ctx.tok == tEOF) {
			return;
		} else {
			expected("function, variable, or type definition");
		}
	}
}

// ================================================================

#define tmp_reg_count  4
#define tmp_reg_first  8
#define tmp_reg_last  11

bool is_tmp_reg(u32 n) {
	return (n >= tmp_reg_first) && (n <= tmp_reg_last);
}

u32 get_reg_tmp() {
	u32 n = tmp_reg_first;
	while (n <= tmp_reg_last) {
		if (!(ctx.regbits & (1 << n))) {
			ctx.regbits |= (1 << n);
			return n;
		}
		n++;
	}
	error("cannot allocate register");
	return 0;
}

void put_reg(u32 r) {
	if ((r < tmp_reg_first) || (r > tmp_reg_last)) {
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
	gen_trace_code("", ctx.pc);
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

// parser does not know internal details like "which register is SP"
// so the backend needs to initialize variable objects for it
void gen_item_from_obj(Item x, Object obj) {
	if (obj->kind == oParam) {
		if (obj->type->kind == tArray) {
			// arrays are passed by reference through parameters
			// maybe this should be better represented?
			u32 r = get_reg_tmp();
			emit_mem(LDW, r, SP, obj->value);
			set_item(x, iRegInd, obj->type, r, 0, 0);
		} else {
			set_item(x, iRegInd, obj->type, SP, obj->value, 0);
		}
	} else if (obj->kind == oVar) {
		set_item(x, iRegInd, obj->type, SP, obj->value, 0);
	} else if (obj->kind == oGlobal) {
		set_item(x, iRegInd, obj->type, SB, obj->value, 0);
	} else if (obj->kind == oFunc) {
		set_item(x, iFunc, obj->type, 0, obj->value, 0);
	} else {
		error("unsupported identifier");
	}
	gen_trace("item_from_obj<<<", x, nil);
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
	gen_trace_code("patch_last_load()", (ctx.pc - 4));
	return true;
}

// load the value of an item into a specific register
void gen_load_reg(Item x, u32 r) {
	gen_trace_n("load_reg", r, x, nil);
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
	} else if (x->kind == iRegInd) {
		if (x->type->size == 4) {
			emit_mem(LDW, r, x->r, x->a);
		} else if (x->type->size == 1) {
			emit_mem(LDB, r, x->r, x->a);
		} else {
			error("cannot load a size %u item\n", x->type->size);
		}
		if (x->r != r) {
			put_reg(x->r);
		}
	} else {
		error("gen_load failed (kind %u)", x->kind);
	}
	x->kind = iReg;
	x->r = r;
	x->a = 0;
	x->b = 0;
	gen_trace("load_reg<<<", x, nil);
}

void gen_discard(Item x) {
	gen_trace("discard", x, nil);
	if (x->kind == iReg) {
		put_reg(x->r);
	}
}

// convert an item to value-in-register format
// if it's not already in that format
void gen_load(Item x) {
	if ((x->kind == iRegInd) && is_tmp_reg(x->r)) {
		gen_load_reg(x, x->r);
	} else if (x->kind != iReg) {
		gen_load_reg(x, get_reg_tmp());
	}
}

void gen_store(Item val, Item adr) {
	gen_trace("gen_store", val, adr);
	gen_load(val);
	if (adr->kind == iRegInd) {
		if (adr->type->size == 4) {
			emit_mem(STW, val->r, adr->r, adr->a);
		} else if (adr->type->size == 1) {
			emit_mem(STB, val->r, adr->r, adr->a);
		} else {
			error("cannot store a size %u item\n", adr->type->size);
		}
		put_reg(val->r);
		put_reg(adr->r);
	} else {
		error("gen_store: invalid target");
	}
}

void gen_address_reg(Item x, i32 r) {
	gen_trace_n("address_reg", r, x, nil);
	if (x->kind != iRegInd) {
		error("internal error, wrong kind");
	}
	if (x->a > 0) {
		emit_opi_n(ADD, r, x->r, x->a);
	} else if(r != x->r) {
		emit_op(MOV, r, 0, x->r);
	}
	if (r != x->r) {
		put_reg(x->r);
		x->r = r;
	}
}

// convert RegInd+off to RegInd+0
// migrate to a tmpreg if not already
void gen_address(Item x) {
	if (!is_tmp_reg(x->r)) {
		gen_address_reg(x, get_reg_tmp());
	} else {
		gen_address_reg(x, x->r);
	}
}


void gen_index(Item x, Item idx) {
	gen_trace("index", x, idx);
	gen_address(x);
	u32 sz = x->type->base->size;
	gen_load(idx);
	// OPT: use shifts for powers of two
	if (sz > 1) {
		emit_opi_n(MUL, idx->r, idx->r, sz);
	}
	// TODO: range check
	emit_op(ADD, x->r, x->r, idx->r);
	put_reg(idx->r);
	x->type = x->type->base;
	gen_trace("index<<<", x, nil);
}

void gen_deref_ptr(Item x) {
	gen_trace("deref_ptr", x, nil);
	gen_load(x);
	if (x->kind != iReg) {
		error("internal - ptr deref failed");
	}
	x->type = x->type->base;
	x->kind = iRegInd;
	x->a = 0;
	gen_trace("deref_ptr<<<", x, nil);
}

void gen_get_ptr(Item x) {
	gen_trace("get_ptr", x, nil);
	if (x->kind != iRegInd) {
		error("internal - get ptr failed");
	}
	x->kind = iReg;
	if (x->a != 0) {
		emit_opi_n(ADD, x->r, x->r, x->a);
		x->a = 0;
	}
	// TODO: can we cache these or be sure to recycle them later?
	x->type = make_type(tPointer, x->type, nil, nil, 0, 4);
	gen_trace("get_ptr<<<", x, nil);
}

u32 gen_branch_cond(Item x, bool sense) {
	gen_trace_n("branch_cond", sense, x, nil);
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
	gen_trace("branch_fwd", nil, nil);
	emit_bi(AL, 0);
	return ctx.pc - 4;
}

void gen_branch_back(u32 addr) {
	gen_trace_n("branch_back", addr, nil, nil);
	emit_bi(AL, (addr - ctx.pc - 4) >> 2);
}

void gen_return(Item x) {
	gen_trace("return", x, nil);
	if (x->type != ctx.type_void) {
		gen_load_reg(x, R0);
	}
	emit_bi(AL, 0);
	add_scope_fixup(find_scope(sFunc));
}

void gen_ref_param(u32 n, Item val) {
	gen_trace_n("ref_param", n, val, nil);
	if (n > 7) {
		error("gen_ref_param - too many parameters");
	}
	gen_address_reg(val, n);
}

void gen_param(u32 n, Item val) {
	gen_trace_n("param", n, val, nil);
	if (n > 7) {
		error("gen_param - too many parameters");
	}
	gen_load_reg(val, n);
}

void gen_builtin(u32 id) {
	gen_trace_n("builtin", id, nil, nil);
	if (id == biPrintHex32) {
		emit_mov(1, 0xFFFF0000);    // MOV R1, IOBASE
		emit_mem(STW, 0, 1, 0x104); // SW R0, [R1, 0x104]
	} else if (id == biPutC) {
		emit_mov(1, 0xFFFF0000);    // MOV R1, IOBASE
		emit_mem(STW, 0, 1, 0x108); // SW R0, [R1, 0x108]
	} else {
		error("unknown builtin function");
	}
}

void gen_save_regs() {
	gen_trace("save_regs", nil, nil);
	u32 n = tmp_reg_first;
	u32 off = ctx.spill_stack;
	while (n <= tmp_reg_last) {
		if (ctx.regbits & (1 << n)) {
			emit_mem(STW, n, SP, off);
			off += 4;
		}
		n++;
	}
}

void gen_restore_regs() {
	gen_trace("restore_regs", nil, nil);
	u32 n = tmp_reg_first;
	u32 off = ctx.spill_stack;
	while (n <= tmp_reg_last) {
		if (ctx.regbits & (1 << n)) {
			emit_mem(LDW, n, SP, off);
			off += 4;
		}
		n++;
	}
}

void gen_call(Item x) {
	gen_trace("call", x, nil);
	gen_save_regs();
	if (x->type->obj->flags & ofBuiltin) {
		// OPT: not all builtins will require save/restore regs
		gen_builtin(x->type->obj->value);
	} else if (x->type->obj->flags & ofDefined) {
		u32 fnpc = x->type->obj->value;
		emit_bi(AL|L, (fnpc - ctx.pc - 4) >> 2);
	} else {
		emit_bi(AL|L, 0);
		add_object_fixup(x->type->obj);
	}
	gen_restore_regs();

	// item becomes the return value
	x->type = x->type->base;
	if (x->type == ctx.type_void) {
		x->kind = iConst;
	} else {
		x->kind = iReg;
		x->r = R0;
		// OPT if we tracked r0 usage we could
		// avoid moving from R0 to a tmp reg here
		gen_load_reg(x, get_reg_tmp());
	}
	x->a = 0;
	x->b = 0;
}

void gen_add_op(u32 op, Item x, Item y) {
	gen_trace_n("add_op", op, x, y);
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
	gen_trace_n("mul_op", op, x, y);
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
	gen_trace_n("rel_op", op, x, y);
	gen_load(x);
	gen_load(y);

	// fuse x and y items into the new Comparison Item x
	x->kind = iComp;
	x->a = x->r;
	x->b = y->r;
	x->r = op;
}

void gen_unary_op(u32 op, Item x) {
	gen_trace_n("unary_op", op, x, nil);
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
	if ((addr == ctx.pc - 4) && (ctx.last_target_pc < ctx.pc)) {
		// if the branch to be patched is the
		// instruction just previously emitted, we
		// can simply erase it
		//
		// provided it's safe to do so (no branches are
		// already arriving here...)
		ctx.pc -= 4;
	} else {
		u32 off = (ctx.pc - addr - 4) >> 2;
		u32 ins = ctx.code[addr >> 2] & 0xFF000000;
		ctx.code[addr >> 2] = ins | (off & 0x00FFFFFF);
		gen_trace_code("fixup_branch_fwd()", addr);
	}

	// note that we have forward branches to this pc
	// (to prevent optimizations from moving code
	// behind them)
	ctx.last_target_pc = ctx.pc;
}

void fixup_branches_fwd(Fixup fixup) {
	while (fixup != nil) {
		fixup_branch_fwd(fixup->pc);
		fixup = fixup->next;
	}
}

void gen_prologue(Object fn) {
	gen_trace("prologue", nil, nil);
	fn->value = ctx.pc;
	emit_opi(SUB, SP, SP, 4 + fn->type->len * 4);
	emit_mem(STW, LR, SP, 0);

	Object param = fn->first;
	u32 off = 4;
	u32 r = 0;
	while (param != nil) {
		emit_mem(STW, r, SP, off);
		r++;
		off += 4;
		param = param->next;
	}
	// set aside space to spill temporary regs
	// OPT: would be nice to only reserve space we need
	ctx.spill_stack = off;
	ctx.local_stack = off + 4 * tmp_reg_count;
	ctx.alloc_stack = ctx.local_stack;
}

void gen_epilogue(Object fn) {
	gen_trace("epilogue", nil, nil);
	if (ctx.regbits) {
		u32 n = tmp_reg_first;
		while (n <= tmp_reg_last) {
			if (ctx.regbits & (1 << n)) {
				fprintf(stderr, "R%u ", n);
			}
			n++;
		}
		fprintf(stderr,"\n");
		error("internal - registers reserved in epilogue");
	}
	if (ctx.local_stack > 0xFFFF) {
		error("using too much stack");
	}

	// patch prologue
	u32 ins = ctx.code[fn->value / 4];
	ctx.code[fn->value / 4] = (ins & 0xFFFF0000) | ctx.local_stack;
	gen_trace_code("patch_prologue()", fn->value);

	// emit epilogue
	emit_mem(LDW, LR, SP, 0);
	emit_opi(ADD, SP, SP, ctx.local_stack);
	emit_br(AL, LR);
}


void gen_start() {
	gen_trace("start", nil, nil);
	emit_mov(SB, 0); // placeholder SB load
	emit_bi(AL, 0);  // placeholder branch to init
}

void gen_end() {
	gen_trace("end", nil, nil);
	String str = make_string("start", 5);
	Object obj = find(str);
	while (obj != nil) {
		if (obj->type->kind != tFunc) {
			error("'start' is not a function\n");
		}
		if (obj->first != nil) {
			error("'start' must have no parameters\n");
		}
		// patch static base load to after the last instruction
		ctx.code[0] |= ctx.pc;
		// patch branch-to-start
		ctx.code[1] |= (obj->value - 8) >> 2;
		// TODO: copy ro globals after code
		// TODO: SB should neg-index into ro, pos-index into rw
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
	n = 0;
	while (n < ctx.alloc_global) {
		if (write(fd, ctx.data + (n/4), sizeof(u32)) != sizeof(u32)) {
			error("error writing '%s'", outname);
		}
		n += 4;
	}
	close(fd);
}

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
	n = 0;
	while (n < ctx.alloc_global) {
		fprintf(fout, "%08x: %08x\n", ctx.pc + n, ctx.data[n >> 2]);
		n += 4;
	}
	fclose(fout);
	if (fin) {
		fclose(fin);
	}
}

// ================================================================


void dump_file_line(const char* fn, u32 offset) {
	int fd = open(fn, O_RDONLY);
	if (fd < 0) {
		return;
	}
	if (lseek(fd, offset, SEEK_SET) != offset) {
		close(fd);
		return;
	}
	char line[256];
	int r = read(fd, line, 255);
	if (r > 0) {
		line[r] = 0;
		int n = 0;
		while (n < r) {
			if (line[n] == '\n') {
				line[n] = 0;
				break;
			}
			n++;
		}
		fprintf(stderr, "\n%s", line);
	}
	close(fd);
}

void dump_type(Type type, bool use_short_name) {
	if (use_short_name && (type->obj != nil)) {
		printf("%s", type->obj->name->text);
	} else if (type->kind == tArray) {
		printf("[%u]", type->len);
		dump_type(type->base, true);
	} else if (type->kind == tRecord) {
		printf("struct {\n");
		Object field = type->first;
		while (field != nil) {
			printf("    %s ", field->name->text);
			dump_type(field->type, true);
			printf(", // off=%u, sz=%u\n", field->value, field->type->size);
			field = field->next;
		}
		printf("}");
	} else {
		printf("%s", type_id_tab[type->kind]);
		if ((type->kind == tPointer) || (type->kind == tSlice)) {
			dump_type(type->base, true);
		}
	}
}

void dump_context() {
	Object obj = ctx.typetab;
	while (obj != nil) {
		printf("type %s ", obj->name->text);
		dump_type(obj->type, false);
		printf("; // sz=%u\n", obj->type->size);
		obj = obj->next;
	}
}

void print_type(Type type) {
	if (type->kind == tArray) {
		fprintf(stderr, "[%u]", type->len);
		print_type(type->base);
	} else if (type->kind == tPointer) {
		fprintf(stderr, "*");
		print_type(type->base);
	} else if (type->kind == tRecord) {
		if (type->obj != nil) {
			fprintf(stderr, "{}%s", type->obj->name->text);
		} else {
			fprintf(stderr, "{}\n");
		}
	} else {
		fprintf(stderr, "%s", type_id_tab[type->kind]);
	}
}

void print_item(Item x) {
	fprintf(stderr, "<%s: r=%d,a=%d,b=%d,t=",
		item_id_tab[x->kind], x->r, x->a, x->b);
	print_type(x->type);
	fprintf(stderr, ">");
}

void gen_trace(const char* fn, Item x, Item y) {
	if (TRACE_CODEGEN) {
		fprintf(stderr, "gen_%-17s", fn);
		if (x != nil) {
			print_item(x);
		}
		if (y != nil) {
			fprintf(stderr, ", ");
			print_item(y);
		}
		fprintf(stderr, "\n");
	}
}
void gen_trace_n(const char* fn, u32 n, Item x, Item y) {
	if (TRACE_CODEGEN) {
		fprintf(stderr, "gen_%-17s0x%x, ", fn, n);
		if (x != nil) {
			print_item(x);
		}
		if (y != nil) {
			fprintf(stderr, ", ");
			print_item(y);
		}
		fprintf(stderr, "\n");
	}
}
void gen_trace_code(const char* msg, u32 pc) {
	if (TRACE_CODEGEN) {
		u32 ins = ctx.code[pc >> 2];
		char buf[64];
		risc5dis(pc, ins, buf);
		fprintf(stderr, "gen_code             '%08x: %08x  %s' %s\n", pc, ins, buf, msg);
	}
}
// ================================================================

int main(int argc, char **argv) {
	const char *outname = "out.bin";
	const char *lstname = nil;
	const char *srcname = nil;
	bool dump = false;
	bool scan_only = false;

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
		} else if (!strcmp(argv[1], "-p")) {
			dump = true;
		} else if (!strcmp(argv[1], "-v")) {
			TRACE_CODEGEN = true;
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

	load(srcname);
	ctx.linenumber = 1;
	ctx.lineoffset = 0;
	// prime the lexer
	scan();

	if (scan_only) {
		ctx.flags |= 1;
		while (true) {
			next();
			print();
			if (ctx.tok == tEOF) {
				return 0;
			}
		}
	}

	gen_start();
	parse_program();
	gen_end();
	gen_write(outname);
	if (lstname != nil) {
		gen_listing(lstname, ctx.filename);
	}
	if (dump) {
		dump_context();
	}

	return 0;
}
