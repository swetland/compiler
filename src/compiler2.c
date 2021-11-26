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
typedef char* str;
typedef char** args;
typedef uint32_t* u32ptr;
#endif

// ------------------------------------------------------------------
// structures

typedef struct StringRec StringRec;
typedef struct CtxRec CtxRec;

typedef struct StringRec* String;
typedef struct CtxRec* Ctx;

struct StringRec {
	String next;
	u32 len;
	char text[0];
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
};

enum {
	cfVisibleEOL   = 1,
	cfAbortOnError = 2,
	cfTraceCodeGen = 3,
};

CtxRec ctx;

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
}

void dump_file_line(const char* fn, u32 offset);

#if C
void error(const char *fmt, ...) {
#else
void error(const char *fmt) {
#endif
	va_list ap;

	fprintf(stderr,"%s:%d: ", ctx.filename, ctx.linenumber);
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

#if 0
void parse_expr(Item x);

String parse_name(const char* what) {
	if (ctx.tok != tIDN) {
		error("expected %s, found %s %u", what, tnames[ctx.tok], ctx.tok);
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
	} else {
		error("invalid expression");
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
				if (ctx.tok == tCPAREN) {
					error("too few parameters for %s()", x->type->obj->name->text);
				}
				if (n != 0) {
					require(tCOMMA);
				}
				ItemRec y;
				parse_expr(&y);
				if (!compatible_type(param->type, y.type, &y)) {
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
	if (ctx.tok == tAND) {
		Scope outer = push_scope(sBlock, nil);

		// if !x goto nope
		gen_branch_cond(x, false);
		add_scope_fixup(outer);

		while (ctx.tok == tAND) {
			next();
			ItemRec y;

			// if !y goto nope
			parse_rel_expr(&y);
			gen_branch_cond(&y, false);
			add_scope_fixup(outer);
		}
		// res = true, goto done
		gen_load_bool1(x, true);
		u32 l0_true = gen_branch_fwd();

		// nope: res = false
		pop_scope();
		gen_load_bool2(x);

		// done:
		fixup_branch_fwd(l0_true);
	}
}

void parse_expr(Item x) {
	parse_and_expr(x);
	if (ctx.tok == tOR) {
		Scope outer = push_scope(sBlock, nil);

		// if x goto yup
		gen_branch_cond(x, true);
		add_scope_fixup(outer);

		while (ctx.tok == tOR) {
			next();
			ItemRec y;

			// if y goto yup
			parse_and_expr(&y);
			gen_branch_cond(&y, true);
			add_scope_fixup(outer);
		}
		// res = false, goto done
		gen_load_bool1(x, false);
		u32 l0_false = gen_branch_fwd();

		// yup: res = true
		pop_scope();
		gen_load_bool2(x);

		// done:
		fixup_branch_fwd(l0_false);
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
		if (!compatible_type(ctx.fn->type->base, x.type, &x)) {
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

u32 parse_array_init(Object var, u32ptr data, u32 dmax, u32 sz) {
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

void parse_struct_init(Object var, u32ptr data) {
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
	ctx.alloc_global = (ctx.alloc_global + 3) & (~3); // round to word

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
		if (!compatible_type(x.type, y.type, &x)) {
			error("incompatible type in assignment");
		}
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
	String fname = string_make(name, strlen(name));
	Type type = make_type(tFunc, rtn, nil, nil, 0, 0);
	type->obj = make_object(oFunc, fname, type, nil, ofBuiltin, id);

	if (p0 != nil) {
		Object param = make_var(oParam, string_make("a", 1), p0, 0, 0);
		type->obj->first = param;
		type->first = param;
		type->len = 1;
		if (p1 != nil) {
			param->next = make_var(oParam, string_make("b", 1), p1, 0, 1);
			type->len = 2;
		}
	}
	make_global(type->obj);
}

void parse_function() {
	Object first = nil;
	Object last = nil;
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

void parse_enum_def() {
	require(tOBRACE);
	u32 val = 0;
	while (ctx.tok != tCBRACE) {
		String name = parse_name("enum tag name");
		Object obj = find(name);
		if (obj != nil) {
			error("cannot redefine %s as enum tag\n", name->text);
		}
		if (ctx.tok == tASSIGN) {
			next();
			val = parse_init_constexpr(ctx.type_int32);
		}
		require(tCOMMA);
		obj = make_var(oConst, name, ctx.type_int32, 0, val);
		obj->next = ctx.scope->first;
		ctx.scope->first = obj;
		val++;
	}
	require(tCBRACE);
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
		} else if (ctx.tok == tENUM) {
			next();
			parse_enum_def();
		} else if (ctx.tok == tEOF) {
			return;
		} else {
			expected("function, variable, or type definition");
		}
	}
}
#endif

// ================================================================

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

	return 0;
}
