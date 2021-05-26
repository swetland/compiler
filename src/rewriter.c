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

#define nil 0

void error(const char *fmt, ...);

typedef uint32_t u32;
typedef int32_t i32;
typedef uint8_t u8;

enum { FNMAXARGS = 8, };

// token classes (tok & tcMASK)
enum {
	tcRELOP = 0x08, tcADDOP = 0x10, tcMULOP = 0x18,
	tcAEQOP = 0x20, tcMEQOP = 0x28, tcMASK = 0xF8,
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
	tASSIGN, tINC, tDEC, tHASH, tARROW,
	// Keywords
	tTYPEDEF, tSTRUCT, tVAR, tENUM,
	tIF, tELSE, tWHILE,
	tBREAK, tCONTINUE, tRETURN,
	tFOR, tSWITCH, tCASE,
	tTRUE, tFALSE, tNIL,
	tIDN, tNUM, tSTR, tTYPE,
	// used internal to the lexer but never returned
	tSPC, tINV, tDQT, tSQT, tMSC, tTAB
} token_t;

char *tnames[] = {
	"<EOF>", "<EOL>", "{",  "}",  "[",   "]",   "(",   ")",
	"==",    "!=",    "<",  "<=", ">",   ">=",  "",    "",
	"+",     "-",     "|",  "^",  "",    "",    "",    "",
	"*",     "/",     "%",  "&",  "&~",  "<<",  ">>",  "",
	"+=",    "-=",    "|=", "^=", "",    "",    "",    "",
	"*=",    "/=",    "%=", "&=", "&~=", "<<=", ">>=", "",
	";",     ":",     ".",  ",",  "~",   "&&",  "||",  "!",
	"=",     "++",    "--", "#", "->",
	"typedef", "struct", "var", "enum",
	"if", "else", "while",
	"break", "continue", "return",
	"for", "switch", "case",
	"true", "false", "nil",
	"<ID>", "<NUM>", "<STR>", "<TYPE>",
	"<SPC>", "<INV>", "<DQT>", "<SQT>", "<MSC>", "<TAB>"
};

u8 lextab[256] = {
	tEOF, tINV, tINV, tINV, tINV, tINV, tINV, tINV,
	tINV, tTAB, tEOL, tSPC, tINV, tSPC, tINV, tINV,
	tINV, tINV, tINV, tINV, tINV, tINV, tINV, tINV,
	tINV, tINV, tINV, tINV, tINV, tINV, tINV, tINV,
	tSPC, tBANG, tDQT, tHASH, tMSC, tPERCENT, tAMP, tSQT,
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

typedef struct StringRec* String;
typedef struct StringRec StringRec;


struct StringRec {
	String next;
	u32 len;
	u32 kind;
	char text[0];
};

#define KindNone 0
#define KindType 1
#define KindKeyword 2

// ------------------------------------------------------------------

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
};

struct CtxRec ctx;

String make_string(const char* text, u32 len, u32 kind) {
	// OPT obviously this wants to be a hash table
	String str = ctx.strtab;
	while (str != nil) {
		if ((str->len == len) && (memcmp(text, str->text, len) == 0)) {
			if ((str->kind != kind) && (kind != tIDN)) {
				error("string '%s' already kind %u\n", str->text, str->kind);
			}
			return str;
		}
		str = str->next;
	}

	str = malloc(sizeof(StringRec) + len + 1);
	str->len = len;
	str->kind = kind;
	memcpy(str->text, text, len);
	str->text[len] = 0;
	str->next = ctx.strtab;
	ctx.strtab = str;

	return str;
}

void make_keyword(const char* text, u32 tok) {
	make_string(text, strlen(text), tok);
}

void make_type(const char* text) {
	make_string(text, strlen(text), tTYPE);
}

int is_type(String str) {
	return str->kind == 0x1000;
}

void init_ctx() {
	memset(&ctx, 0, sizeof(ctx));

	make_type("u8");
	make_type("u32");
	make_type("i32");
	make_type("void");
	make_type("str");
	make_type("strptr");
	make_type("bool");
	make_type("token_t");

	// pre-intern keywords
	make_keyword("if", tIF);
	//make_keyword("for", tFOR);
	make_keyword("nil", tNIL);
	make_keyword("else", tELSE);
	make_keyword("enum", tENUM);
	make_keyword("true", tTRUE);
	make_keyword("false", tFALSE);
	make_keyword("typedef", tTYPEDEF);
	make_keyword("break", tBREAK);
	make_keyword("while", tWHILE);
	make_keyword("struct", tSTRUCT);
	make_keyword("return", tRETURN);
	make_keyword("continue", tCONTINUE);
}

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

void error(const char *fmt, ...) {
	va_list ap;

	fprintf(stderr,"\n\n%s:%d: ", ctx.filename, ctx.linenumber);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	if (ctx.linenumber > 0) {
		dump_file_line(ctx.filename, ctx.lineoffset);
	}
	fprintf(stderr, "\n\n");
	exit(1);
}

void load(const char* filename) {
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
			nc = scan();
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
			ctx.tmp[n] = 0;
			error("constant string too large '%s'", ctx.tmp);
		}
	}
	ctx.tmp[n] = 0;
	return tSTR;
}

token_t scan_keyword(u32 len) {
	ctx.tmp[len] = 0;
	String idn = make_string(ctx.tmp, len, tIDN);
	ctx.ident = idn;

	return idn->kind;
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

token_t _next(int ws) {
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
			else if (nc == '>') { tok = tARROW; nc = scan(); }
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
				if (ws) printf("/");
				// comment -- consume until EOL or EOF
				while ((nc != '\n') && (nc != 0)) {
					if (ws) printf("%c", nc);
					nc = scan();
				}
				continue;
			}
		} else if (tok == tHASH) {
			while ((nc != '\n') && (nc != 0)) {
				nc = scan();
			}
			continue;
		} else if (tok == tEOL) {
			ctx.linenumber++;
			ctx.lineoffset = ctx.byteoffset;
			//ctx.xref[ctx.pc / 4] = ctx.linenumber;
			//if (ctx.flags & cfVisibleEOL) {
			//	return tEOL;
			//}
			if (ws) printf("\n");
			continue;
		} else if (tok == tSPC) {
			if (ws) printf(" ");
			continue;
		} else if (tok == tTAB) {
			if (ws) printf("\t");
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
	return (ctx.tok = _next(1));
}

token_t nextq() {
	return (ctx.tok = _next(0));
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
		printf("%u ", ctx.num);
	} else if (ctx.tok == tIDN) {
		printf("@%s ", ctx.tmp);
	} else if (ctx.tok == tTYPE) {
		printf("@@%s ", ctx.tmp);
	} else if (ctx.tok == tEOL) {
		printf("\n");
	} else if (ctx.tok == tSTR) {
		printstr();
	} else {
		printf("%s ", tnames[ctx.tok]);
	}
}

void emit() {
	if (ctx.tok == tNUM) {
		printf("%u", ctx.num);
	} else if (ctx.tok == tIDN) {
		printf("%s", ctx.tmp);
	} else if (ctx.tok == tTYPE) {
		printf("%s", ctx.tmp);
	} else if (ctx.tok == tSTR) {
		printstr();
	} else {
		printf("%s", tnames[ctx.tok]);
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
	emit();
	next();
}

void requireq(token_t tok) {
	expect(tok);
	nextq();
}

void parse_enum() {
	printf("enum ");
	require(tOBRACE);
	while (ctx.tok != tCBRACE) {
		emit();
		next();
	}
	require(tCBRACE);
	require(tSEMI);
}

void parse_expr() {
	while (ctx.tok != tCPAREN) {
		if (ctx.tok == tOPAREN) {
			printf("(");
			next();
			parse_expr();
		} else {
			emit();
			next();
		}
	}
	require(tCPAREN);
	printf(")");
}

void parse_block() {
	unsigned start = 1;
	while (ctx.tok != tCBRACE) {
		if (start) {
			start = 0;
		}
		if (ctx.tok == tOPAREN) {
			printf("(");
			next();
			parse_expr();
		} else if (ctx.tok == tOBRACE) {
			printf("{");
			next();
			parse_block();
			start = 1;
		} else if (ctx.tok == tSEMI) {
			printf(";");
			next();
			start = 1;
		} else {
			emit();
			next();
		}
	}
	require(tCBRACE);
}
		
void parse_func(String type, String name) {
	printf("func %s(", name->text);
	while (ctx.tok != tCPAREN) {
		String pt = ctx.ident;
		nextq();
		String pn = ctx.ident;
		nextq();
		printf("%s %s", pn->text, pt->text);
		if (ctx.tok == tCOMMA) {
			printf(",");
			next();
		}
	}
	require(tCPAREN);
	if (ctx.tok == tSEMI) {
		printf(" %s;", type->text);
		next();
		return;
	}
	printf("%s ", type->text);
	require(tOBRACE);
	parse_block();
}

void parse_array(String type, String name) {
	u32 n = ctx.num;
	if (ctx.tok == tCBRACK) {
		next();
		printf("var %s []%s", name->text, type->text);
	} else {
		requireq(tNUM);
		requireq(tCBRACK);
		printf("var %s [%u]%s", name->text, n, type->text);
	}
	if (ctx.tok == tSEMI) {
		printf(";");
		next();
	} else if (ctx.tok == tASSIGN) {
		printf(" =");
		next();
		require(tOBRACE);
		while (ctx.tok != tCBRACE) {
			emit();
			next();
		}
		require(tCBRACE);
		require(tSEMI);
	} else {
		error("LOST");
	}
}

void parse_program() {
	next();

	for (;;) {
		if (ctx.tok == tENUM) {
			nextq();
			parse_enum();
		} else if (ctx.tok == tTYPE) {
			String type = ctx.ident;
			requireq(tTYPE);
			String ident = ctx.ident;
			requireq(tIDN);
			if (ctx.tok == tOBRACK) { // array
				next();
				parse_array(type, ident);
			} else if(ctx.tok == tOPAREN) { // func
				next();
				parse_func(type, ident);
			} else { // global var
				printf("var %s %s", ident->text, type->text);
				while (ctx.tok != tSEMI) {
					emit();
					next();
				}
				require(tSEMI);
			}
		} else if (ctx.tok == tTYPEDEF) {
			nextq();
			requireq(tSTRUCT);
			String t1 = ctx.ident;
			nextq();
			if (ctx.tok == tSTAR) {
				nextq();
				String t2 = ctx.ident;
				next();
				t2->kind = tTYPE;
				printf("type %s *%s", t2->text, t1->text);
			} else {
				next();
				t1->kind = tTYPE;
				printf("type %s", t1->text);
			}
			require(tSEMI);
		} else if (ctx.tok == tSTRUCT) {
			nextq();
			String n = ctx.ident;
			nextq();
			n->kind = tTYPE;
			printf("type %s struct ", n->text);
			require(tOBRACE);
			while (ctx.tok != tCBRACE) {
				emit();
				next();
			}
			require(tCBRACE);
			require(tSEMI);
		} else if (ctx.tok == tEOF) {
			return;
		} else {
			expected("top level entity");
		}
	}
}

// ================================================================

int main(int argc, char **argv) {
	const char *outname = "out.c";
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
		} else if (!strcmp(argv[1], "-p")) {
			dump = true;
		} else if (!strcmp(argv[1], "-s")) {
			scan_only = true;
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

	parse_program();

	return 0;
}
