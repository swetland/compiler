// Copyright 2020, Brian Swetland <swetland@frotz.net>
// Licensed under the Apache License, Version 2.0.

// ------------------------------------------------------------------

// R0 is used for returns
// R11 is for instruction combo temporary
// FP, SB, SP, LR have fixed uses
enum {
	tmp_reg_count = 10,
	tmp_reg_first = 1,  // 8,
	tmp_reg_last  = 10, // 11,
};

bool is_tmp_reg(u32 n) {
	return (n >= tmp_reg_first) && (n <= tmp_reg_last);
}

u32 regbits;

u32 get_reg_tmp() {
	u32 n = tmp_reg_first;
	while (n <= tmp_reg_last) {
		if (!(regbits & (1 << n))) {
			regbits |= (1 << n);
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
	if (!(regbits & (1 << r))) {
		error("freeing non-allocated register %u\n", r);
	}
	regbits = regbits & (~(1 << r));
}

void emit(u32 ins) {
	ctx.code[ctx.pc / 4] = ins;
	//gen_trace_code("", ctx.pc);
	ctx.pc = ctx.pc + 4;
}

enum {
	R0 = 0, R1 = 1, R2 = 2, R3 = 3, R4 = 4, R5 = 5, R6 = 6, R7 = 7,
	R8 = 9, R9 = 9, R10 = 10, R11 = 11, FP = 12, SB = 13, SP = 14, LR = 15,
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
void emit_mov(u32 dst, u32 src) {
	if (dst != src) {
		emit_op(MOV, dst, 0, src);
	}
}

// mov (load immediate) using the appropriate one or two
// instruction form for the immediate argument
void emit_movi(u32 a, u32 n) {
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

#if 0
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
#endif

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

// ------------------------------------------------------------------

void gen_branch_back(u32 op, u32 addr) {
	emit_bi(op, (addr - ctx.pc - 4) >> 2);
}

void gen_branch_fwd(u32 op, Fixup list) {
	fixup_add_list(list, ctx.pc);
	emit_bi(op, 0);
}

void gen_branch_sym(u32 op, Symbol sym) {
	if (sym->flags & SYM_IS_PLACED) {
		gen_branch_back(op, sym->value);
	} else {
		fixup_add_sym(sym, ctx.pc);
		emit_bi(op, 0);
	}
}

// patch branch instruction at addr to
// branch to current pc
void fixup_branch_fwd(u32 addr) {
	u32 off = (ctx.pc - addr - 4) >> 2;
	u32 ins = ctx.code[addr >> 2] & 0xFF000000;
	ctx.code[addr >> 2] = ins | (off & 0x00FFFFFF);
}

void fixup_branches_fwd(Fixup fixup) {
	while (fixup != nil) {
		fixup_branch_fwd(fixup->addr);
		fixup = fixup->next;
	}
}

enum {
	VAL_IMM    = 0x0001, // immediate value
	VAL_REG    = 0x0002, // value in register
	VAL_GLOBAL = 0x0010, // SB offset
	VAL_PARAM  = 0x0020, // FP +offset
	VAL_LOCAL  = 0x0040, // FP -offset
	VAL_ADDR   = 0x0080, // address
	VAL_RO     = 0x1000, // read only
	VAL_LEFT   = 0x00F0, // asignable if non-ro
};

typedef struct ValRec ValRec;
typedef struct ValRec* Val;

struct ValRec {
	u32 kind;
	Type type;
	u32 n;
};

u32 loop_continue = 0;
Fixup loop_exit = nil;
Fixup func_exit = nil;

void gen_block(Ast node);
u32 gen_expr(Ast node);

void sym_get_loc(Symbol sym, u32* base, i32* offset) {
	if (sym->kind == SYM_LOCAL) {
		*base = FP;
		*offset = -(4 + sym->value);
	} else if (sym->kind == SYM_PARAM) {
		*base = FP;
		*offset = 8 + sym->value;
	} else if (sym->kind == SYM_GLOBAL) {
		*base = SB;
		*offset = sym->value;
	} else {
		error("non-register-relative symbol");
	}
}

u32 gen_assign(Symbol sym, Ast expr) {
	fprintf(stderr,"gen_assign()\n");
	u32 base;
	i32 offset;
	sym_get_loc(sym, &base, &offset);
	//XXX type compat
	u32 r = gen_expr(expr);
	emit_mem(STW, r, base, offset);
	return r;
}

u32 gen_call(Ast node) {
	fprintf(stderr,"gen_call()\n");
	Symbol sym = node->child->sym;
	Ast arg = node->child->next;
	emit_opi(SUB, SP, SP, 4 * sym->type->len);
	u32 n = 0;
	while (arg != nil) {
		u32 r = gen_expr(arg);
		emit_mem(STW, r, SP, 4 * n);
		put_reg(r);
		arg = arg->next;
		n = n + 1;
	}
	gen_branch_sym(AL|L, sym);
	emit_opi(ADD, SP, SP, 4 * sym->type->len);

	// return is in r0, if it exists
	return 0;
}

u32 gen_lexpr(Ast node) {
	return 0;
}

u32 gen_binop(Ast node, u32 op) {
	fprintf(stderr, "gen_binop()\n");
	u32 left = gen_expr(node->child);
	u32 right = gen_expr(node->child->next);
	u32 res = get_reg_tmp();
	emit_op(op, res, left, right);
	put_reg(left);
	put_reg(right);
	return res;
}

u32 gen_expr(Ast node) {
	fprintf(stderr,"gen_expr()\n");
	if (node->kind == AST_U32) {
		u32 r = get_reg_tmp();
		emit_movi(r, node->ival);
		return r;
	} else if (node->kind == AST_NAME) {
		u32 base;
		i32 offset;
		sym_get_loc(node->sym, &base, &offset);
		u32 r = get_reg_tmp();
		emit_mem(LDW, r, base, offset);
		return r;
	} else if (node->kind == AST_BINOP) {
		u32 op = node->ival;
		if (op == tASSIGN) {
			if (node->child->kind != AST_NAME) {
				error("unhandled complex assignment");
			}
			return gen_assign(node->child->sym, node->child->next);
		} else if ((op & tcMASK) == tcRELOP) {
			error("sorry");
		} else if ((op & tcMASK) == tcADDOP) {
			op = add_op_to_ins_tab[op - tPLUS];
			return gen_binop(node, op);
		} else if ((op & tcMASK) == tcMULOP) {
			op = mul_op_to_ins_tab[op - tSTAR];
			return gen_binop(node, op);
		} else {
			error("gen_expr cannot handle binop %s\n", tnames[op]);
		}
	} else if (node->kind == AST_UNOP) {
		error("sorry no unops");
	} else if (node->kind == AST_CALL) {
		return gen_call(node);
	} else {
		error("gen_expr cannot handle %s\n", ast_kind[node->kind]);
	}
	return 0;
}

void gen_while(Ast node) {
	// save branch targets
	u32 old_loop_continue = loop_continue;
	Fixup old_loop_exit = loop_exit;

	FixupRec list;
	list.next = nil;
	loop_exit = &list;

	loop_continue = ctx.pc;
	u32 r = gen_expr(node->child);
	put_reg(r);

	// XXX flow

	gen_block(node->child->next);
	emit_br(AL, loop_continue);

	// patch breaks
	fixup_branches_fwd(loop_exit->next);

	// restore branch targets
	loop_continue = old_loop_continue;
	loop_exit = old_loop_exit;
}

void gen_if_else(Ast node) {
	gen_expr(node->child);
	Ast ifthen = node->child->next;
	Ast ifelse = ifthen->next;
	gen_block(ifthen);
	if (ifelse != nil) {
		gen_block(ifelse);
	}
}

void gen_stmt(Ast node) {
	fprintf(stderr,"gen_stmt()\n");
	u32 kind = node->kind;
	if (kind == AST_EXPR) {
		u32 r = gen_expr(node);
		put_reg(r);
	} else if (kind == AST_LOCAL) {
		if (node->child) {
			u32 r = gen_assign(node->sym, node->child);
			put_reg(r);
		}
	} else if (kind == AST_IF) {
		gen_if_else(node);
	} else if (kind == AST_WHILE) {
		gen_while(node);
	} else if (kind == AST_RETURN) {
		if (node->child) {
			u32 r = gen_expr(node->child);
			emit_mov(0, r);
			put_reg(r);
		}
		gen_branch_fwd(AL, func_exit);
	} else if (kind == AST_BREAK) {
		gen_branch_fwd(AL, loop_exit);
	} else if (kind == AST_CONTINUE) {
		gen_branch_back(AL, loop_continue);
	} else {
		error("gen_stmt cannot handle %s\n", ast_kind[kind]);
	}
}

void gen_block(Ast node) {
	fprintf(stderr,"gen_block()\n");
	node = node->child;
	while (node != nil) {
		gen_stmt(node);
		node = node->next;
	}
}

// before prologue  after prologue
// ---------------  --------------
//       arg2             oldarg2
//       arg1             oldarg1
// FP -> arg0             oldarg0
//       lrsave           oldlr
//       fpsave           oldfp   <-+
//       loc0             oldloc0   |
//       loc1             oldloc1   |
//       ...              ...       |
//       locn             oldlocn   |
//       callarg2         arg2      |
//       callarg1         arg1      |
// SP -> callarg0         arg0      |
//                        lrsave    |
//                  FP -> fpsave ---+
//                        loc0
//                        loc1
//                        ...
//                  SP -> locn

void gen_func(Ast node) {
	fprintf(stderr,"gen_func()\n");

	// local space plus saved lr and fp
	u32 x = node->sym->type->size + 8;

	node->sym->value = ctx.pc;
	node->sym->flags |= SYM_IS_PLACED;

	// patch previous calls now that we have an entry address
	fixup_branches_fwd(node->sym->fixups);
	node->sym->fixups = nil; // XXX discard

	// generate prologue
	emit_opi(SUB, SP, SP, x);
	emit_mem(STW, LR, SP, x - 4);
	emit_mem(STW, FP, SP, x - 8);
	emit_opi(ADD, FP, SP, x - 8);

	// setup list of branches-to-epilogue
	FixupRec list;
	list.next = nil;
	func_exit = &list;

	// generate body
	gen_block(node->child);

	// patch branches to epilogue
	fixup_branches_fwd(list.next);

	// generate epilogue
	emit_mem(LDW, LR, FP, 4);
	emit_mem(LDW, FP, FP, 0);
	emit_opi(ADD, SP, SP, x);
	emit_br(AL, LR);
}

void gen_risc5_simple(Ast node) {
	fprintf(stderr, "gen_risc5_simple()\n");

	emit_movi(SB, 0); // placeholder SB load
	emit_bi(AL, 0);  // placeholder branch to init

	node = node->child;
	while (node != nil) {
		if (node->kind == AST_FUNC) {
			gen_func(node);
		}
		node = node->next;
	}

	Symbol sym = symbol_find(string_make("start", 5));
	if (sym == nil) {
		error("no 'start' function\n");
	}
	if (sym->type->kind != TYPE_FUNC) {
		error("'start' is not a function\n");
	}
	if (sym->first != nil) {
		error("'start' must have no parameters\n");
	}

	// patch static base load to after the last instruction
	ctx.code[0] |= ctx.pc;
	// patch branch-to-start
	ctx.code[1] |= (sym->value - 8) >> 2;

	// TODO: copy ro globals after code
	// TODO: SB should neg-index into ro, pos-index into rw
}
