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

u32 regbits = 0;
u32 regcount = 0;

u32 get_reg_tmp() {
	u32 n = tmp_reg_first;
	while (n <= tmp_reg_last) {
		if (!(regbits & (1 << n))) {
			regbits |= (1 << n);
			regcount++;
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
	regcount--;
}

bool is_reg_busy(u32 r) {
	return regbits & (1 << r);
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
	if (op == MOD) {
		emit_op(DIV, a, b, c);
		emit_op(MOV_H, a, 0, 0);
	} else {
		emit((op << 16) | (a << 24) | (b << 20) | c);
	}
}
void emit_opi_u16(u32 op, u32 a, u32 b, u32 n) {
	if (op == MOD) {
		emit_opi_u16(DIV, a, b, n);
		emit_op(MOV_H, a, 0, 0);
	} else {
		emit(((0x4000 | op) << 16) | (a << 24) | (b << 20) | (n & 0xffff));
	}
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
		emit_opi_u16(MOV, a, 0, n);
	} else if (m == 0xFFFF) {
		emit_opi_u16(MOV | 0x1000, a, 0, n);
	} else {
		emit_opi_u16(MHI, a, 0, m);
		if ((n & 0xFFFF) != 0) {
			emit_opi_u16(IOR, a, a, n);
		}
	}
}

// immediate op, using a temporary register and register op if the
// immediate argument does not fit the single instruction form
void emit_opi(u32 op, u32 a, u32 b, u32 n) {
	u32 m = n >> 16;
	if (m == 0) {
		emit_opi_u16(op, a, b, n);
	} else if (m == 0xFFFF) {
		emit_opi_u16(op | 0x1000, a, b, n);
	} else {
		emit_opi_u16(MHI, R11, 0, m);
		if ((n & 0xFFFF) != 0) {
			emit_opi_u16(IOR, R11, R11, n);
		}
		emit_op(op, a, b, R11);
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

// ------------------------------------------------------------------

void gen_branch(u32 op, u32 addr) {
	emit_bi(op, (addr - ctx.pc - 4) >> 2);
}

void gen_branch_fwd(u32 op, Fixup list) {
	fixup_add_list(list, ctx.pc);
	emit_bi(op, 0);
}

void gen_branch_sym(u32 op, Symbol sym) {
	if (sym->flags & SYM_IS_PLACED) {
		gen_branch(op, sym->value);
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

Ast err_last_func = nil;
Ast err_ast = nil;

void gen_trace(str msg) {
//	fprintf(stderr, "%p %p %s\n", err_last_func, err_ast, msg);
}

void gen_src_xref(Ast node) {
	ctx.xref[ctx.pc/4] = node->srcloc;
}

void dump_error_ctxt() {
	fprintf(stderr, "\n");
	if (err_last_func) {
		ast_dump(stderr, err_last_func, err_ast);
	}
	fprintf(stderr, "\n");
}

// obtain base register and offset
// for the memory backing a Symbol
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

u32 gen_addr_expr(Ast expr, Type type) {
	if (expr->kind == AST_NAME) {
		u32 base;
		i32 offset;
		sym_get_loc(expr->sym, &base, &offset);
		u32 r = get_reg_tmp();
		if (expr->sym->flags & SYM_IS_REFERENCE) {
			// reference variable, so its content is an address
			// return that
			emit_mem(LDW, r, base, offset);
		} else {
			// inline variable, so its base + offset is the
			// address for its content
			emit_opi(ADD, r, base, offset);
		}
		return r;
	} else {
		err_ast = expr;
		error("gen_addr_expr cannot handle %s", ast_kind[expr->kind]);
	}
	return 0;
}

u32 gen_assign_expr(Ast expr, Symbol sym) {
	if (sym->flags & SYM_IS_REFERENCE) {
		fprintf(stderr,"AE REF\n");
		return gen_addr_expr(expr, sym->type);
	} else if (sym->type->kind == TYPE_POINTER) {
		fprintf(stderr,"AE PTR\n");
		return gen_addr_expr(expr, sym->type->base);
	} else {
		fprintf(stderr,"AE EXP\n");
		return gen_expr(expr);
	}
}

u32 gen_assign(Ast lhs, Ast expr) {
	gen_trace("gen_assign()");

	if ((lhs->kind == AST_LOCAL) ||
	    (lhs->kind == AST_NAME)) {
		u32 base;
		i32 offset;
		sym_get_loc(lhs->sym, &base, &offset);
		//XXX type compat
		u32 r = gen_assign_expr(expr, lhs->sym);
		emit_mem(STW, r, base, offset);
		return r;
	} else if (lhs->kind == AST_INDEX) {
		error("wip");
	} else {
		err_ast = lhs;
		error("illegal on lhs (%s)", ast_kind[lhs->kind]);
	}
	return 0;
}

u32 reg_save(u32 base) {
	u32 r = tmp_reg_first;
	u32 n = 0;
	while (r <= tmp_reg_last) {
		if (regbits & (1 << r)) {
			emit_mem(STW, r, SP, base + n);
			n += 4;
		}
		r++;
	}
	u32 mask = regbits;
	regbits = 0;
	return mask;
}

void reg_restore(u32 base, u32 mask) {
	if (regbits != 0) {
		error("register restore collision");
	}
	regbits = mask;
	u32 r = tmp_reg_first;
	u32 n = 0;
	while (r <= tmp_reg_last) {
		if (regbits & (1 << r)) {
			emit_mem(LDW, r, SP, base + n);
			n += 4;
		}
		r++;
	}
}

u32 gen_call(Ast node) {
	gen_src_xref(node);
	gen_trace("gen_call()");
	Symbol sym = node->c0->sym;
	Ast arg = node->c2;
	Symbol param = sym->first;

	if (sym->flags & SYM_IS_BUILTIN) {
		u32 r = gen_expr(arg);
		emit_movi(R11, 0xffff0000);
		emit_mem(STW, r, R11, 0x100 + sym->value * 4);
		put_reg(r);
	} else {
		u32 sizeregs = 4 * regcount;
		if ((sym->type->len > 0) || (sizeregs > 0)) {
			u32 sizeargs = 4 * sym->type->len;
			emit_opi(SUB, SP, SP, sizeregs + sizeargs);
			u32 mask = reg_save(sizeargs);
			u32 n = 0;
			while (arg != nil) {
				u32 r;
				if (param->flags & SYM_IS_REFERENCE) {
					// XXX or ptr type?
					r = gen_addr_expr(arg, param->type);
				} else {
					r = gen_expr(arg);
				}
				emit_mem(STW, r, SP, 4 * n);
				put_reg(r);
				arg = arg->c2;
				param = param->next;
				n = n + 1;
			}
			gen_branch_sym(AL|L, sym);
			reg_restore(sizeargs, mask);
			emit_opi(ADD, SP, SP, sizeregs + sizeargs);
		} else {
			// no args or temporaries to save
			gen_branch_sym(AL|L, sym);
		}
	}
	// return is in r0, if it exists
	// stash it somewhere where it won't get stomped
	// by other calls in this expr
	u32 r = get_reg_tmp();
	emit_mov(r, R0);
	return r;
}

u32 gen_lexpr(Ast node) {
	return 0;
}

u32 gen_binop(Ast node, u32 op) {
	gen_trace( "gen_binop()");
	u32 left = gen_expr(node->c0);
	u32 right = gen_expr(node->c1);
	u32 res = get_reg_tmp();
	emit_op(op, res, left, right);
	put_reg(left);
	put_reg(right);
	return res;
}

u32 gen_relop(Ast node, u32 cc) {
	gen_trace("gen_relop()");
	u32 left = gen_expr(node->c0);
	u32 right = gen_expr(node->c1);
	u32 res = get_reg_tmp();
	emit_movi(res, 1);
	emit_op(SUB, left, left, right);
	gen_branch(cc, ctx.pc + 8);
	emit_movi(res, 0);
	put_reg(left);
	put_reg(right);
	return res;
}

u32 gen_logical_op(Ast node, u32 cc, u32 sc) {
	u32 r = gen_expr(node->c0);
	emit_mov(R11, r); // set z flag
	put_reg(r);
	u32 l0_br_sc = ctx.pc;
	emit_bi(cc, 0);
	r = gen_expr(node->c1);
	emit_mov(R11, r); // set z flag
	put_reg(r);
	u32 l1_br_sc = ctx.pc;
	emit_bi(cc, 0);
	r = get_reg_tmp();
	emit_movi(r, !sc);
	u32 l2_br_exit = ctx.pc;
	emit_bi(AL, 0);
	fixup_branch_fwd(l0_br_sc);
	fixup_branch_fwd(l1_br_sc);
	emit_movi(r, sc);
	fixup_branch_fwd(l2_br_exit);
	return r;
}

u32 gen_array_addr(Ast node) {
	err_ast = node;
	if (node->type->kind != TYPE_ARRAY) {
		error("cannot deref non-array type");
	}
	if (node->kind == AST_NAME) {
		u32 base;
		i32 offset;
		sym_get_loc(node->sym, &base, &offset);
		u32 r = get_reg_tmp();
		if (node->sym->flags & SYM_IS_REFERENCE) {
			// some symbols are tagged as by reference,
			// in which case we load the address
			emit_mem(LDW, r, base, offset);
		} else {
			// otherwise the array is inline, so we
			// just fixup the offset
			emit_opi(ADD, r, base, offset);
		}
		return r;
	} else if (node->kind == AST_INDEX) {
		error("not ready for [][]");
	} else {
		error("cannot dereference this");
	}
	return 0;
}

u32 gen_array_read(Ast node) {
	u32 raddr = gen_array_addr(node->c0);
	u32 roff = gen_expr(node->c1);

	u32 sz = node->c0->type->base->size;
	if (sz > 1) {
		emit_opi(MUL, roff, roff, sz);
	}
	emit_op(ADD, raddr, raddr, roff);
	if (sz == 1) {
		emit_mem(LDB, roff, raddr, 0);
	} else {
		emit_mem(LDW, roff, raddr, 0);
	}
	put_reg(raddr);
	return roff;
}

u32 gen_expr(Ast node) {
	err_ast = node;
	gen_src_xref(node);
	gen_trace("gen_expr()");
	if (node->kind == AST_U32) {
		u32 r = get_reg_tmp();
		emit_movi(r, node->ival);
		return r;
	} else if (node->kind == AST_NAME) {
		u32 r = get_reg_tmp();
		// XXX type checking here or before
		if (node->sym->kind == SYM_CONST) {
			emit_movi(r, node->sym->value);
		} else {
			u32 base;
			i32 offset;
			sym_get_loc(node->sym, &base, &offset);
			emit_mem(LDW, r, base, offset);
		}
		return r;
	} else if (node->kind == AST_BINOP) {
		u32 op = node->ival;
		if (op == tASSIGN) {
			if (node->c0->kind != AST_NAME) {
				error("unhandled complex assignment");
			}
			return gen_assign(node->c0, node->c1);
		} else if ((op & tcMASK) == tcRELOP) {
			return gen_relop(node, rel_op_to_cc_tab[op - tEQ]);
		} else if ((op & tcMASK) == tcADDOP) {
			return gen_binop(node, add_op_to_ins_tab[op - tPLUS]);
		} else if ((op & tcMASK) == tcMULOP) {
			return gen_binop(node, mul_op_to_ins_tab[op - tSTAR]);
		} else if (op == tOR) {
			return gen_logical_op(node, NE, 1);
		} else if (op == tAND) {
			return gen_logical_op(node, EQ, 0);
		} else {
			error("gen_expr cannot handle binop %s\n", tnames[op]);
		}
	} else if (node->kind == AST_UNOP) {
		u32 op = node->ival;
		u32 r = gen_expr(node->c0);
		if (op == tMINUS) {
			emit_movi(R11, 0);
			emit_op(SUB, r, R11, r);
		} else if (op == tNOT) {
			emit_opi(XOR, r, r, 0xffffffff);
		} else if (op == tBANG) {
			emit_opi(XOR, r, r, r);
		} else {
			error("gen_expr cannot handle unop %s\n", tnames[op]);
		}
		return r;
	} else if (node->kind == AST_CALL) {
		return gen_call(node);
	} else if (node->kind == AST_INDEX) {
		return gen_array_read(node);
	} else {
		error("gen_expr cannot handle %s\n", ast_kind[node->kind]);
	}
	return 0;
}

void gen_while(Ast node) {
	gen_trace("gen_while()");
	// save branch targets
	u32 old_loop_continue = loop_continue;
	Fixup old_loop_exit = loop_exit;

	FixupRec list;
	list.next = nil;
	loop_exit = &list;

	loop_continue = ctx.pc;
	u32 r = gen_expr(node->c0);

	emit_mov(R11, r); // set z flag
	put_reg(r);
	gen_branch_fwd(EQ, &list);

	gen_block(node->c1);

	gen_branch(AL, loop_continue);

	// patch breaks
	fixup_branches_fwd(loop_exit->next);

	// restore branch targets
	loop_continue = old_loop_continue;
	loop_exit = old_loop_exit;
}

void gen_if_else(Ast node) {
	// fixups for jumps to the very end
	FixupRec if_exit;
	if_exit.next = nil;

	gen_trace("gen_if()");
	// IF contains one or more IFELSE nodes
	node = node->c0;

	// compute if expr
	// branch ahead if false;
	u32 r = gen_expr(node->c0);
	emit_mov(R11, r); // set z flag;
	put_reg(r);
	u32 l0_br_false = ctx.pc;
	emit_bi(EQ, 0);

	// exec then block
	gen_block(node->c1);

	node = node->c2;
	while (node != nil) {
		// jump from end of 'then' block to end
		gen_branch_fwd(AL, &if_exit);

		// patch false jump (over 'then' block) to here
		fixup_branch_fwd(l0_br_false);

		if (node->kind == AST_IFELSE) { // ifelse ...
			gen_trace("gen_ifelse()");
			r = gen_expr(node->c0);
			emit_mov(R11, r); // set z flag
			put_reg(r);
			l0_br_false = ctx.pc;
			emit_bi(EQ, 0);
			gen_block(node->c1);
			node = node->c2;
		} else { // else ...
			gen_trace("gen_else()");
			gen_block(node);

			// done, patch up earlier jumps to exit
			fixup_branches_fwd(if_exit.next);
			return;
		}
	}

	// patch false jump (over previous 'then' block) to here
	fixup_branch_fwd(l0_br_false);

	// done, patch up earlier jumps to exit
	fixup_branches_fwd(if_exit.next);
}

void gen_block(Ast node);

void gen_stmt(Ast node) {
	err_ast = node;
	gen_src_xref(node);
	gen_trace("gen_stmt()\n");
	u32 kind = node->kind;
	if (kind == AST_EXPR) {
		u32 r = gen_expr(node->c0);
		put_reg(r);
	} else if (kind == AST_LOCAL) {
		if (node->c0) {
			u32 r = gen_assign(node, node->c0);
			put_reg(r);
		}
	} else if (kind == AST_IF) {
		gen_if_else(node);
	} else if (kind == AST_WHILE) {
		gen_while(node);
	} else if (kind == AST_RETURN) {
		if (node->c0) {
			u32 r = gen_expr(node->c0);
			emit_mov(0, r);
			put_reg(r);
		}
		gen_branch_fwd(AL, func_exit);
	} else if (kind == AST_BREAK) {
		gen_branch_fwd(AL, loop_exit);
	} else if (kind == AST_CONTINUE) {
		gen_branch(AL, loop_continue);
	} else if (kind == AST_BLOCK) {
		gen_block(node);
	} else {
		error("gen_stmt cannot handle %s\n", ast_kind[kind]);
	}
}

void gen_block(Ast node) {
	gen_trace("gen_block()\n");
	gen_src_xref(node);
	node = node->c2;
	while (node != nil) {
		gen_stmt(node);
		node = node->c2;
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
	err_last_func = node;
	err_ast = node;

	gen_src_xref(node);
	gen_trace("gen_func()\n");

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
	gen_block(node->c0);

	// patch branches to epilogue
	fixup_branches_fwd(list.next);

	// generate epilogue
	emit_mem(LDW, LR, FP, 4);
	emit_mem(LDW, FP, FP, 0);
	emit_opi(ADD, SP, SP, x);
	emit_br(AL, LR);
}

void gen_risc5_simple(Ast node) {
	gen_trace( "gen_risc5_simple()\n");

	emit_movi(SB, 0); // placeholder SB load
	emit_bi(AL, 0);  // placeholder branch to init

	node = node->c2;
	while (node != nil) {
		if (node->kind == AST_FUNC) {
			gen_func(node);
		}
		node = node->c2;
	}

	err_last_func = nil;

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
