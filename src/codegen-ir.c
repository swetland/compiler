// Copyright 2021, Brian Swetland <swetland@frotz.net>
// Licensed under the Apache License, Version 2.0.

#include "ir.h"

// architecture-specific

#define REG_PHYS_COUNT 16

str reg_phys_name[REG_PHYS_COUNT] = {
	"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",
	"r8", "r9", "r10", "r11", "fp", "sb", "sp", "lr",
};

#define REG_R0 (0)
#define REG_FP (12)
#define REG_SB (13)
#define REG_SP (14)
#define REG_LR (15)

// for errors, etc
#define REG_NONE -1

#define REG_VIRT_0 -2

// ------------------------------------------------------------------

Ast err_last_func = nil;
Ast err_ast = nil;

void dump_error_ctxt() {
	fprintf(stderr, "\n");
	if (err_last_func) {
		ast_dump(stderr, err_last_func, err_ast);
	}
	fprintf(stderr, "\n");
}

// global labels start at -1 ...
i32 global_next = 1;
str global_names[1024];

i32 label_get_global(str name) {
	i32 n = 1;
	while (n < global_next) {
		if (global_names[n] == name) {
			return -n;
		}
		n++;
	}
	global_next = n + 1;
	global_names[n] = name;
	return -n;
}

// local labels start at 0..
// virtual registers start at 0...
Inst ins_last_placed = nil;
i32 reg_next = REG_VIRT_0;

i32 reg_get() {
	i32 r = reg_next;
	reg_next = r - 1;
	return r;
}

// table of labels
i32 label_next = 0;
Inst label_idx[1024];
BBlock bb_idx[1024];

Inst _inst_make(u32 op, i32 a, i32 b, i32 c) {
	// add opcode-specific property bitflags to allow
	// for quick property testing
	op |= ins_props[op & INS_OP_MASK];

	Inst ins = malloc(sizeof(InstRec));
	ins->next = nil;
	ins->prev = nil;
	ins->op = op;
	ins->a = a;
	ins->b = b;
	ins->c = c;
	return ins;
}

// obtain a (detached) label instruction
// branches may target it before it is placed
// with inst_label()
i32 label_get() {
	i32 a = label_next;
	Inst ins = _inst_make(INS_LABEL, a, 0, 0);
	label_idx[a] = ins;
	label_next = a + 1;
	return a;
}

// allocate an instruction and add it to the graph
i32 inst_make(u32 op, i32 a, i32 b, i32 c) {
	Inst ins = _inst_make(op, a, b, c);
	ins->prev = ins_last_placed;
	ins_last_placed->next = ins;
	ins_last_placed = ins;
	return a;
}

// place a label obtained with label_get()
i32 inst_label(i32 a) {
	if ((a < 0) || (a >= label_next)) {
		error("invalid label #%d\n", a);
	}
	Inst ins = label_idx[a];
	if (ins->b != 0) {
		error("label #%d placed twice!", a);
	}
	// mark as placed
	ins->b = 1;
	// add to graph
	ins->prev = ins_last_placed;
	ins_last_placed->next = ins;
	ins_last_placed = ins;
	return a;
}

// branch instructions use this to validate targets
void inst_use_label(i32 a) {
	if ((a < 0) || (a >= label_next)) {
		error("invalid label #%d\n", a);
	}
	// bump edge count
	label_idx[a]->c ++;
}

i32 inst_label_global(str name) {
	return inst_make(INS_LABEL, label_get_global(name), 1, 0);
}

i32 inst_alu(u32 op, i32 b, i32 c) {
	if (op > INS_XOR) error("inst_alu invalid alu op");
	return inst_make(op, reg_get(), b, c);
}
i32 inst_alui(u32 op, i32 b, i32 imm) {
	if (op > INS_XOR) error("inst_alui invalid alu op");
	return inst_make(op | INF_C_IMM, reg_get(), b, imm);
}
void inst_aluix(u32 op, i32 a, i32 b, i32 imm) {
	if (op > INS_XOR) error("inst_alui invalid alu op");
	inst_make(op | INF_C_IMM, a, b, imm);
}
i32 inst_phi(i32 b, i32 c) {
	return inst_make(INS_PHI, reg_get(), b, c);
}
i32 inst_movi(i32 imm) {
	return inst_make(INS_MOV | INF_C_IMM, reg_get(), 0, imm);
}
i32 inst_mov(i32 c) {
	return inst_make(INS_MOV, reg_get(), 0, c);
}
i32 inst_movx(i32 a, i32 c) {
	return inst_make(INS_MOV, a, 0, c);
}
i32 inst_ldb(i32 b, i32 c) {
	return inst_make(INS_LD | INF_SZ_U8, reg_get(), b, c);
}
i32 inst_ldw(i32 b, i32 c) {
	return inst_make(INS_LD | INF_SZ_U32, reg_get(), b, c);
}
i32 inst_ldbi(i32 b, i32 imm) {
	return inst_make(INS_LD | INF_SZ_U8 | INF_C_IMM, reg_get(), b, imm);
}
i32 inst_ldwi(i32 b, i32 imm) {
	return inst_make(INS_LD | INF_SZ_U32 | INF_C_IMM, reg_get(), b, imm);
}
void inst_ldwix(i32 a, i32 b, i32 imm) {
	inst_make(INS_LD | INF_SZ_U32 | INF_C_IMM, a, b, imm);
}
void inst_stb(i32 a, i32 b, i32 c) {
	inst_make(INS_ST | INF_SZ_U8, a, b, c);
}
void inst_stw(i32 a, i32 b, i32 c) {
	inst_make(INS_ST | INF_SZ_U32, a, b, c);
}
void inst_stbi(i32 a, i32 b, i32 imm) {
	inst_make(INS_ST | INF_SZ_U8 | INF_C_IMM, a, b, imm);
}
void inst_stwi(i32 a, i32 b, i32 imm) {
	inst_make(INS_ST | INF_SZ_U32 | INF_C_IMM, a, b, imm);
}
i32 inst_br(i32 label) {
	inst_use_label(label);
	return inst_make(INS_B, label, 0, 0);
}
i32 inst_br_cmp(u32 op, i32 label, i32 b, i32 c) {
	inst_use_label(label);
	if ((op < INS_BEQ) || (op > INS_BGE)) {
		error("inst_branch_cond inavlid branch op");
	}
	return inst_make(op, label, b, c);
}
i32 inst_br_cmpi(u32 op, i32 label, i32 b, i32 imm) {
	inst_use_label(label);
	if ((op < INS_BEQ) || (op > INS_BGE)) {
		error("inst_branch_condi inavlid branch op");
	}
	return inst_make(op | INF_C_IMM, label, b, imm);
}
void inst_call(i32 label) {
	if (label >= 0) {
		error("cannot CALL local label #%d\n", label);
	}
	inst_make(INS_CALL, label, 0, 0);
}
void inst_ret(i32 a) {
	inst_make(INS_RET, a, 0, 0);
}

u32 rel_op_to_ins_tab[6] = { INS_BEQ, INS_BNE, INS_BLT, INS_BLE, INS_BGT, INS_BGE };
u32 rel_op_to_inv_ins_tab[6] = { INS_BNE, INS_BEQ, INS_BGE, INS_BGT, INS_BLE, INS_BLT };
u32 add_op_to_ins_tab[4] = { INS_ADD, INS_SUB, INS_OR, INS_XOR };
u32 mul_op_to_ins_tab[6] = { INS_MUL, INS_SDIV, INS_SREM, INS_AND, INS_LSL, INS_ASR };

// current loop continue and exit labels
i32 loop_continue = 0;
i32 loop_exit = 0;
// current function exit label
i32 func_exit = 0;

void gen_block(Ast node);
i32 gen_expr(Ast node);

void gen_trace(str msg) {
//	fprintf(stderr, "%p %p %s\n", err_last_func, err_ast, msg);
}

void gen_src_xref(Ast node) {
	ctx.xref[ctx.pc/4] = node->srcloc;
}

// obtain base register and offset
// for the memory backing a Symbol
void sym_get_loc(Symbol sym, i32* base, i32* offset) {
	if (sym->kind == SYM_LOCAL) {
		*base = REG_FP;
		*offset = -(4 + sym->value);
	} else if (sym->kind == SYM_PARAM) {
		*base = REG_FP;
		*offset = 8 + sym->value;
	} else if (sym->kind == SYM_GLOBAL) {
		*base = REG_SB;
		*offset = sym->value;
	} else {
		error("non-register-relative symbol");
	}
}

i32 gen_addr_expr(Ast node) {
	if (node->kind == AST_DEREF) {
		return inst_ldwi(gen_addr_expr(node->c0), 0);
	} else if (node->kind == AST_INDEX) {
		i32 esz = node->type->size;
		i32 raddr = gen_addr_expr(node->c0);
		i32 roff = gen_expr(node->c1);
		if (esz > 1) {
			roff = inst_alui(INS_MUL, roff, esz);
		}
		return inst_ldw(raddr, roff);
	} else if (node->kind == AST_FIELD) {
		i32 raddr = gen_addr_expr(node->c0);
		i32 off = node->c1->sym->value;
		// HANDLE non-word-sized
		return inst_alui(INS_ADD, raddr, off);
	} else if (node->kind == AST_SYMBOL) {
		i32 base;
		i32 offset;
		sym_get_loc(node->sym, &base, &offset);
		return inst_alui(INS_ADD, base, offset);
	} else if (node->kind == AST_ADDROF) {
		return gen_addr_expr(node->c0);
	} else {
		err_ast = node;
		error("gen_addr_expr cannot handle %s", ast_kind[node->kind]);
		return -1;
	}
}

i32 gen_assign(Ast lhs, Ast expr) {
	gen_trace("gen_assign()");

	i32 base;
	i32 offset;
	if (lhs->kind == AST_SYMBOL) {
		sym_get_loc(lhs->sym, &base, &offset);
	} else {
		base = gen_addr_expr(lhs);
		offset = 0;
	}
	i32 rval = gen_expr(expr);

	if (lhs->type->size == 4) {
		inst_stwi(rval, base, offset);
		return rval;
	} else if (lhs->type->size == 1) {
		inst_stbi(rval, base, offset);
		return rval;
	} else {
		err_ast = lhs;
		error("unexpected size %u store", lhs->type->size);
		return -1;
	}
}

i32 gen_call(Ast node) {
	gen_src_xref(node);
	gen_trace("gen_call()");
	Symbol sym = node->c0->sym;
	Ast arg = node->c2;
	Symbol param = sym->first;

	if (sym->flags & SYM_IS_BUILTIN) {
		// store to special io range
		i32 rval = gen_expr(arg);
		i32 raddr = inst_movi(0xFFFF0000);
		inst_stwi(rval, raddr, 0x100 + sym->value * 4);
	} else {
		u32 sizeargs = 4 * sym->type->len;
		if (sizeargs > 0) {
			inst_aluix(INS_SUB, REG_SP, REG_SP, sizeargs);
			u32 n = 0;
			while (arg != nil) {
				u32 r;
				if (param->type->kind == TYPE_POINTER) {
					// XXX or ptr type?
					r = gen_addr_expr(arg);
				} else {
					r = gen_expr(arg);
				}
				inst_stwi(r, REG_SP, 4 * n);
				arg = arg->c2;
				param = param->next;
				n = n + 1;
			}
			inst_call(label_get_global(sym->name->text));
			inst_aluix(INS_ADD, REG_SP, REG_SP, sizeargs);
		} else {
			inst_call(label_get_global(sym->name->text));
		}
	}
	if (sym->type->base != ctx.type_void) {
		// return is in phys r0
		return inst_mov(REG_R0);
	} else {
		return REG_NONE;
	}
}

i32 gen_binop(Ast node, u32 op) {
	gen_trace("gen_binop()");
	i32 left = gen_expr(node->c0);
	i32 right = gen_expr(node->c1);
	return inst_alu(op, left, right);
}

i32 gen_relop(Ast node, u32 op) {
	gen_trace("gen_relop()");
	i32 left = gen_expr(node->c0);
	i32 right = gen_expr(node->c1);

	i32 label = label_get();
	i32 rtrue = inst_movi(1);
	inst_br_cmp(op, label, left, right);
	i32 rfalse = inst_movi(0);
	inst_label(label);
	return inst_phi(rtrue, rfalse);
}

i32 gen_branch_if_expr_false(Ast node, i32 label) {
	if (ast_kind_is_relop(node->kind)) {
		u32 op = rel_op_to_inv_ins_tab[node->kind - AST_EQ];
		i32 left = gen_expr(node->c0);
		i32 right = gen_expr(node->c1);
		return inst_br_cmp(op, label, left, right);
	} else {
		i32 r = gen_expr(node);
		return inst_br_cmpi(INS_BEQ, label, r, 0);
	}
}


i32 gen_short_circuit_op(Ast node, u32 cc, u32 sc) {
#if 0
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
#endif
	return -1;
}

i32 gen_array_read(Ast node) {
	i32 raddr = gen_addr_expr(node->c0);
	i32 roff = gen_expr(node->c1);

	u32 sz = node->c0->type->base->size;
	if (sz > 1) {
		roff = inst_alui(INS_MUL, roff, sz);
	}
	if (sz == 1) {
		return inst_ldb(raddr, roff);
	} else {
		return inst_ldw(raddr, roff);
	}
}

i32 gen_struct_read(Ast node) {
	u32 raddr = gen_addr_expr(node->c0);
	u32 off = node->c1->sym->value;
	u32 sz = node->c1->type->size;
	if (sz == 1) {
		return inst_ldbi(raddr, off);
	} else if (sz == 4) {
		return inst_ldwi(raddr, off);
	} else {
		err_ast = node;
		error("unsupported field size");
		return -1;
	}
}

i32 gen_expr(Ast node) {
	err_ast = node;
	gen_src_xref(node);
	gen_trace("gen_expr()");
	u32 kind = node->kind;
	if (kind == AST_CONST) {
		return inst_movi(node->ival);
	} else if (kind == AST_SYMBOL) {
		// XXX type checking here or before
		if (node->sym->kind == SYM_CONST) {
			return inst_movi(node->sym->value);
		} else {
			i32 base;
			i32 offset;
			//XXX size
			sym_get_loc(node->sym, &base, &offset);
			return inst_ldwi(base, offset);
		}
	} else if (ast_kind_is_relop(kind)) {
		return gen_relop(node, rel_op_to_ins_tab[kind - AST_EQ]);
	} else if (ast_kind_is_addop(kind)) {
		return gen_binop(node, add_op_to_ins_tab[kind - AST_ADD]);
	} else if (ast_kind_is_mulop(kind)) {
		return gen_binop(node, mul_op_to_ins_tab[kind - AST_MUL]);
	} else if (kind == AST_BOOL_OR) {
		return gen_short_circuit_op(node, INS_BNE, 1);
	} else if (kind == AST_BOOL_AND) {
		return gen_short_circuit_op(node, INS_BEQ, 0);
	} else if (kind == AST_ASSIGN) {
		return gen_assign(node->c0, node->c1);
	} else if (kind == AST_NEG) {
		i32 r = gen_expr(node->c0);
		i32 z = inst_movi(0);
		return inst_alu(INS_SUB, z, r);
	} else if (kind == AST_NOT) {
		i32 r = gen_expr(node->c0);
		return inst_alui(INS_XOR, r, 0xffffffff);
	} else if (kind == AST_BOOL_NOT) {
		u32 r = gen_expr(node->c0);
		return inst_alui(INS_XOR, r, r);
	} else if (kind == AST_CALL) {
		return gen_call(node);
	} else if (kind == AST_INDEX) {
		return gen_array_read(node);
	} else if (kind == AST_FIELD) {
		return gen_struct_read(node);
	} else {
		error("gen_expr cannot handle %s\n", ast_kind[node->kind]);
	}
	return 0;
}

void gen_while(Ast node) {
	gen_trace("gen_while()");

	// save branch targets
	i32 old_loop_continue = loop_continue;
	i32 old_loop_exit = loop_exit;

	loop_continue = label_get();
	loop_exit = label_get();

	inst_label(loop_continue);

	gen_branch_if_expr_false(node->c0, loop_exit);

	gen_block(node->c1);

	inst_br(loop_continue);

	inst_label(loop_exit);

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
	i32 l0_br_false = gen_branch_if_expr_false(node->c0, label_get());

	i32 l_exit = label_get();

	// exec then block
	gen_block(node->c1);

	node = node->c2;
	while (node != nil) {
		// jump from end of 'then' block to end
		inst_br(l_exit);

		// false jump (over 'then' block) target is here
		inst_label(l0_br_false);

		if (node->kind == AST_IFELSE) { // ifelse ...
			gen_trace("gen_ifelse()");
			l0_br_false = gen_branch_if_expr_false(node->c0, label_get());

			gen_block(node->c1);
			node = node->c2;
		} else { // else ...
			gen_trace("gen_else()");
			gen_block(node);

			// done, place exit label
			inst_label(l_exit);
			return;
		}
	}

	// place final false label and exit label
	inst_label(l0_br_false);
	inst_label(l_exit);
}

void gen_block(Ast node);

void gen_stmt(Ast node) {
	err_ast = node;
	gen_src_xref(node);
	gen_trace("gen_stmt()\n");
	u32 kind = node->kind;
	if (kind == AST_EXPR) {
		gen_expr(node->c0);
	} else if (kind == AST_IF) {
		gen_if_else(node);
	} else if (kind == AST_WHILE) {
		gen_while(node);
	} else if (kind == AST_RETURN) {
		if (node->c0) {
			inst_movx(REG_R0, gen_expr(node->c0));
		}
		inst_br(func_exit);
	} else if (kind == AST_BREAK) {
		inst_br(loop_exit);
	} else if (kind == AST_CONTINUE) {
		inst_br(loop_continue);
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

Inst gen_func(Ast node) {
	err_last_func = node;
	err_ast = node;

	gen_trace("gen_func()\n");
	gen_src_xref(node);

	InstRec head;

	// setup initial state
	ins_last_placed = &head;
	reg_next = REG_VIRT_0;
	label_next = 0;

	inst_label(label_get());
	Inst first = ins_last_placed;

	// local space plus saved lr and fp
	u32 x = node->sym->type->size + 8;

	// generate prologue
#if 0
	node->sym->value = ctx.pc;
	node->sym->flags |= SYM_IS_PLACED;
#endif
	inst_aluix(INS_SUB, REG_SP, REG_SP, x);
	inst_stwi(REG_LR, REG_SP, x - 4);
	inst_stwi(REG_FP, REG_SP, x - 8);
	inst_aluix(INS_ADD, REG_FP, REG_SP, x - 8);

	// save for use by return, etc
	func_exit = label_get();

	// generate body
	gen_block(node->c0);

	// generate epilogue
	inst_label(func_exit);
	inst_ldwix(REG_LR, REG_FP, 4);
	inst_ldwix(REG_FP, REG_FP, 0);
	inst_aluix(INS_ADD, REG_SP, REG_SP, x);
	inst_ret(REG_LR);

	inst_label(label_get());

	head.next->prev = nil;
	return head.next;
}

void inst_disasm(FILE* fp, Inst ins);

i32 bb_id_counter = 0;

BBlock make_bblock(i32 pcount) {
	i32 sz = sizeof(BBlockRec) + pcount * 4;
	BBlock bb = malloc(sz);
	memset(bb, 0, sz);
	//bb->pcount = pcount;
	bb->id = bb_id_counter;
	bb_id_counter++;
	return bb;
}

bool inst_is_label(Inst ins) {
	return ins->op & INF_IS_LABEL;
}
bool inst_is_uncond_branch(Inst ins) {
	return ins->op & INF_IS_B_UC;
}
bool inst_is_cond_branch(Inst ins) {
	return ins->op & INF_IS_B_CC;
}
bool inst_is_branch(Inst ins) {
	return ins->op & INF_IS_B;
}
bool inst_is_return(Inst ins) {
	return ins->op & INF_IS_RET;
}

// dispose of an instruction
// - removes from graph
// - adjusts refcount for target label if a branch op
// - DOES NO HOUSEKEEPING for labels (remove at your own risk)
Inst opt_inst_destroy(Inst ins) {
	u32 op = ins->op & INS_OP_MASK;
	if (inst_is_branch(ins)) {
		Inst tgt = label_idx[ins->a];
		// decrement inbound count on branch target
		tgt->c --;
	}
	ins->op = INS_DEAD;

	Inst next = ins->next;
	Inst prev = ins->prev;

	next->prev = prev;
	prev->next = next;

	ins->next = nil;
	ins->prev = nil;

	free(ins);
	return next;
}

// clean up messes leftover from code generation
// - remove unconditional branches to the next instruction
// - remove labels with no inbounds
// - merge adjacent labels
// - insert labels after flow control departures from straightline
//   if such a label does not already exist
// - remove code between unconditional branches and labels
BBlock opt_func_label_cleanup(Inst first, Inst last) {
	// skip the global label and start with the local label L0
	Inst ins = first->next;
	while (ins != last) {
		if (inst_is_uncond_branch(ins) && !inst_is_label(ins->next)) {
			// any code between an uncond branch and a label is dead
			opt_inst_destroy(ins->next);
			continue;
		}
		if (inst_is_uncond_branch(ins)) {
			// do we target a label between us and the next inst?
			Inst x = ins->next;
			bool remove = false;
			// in case we've replaced its target, obtain the label
			// id of the target via the label index table
			i32 id = label_idx[ins->a]->a;
			while (inst_is_label(x) && (x != last)) {
				if (x->a == id) {
					// we're uncond jumping to an immediately
					// following label.
					remove = true;
					break;
				}
				x = x->next;
			}
			if (remove) {
				ins = opt_inst_destroy(ins);
				continue;
			}
		}
		if (inst_is_label(ins)) {
			if ((ins->c == 0) && !(inst_is_cond_branch(ins->prev))) {
				// nobody jumps here
				// remove label
				label_idx[ins->a] = nil;
				ins = opt_inst_destroy(ins);
				continue;
			}
			if (inst_is_label(ins->prev)) {
				// if prev was also a label, merge with it...
				// 1. acquire its refcount
				ins->prev->c += ins->c;
				// 2. acquire its slot (redirect all inbounds to us)
				label_idx[ins->prev->a] = ins;
				// 3. remove from instruction stream
				ins->op = INS_DEAD;
				ins = opt_inst_destroy(ins);
				continue;
			}
		}
		if (inst_is_cond_branch(ins) && !inst_is_label(ins->next)) {
			// flow control point not followed by a label
			Inst next = ins->next;
			ins_last_placed = ins;
			inst_label(label_get());
			ins_last_placed->next = next;
			next->prev = ins_last_placed;
		}
		if (inst_is_return(ins)) {
			// returns are treated as B Llast
			// from a flow-control standpoint
			last->c++;
		}
		ins = ins->next;
	}

	// for each non-redirected label, create a basic block
	// sized for the max possible inbound edges of that label
	i32 n = 0;
	BBlock bblast = nil;
	while (n < label_next) {
		if ((label_idx[n] != nil) && (label_idx[n]->a == n)) {
			BBlock bb = make_bblock(label_idx[n]->c + 1);
			bb_idx[n] = bb;
			bb_idx[n]->first = label_idx[n];
			if (bblast != nil) {
				bblast->list = bb;
			}
			bblast = bb;
		} else {
			bb_idx[n] = nil;
		}
		n++;
	}

	// For each branch instruction, fixup any indirections
	// so they point at their true target label.
	// Hook up prev[] and next[] pointers between bblocks
	ins = first->next;
	BBlock bb = bb_idx[0];
	bb->flags |= BB_FIRST;
	while (ins != nil) {
		if (inst_is_label(ins)) {
			bb->last = ins;
			BBlock nextbb = bb_idx[ins->a];
			// if control flows from previous instruction
			// (last in prev bb) to this one, bidirectionally
			// link us
			if (!inst_is_uncond_branch(ins->prev) &&
			    !inst_is_return(ins->prev)) {
				nextbb->prev[nextbb->pcount] = bb;
				nextbb->pcount++;
				if (bb->next[0] == nil) {
					bb->next[0] = nextbb;
				} else {
					bb->next[1] = nextbb;
				}
			}
			bb = nextbb;
		} else if (inst_is_branch(ins)) {
			Inst target = label_idx[ins->a];
			if (ins->a != target->a) {
				// we've been redirected, let's fix up
				ins->a = target->a;
			}
			BBlock tbb = bb_idx[target->a];

			// we're branching from bb to tbb
			tbb->prev[tbb->pcount] = bb;
			tbb->pcount++;
			if (bb->next[0] == nil) {
				bb->next[0] = tbb;
			} else {
				bb->next[1] = tbb;
			}
		} else if (inst_is_return(ins)) {
			BBlock tbb = bb_idx[last->a];

			// return is like a branch to the last label/bb
			tbb->prev[tbb->pcount] = bb;
			tbb->pcount++;
			if (bb->next[0] == nil) {
				bb->next[0] = tbb;
			} else {
				bb->next[1] = tbb;
			}
		}
		ins = ins->next;
	}

	bb->flags |= BB_LAST;

	// first bb has pcount 0 but prev[0] points to last for convenience
	bb_idx[0]->prev[0] = bb;
	return bb_idx[0];
}

BBlock opt_func(Ast node, Inst first, Inst last) {
	BBlock bblist = opt_func_label_cleanup(first, last);
	return bblist;
}


void dis_func(FILE* fp, Inst ins, str name) {
	fprintf(fp,"@%s:\n", name);
	while (ins != nil) {
		inst_disasm(fp, ins);
		ins = ins->next;
	}
}

void graph_func(FILE* fp, str fn, BBlock blocks) {
	fprintf(fp,
		"digraph g {\n"
		"graph [ rankdir=TB; ];\n"
		"node [ shape=plain; ];\n");
	BBlock bb = blocks;
	while (bb != nil) {
		fprintf(fp, "\"%p\" [ label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">\n", bb);
		Inst ins = bb->first;
		if (bb == blocks) {
			fprintf(fp,"<TR><TD BGCOLOR=\"gray\" ALIGN=\"left\">@%s:</TD></TR>\n", fn);
		}
		while (ins != bb->last) {
			if (inst_is_label(ins)) {
				fprintf(fp,"<TR><TD BGCOLOR=\"gray\" ALIGN=\"left\">");
			} else {
				fprintf(fp,"<TR><TD ALIGN=\"left\">");
			}
			inst_disasm(fp, ins);
			fprintf(fp,"</TD></TR>\n");
			ins = ins->next;
		}
		fprintf(fp, "</TABLE>>; ];\n");
		if (bb->next[0]) {
			fprintf(fp, "\"%p\" -> \"%p\" [ ];\n", bb, bb->next[0]);
		}
		if (bb->next[1]) {
			fprintf(fp, "\"%p\" -> \"%p\" [ ];\n", bb, bb->next[1]);
		}
		bb = bb->list;
	}
	fprintf(fp, "}\n");
}

void gen_program(Ast node) {
	gen_trace( "gen_risc5_simple()\n");

	// TODO: program prologue

	node = node->c2;
	while (node != nil) {
		if (node->kind == AST_FUNC) {
			str fn = node->sym->name->text;
			Inst first = gen_func(node);
			Inst last = ins_last_placed;
			char tmp[256];
			sprintf(tmp, "debug/%s.ast.ir", fn);
			FILE* fp = fopen(tmp, "w");
			if (fp != nil) {
				dis_func(fp, first, fn);
				fclose(fp);
			}
			BBlock bblocks = opt_func(node, first, last);
			sprintf(tmp, "debug/%s.bb.ir", fn);
			fp = fopen(tmp, "w");
			if (fp != nil) {
				dis_func(fp, first, fn);
				fclose(fp);
			}
			sprintf(tmp, "debug/%s.bb.dot", fn);
			fp = fopen(tmp, "w");
			if (fp != nil) {
				graph_func(fp, fn, bblocks);
				fclose(fp);
			}
		}
		node = node->c2;
	}

	err_last_func = nil;

	// TODO: program epilogue, globals
}

void prreg(FILE* fp, i32 n) {
	if ((n >= 0) && (n < REG_PHYS_COUNT)) {
		fprintf(fp, "$%s", reg_phys_name[n]);
	} else if (n < -1) {
		fprintf(fp, "%%%u", (-n) + 1);
	} else {
		fprintf(fp, "<ERROR>");
	}
}

void prarg(FILE* fp, i32 n, i32 op) {
	if (op & INF_C_IMM) {
		fprintf(fp, "%d", n);
		return;
	} else {
		prreg(fp, n);
	}
}

void prlabel(FILE* fp, i32 n) {
	if (n >= 0) {
		fprintf(fp, "L%u", label_idx[n]->a);
	} else {
		fprintf(fp, "@%s", global_names[-n]);
	}
}

void inst_disasm(FILE* fp, Inst ins) {
	u32 op = ins->op & INS_OP_MASK;
	//fprintf(fp, "(%08x %08x %08x %08x)\n", ins->op, ins->a, ins->b, ins->c);
	if (op == INS_LABEL) {
		prlabel(fp, ins->a);
		fprintf(fp, ":"); // count=%u", ins->c);
	} else if (op == INS_MOV) {
		fprintf(fp, "\tmov ");
		prreg(fp, ins->a);
		fprintf(fp, ", ");
		prarg(fp, ins->c, ins->op);
	} else if ((op == INS_LD) || (op == INS_ST)) {
		str sz = isz_name[(ins->op & INF_SZ_MASK) >> INF_SZ_SHIFT];
		fprintf(fp, "\t%s%s ", ins_name[op], sz);
		prreg(fp, ins->a);
		fprintf(fp, ", [");
		prreg(fp, ins->b);
		fprintf(fp, ", ");
		prarg(fp, ins->c, ins->op);
		fprintf(fp, "]");
	} else if (op <= INS_PHI) {
		fprintf(fp, "\t%s ", ins_name[op]);
		prreg(fp, ins->a);
		fprintf(fp, ", ");
		prreg(fp, ins->b);
		fprintf(fp, ", ");
		prarg(fp, ins->c, ins->op);
	} else if (op == INS_B) {
		fprintf(fp, "\tb ");
		prlabel(fp, ins->a);
	} else if ((op >= INS_BEQ) && (op <= INS_BGE)) {
		fprintf(fp, "\t%s ", ins_name[op]);
		prlabel(fp, ins->a);
		fprintf(fp, ", ");
		prreg(fp, ins->b);
		fprintf(fp, ", ");
		prarg(fp, ins->c, ins->op);
	} else if (op == INS_CALL) {
		fprintf(fp, "\tcall ");
		prlabel(fp, ins->a);
	} else if (op == INS_RET) {
		fprintf(fp, "\tret ");
		prreg(fp, ins->a);
	} else {
		fprintf(fp, "\tinval 0x%x", ins->op);
	}
	fprintf(fp, "\n");
}

void binary_write(const char* outname) {
}

void listing_write(const char* listfn, const char* srcfn) {
}
