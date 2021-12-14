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
Inst ins_last = nil;
i32 reg_next = REG_VIRT_0;
i32 label_next = 0;

i32 reg_get() {
	i32 r = reg_next;
	reg_next = r - 1;
	return r;
}

i32 label_get() {
	i32 n = label_next;
	label_next = n + 1;
	return n;
}

i32 inst_make(u32 op, i32 a, i32 b, i32 c) {
	Inst ins = malloc(sizeof(InstRec));
	ins->next = nil;
	ins->op = op;
	ins->a = a;
	ins->b = b;
	ins->c = c;
	ins_last->next = ins;
	ins_last = ins;
	return a;
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
i32 inst_label(i32 a) {
	return inst_make(INS_LABEL, a, 0, 0);
}
i32 inst_label_global(str name) {
	return inst_label(label_get_global(name));
}
i32 inst_br(i32 label) {
	return inst_make(INS_B, label, 0, 0);
}
i32 inst_br_cmp(u32 op, i32 label, i32 b, i32 c) {
	if ((op < INS_BEQ) || (op > INS_BGE)) {
		error("inst_branch_cond inavlid branch op");
	}
	return inst_make(op, label, b, c);
}
i32 inst_br_cmpi(u32 op, i32 label, i32 b, i32 imm) {
	if ((op < INS_BEQ) || (op > INS_BGE)) {
		error("inst_branch_condi inavlid branch op");
	}
	return inst_make(op | INF_C_IMM, label, b, imm);
}
void inst_call(i32 label) {
	inst_make(INS_CALL, label, 0, 0);
}
void inst_ret(i32 a) {
	inst_make(INS_RET, a, 0, 0);
}

u32 rel_op_to_ins_tab[6] = { INS_BEQ, INS_BNE, INS_BLT, INS_BLE, INS_BGT, INS_BGE };
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

	i32 raddr = gen_addr_expr(lhs);
	i32 rval = gen_expr(expr);

	if (lhs->type->size == 4) {
		inst_stwi(rval, raddr, 0);
		return rval;
	} else if (lhs->type->size == 1) {
		inst_stbi(rval, raddr, 0);
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

	u32 r = gen_expr(node->c0);

	// branch to exit if false
	inst_br_cmpi(INS_BEQ, loop_exit, r, 0);

	gen_block(node->c1);

	inst_br(loop_continue);

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
	i32 r = gen_expr(node->c0);

	i32 l0_br_false = inst_br_cmpi(INS_BEQ, label_get(), r, 0);

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
			r = gen_expr(node->c0);

			i32 l0_br_false = inst_br_cmpi(INS_BEQ, label_get(), r, 0);

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
	ins_last = &head;
	reg_next = REG_VIRT_0;
	label_next = 0;

	inst_label_global(node->sym->name->text);
	Inst first = ins_last;

	// local space plus saved lr and fp
	u32 x = node->sym->type->size + 8;

	// generate prologue
	inst_label(label_get());
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

	return head.next;
}

void inst_disasm(FILE* fp, Inst ins);

void gen_program(Ast node) {
	gen_trace( "gen_risc5_simple()\n");

	// TODO: program prologue

	node = node->c2;
	while (node != nil) {
		if (node->kind == AST_FUNC) {
			Inst ins = gen_func(node);
			fprintf(stderr,"\n------------------\n");
			while (ins != nil) {
				inst_disasm(stderr, ins);
				ins = ins->next;
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
		fprintf(fp, "L%u", n);
	} else {
		fprintf(fp, "@%s", global_names[-n]);
	}
}

void inst_disasm(FILE* fp, Inst ins) {
	u32 op = ins->op & INS_OP_MASK;
	//fprintf(fp, "(%08x %08x %08x %08x)\n", ins->op, ins->a, ins->b, ins->c);
	if (op == INS_LABEL) {
		prlabel(fp, ins->a);
		fprintf(fp, ":");
	} else if (op == INS_MOV) {
		fprintf(fp, "\tmov ");
		prreg(fp, ins->a);
		fprintf(fp, ", ");
		prarg(fp, ins->c, ins->op);
	} else if ((op == INS_LD) || (op == INS_ST)) {
		str sz = isz_name[(ins->op & INF_SZ_MASK) >> 8];
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
		fprintf(stderr, "\tcall ");
		prlabel(fp, ins->a);
	} else if (op == INS_RET) {
		fprintf(stderr, "\tret ");
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
