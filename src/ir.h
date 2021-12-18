// Copyright 2021, Brian Swetland <swetland@frotz.net>
// Licensed under the Apache License, Version 2.0.

enum {
	// arg c is imm, for ALU and Bcc ops
	INF_C_IMM =    0x0100,

	// size flags for LD/ST ops
	INF_SZ_U8 =    0x0000,
	INF_SZ_U16 =   0x0040,
	INF_SZ_U32 =   0x0080,
	INF_SZ_MASK =  0x00C0,
	INF_SZ_SHIFT = 6,

	// property bits
	INF_IS_B_UC  = 0x0200, // is uncondition branch
	INF_IS_B_CC  = 0x0400, // is conditional branch
	INF_IS_B     = 0x0600, // is any kind of branch
	INF_IS_RET   = 0x0800, // is a return op
	INF_IS_EOBB  = 0x0E00, // is a branch or return (end of bb)
	INF_IS_LABEL = 0x1000, // is a label (start of bb)
	INF_IS_ALU   = 0x2000, // add, sub, and, ...
	INF_IS_MEM   = 0x4000, // load or store
	INF_IS_PHI   = 0x8000,
};

enum {
	INS_ADD,    // ADD Ra, Rb, Rc       Ra = Rb + Rc
	INS_SUB,    // SUBI Ra, Rb, IMMc    Ra = Rb + IMMc
	INS_MUL,    // ...
	INS_UDIV,
	INS_SDIV,
	INS_UREM,
	INS_SREM,

	INS_LSL,
	INS_LSR,
	INS_ASR,
	INS_AND,
	INS_OR,
	INS_XOR,

	INS_MOV,   // MOV Ra, Rc           Ra = Rc

	INS_LD,    // LDW Ra, Rb, Rc       Ra = MEM[Rb + Rc]
	INS_ST,    // STBI Ra, Rb, IMMc    MEM[Rb + IMMc] = Ra

	INS_PHI,

	INS_LABEL, // La:

	INS_B,     // B La
	INS_BEQ,   // BEQ La, Ra, Rb   BEQI La, Rb, IMMc
	INS_BNE,   // ...
	INS_BLT,
	INS_BLE,
	INS_BGT,
	INS_BGE,
	INS_CALL,  // CALL Global
	INS_RET,

	INS_DEAD,

	INS_COUNT, // total unique instructions

	INS_OP_MASK = 0x3F,
};

i32 ins_props[INS_COUNT] = {
	INF_IS_ALU, INF_IS_ALU, INF_IS_ALU, INF_IS_ALU, INF_IS_ALU, INF_IS_ALU, INF_IS_ALU,
	INF_IS_ALU, INF_IS_ALU, INF_IS_ALU, INF_IS_ALU, INF_IS_ALU, INF_IS_ALU,
	0, INF_IS_MEM, INF_IS_MEM, INF_IS_PHI, INF_IS_LABEL,
	INF_IS_B_UC, INF_IS_B_CC, INF_IS_B_CC, INF_IS_B_CC, INF_IS_B_CC, INF_IS_B_CC, INF_IS_B_CC,
	0, INF_IS_RET, 0,
};

str ins_name[INS_COUNT] = {
	"add", "sub", "mul", "udiv", "sdiv", "urem", "srem",
	"lsl", "lsr", "asr", "and", "or", "xor",
	"mov", "ld", "st", "phi", "label",
	"b", "beq", "bne", "blt", "ble", "bgt", "bge", "call", "ret",
	"dead",
};
str isz_name[4] = {
	"b", "h", "w", "d"
};

typedef struct InstRec InstRec;
typedef struct BBlockRec BBlockRec;
typedef struct InstRec* Inst;
typedef struct BBlockRec* BBlock;

struct InstRec {
	Inst next;
	Inst prev;
	i32 op;
	i32 a;
	i32 b;
	i32 c;
};

enum {
	BB_FIRST = 0x0001,
	BB_LAST = 0x0002,
};

struct BBlockRec {
	Inst first;
	Inst last;
	i32 flags;
	i32 id;

	i32 pcount;
	BBlock list;
	BBlock next[2];
	BBlock prev[0];
};
