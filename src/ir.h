// Copyright 2021, Brian Swetland <swetland@frotz.net>
// Licensed under the Apache License, Version 2.0.

enum {
	// arg c is imm, for ALU and Bcc ops
	INF_C_IMM =   0x080,

	// size flags for LD/ST ops
	INF_SZ_U8 =   0x000,
	INF_SZ_U16 =  0x100,
	INF_SZ_U32 =  0x200,
	INF_SZ_MASK = 0x300,
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

	INS_PHI,

	INS_MOV,   // MOV Ra, Rc           Ra = Rc

	INS_LD,    // LDW Ra, Rb, Rc       Ra = MEM[Rb + Rc]
	INS_ST,    // STBI Ra, Rb, IMMc    MEM[Rb + IMMc] = Ra

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

	INS_COUNT, // total unique instructions

	INS_OP_MASK = 0x7F,
};

str ins_name[INS_COUNT] = {
	"add", "sub", "mul", "udiv", "sdiv", "urem", "srem",
	"lsl", "lsr", "asr", "and", "or", "xor",
	"phi", "mov", "ld", "st", "label",
	"b", "beq", "bne", "blt", "ble", "bgt", "bge", "call", "ret",
};
str isz_name[4] = {
	"b", "h", "w", "d"
};

typedef struct InstRec InstRec;
typedef struct InstRec* Inst;

struct InstRec {
	Inst next;
	i32 op;
	i32 a;
	i32 b;
	i32 c;
};
