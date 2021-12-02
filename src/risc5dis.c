// Copyright 2020, Brian Swetland <swetland@frotz.net>
// Licensed under the Apache License, Version 2.0.

#include <string.h>
#include <stdio.h>

#include "risc5.h"

static char *append_str(char *buf, const char *s) {
	while (*s) *buf++ = *s++;
	return buf;
}

static char *append_i32(char *buf, int32_t n) {
	return buf + sprintf(buf, "%d", n);
}

static char *append_u32(char *buf, int32_t n) {
	return buf + sprintf(buf, "0x%x", n);
}

static const char* regname[16] = {
	"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",
	"r8", "r9", "r10", "r11", "fp", "sb", "sp", "lr",
};
#define R(n) regname[(n) & 15]

static const char* opname[16] = {
	"mov", "lsl", "asr", "ror", "and", "ann", "ior", "xor",
	"add", "sub", "mul", "div", "fad", "fsb", "fml", "fdv",
};
#define OP(n) opname[(n) & 15]

static const char* ccname[16] = {
	"mi", "eq", "cs", "vs", "ls", "lt", "le", "",
	"pl", "ne", "cc", "vc", "hi", "ge", "gt", "nv",
};
#define CC(n) ccname[(n) & 15]

typedef struct {
	uint32_t mask;
	uint32_t bits;
	const char* fmt;
} ins_t;

static ins_t instab[] = {
#include "../out/risc5ins.h"
};

void risc5dis(uint32_t pc, uint32_t ins, char *out) {
	unsigned n = 0;
	while ((ins & instab[n].mask) != instab[n].bits) n++;
	const char* fmt = instab[n].fmt;
	char x;

	unsigned mo = ins & 0xFFFFF;
	unsigned bo = ins & 0xFFFFFF;
	if (mo & 0x80000) mo |= 0xFFF00000;
	if (bo & 0x800000) bo |= 0xFF000000;

	while ((x = *fmt++) != 0) {
		if (x != '%') {
			*out++ = x;
			continue;
		}
		switch (*fmt++) {
		case 'C': out = append_str(out, CC((ins >> 24) & 15)); break;
		case 'a': out = append_str(out, R((ins >> 24) & 15)); break;
		case 'b': out = append_str(out, R((ins >> 20) & 15)); break;
		case 'o': out = append_str(out, OP((ins >> 16) & 15)); break;
		case 'c': out = append_str(out, R(ins & 15)); break;
		case 'n': out = append_i32(out, ins & 0xFFFF); break;
		case 's': out = append_u32(out, (ins & 0xFFFF) | 0xFFFF0000); break;
		case 'N': out = append_u32(out, (ins & 0xFFFF) << 16); break;
		case 'm': out = append_i32(out, mo); break;
		case 'B': out = append_u32(out, pc + 4 + (bo << 2)); break;
		}
	}
	*out = 0;
}
