// Copyright 2020, Brian Swetland <swetland@frotz.net>
// Licensed under the Apache License, Version 2.0.

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>

static const char* opname[16] = {
	"MOV", "LSL", "ASR", "ROR", "AND", "ANN", "IOR", "XOR",
	"ADD", "SUB", "MUL", "DIV", "FAD", "FSB", "FML", "FDV",
};
const char* OP(uint32_t n) { return opname[n & 15]; }

static const char* ccname[16] = {
	"MI", "EQ", "CS", "VS", "LS", "LT", "LE", "",
	"PL", "NE", "CC", "VC", "HI", "GE", "GT", "NV",
};
const char* CC(uint32_t n) { return ccname[n & 15]; }

static const char* regname[16] = {
	"R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7",
	"R8", "R9", "R10", "R11", "MT", "SB", "SP", "LR",
};
const char* R(uint32_t n) { return regname[n & 15]; }

void disasm(uint32_t pc, uint32_t ins) {
	uint32_t t = ins >> 28;
	uint32_t a = (ins >> 24) & 15;
	uint32_t b = (ins >> 20) & 15;
	uint32_t o = (ins >> 16) & 15;
	uint32_t c = ins & 15;
	uint32_t i16 = ins & 0xffff;
	uint32_t i20 = ins & 0xfffff;
	uint32_t i24 = ins & 0xffffff;
	if (i16 & 0x8000) i16 |= 0xffff0000;
	if (i20 & 0x80000) i20 |= 0xfff00000;
	if (i24 & 0x800000) i24 |= 0xff000000;

	printf("%08x: %08x  ", pc, ins);
	switch (t) {
	//     --uv
	case 0b0000:
	case 0b0001:
	case 0b0010:
	case 0b0011:
		if (o == 0) { // MOV ignores Rb
			switch (t & 3) {
			case 0b00:
			case 0b01:
				printf("MOV %s, %s\n", R(a), R(c));
				break;
			case 0b10:
				printf("MOV %s, H\n", R(a));
				break;
			case 0b11:
				printf("MOV %s, NZCF\n", R(a));
				break;
			}
		} else {
			const char* op = OP(o);
			switch (o) { // u-bit modifiers
			case 8: if (t & 2) op = "ADC"; break;
			case 9: if (t & 2) op = "SBC"; break;
			case 10: if (t & 2) op = "UMUL"; break;
			}
			printf("%s %s, %s, %s\n", op, R(a), R(b), R(c));
		}
		break;
	case 0b0100:
	case 0b0101:
	case 0b0110:
	case 0b0111:
		if (o == 0) {
			printf("MOV %s, %d\n", R(a), (t & 1) ? (i16 << 16) : i16);
		} else {
			const char* op = OP(o);
			switch (o) { // u-bit modifiers
			case 8: if (t & 2) op = "ADC"; break;
			case 9: if (t & 2) op = "SBC"; break;
			case 10: if (t & 2) op = "UMUL"; break;
			}
			printf("%s %s, %s, %d\n", op, R(a), R(b), i16);
		}
		break;
	case 0b1000:
		printf("LW %s, [%s, %d]\n", R(a), R(b), i20);
		break;
	case 0b1001:
		printf("LB %s, [%s, %d]\n", R(a), R(b), i20);
		break;
	case 0b1010:
		printf("SW %s, [%s, %d]\n", R(a), R(b), i20);
		break;
	case 0b1011:
		printf("SB %s, [%s, %d]\n", R(a), R(b), i20);
		break;
	case 0b1100:
		switch ((ins >> 4) & 15) {
		case 0b0000:
			printf("B%s %s\n", CC(a), R(c));
			break;
		case 0b0001:
			printf("RTI %s\n", R(c));
			break;
		case 0b0010:
			printf("%s\n", ins & 1 ? "STI" : "CLI");
			break;
		default:
			printf("??? %08x\n", ins);
			break;
		}
		break;
	case 0b1101:
		printf("BL%s %s\n", CC(a), R(c));
		break;
	case 0b1110:
		printf("B%s %d\n", CC(a), i24);
		break;
	case 0b1111:
		printf("BL%s %d\n", CC(a), i24);
		break;
	default:
		printf("??? %08x\n", ins);
		break;
	}
}

const char* readname(int fd) {
	static char name[64];
	for (int n = 0; n < 64; n++) {
		if (read(fd, name + n, 1) != 1) exit(1);
		if (name[n] == 0) return name;
	}
	exit(1);
	return NULL;
}

uint32_t readint(int fd) {
	uint32_t n;
	if (read(fd, &n, 4) != 4) exit(1);
	return n;
}

uint32_t readbyte(int fd) {
	uint8_t n;
	if (read(fd, &n, 1) != 1) exit(1);
	return n;
}

uint32_t dishdr(int fd) {
	const char* name = readname(fd);
	uint32_t key = readint(fd);
	uint32_t cls = readbyte(fd);
	uint32_t size = readint(fd);
	printf("[ name='%s', key=%08x, class=%02x, size=%u ]\n",
		name, key, cls, size);
	printf("[ imports:");
	for (;;) {
		name = readname(fd);
		if (name[0] == 0) break;
		printf(" %s(%08x)", name, readint(fd));
	}
	printf(" ]\n");
	// type descrs?
	uint32_t n = readint(fd) / 4;
	printf("[ typedesc=%u", n);
	while (n > 0) { readint(fd); n--; }
	// data?
	n = readint(fd);
	printf(", data=%08x", n);
	// stringdata
	n = readint(fd);
	printf(", stringdata=%u", n);
	while (n > 0) { readbyte(fd); n--; }
	// instructions
	n = readint(fd);
	printf(", instructions=%u ]\n", n);
	return n;
}

int main(int argc, char** argv) {
	int fd;
	if (argc != 2) return -1;
	if ((fd = open(argv[1], O_RDONLY)) < 0) return -1;

	uint32_t count;
	count = strlen(argv[1]);
	if ((count > 5) && (!strcmp(argv[1] + count - 4, ".rsc"))) {
		count = dishdr(fd);
	} else {
		count = 0xffffffff;
	}

	uint32_t ins;
	uint32_t pc = 0;
	while (count > 0) {
		count--;
		if (read(fd, &ins, sizeof(ins)) != sizeof(ins)) break;
		disasm(pc, ins);
		pc += 4;
	}
	return 0;
}
