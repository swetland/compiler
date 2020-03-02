// Copyright 2020, Brian Swetland <swetland@frotz.net>
// Licensed under the Apache License, Version 2.0.

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>

#include "risc5.h"

void disasm(uint32_t pc, uint32_t ins) {
	char buf[256];
	risc5dis(pc, ins, buf);
	printf("%08x: %08x  %s\n", pc, ins, buf);
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
