#include <stdio.h>

#include "risc5emu.h"

uint32_t data[] = {
	0x40000030, // mov r0, 0x40
	0x51000100, // mov r1, 0xffff0100
	0xA0100000, // sw r0, [r1, 0]
	0xE7FFFFFE, // b -3
};

int main(int argc, char** argv) {
	risc_t *r = risc_new();

	for (unsigned n = 0; n < sizeof(data); n += 4) {
		risc_store_word(r, n, data[n/4]);
	}

	risc_run(r, 100);
	return 0;
}
