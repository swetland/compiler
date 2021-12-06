#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>

#include "risc5emu.h"

int main(int argc, char** argv) {
	bool trace = false;
	bool no_cycle_limit = false;
	const char* fn = NULL;
	int args = 0;

	while (argc > 1) {
		if (!strcmp(argv[1], "--")) {
			args = argc - 2;
			argv += 2;
			break;
		} else if (!strcmp(argv[1], "-n")) {
			no_cycle_limit = true;
		} else if (!strcmp(argv[1], "-t")) {
			trace = true;
		} else if (argv[1][0] == '-') {
			fprintf(stderr, "r5e: unknown option: %s\n", argv[1]);
			return -1;
		} else {
			if (fn != NULL) {
				fprintf(stderr, "r5e: multiple images not supported\n");
				return -1;
			}
			fn = argv[1];
		}
		argc--;
		argv++;
	}

	if (fn == NULL) {
		fprintf(stderr,
"usage: r5e <options>* <binary> [ -- <arg>* ]\n"
"\n"
"options:   -n    no cycle limit (run forever)\n"
"           -t    trace execution\n"
"\n"
"args:      passed on to emulated program\n"
);
		return -1;
	}

	risc_t *r = risc_new(false);

	// load image
	int fd = open(fn, O_RDONLY);
	if (fd < 0) {
		fprintf(stderr, "r5e: cannot open: %s\n", fn);
		return -1;
	}
	unsigned n = 0;
	uint32_t w;
	while (read(fd, &w, sizeof(w)) == 4) {
		risc_store_word(r, n, w);
		n += 4;
	}

	// setup entry environment
	uint32_t sp = 0x100000;

	// exit shim
	sp -= 16;
	uint32_t lr = sp;
	risc_store_word(r, lr+0, 0x51000000); // mov r1, 0xffff0000
	risc_store_word(r, lr+4, 0xa0100100); // stw r0, [r1, 256]
	risc_store_word(r, lr+8, 0xe7ffffff); // b .
	// point LR at shim
	risc_set_register(r, 15, lr);

	// r0/r1 is an [][]byte of commandline args
	uint32_t r0 = 0;
	uint32_t r1 = 0;
	if (args) {
		sp -= args * 8;
		uint32_t p = sp;
		r0 = p;
		r1 = args;
		while (args > 0) {
			fprintf(stderr, "E %s\n", argv[0]);
			uint32_t n = strlen(argv[0]);
			sp -= (n + 3) & (~3);
			for (uint32_t i = 0; i < n; i++) {
				risc_store_byte(r, sp + i, argv[0][i]);
			}
			risc_store_word(r, p+0, sp);
			risc_store_word(r, p+4, n);
			p += 8;
			args--;
			argv++;
		}
	}
	risc_set_register(r, 0, r0);
	risc_set_register(r, 1, r1);

	// set SP
	risc_set_register(r, 14, sp);

	if (trace) {
		risc_trace(r, true);
		printf("                    SP = %08x\n", sp);
		printf("                    LR = %08x (exit shim)\n", lr);
		printf("                    R0 = %08x **argv\n", r0);
		printf("                    R1 = %08x argc\n", r1);
	}

	do {
		risc_run(r, 100000000);
	} while (no_cycle_limit);

	return 0;
}
