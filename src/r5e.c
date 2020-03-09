#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>

#include "risc5emu.h"

uint32_t data[] = {
	0x4000FFD2, // mov r0, 0xFFD2
	0x51000100, // mov r1, 0xffff0100
	0xA0100000, // sw r0, [r1, 0]
	0xE7FFFFFF, // b -1
};

int main(int argc, char** argv) {
	bool trace = false;
	const char* fn = NULL;

	while (argc > 1) {
		if (!strcmp(argv[1], "-t")) {
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

	risc_t *r = risc_new(trace);

	if (fn == NULL) {
		for (unsigned n = 0; n < sizeof(data); n += 4) {
			risc_store_word(r, n, data[n/4]);
		}
	} else {
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
		//fprintf(stderr,"r5e: loaded %u bytes from '%s'\n", n, fn);
	}

	risc_run(r, 100000);
	return 0;
}
