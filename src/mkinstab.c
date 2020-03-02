// Copyright 2019, Brian Swetland <swetland@frotz.net>
// Licensed under the Apache License, Version 2.0.

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>

int main(int argc, char** argv) {
	char line[128];
	while (fgets(line, sizeof(line), stdin) != NULL) {
		unsigned end = strlen(line);
		while (end > 0) {
			end--;
			if (!isspace(line[end])) break;
			line[end] = 0;
		}
		if ((line[0] == 0) || (line[0] == '#') ||
			isspace(line[0]) || (end < 34)) {
			continue;
		}
		uint32_t mask = 0, bits = 0;
		for (unsigned n = 0; n < 32; n++) {
			uint32_t bit = 1U << (31 - n);
			switch (line[n]) {
			case '1':
				mask |= bit;
				bits |= bit;
				break;
			case '0':
				mask |= bit;
				break;
			} 
		}
		printf("{ 0x%08x, 0x%08x, \"%s\" },\n", mask, bits, line + 33);
	}
	return 0;
}
