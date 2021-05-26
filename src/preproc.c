// Copyright 2021, Brian Swetland <swetland@frotz.net>
// Licensed under the Apache License, Version 2.0.

#include <stdio.h>
#include <string.h>

int main(int argc, char** argv) {
	char line[1024];
	int discard = 0;

	while (fgets(line, 1024, stdin) != NULL) {
		if (strncmp(line, "#if C", 5) == 0) {
			discard = 1;
			continue;
		}
		if (strncmp(line, "#else", 5) == 0) {
			discard = 0;
			continue;
		}
		if (strncmp(line, "#endif", 6) == 0) {
			discard = 0;
			continue;
		}
		if (discard) {
			continue;
		}
		fputs(line, stdout);
	}
	return 0;
}

