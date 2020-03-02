// Copyright 2020, Brian Swetland <swetland@frotz.net>
// Licensed under the Apache License, Version 2.0.

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>

#include "fs.h"

int read_sector(int fd, uint32_t sector, void* buffer) {
	if (sector % 29) {
		fprintf(stderr, "invalid sector $%u\n", sector);
	}
	sector = (sector / 29) - 1;
	if (pread(fd, buffer, 1024, 1024 * sector) != 1024) {
		fprintf(stderr, "cannot read sector #%u\n", sector);
		return -1;
	} else {
		return 0;
	}
}

char* fixfn(char* fn) {
	for (int i = 0; i < 32; i++) {
		if ((fn[i] < ' ') || (fn[i] > 127)) fn[i] = '.';
	}
	fn[31] = 0;
	return fn;
}

int dump_dir(int fd, uint32_t sector) {
	ofs_dir_page dp;
	ofs_file f;
	if (read_sector(fd, sector, &dp)) return -1;
	if (dp.mark != OFS_DIR_MARK) {
		fprintf(stderr, "dir page @%u bad mark 0x%08x\n", sector, dp.mark);
		return -1;
	}
	printf("      m=%u p0=%u\n", dp.m, dp.p0);
	if (dp.p0) dump_dir(fd, dp.p0);
	for (int e = 0; e < 24; e++) {
		printf("e[%02d] fn='%s' adr=%u p=%u\n", e,
			dp.e[e].filename, dp.e[e].adr, dp.e[e].p);
		if (dp.e[e].p) dump_dir(fd, dp.e[e].p);
#if 0 
		if (read_sector(fd, dp.e[e].adr, &f)) return -1;
		if (f.mark != OFS_FILE_MARK) {
			fprintf(stderr, "file sector @%u bad mark 0x%08x\n",
				dp.e[e].adr, f.mark);
			return -1;
		}
		printf("      aleng=%u bleng=%u date=%u\n",
			f.aleng, f.bleng, f.date);
		printf("      sectors [");
		for (int s = 0; s < 64; s++) {
			if (f.sector[s]) printf(" %u", f.sector[s]);
		}
		printf(" ]\n");
#endif
	}
	return 0;
}

int main(int argc, char** argv) {
	int fd;
	if (argc != 2) return -1;
	if ((fd = open(argv[1], O_RDONLY)) < 0) {
		fprintf(stderr, "cannot open '%s'\n", argv[1]);
		return -1;
	}

	dump_dir(fd, 29);
}
