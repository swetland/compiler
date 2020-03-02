// Copyright 2020, Brian Swetland <swetland@frotz.net>
// Licensed under the Apache License, Version 2.0.

#include <stdint.h>

#define OFS_FILE_MARK 0x9BA71D86
#define OFS_DIR_MARK  0x9B1EA38D

typedef struct {
	uint32_t mark;
	uint8_t filename[32];
	uint32_t aleng, bleng, date;
	uint32_t extension[12];
	uint32_t sector[64];
	uint32_t data[672];
} ofs_file;

typedef struct {
	uint8_t filename[32];
	uint32_t adr;
	uint32_t p;
} ofs_dir_entry;

typedef struct {
	uint32_t mark;
	uint32_t m;
	uint32_t p0;
	uint8_t pad[52];
	ofs_dir_entry e[24];
} ofs_dir_page;
