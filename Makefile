
all: bin/tlc bin/fs bin/r5d bin/r5e bin/mkinstab

clean:
	rm -rf bin out

CFLAGS := -Wall -O2 -g
CC := gcc

bin/tlc: src/tlc.c
	@mkdir -p bin
	$(CC) -o $@ $(CFLAGS) src/tlc.c

bin/fs: src/fs.c src/fs.h
	@mkdir -p bin
	$(CC) -o $@ $(CFLAGS) src/fs.c

bin/r5d: src/r5d.c src/risc5dis.c
	@mkdir -p bin
	$(CC) -o $@ $(CFLAGS) src/r5d.c src/risc5dis.c

bin/r5e: src/r5e.c src/risc5emu.c src/risc5emu-fp.c src/risc5dis.c
	@mkdir -p bin
	$(CC) -o $@ $(CFLAGS) src/r5e.c src/risc5emu.c src/risc5emu-fp.c src/risc5dis.c

bin/mkinstab: src/mkinstab.c
	@mkdir -p bin
	$(CC) -o $@ $(CFLAGS) src/mkinstab.c

out/risc5ins.h: src/risc5ins.txt bin/mkinstab
	@mkdir -p out
	bin/mkinstab < src/risc5ins.txt > $@

src/risc5dis.c: src/risc5.h out/risc5ins.h
src/risc5emu.c: src/risc5emu.h src/risc5.h
src/risc5emu-fp.c: src/risc5emu-fp.h
src/r5d.c: src/risc5.h
src/r5e.c: src/risc5emu.h

