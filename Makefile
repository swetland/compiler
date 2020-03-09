
all: bin/compiler bin/fs bin/r5d bin/r5e bin/mkinstab out/test/summary.txt

clean:
	rm -rf bin out

CFLAGS := -Wall -O2 -g -Iexternal/oberon-risc-emu -Isrc
CC := gcc

bin/compiler: src/compiler.c src/risc5dis.c out/risc5ins.h
	@mkdir -p bin
	$(CC) -o $@ $(CFLAGS) src/compiler.c src/risc5dis.c

bin/fs: src/fs.c src/fs.h
	@mkdir -p bin
	$(CC) -o $@ $(CFLAGS) src/fs.c

bin/r5d: src/r5d.c src/risc5dis.c out/risc5ins.h
	@mkdir -p bin
	$(CC) -o $@ $(CFLAGS) src/r5d.c src/risc5dis.c

RISC5EMU_SRC := \
	external/oberon-risc-emu/risc5emu.c \
	external/oberon-risc-emu/risc5emu-fp.c \

bin/r5e: src/r5e.c src/risc5dis.c $(RISC5EMU_SRC)
	@mkdir -p bin
	$(CC) -o $@ $(CFLAGS) src/r5e.c src/risc5dis.c $(RISC5EMU_SRC)

bin/mkinstab: src/mkinstab.c
	@mkdir -p bin
	$(CC) -o $@ $(CFLAGS) src/mkinstab.c

out/risc5ins.h: src/risc5ins.txt bin/mkinstab
	@mkdir -p out
	bin/mkinstab < src/risc5ins.txt > $@

out/test/%.txt: test/%.src bin/compiler bin/r5d ./runtest.sh
	@mkdir -p out/test
	@rm -f $@
	@./runtest.sh $< $@

SRCTESTS := $(sort $(wildcard test/*.src))
ALLTESTS := $(patsubst test/%.src,out/test/%.txt,$(SRCTESTS))

out/test/summary.txt: $(ALLTESTS)
	@cat $(ALLTESTS) > out/test/summary.txt

