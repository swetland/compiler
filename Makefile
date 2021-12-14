
all: bin/cold bin/cast bin/cir bin/fs bin/r5d bin/r5e bin/mkinstab 
	
runtests: out/test/summary.txt

clean:
	rm -rf bin out

out/compiler.src: out/compiler.txt bin/rewriter
	@mkdir -p out
	bin/rewriter out/compiler.txt > out/compiler.src

out/compiler.txt: src/compiler.c bin/preproc
	@mkdir -p out
	bin/preproc < src/compiler.c > out/compiler.txt

CFLAGS := -Wall -g -Iexternal/oberon-risc-emu -Isrc -fno-builtin
CFLAGS += -Wno-unused-but-set-variable -Wno-unused-variable
#CFLAGS := -O2

CC := gcc

bin/cold: src/compiler.c src/risc5dis.c out/risc5ins.h
	@mkdir -p bin
	$(CC) -o $@ $(CFLAGS) -Wno-unused-result -DC src/compiler.c src/risc5dis.c

bin/cast: src/compiler2.c src/codegen-risc5-simple.c src/risc5dis.c out/risc5ins.h
	@mkdir -p bin
	$(CC) -o $@ $(CFLAGS) -Wno-unused-result -DC src/compiler2.c src/risc5dis.c

bin/cir: src/compiler2.c src/codegen-ir.c src/risc5dis.c out/risc5ins.h
	@mkdir -p bin
	$(CC) -o $@ $(CFLAGS) -Wno-unused-result -DIR -DC src/compiler2.c src/risc5dis.c

bin/rewriter: src/rewriter.c
	@mkdir -p bin
	$(CC) -o $@ $(CFLAGS) src/rewriter.c

bin/preproc: src/preproc.c
	@mkdir -p bin
	$(CC) -o $@ $(CFLAGS) src/preproc.c

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
	$(CC) -o $@ $(CFLAGS) -O2 src/r5e.c src/risc5dis.c $(RISC5EMU_SRC)

bin/mkinstab: src/mkinstab.c
	@mkdir -p bin
	$(CC) -o $@ $(CFLAGS) src/mkinstab.c

out/risc5ins.h: src/risc5ins.txt bin/mkinstab
	@mkdir -p out
	bin/mkinstab < src/risc5ins.txt > $@

# have to have two rules here otherwise tests without .log files
# fail to be compiled by the rule that depends on src+log *or*
# we fail to depend on the .log for tests with both...

out/test/%.txt: test/%.src test/%.log bin/cold bin/r5e test/runtest.sh
	@mkdir -p out/test
	@rm -f $@
	@test/runtest.sh $< $@

out/test/%.txt: test/%.src bin/cold bin/r5e test/runtest.sh
	@mkdir -p out/test
	@rm -f $@
	@test/runtest.sh $< $@

out/test2/%.txt: test/%.src test/%.log bin/cast bin/r5e test/runtest2.sh
	@mkdir -p out/test2
	@rm -f $@
	@test/runtest2.sh $< $@

out/test2/%.txt: test/%.src bin/cast bin/r5e test/runtest2.sh
	@mkdir -p out/test2
	@rm -f $@
	@test/runtest2.sh $< $@

SRCTESTS := $(sort $(wildcard test/*.src))
ALLTESTS := $(patsubst test/%.src,out/test/%.txt,$(SRCTESTS))
ALLTESTS += $(patsubst test/%.src,out/test2/%.txt,$(SRCTESTS))

out/test/summary.txt: $(ALLTESTS)
	@cat $(ALLTESTS) > out/test/summary.txt

