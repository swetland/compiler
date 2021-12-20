
SRCS := hello.c ssa2.c fib.c gcd.c call.c bigfunc.c struct.c

TARGETS := $(patsubst %.c,%.m2r.ll,$(SRCS)) $(patsubst %.c,%.ll,$(SRCS))

all: all-targets

# -O + disable passes avoids tagging with "optnone"
# which interferes with further passes requested by the opt tool
%.bc: %.c
	clang -c -O -Xclang -disable-llvm-passes -emit-llvm $< -o $@

%.ll: %.bc
	llvm-dis-10 $<

%.m2r.bc: %.bc
	opt-10 -mem2reg $< -o $@

all-targets: $(TARGETS)

clean:
	rm -f *.bc *.ll
