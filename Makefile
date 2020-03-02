
all: bin/tlc bin/fs bin/r5dis

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

bin/r5dis: src/r5dis.c
	@mkdir -p bin
	$(CC) -o $@ $(CFLAGS) src/r5dis.c
