# Notebook

A place to jot down ideas and/or commentary about the work in progress.

### 2020 Mar 10 - Non-optimal codegen

When a break / continue / return takes control out of a scope, we're
not smart enough to discard stuff like the jump-around-else emitted
at the end of an if block, resulting in a dead branch op:

```text
	if (n < 2) {
		return n;

00000020: 80e00004  ldw r0, [sp, 4]
00000024: e700000d  b 0x5c
00000028: e700000c  b 0x5c
```

We move return values from r0 to a temp register to cover cases where
we may immediately need r0 again (another call, etc) but that means
extra instructions and copies in other cases like `return fib(n - 1) + fib(n - 2);`

```text
0000002c: 88e00004  ldw r8, [sp, 4]   # get n
00000030: 40890001  sub r0, r8, 1     # r0 = n - 1
00000034: f7fffff3  bl 0x4            # r0 = fib(r0)
00000038: 08000000  mov r8, r0        # r0 -> tmp0
0000003c: 89e00004  ldw r9, [sp, 4]   # get n
00000040: 40990002  sub r0, r9, 2     # r0 = n - 2
00000044: a8e00008  stw r8, [sp, 8]   # save tmp0
00000048: f7ffffee  bl 0x4            # r0 = fib(r0)
0000004c: 88e00008  ldw r8, [sp, 8]   # restore tmp0
00000050: 09000000  mov r9, r0        # r0 -> tmp1
00000054: 00880009  add r0, r8, r9    # r0 = tmp0 + tmp1
00000058: e7000000  b 0x5c            # jump to epilogue (return)
```

### 2020 Mar 09 - Simplifying the Code

Up til now I had been passing around a "compiler context" pointer, so
almost every method looked like `parse_xyz(Ctx ctx, ...)` with a sort of
explicit this pointer as the first argument.

This caused a lot of clutter, and it will not help performance in any
useful way once it's self-hosting (since globals are loaded relative
to a global pointer register, whereas pointers first have to be grabbed
relative to sp and then further dereferenced).

It is useful from a namespace clutter standpoint to keep the struct
rather than 20-some stray globals.

See: [commit 1086c2aa1](https://github.com/swetland/compiler/commit/1086c2aa1133ed44b462b9a360f180ef9f0a851e)

### 2020 Mar 09 - Path to Self-Hosting

As a big goal of the project is for the compiler to be self-hosted, able
to compile itself, and the bigger and more complex it and the language
gets the more work converting the source base over is likely to be, I've
been thinking about the simplest path to a MVP self-hosting compiler.

Expressions and simple flow control (if/else, while, break, return)
will be nearly identical to C.  Basic function and structure declarations
will be very similar (mostly flipping the name/type order and some slight
punctuation changes).

My thinking is by writing the compiler in a strict subset of C that is
very regular it should be pretty simple to mechanically translate the
compiler with a script.  Ideally entirely by script so I could maintain
the compiler in both languages long enough to do some "co-simulation"
style testing to ensure I'm getting identical compilation from both.

The remaining pieces needed to hit the MVP target are, I believe:

- [ ] finish if/while flow control
- [ ] support for enums, or convert enums to consts
- [ ] support for structs
- [ ] support for arrays
- [ ] support for pointers
- [ ] support for globals, init'ing gp
- [ ] support for open/close/read/write "syscalls"
- [ ] mini std library (malloc-analogue, alloc-only heap, string utils)
- [ ] rewrite lexer to use if/else instead of case
- [ ] emulator support for commandline args passing

Okay, that's still a decent chunk of work.  But most of it is straight
forward for a base implementation.

We can survive for a while without floating point, structure extension,
whatever case/match mechanism ends up looking like, etc, etc.  Data
structures are all about dumb, single-linked lists for now.  Hashtables
or Trees can come much later in the process.

I need to make some decisions on pointers vs references.

I should just make ctx global -- passing it like a this pointer clutters
up the code and doesn't buy much.  And it'll be slower for quite a while,
since for now param -> ldw -> pointer-in-reg -> ldw -> value in struct is
more work than just loading relative to the global pointer.


