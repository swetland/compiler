# Notebook

A place to jot down ideas and/or commentary about the work in progress.

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


