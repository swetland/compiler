# Why Write a Compiler?

I think my primary motivation is simply that I had never done so before.

Across 30-some years programming, I've written a bunch of assemblers,
several interpreted languages, a number of bytecode runtimes, a linker/preprocessor
for java class files and bytecode into a smaller/lighter bytecode (for
the hiptop family devices), various disassembler, and a pile of domain specific
configuration languages, but never a full top-to-bottom compiled programming language
that starts with source code and ends up with executable machine instructions of
some form or another.

It feels like one of those things that everybody should do at least once.

### Open Source FPGA Toolchains

Advances in the state of the art of open source FPGA toolchains have made doing
"real" digital design stuff without awful vendor tools possible, which re-kindled
my interest in this space.  It's pretty trivial to build a little CPU on verilog.
It's not hard to crank out a little assembler for it.  But if you're doing anything
serious, writing a lot of software in assembly is just not that fun or efficient,
at least not for me.  I'd like to have a little systems compiler amongst my tools
that I could easily target various little CPUs with.

### The Big Compilers are Way Too Big

GCC and LLVM/clang are open source.  And very powerful.  And frickin' enormous.

So while I could invest in learning how to make use of, say, LLVM to build my own
frontends and share backends, or write backends for either, both of them are quite
enormous, complex source bases, and I'm not super excited about dealing with giant
piles of other peoples' software.

### Self-Hosting and Embedded Platforms

While I'm writing the initial version in C, once I have sufficient feature coverage
I hope to mechanically translate the C version and migrate to building the compiler
with itself.  A Self-Hosting Compiler is another one of those "it'd be fun to do
that at least once" sort of things.

I'd also like to be able to use the compiler not just to build for but run on small
embedded devices, retro computing systems, or experimental small platforms.  On the
scale of single-digit megabytes of memory or less at the low end.  That definitely
rules out GCC and clang.

### At Most, I Want "Just Enough" Optimization

Modern optimizing compilers are amazing things, but they increasingly seem to be
getting to clever for their own good (or at least mine).  For systems programming,
especially, I really don't want the compiler to silently drop code or massively
rearrange it.  I want to be able to rely on the compiler mostly doing what I tell
it and not getting all inventive about "undefined behaviour."

C and C++ have a bunch of undefined behaviour around integer and unsigned math,
for example, which result in a weird gap between what the underlying machine does
if you ask it and what a modern compiler considers "valid", and if "not valid"
happily pretends like it doesn't matter.  In a way I'm looking for a bit more of
the "high level assembly language" that C is often accused of being while it is
increasingly not...

### They Still Haven't Built me a "Better C"

Go comes close, but its insistence of green threads, userspace scheduling, garbage
collection, largish libraries, large runtime memory footprint, and really awkward
interworking with native C/ELF ABI code are deal breakers.

Rust is more pragmatic about inteworking with C/C++ native code and native platform
threading (yay), but still suffers from a large standard library that doesn't seem
to layer/subset well, resulting in even small programs being several megabytes in size.
I feel like its heart is in the right place with the lifetime tracking stuff, but it
feels rather awkward to code with.  It also suffers from very slow compilation.

And so on and so on.

This project gives me a chance to do my own experimenting, and while I am skeptical
that I shall succeed wildly where all others have failed, at least I'll have fun trying.
