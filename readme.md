
# Unnamed Compiled Systems Language Project

> "O. Inspired by Oberon and gO, and it's like C without the sharp edges." - @adamwp

It doesn't really have a name yet.  The project is still very early.  The syntax and features are
incomplete and will change.  It is way, way too early to do much of anything but watch
me tinker with things, building this incrementally.

The general plan is a small compiled systems language (in complexity, source size, and binary size)
borrowing syntax from some of my favorite "braces languages", C, Go, and Rust, aiming to be a bit
safer than C, and suitable for small, embedded, self-hosted systems.

### [Why write a compiler?](docs/why-write-a-compiler.md)

### [Resources and Inspiration](docs/resources-and-inspiration.md)

### [Notebook](docs/notebook.md) 

### Status

It's currently compiling a subset of the work-in-progress language and generating binaries for
the Project Oberon [Risc 5 Architecture](docs/project-oberon-risc5-architecture.txt).  

I plan to also support RISCV (RV32I) as a target soonish, and if the project keeps moving
will eventually target X86-64 because why not.

It's presently written in C, but once the core language is suitably featureful and codegen is
reasonably reliable I plan to translate it to itself and become self-hosted.
