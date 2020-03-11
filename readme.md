
# Unnamed Compiled Systems Language Project

> "O. Inspired by Oberon and gO, and it's like C without the sharp edges." - @adamwp

It doesn't really have a name yet.  The project is still very early.  
The syntax and features are incomplete and will change.  It is way, way
too early to do much of anything but watch me tinker with things,
building this incrementally.

The general plan is a small compiled systems language (in complexity,
source size, and binary size) borrowing syntax from some of my favorite
"braces languages", C, Go, and Rust, aiming to be a bit safer than C,
and suitable for small, embedded, self-hosted systems.

### [Why write a compiler?](docs/why-write-a-compiler.md)
Do you have to have a reason?

### [Resources and Inspiration](docs/resources-and-inspiration.md)
Documents and things that inspired this adventure.

### [Notebook](docs/notebook.md) 
A place for commentary, ideas, and random jottings.

### [Todo List](docs/todo.md)
Little boxes to be ticked when tasks are completed.

### Status

It's currently compiling a subset of the work-in-progress language and
generating binaries for the Project Oberon
[Risc 5 Architecture](docs/project-oberon-risc5-architecture.txt).  

I plan to also support RISCV (RV32I) as a target soonish, and if the
project keeps moving will eventually target X86-64 because why not.

It's presently written in C, but once the core language is suitably
featureful and codegen is reasonably reliable I plan to translate it
to itself and become self-hosted.

### License and Third Party Code

All of the code here is provided under the
[Apache 2.0 License](https://www.apache.org/licenses/LICENSE-2.0.txt)
with the exception of third party modules which live under the
[external/](external/) directory.

Notably, [external/oberon-risc-emu/](external/oberon-risc-emu/) contains
the core RISC5 Emulator implemention from Peter Dr Wachter's excellent
[Oberon Risc Emu](https://github.com/pdewacht/oberon-risc-emu), which is
under a [BSD-like license](external/oberon-risc-emu/LICENSE).

I borrowed just the CPU emulation for a simple commandline runtime.  The
original project is a full Project Oberon Workstation emulator.

