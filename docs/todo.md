# Task Lists, Reminders, Etc.

### Compiler

- [ ] support for slices & string constants
- [ ] break/continue to label
- [ ] const / readonly
- [ ] type inference for var definitions
- [ ] support for named enums
- [x] support for global anonymous enums
- [x] continue
- [x] support for arrays
- [x] support for pointers
- [x] support for globals, init'ing gp
- [x] support for structs
- [x] type definition
- [x] finish if/while flow control

### Emulator / Runtime

- [ ] support for open/close/read/write "syscalls"
- [ ] mini std library (malloc-analogue, alloc-only heap, string utils)
- [x] emulator support for commandline args passing

### Self-Hosting Cleanup

- [ ] printf?
- [ ] punch though stdio, etc
- [ ] abandon all-source-in-ram approach
- [x] eliminate #defines
- [x] rewrite lexer to use if/else instead of case
- [x] table driven lexing

### Syntax

- [ ] `type Foo struct { ... }` vs `struct Foo { ... }`
- [ ] semicolons - optional? mandatory? eliminate?
- [ ] colon between param/field name and type (RustLike) or not (GoLike)?
- [ ] naming scheme for PODs (`i32` vs `int32` vs `int32_t`, etc)

### Optimizations

- [ ] use shifts for power-of-two integer mul/div
- [ ] lazy spilling of parameters
- [ ] lazy spilling of LR / add LR to tmpreg list

### Portability / Modularity

- [ ] RISCV RV32I backend
- [ ] X86-64 backend
- [ ] third party writes a backend
