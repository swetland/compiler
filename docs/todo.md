# Task Lists, Reminders, Etc.

### Compiler

- [ ] support for structs
- [ ] support for arrays
- [ ] support for slices
- [ ] support for pointers
- [ ] support for globals, init'ing gp
- [ ] support for enums, or convert enums to consts
- [ ] continue
- [ ] break/continue to label
- [ ] const / readonly
- [ ] type inference for var definitions
- [x] type definition
- [x] finish if/while flow control

### Emulator / Runtime

- [ ] support for open/close/read/write "syscalls"
- [ ] mini std library (malloc-analogue, alloc-only heap, string utils)
- [x] emulator support for commandline args passing

### Self-Hosting Cleanup

- [ ] rewrite lexer to use if/else instead of case
- [ ] eliminate #defines

### Syntax

- [ ] `type Foo struct { ... }` vs `struct Foo { ... }`
- [ ] semicolons - optional? mandatory? eliminate?
- [ ] colon between param/field name and type (RustLike) or not (GoLike)?
- [ ] naming scheme for PODs

### Optimizations

- [ ] use shifts for power-of-two integer mul/div
- [ ] lazy spilling of parameters
- [ ] lazy spilling of LR / add LR to tmpreg list

### Portability / Modularity

- [ ] RISCV RV32I backend
