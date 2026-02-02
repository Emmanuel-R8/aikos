# Research: Maiko Emulator Implementation in Zig

**Date**: 2025-12-04
**Feature**: Maiko Emulator Implementation in Zig
**Phase**: Phase 0 - Research

## Research Objectives

Determine technical approach, language features, dependencies, and implementation patterns for implementing Maiko emulator in Zig programming language.

## Research Tasks

### Task 1: Zig Language Features for VM Implementation

**Research Question**: What Zig language features are best suited for implementing a bytecode VM?

**Findings**:

- **Memory Management**: Zig's explicit memory management with allocators provides fine-grained control needed for VM memory management
- **C Interop**: Zig's excellent C interop enables using SDL libraries directly without bindings
- **Error Handling**: Zig's error unions provide type-safe error handling without exceptions
- **Comptime**: Zig's compile-time execution can be used for opcode dispatch optimization
- **No Hidden Allocations**: Zig's "no hidden allocations" guarantee helps with GC implementation

**Decision**: Use Zig's standard library allocators, C interop for SDL, error unions for error handling, and comptime for opcode table generation.

**Rationale**: Zig's explicit memory management aligns with VM requirements. C interop simplifies SDL integration. Error unions provide better error handling than C's error codes.

**Alternatives Considered**:

- Custom allocators: More complex, standard allocators sufficient for MVP
- Zig bindings for SDL: C interop simpler and more direct
- Exception-based error handling: Zig doesn't support exceptions, error unions are idiomatic

### Task 2: SDL Version Selection

**Research Question**: Should we use SDL2 or SDL3 for the Zig implementation?

**Findings**:

- **SDL2**: Mature, stable, widely available, well-documented
- **SDL3**: Newer API, better performance, modern design, improved cross-platform support
- **Zig Compatibility**: Both work with Zig C interop
- **Maiko C Implementation**: Uses SDL2 primarily, SDL3 support added recently
- **User Requirement**: Explicit specification requires SDL3 (spec.md FR-003)

**Decision**: Use SDL3 for the Zig implementation.

**Rationale**: SDL3 provides a modern API with better performance and improved cross-platform support. The specification explicitly requires SDL3 (FR-003), and SDL3 aligns with future-proofing the implementation. While SDL3 is newer, it provides better long-term maintainability and matches the explicit requirement in the specification.

**Alternatives Considered**:

- SDL2: More mature but doesn't meet specification requirement
- Both versions: Adds unnecessary complexity, SDL3 sufficient and required

### Task 3: Memory Layout Compatibility Strategy

**Research Question**: How to maintain exact memory layout compatibility with C implementation?

**Findings**:

- **Struct Packing**: Zig supports `packed struct` for exact byte layout matching
- **Alignment**: Zig's `align()` attribute can match C struct alignment
- **Endianness**: Zig's `@byteSwap()` handles endianness conversion
- **Memory Mapping**: Zig can use `std.mem.bytesAsSlice()` for memory-mapped access

**Decision**: Use `packed struct` for data structures, explicit alignment attributes, and byte swapping utilities for cross-platform compatibility.

**Rationale**: Exact memory layout is critical for sysout file compatibility. Zig's struct packing ensures byte-for-byte compatibility with C structures.

**Alternatives Considered**:

- Normal structs: May have different padding, breaks compatibility
- Manual byte manipulation: More error-prone, struct packing is cleaner

### Task 4: Build System Approach

**Research Question**: How to structure Zig build system for VM implementation?

**Findings**:

- **build.zig**: Zig's standard build system, declarative configuration
- **Package Management**: Zig 0.15.1 has built-in package manager
- **Cross-compilation**: Zig supports cross-compilation natively
- **Dependencies**: Can use system libraries or Zig packages

**Decision**: Use `build.zig` with system SDL3 library, Zig standard library for core functionality.

**Rationale**: Zig's build system is simple and powerful. System SDL3 avoids dependency management complexity. Standard library provides all needed utilities.

**Alternatives Considered**:

- Zig package for SDL: System library simpler, more reliable
- External build system: Zig build system is sufficient

### Task 5: Testing Strategy

**Research Question**: How to test Zig implementation against C implementation?

**Findings**:

- **Zig Test Framework**: Built-in `zig test` command
- **Reference Behaviors**: Test cases from rewrite documentation
- **Sysout Compatibility**: Load same sysout files, compare execution results
- **Integration Tests**: Test against real Lisp programs

**Decision**: Use Zig test framework with reference behavior test cases, sysout file loading tests, and comparison tests against C implementation.

**Rationale**: Zig's test framework is integrated and easy to use. Reference behaviors provide concrete test cases. Sysout compatibility tests ensure correctness.

**Alternatives Considered**:

- External test framework: Zig test framework sufficient
- Manual testing only: Automated tests essential for correctness

### Task 6: Error Handling Patterns

**Research Question**: What error handling pattern fits VM implementation?

**Findings**:

- **Error Unions**: Zig's `!Type` syntax for error handling
- **Error Sets**: Define specific error types
- **Error Propagation**: Use `try` and `catch` keywords
- **VM Errors**: Need VM-specific error types (stack overflow, invalid opcode, etc.)

**Decision**: Use error unions with custom error sets for VM errors. Propagate errors using `try` keyword.

**Rationale**: Error unions provide type-safe error handling. Custom error sets enable specific VM error reporting. `try` keyword simplifies error propagation.

**Alternatives Considered**:

- Return codes: Less type-safe, error unions preferred
- Panic on errors: Too harsh, need graceful error handling

### Task 7: Opcode Dispatch Implementation

**Research Question**: How to implement opcode dispatch in Zig?

**Findings**:

- **Switch Statement**: Zig supports switch with opcode values
- **Comptime**: Can generate opcode table at compile time
- **Function Pointers**: Zig supports function pointers for opcode handlers
- **Performance**: Switch is fast, comptime can optimize

**Decision**: Use switch statement for opcode dispatch with comptime-generated opcode table. Each opcode maps to handler function.

**Rationale**: Switch statement is idiomatic Zig and performs well. Comptime can optimize opcode table generation. Function pointers enable clean opcode handler organization.

**Alternatives Considered**:

- Computed goto: Zig doesn't support computed goto, switch is equivalent
- Hash table dispatch: Overhead unnecessary, switch is direct

## Key Decisions Summary

| Decision | Choice | Rationale |
|----------|--------|-----------|
| SDL Version | SDL3 | Modern API, better performance, required by specification (FR-003) |
| Memory Layout | Packed structs | Ensures exact compatibility with C structures |
| Build System | Zig build.zig | Native, simple, supports cross-compilation |
| Error Handling | Error unions | Type-safe, idiomatic Zig |
| Opcode Dispatch | Switch statement | Fast, idiomatic, can use comptime optimization |
| Testing | Zig test + reference behaviors | Integrated framework, concrete test cases |

## Unresolved Questions

None - all research questions resolved.

## Next Steps

Proceed to Phase 1: Design to create data model, contracts, and quickstart guide.
