#import "../core/common.typ": *

= Core Virtual Machine Specification

*Navigation*: Data Structures | Instruction Set | Memory Management | Execution Model | Implementation Notes

Complete specification of the Maiko Interlisp virtual machine core concepts, including number representations, arithmetic operations, memory management, and execution semantics. This document serves as the primary reference for understanding the VM architecture and implementation details.

== AI Assistant Entry Points

This document is organized for AI assistant navigation and reference:

- *For Arithmetic Operations*: See §2.1 Number Representations, §2.2 Arithmetic Operations
- *For Memory Management*: See §3 Memory Management, §4 Stack Management
- *For Opcode Implementation*: See §2.3 Arithmetic Result Processing, §5 Execution Semantics
- *For Type Systems*: See §2.1 Number Representations, §3.1 Type Integration
- *For Stack Operations*: See §4 Stack Management, §4.3 Stack Effects
- *For Error Handling*: See §2.4 Error Handling, §5.3 Error Recovery
- *For Performance*: See §2.5 Performance Characteristics, §3.4 Memory Allocation Patterns

== Number Representation Systems

=== SMALLP (Immediate) Integers

*Range*: -65536 to 65535 (-2^16^ to 2^16^ - 1)

*Format*: `[segment(16 bits) | value(16 bits)]`

*Segments*:
- `S_POSITIVE` (0x0000): Non-negative values
- `S_NEGATIVE` (0xF000): Negative values

*Storage*: Values embedded directly in LispPTR without heap allocation

*Use Case*: Most common integers in Lisp programs

=== FIXP (Boxed) Integers

*Range*: -2,147,483,648 to 2,147,483,647 (-2^31^ to 2^31^ - 1)

*Format*: Pointer to 4-byte memory location

*Type Tag*: `TYPE_FIXP` in high bits of LispPTR

*Storage*: Requires heap allocation for large values

*Use Case*: Large integers exceeding SMALLP range

=== Floating-Point Numbers

*Types*: Single-precision floats stored in boxed format

*Operations*: Automatic fallback for overflow or complex calculations

*Type Tag*: `TYPE_FLOATP`

*Use Case*: Real number arithmetic and complex calculations

*Operations*: Automatic fallback for overflow or complex calculations

*Type Tag*: `TYPE_FLOATP`

*Use Case*: Real number arithmetic and complex calculations

== Arithmetic Operations

=== Core Arithmetic Operations

#table(
  columns: 4,
  [*Operation*], [*Opcode*], [*Stack Effect*], [*Description*],
  [`IPLUS2`], [`0324`], `[tos-1 tos] → [result]`], [Binary addition with overflow detection],
  [`IDIFFERENCE`], [`0331`], `[tos-1 tos] → [result]`], [Binary subtraction with overflow detection],
  [`ITIMES2`], [`0332`], `[tos-1 tos] → [result]`], [Binary multiplication with overflow detection],
  [`IQUOTIENT`], [`0333`], `[tos-1 tos] → [result]`], [Integer division with error handling],
  [`IREMAINDER`], [`0334`], `[tos-1 tos] → [result]`], [Integer modulo operation],
)

=== N_ARITH_SWITCH Dispatch Logic

The `N_ARITH_SWITCH()` macro determines optimal result representation:

#codeblock(lang: "pseudocode", [
function N_ARITH_SWITCH(result: int32):
    if result fits in SMALLP range:
        return SMALLP immediate value
    else:
        allocate FIXP box
        store result in box
        return pointer to FIXP box
])

*Optimization*: Small results avoid heap allocation through immediate representation

*Memory Management*: FIXP allocation integrates with garbage collector

=== Overflow Detection Strategies

==== Compile-Time Detection (USE_OVERFLOW_BUILTINS)

#codeblock(lang: "c", [
if (__builtin_sadd_overflow(arg1, arg2, &result)) {
    ERROR_EXIT(tos);  // Trigger Lisp error
}
])

*Advantages*: Faster execution, reliable overflow detection

*Compatibility*: GCC/Clang builtin functions

*Platforms*: Linux, macOS, Windows (with appropriate compiler)

==== Runtime Detection (Portable)

#codeblock(lang: "c", [
result = arg1 + arg2;
if (((arg1 >= 0) == (arg2 >= 0)) &&
    ((result >= 0) != (arg1 >= 0))) {
    ERROR_EXIT(tos);
}
])

*Advantages*: Works with any compiler

*Method*: Sign analysis of operands vs result

*Performance*: Slight overhead but portable

=== Unified Floating-Point Fallback

==== Automatic Type Coercion

#codeblock(lang: "pseudocode", [
function plus2(arg1, arg2):
    try integer arithmetic
    catch overflow or type error:
        return fplus2(arg1, arg2)  // Float fallback
])

*Trigger Conditions*:
- Integer overflow beyond 32-bit range
- Type errors (non-numeric operands)
- Mixed numeric types requiring coercion

==== Fallback Operation Mapping

#table(
  columns: 2,
  [*Integer Operation*], [*Floating-Point Fallback*],
  [`plus2()`], [`fplus2()`],
  [`difference()`], [`fdifference()`],
  [`times2()`], [`ftimes2()`],
  [`greaterp()`], [`fgreaterp()`],
  [`quotient()`], [`fquotient()`],
)

*Implementation*: Each integer operation has corresponding floating-point variant

*Type Preservation*: Results maintain appropriate type information

== Memory Management

=== Virtual Memory Architecture

*Structure*: Complete Lisp address space allocated as contiguous block

*Size*: Determined by `process_size` from sysout IFPAGE

*Addressing*: 32-bit LispPTR values with type information in high bits

*Page Size*: 512 bytes (256 DLwords) per page

*Translation*: FPtoVP table maps file pages to virtual pages

=== Type System Integration

*Type Tags*: High bits of LispPTR encode object types

*Common Types*:
- `TYPE_FIXP`: Boxed integers
- `TYPE_FLOATP`: Floating-point numbers
- `TYPE_LISTP`: Cons cells and lists
- `TYPE_SYMBOL`: Symbol objects
- `TYPE_STRING`: String objects

*Type Checking*: Runtime type validation for operations

=== Memory Allocation Patterns

- *Immediate Values*: No allocation (SMALLP, characters, etc.)
- *Boxed Objects*: Heap allocation with type tags
- *Arrays/Strings*: Contiguous allocation with length headers
- *Cons Cells*: Specialized allocation with CDR coding

== Stack Management

=== Stack Frame Structure

*Frame Size*: 10 DLwords (20 bytes) per activation record

*Frame Layout*:
- `flags + usecount`: Frame flags and reference count
- `alink`: Activation link (previous frame)
- `fnheader`: Function header pointer
- `nextblock`: Next free stack block
- `pc`: Program counter (return address)
- `name_table`: Local variable name table
- `blink`: Binding link
- `clink`: Closure link
- `locals`: Local variable storage

=== Stack Frame Addressing

*CRITICAL*: Frame pointers in IFPAGE are `DLword` StackOffsets, NOT LispPTR values!

- `currentfxp`: DLword offset from Stackspace base
- `stackbase`: DLword offset from Stackspace base
- `endofstack`: DLword offset from Stackspace base

*C Conversion*: `NativeAligned2FromStackOffset(DLword StackOffset)`

=== Stack Operations

*Push Operations*:
- `tosPush()`: Push value to top of stack
- `tosHardPush()`: Push with stack extension if needed

*Pop Operations*:
- `tosPop()`: Pop value from top of stack
- `popStack()`: Pop with error checking

*Stack Extension*:
- `extendStack()`: Allocate additional stack blocks
- Automatic growth when stack limits reached

=== Stack Effects Documentation

All operations document stack effects using notation: `[before] → [after]`

#table(
  columns: 3,
  [*Operation*], [*Stack Effect*], [*Description*],
  [`IPLUS2`], `[arg1 arg2] → [result]`], [Consumes 2 args, produces 1 result],
  [`CAR`], `[cons] → [car_value]`], [Extract first element],
  [`CDR`], `[cons] → [cdr_value]`], [Extract rest of list],
  [`CONS`], `[car cdr] → [new_cons]`], [Create new cons cell],
)

*Net Effect*: Most operations maintain or reduce stack depth

*Error Conditions*: Stack underflow, type errors

== List Operations and CDR Coding

=== Cons Cell Representation

*Structure*:
#codeblock(lang: "c", [
struct ConsCell {
    LispPTR car_field;    /* First element or data */
    DLword cdr_code;      /* CDR encoding scheme */
};
])

*Memory Layout*: 6 bytes (4 + 2) per cons cell

*Type Tag*: `TYPE_LISTP` in LispPTR high bits

=== CDR Coding System

The CDR field uses sophisticated encoding to compress list patterns:

==== CDR_NIL (8 or 128)
- *Meaning*: CDR is NIL (list terminator)
- *Usage*: Most common case for proper lists
- *Encoding*: Special marker value

==== CDR_ONPAGE (base + offset)
- *Meaning*: CDR points to cell on same memory page
- *Encoding*: `8 + (offset >> 1)` (NEWCDRCODING)
- *Range*: Up to 7 cells forward (NEW) or 127 (OLD)

==== CDR_INDIRECT (0)
- *Meaning*: CAR field contains pointer to actual CDR cell
- *Usage*: Complex or distant CDR relationships
- *Overhead*: Additional indirection

==== CDR_MAXINDIRECT (1-7 or 1-127)
- *Meaning*: CDR points to cell on different page
- *Encoding*: Direct offset in cdr_code field
- *Calculation*: `page_base + (cdr_code << 1)`

=== NEWCDRCODING vs OLD Compatibility

*NEWCDRCODING (Optimized)*:
- Smaller offsets (max 7 cells forward)
- Uses XOR addressing for cell location
- More compact representation for small lists

*OLD (Legacy)*:
- Larger offsets (up to 127 cells forward)
- Direct page base + offset calculation
- Maintains compatibility with older data formats

=== Garbage Collector Integration

List operations integrate with GC through `GCLOOKUP()`:

#codeblock(lang: "c", [
GCLOOKUP(old_value, DELREF);  // Decrease reference count
GCLOOKUP(new_value, ADDREF);  // Increase reference count
])

*Purpose*: Ensures proper memory management and prevents premature garbage collection

*Integration Points*: All destructive operations (rplaca, rplacd)

== Execution Semantics

=== Dispatch Loop Algorithm

#codeblock(lang: "mermaid", [
graph TD
    Start[Start VM] --> Init[Initialize Stack]
    Init --> Enter[Enter Dispatch Loop]
    Enter --> Fetch[Fetch Instruction]
    Fetch --> Decode[Decode Opcode]
    Decode --> Execute[Execute Handler]
    Execute --> Update[Update PC]
    Update --> CheckIRQ{Check Interrupts}
    CheckIRQ -->|Pending| HandleIRQ[Handle Interrupt]
    CheckIRQ -->|None| Fetch
    HandleIRQ --> Fetch
    Fetch -->|Error| Error[Error Handler]
    Error -->|Recover| Fetch
    Error -->|Fatal| Exit[Exit VM]
])

=== Program Counter Management

*PC Units*: Byte offsets in virtual memory (not DLword addresses)

*PC Updates*:
- *Sequential*: `PC += instruction_length`
- *Jumps*: `PC = jump_target` (byte offset)
- *Function Calls*: `PC = function_start` (from fnheader)

*Critical Distinction*: Function calls use `fnheader.startpc`, returns use saved PC in frame

=== Interrupt Handling

*Interrupt Sources*:
- Timer interrupts
- I/O completion
- User interrupts (Ctrl+C)

*Interrupt State*: Stored in `INTSTAT` structure

*Processing*: Integrated into main dispatch loop

== Error Handling and Recovery

=== Error Categorization

*Critical Errors* (immediate termination):
- Stack overflow/underflow
- Invalid memory access
- Corrupted VM state

*Recoverable Errors*:
- Type mismatches
- Invalid opcodes
- Arithmetic overflow

*Warning Conditions*:
- Performance issues
- Resource limits

=== Error Recovery Strategies

==== Critical Error Handling

#codeblock(lang: "pseudocode", [
switch (error_type):
    case STACK_OVERFLOW, STACK_UNDERFLOW:
    case INVALID_ADDRESS, MEMORY_ACCESS_FAILED:
    case INVALID_STACK_POINTER, INVALID_FRAME_POINTER:
        log_critical_error()
        terminate_execution()
        break

    case INVALID_OPCODE:
        log_debug_info()
        skip_instruction()  // Continue for debugging
        break

    default:
        log_error()
        continue_execution()
])

==== Arithmetic Error Handling

#codeblock(lang: "c", [
if (arithmetic_overflow_detected) {
    ERROR_EXIT(current_tos_value);
}
])

*Lisp Integration*: Errors manifest as Lisp conditions

*Recovery Options*: Program may handle via Lisp error system

== Performance Characteristics

=== SMALLP Optimization

- *Zero Allocation*: Immediate values avoid heap operations
- *Fast Execution*: No memory management overhead
- *Range Limitation*: Values must fit in 16-bit signed range

=== FIXP Trade-offs

- *Memory Cost*: Heap allocation + GC tracking
- *Full Range*: 32-bit signed integer support
- *GC Overhead*: Must be tracked by garbage collector

=== CDR Coding Performance

- *NEWCDRCODING*: Faster access for small lists
- *OLD Coding*: Compatible but slower for large structures
- *Indirection*: Performance cost for complex relationships

=== Builtin vs Runtime Detection

- *Compile-time builtins*: ~10-20% faster, preferred when available
- *Runtime detection*: Portable across all compilers, slight performance cost

== Cross-Implementation Notes

=== C Implementation (maiko/src/)

*Core Files*:
- `arithops.c`: Arithmetic operations with overflow detection
- `car-cdr.c`: List operations with CDR coding
- `main.c`: VM initialization and dispatch loop
- `ldsout.c`: Sysout loading and memory setup

*Key Characteristics*:
- Comprehensive error handling
- Extensive debug instrumentation
- Platform-specific optimizations
- Full CDR coding support

=== Zig Implementation (zaiko/src/)

*Architecture Mapping*:
- Direct translation of C logic to Zig
- Overflow detection using Zig's checked operations
- Type system integration with Zig union types
- Memory management through Zig allocators

*Performance Goals*:
- Match C performance characteristics
- Leverage Zig's compile-time features
- Maintain compatibility with C semantics

=== Implementation Status

*Completed*:
- Arithmetic operations with overflow detection
- CDR coding system implementation
- Stack management and frame handling
- Memory allocation and garbage collection
- Sysout loading and IFPAGE processing

*In Progress*:
- Full opcode set implementation
- SDL2 display integration
- Network and I/O subsystems
- Performance optimization

== Testing and Validation

=== Unit Test Requirements

#codeblock(lang: "pseudocode", [
test_arithmetic_operations():
    // SMALLP range tests
    assert(plus2(1, 2) == 3)  // SMALLP result
    assert(plus2(32767, 1) == FIXP(32768))  // Overflow to FIXP

    // Overflow detection
    assert(plus2(MAXINT, 1) triggers ERROR_EXIT)

    // Type coercion
    assert(plus2(1, 1.0) == 2.0)  // Float result

    // Stack effects
    verify_stack_depth(before - 2, after + 1)
])

=== Integration Test Requirements

#codeblock(lang: "pseudocode", [
test_vm_execution():
    // Compare with C emulator traces
    run_parallel_execution(c_emulator, zig_emulator)
    compare_stack_states_at_each_step()
    compare_memory_contents_after_operations()

    // CDR coding validation
    test_list_operations('(a b c d e))
    verify_cdr_coding_efficiency()
    test_indirect_cdr_access()

    // Memory management
    test_garbage_collection()
    verify_no_memory_leaks()
    test_fixp_allocation()
])

=== Edge Case Coverage

- *Boundary Values*: MININT, MAXINT, ZERO, NIL
- *Type Combinations*: int+int, int+float, list operations
- *Memory Conditions*: Heap full, stack overflow, page boundaries
- *CDR Coding*: All encoding schemes, page transitions
- *Error Recovery*: All error paths, recovery mechanisms

== References and Cross-Links

- *C Implementation*: `maiko/src/arithops.c`, `maiko/src/car-cdr.c`
- *Zig Implementation*: `zaiko/src/vm/opcodes/`, `zaiko/src/memory/`
- *Type System*: `specifications/data-structures/`
- *Memory Management*: `specifications/memory-management/`
- *Execution Model*: `specifications/vm-core/execution-model.typ`
- *Stack Management*: `specifications/vm-core/stack-management.typ`
- *Instruction Set*: `specifications/instruction-set/`
- *Critical Debugging*: `CRITICAL_DEBUGGING_TECHNIQUE.typ`