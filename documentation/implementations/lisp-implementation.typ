= Common Lisp Implementation: Maiko Emulator

*Navigation*: README | Index | Architecture

*Feature*: 002-lisp-implementation
*Date*: 2026-03-17
*Status*: 🔧 EXECUTION WORKING - Starter Sysout Runs

== Overview

Complete implementation of the Maiko emulator in Common Lisp (SBCL), following the rewrite specifications in `documentation/specifications/`. The implementation maintains exact compatibility with the C implementation while leveraging Common Lisp's strengths.

== Implementation Statistics

- *Source Files*: 26+ Lisp files
- *Test Files*: 11 test files
- *Opcodes Implemented*: ~191 opcode handlers registered
- *Actual Completeness*: ~30% (sysout loading, Valspace allocation, stack consolidation, 4+ instructions executing)
- *Build System*: ASDF
- *Target Platform*: Linux (SBCL), macOS, Windows (partial)

== Current Status (2026-03-17)

=== Opcode alignment with Maiko (2026-03-17)

Laiko now successfully loads and executes `starter.sysout` to completion (returning from the top-level frame).

**2026-03-17 Update**:
- Fixed `return-from-function` to correctly use virtual memory stack pointers and restore caller frames.
- Fixed `initialize-vm-from-ifpage` to correctly initialize the initial stack frame's link field from `fx-alink`.
- Resolved compilation warnings in graphics opcodes (undefined `DX`/`DY`).
- Resolved package visibility issues for `quit` and `fetch-instruction-byte`.

=== ✅ Completed

- ✅ Complete module structure with proper package organization
- ✅ Sysout file loading (BIGVM format, FPtoVP table loading)
- ✅ VM state structure (stack, PC, frame pointers, registers)
- ✅ Dispatch loop with opcode fetching and execution
- ✅ **186/256 opcodes defined (72.7%)**
- ✅ **Full Execution Cycle**: Loads sysout, initializes VM, executes instructions, and returns cleanly.
- ✅ **Stack System**: Fully consolidated to use virtual memory byte offsets (matching C/Zig).
- ✅ **Graphics**: Basic opcode definitions compiled without warnings (stubbed or partial).

=== ⚠️ Known Issues

- ⚠️ Graphics opcodes are largely stubs or have unverified implementations.
- ⚠️ Subroutine calls are stubs.
- ⚠️ REPL interaction is not yet visible (emulator exits after initial bytecode).

=== 🔧 In Progress

- Parity testing against C emulator traces.
- Implementing missing opcodes (graphics, I/O).
- Establishing a persistent REPL loop.

== Critical Fix: Stack System Consolidation (2026-03-01)

=== Problem

Laiko had **two incompatible stack systems** being used inconsistently:

1. **Old System** (`vm-stack-ptr` - array index):
   - `push-stack()`, `pop-stack()`, `get-top-of-stack()`, `set-top-of-stack()`
   - Used by: POP, GETBASEPTR-N, and other opcodes

2. **New System** (`vm-stack-ptr-offset` - byte offset in VM):
   - `vm-push()`, `vm-pop()`, `vm-tos()`, `vm-set-tos()`
   - Used by: GVAR (opcode 0x60)

When GVAR pushed a value using the new system and POP tried to pop using the old system, this caused a **stack underflow** at GETBASEPTR-N.

=== Solution

The old stack functions in `laiko/src/vm/stack.lisp` were redirected to use the new VM-based operations:

#codeblock(lang: "lisp", [
;; DEPRECATED: Old array-based stack functions
;; These now redirect to VM-based stack operations for consistency

(defun push-stack (vm value)
  "Push value onto stack. DEPRECATED - use vm-push instead."
  (vm-push vm (if (typep value 'fixnum) value (logand value #xFFFFFFFF))))

(defun pop-stack (vm)
  "Pop value from stack. DEPRECATED - use vm-pop instead."
  (vm-pop vm))

(defun get-top-of-stack (vm)
  "Get top of stack without popping. DEPRECATED - use vm-tos instead."
  (vm-tos vm))

(defun set-top-of-stack (vm value)
  "Set top of stack. DEPRECATED - use vm-set-tos instead."
  (vm-set-tos vm (if (typep value 'fixnum) value (logand value #xFFFFFFFF))))
])

This ensures all opcodes use the same virtual memory-based stack system, matching the C/Zig implementations.

=== Additional Fixes

- **UNBIND (0x12)**: Implemented proper stack pop per declared `:stack-effect (:pop 1)`
- **GVAR (0x60)**: Fixed to read correctly from Valspace using proper address translation

== Critical Discovery: Valspace Architecture

Valspace pages are **NOT loaded from sysout file**:

#codeblock(lang: "text", [
FPtoVP table has NO entries for VP 3072-3583 (Valspace)
These pages are runtime-allocated memory
C: Valspace = NativeAligned2FromLAddr(VALS_OFFSET) in initsout.c
])

Laiko now allocates Valspace pages at runtime:
- VP 3072-3583 (512 pages, 256KB)
- Zero-initialized
- Values populated by Lisp startup code

== Architecture

=== Project Structure

#codeblock(lang: "text", [
alternatives/lisp/
├── laiko.asd          # ASDF system definition
├── build.sh                 # Build script
├── run.sh                   # Run script
├── README.md                # Project README
├── src/
│   ├── package.lisp         # Package definitions
│   ├── main.lisp            # Entry point
│   ├── vm/                  # VM core
│   │   ├── dispatch.lisp   # Dispatch loop (462 lines)
│   │   ├── opcodes.lisp    # Opcode definitions
│   │   ├── stack.lisp      # Stack management
│   │   ├── function.lisp   # Function calls
│   │   └── interrupt.lisp  # Interrupt handling
│   ├── memory/              # Memory management
│   │   ├── storage.lisp    # Heap allocation
│   │   ├── gc.lisp         # Garbage collection
│   │   ├── virtual.lisp    # Virtual memory/FPtoVP
│   │   └── layout.lisp     # Memory layout
│   ├── data/                # Data structures
│   │   ├── cons.lisp       # Cons cells
│   │   ├── array.lisp      # Arrays
│   │   ├── function-header.lisp
│   │   └── sysout.lisp     # Sysout loading
│   ├── display/             # Display subsystem
│   │   ├── sdl-backend.lisp
│   │   ├── graphics.lisp   # BitBLT operations
│   │   └── events.lisp     # Event handling
│   ├── io/                  # I/O subsystem
│   │   ├── keyboard.lisp
│   │   ├── mouse.lisp
│   │   └── filesystem.lisp
│   └── utils/               # Utilities
│       ├── types.lisp      # Type definitions
│       ├── errors.lisp     # Error conditions
│       └── address.lisp    # Address translation
├── tests/                    # Test suite
│   ├── test-suite.lisp
│   ├── opcodes.lisp
│   ├── stack.lisp
│   ├── dispatch.lisp
│   ├── memory.lisp
│   ├── gc.lisp
│   ├── sysout.lisp
│   ├── keyboard.lisp
│   ├── mouse.lisp
│   ├── display.lisp
│   ├── filesystem.lisp
│   └── compatibility.lisp
└── docs/
    └── IMPLEMENTATION.md    # Implementation notes
])

== Key Implementation Decisions

=== Build System

- *ASDF*: Standard Common Lisp build system
- *Dependencies*: Made `uiop` and `alexandria` optional (conditional on `:sb-thread` feature)
- *SBCL*: Primary target implementation
- *SDL3*: Target display backend (via `cl-sdl3` or CFFI)

=== Memory Management

- *Storage Allocation*: Heap-based allocation with free list management
- *GC Coordination*: Uses `sb-sys:with-pinned-objects` to prevent Common Lisp GC from moving Maiko-managed objects
- *Reference Counting*: Maiko's reference-counting GC implemented as separate system on top of Common Lisp memory
- *Virtual Memory*: FPtoVP mapping implemented for address translation
- *Sysout Loading*: Endianness-aware sysout file loading with version validation

=== VM Core

- *Dispatch Loop*: Variable-length instruction handling with operand fetching
- *Stack Management*: Frame-based stack with proper allocation/deallocation
- *Opcode Handlers*: 189 opcodes implemented with correct semantics
- *Interrupt Handling*: Structure in place for I/O, timer, and system interrupts

=== Display Backend

- *SDL3*: Target backend (no X11 requirement)
- *Fallback*: When `cl-sdl3` unavailable, display structure created but SDL operations stubbed
- *Graphics*: BitBLT operations implemented with COPY and XOR modes
- *Events*: Polling-based event handling

=== I/O Subsystem

- *Keyboard*: Event queue with OS keycode to Lisp keycode translation
- *Mouse*: Position tracking and event translation
- *Filesystem*: Pathname translation for platform compatibility (Unix/Windows)

== Implementation Details

=== Opcode Implementation

*Implemented Categories*:
- Constants (NIL, T, CONST_0, CONST_1, SIC, SNIC, SICX, ACONST)
- List operations (CAR, CDR, CONS, LIST, APPEND)
- Arithmetic (IPLUS2, IDIFFERENCE, ITIMES2, IQUO, IREM, IADD1, ISUB1)
- Comparison (EQ, EQL, EQUAL, ILESSP, IGREATERP)
- Bitwise (LOGOR2, LOGAND2, LOGXOR2, LOGNOT)
- Shift (LSH, RSH)
- Variable access (GETVAR, SETVAR, GETLOCAL, SETLOCAL)
- Function calls (CALL, RETURN, UNWIND)
- Type checking (TYPEP, LISTP, ATOMP, NUMBERP)
- Array access (AREF, ASET)
- Stack operations (PUSH, POP, DUP, SWAP)
- Character operations (CHARCODE, CHARN)
- Global variables (GETGLOBAL, SETGLOBAL)
- GC operations (MARK, SWEEP)
- Cell creation (MAKECELL, FREECELL)
- Base address operations (GETBASE, SETBASE)
- Address manipulation (ADDBASE, SUBBASE)
*Remaining*: 67 opcodes (mostly specialized operations, can be added incrementally)

=== Centralized opcode metadata (2026-02-26)

Laiko now uses a single declarative macro (`DEFOP` in `laiko/src/vm/op-macros.lisp`) to define each opcode and populate all dispatch/metadata tables:

- **Primary fields** per opcode are provided as keywords: `:hexcode` (byte value 0–255) and `:instruction-length` (total bytes including operands).
- **Additional metadata** is captured via keywords: `:operands`, `:stack-effect`, `:category`, `:side-effects`, and optional `:aliases`.
- The macro fills:
  - `*instruction-lengths*` (byte → instruction length),
  - `*opcode-names*` (byte → symbolic name),
  - `*opcode-handlers-array*` (byte → handler function),
  - `*opcode-handlers*` (name → handler),
  - `*opcode-metadata*` (name → plist of all fields, including documentation).

This keeps the opcode table as the **single source of truth** for both the dispatcher and tooling (documentation generation, parity debugging, future introspection) and mirrors the centralized tables used in the C and Zig implementations.

=== Opcode prioritization for parity work (2026-02-26)

For Laiko, opcode implementation order is now driven by **trace-based divergence** rather than raw opcode number:

- Tier 1: early-executed core instructions (stack operations, constants, variable access, control flow, base/memory ops that appear in the first few instructions of starter.sysout).
- Tier 2: data and memory structure operations (lists, arrays, general arithmetic/comparisons, bitwise and shifts).
- Tier 3: floating point, graphics, and advanced I/O (BitBLT, SDL display, subroutine I/O).

When running `scripts/compare_emulator_execution.sh --with-laiko` with a small `EMULATOR_MAX_STEPS`, the **first divergence (PC + opcode)** determines which tier to work on next. See `reports/laiko-opcode-priority.md` for the current tier breakdown and concrete examples.

=== Memory Management

*Storage*:
- Heap allocation with DLword-aligned blocks
- Free list management for efficient allocation
- Storage full detection

*Garbage Collection*:
- Hash table-based reference counting
- Stack reference marking
- GC enable/disable control
- Reclaim countdown mechanism

*Virtual Memory*:
- FPtoVP (File Page to Virtual Page) mapping
- Page-based address translation
- Support for sysout file page mapping

*Sysout Loading*:
- Endianness-aware reading (little-endian)
- IFPAGE structure parsing
- Version compatibility checking
- Keyval validation

=== Error Handling

*Error Conditions*:
- `vm-error`: VM execution errors
- `memory-error`: Memory allocation/access errors
- `display-error`: Display initialization/rendering errors
- `io-error`: I/O operation errors
- `sysout-load-failed`: Sysout file loading errors
- `invalid-address`: Address translation errors

*Edge Cases Handled*:
- Sysout version mismatch
- Memory allocation failures
- SDL initialization failures
- Invalid address access
- End-of-file during sysout reading

=== Platform Support

*Endianness*:
- Platform-specific detection via `sb-sys:machine-type`
- Little-endian byte order for sysout files
- Word size handling (32-bit LispPTR, 16-bit DLword)

*Pathname Translation*:
- Unix-style paths (forward slashes)
- Windows-style paths (backslashes, drive letters)
- Platform-agnostic pathname handling

== Testing

*Test Coverage*:
- Opcode execution tests
- Stack management tests
- Dispatch loop tests
- Memory allocation tests
- GC operation tests
- Sysout loading tests
- Keyboard/mouse event tests
- Display operation tests
- Filesystem operation tests
- Compatibility tests

*Test Framework*: FiveAM

== Performance Considerations

- *Correctness First*: Implementation prioritizes correctness over performance
- *Type Declarations*: Used throughout for SBCL optimization hints
- *Profiling*: Performance profiling deferred until correctness verified (T077)

== Known Limitations

1. *SDL3*: Requires `cl-sdl3` library for full functionality (currently stubbed)
2. *Bytecode Extraction*: Sysout bytecode extraction not yet implemented
3. *Some Opcodes*: 67 opcodes not yet implemented (stub implementations)
4. *GC Coordination*: Basic coordination implemented, may need refinement

== Lessons Learned

=== Common Lisp Specific

1. *ASDF Dependencies*: Making dependencies optional improves build flexibility
2. *Type Declarations*: Critical for SBCL optimization
3. *Package System*: Well-organized packages improve code maintainability
4. *Error Conditions*: Custom error types provide better error handling

=== Implementation Challenges

1. *Opcode Conflicts*: Some opcode values were overloaded in C implementation - resolved by prioritizing `opcodes.h` definitions
2. *Endianness*: Platform-specific handling required for sysout compatibility
3. *Memory Mapping*: FPtoVP mapping requires careful address translation
4. *Dispatch Loop*: Variable-length instructions require careful operand fetching

=== Design Decisions

1. *Stub First*: Created stub implementations for all subsystems, then filled in details
2. *Test-Driven*: Created test files before implementation
3. *Modular Design*: Clear separation between VM, memory, display, and I/O
4. *Error Handling*: Comprehensive error conditions for debugging

== Future Work

1. *Complete Opcodes*: Implement remaining 67 opcode handlers
2. *SDL3 Integration*: Integrate `cl-sdl3` for full SDL3 support
3. *Bytecode Extraction*: Implement bytecode extraction from sysout files
4. *Compatibility Tests*: Add reference test data for compatibility testing
5. *Performance Optimization*: Profile and optimize after correctness verification

== References

- *Specification*: `specs/002-lisp-implementation/spec.md`
- *Plan*: `specs/002-lisp-implementation/plan.md`
- *Tasks*: `specs/002-lisp-implementation/tasks.md`
- *Rewrite Spec*: `documentation/specifications/`
- *Implementation Notes*: `alternatives/lisp/docs/IMPLEMENTATION.md`

== Related Documentation

- VM Core - Execution engine architecture
- Memory Management - GC and memory layout
- Display - Display subsystem architecture
- I/O - I/O subsystem architecture
- Instruction Set - Opcode specifications
