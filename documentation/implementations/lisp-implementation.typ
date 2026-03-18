= Common Lisp Implementation: Maiko Emulator

*Navigation*: README | Index | Architecture

*Feature*: 002-lisp-implementation
*Date*: 2026-03-19 03:53
*Status*: 🔧 PARITY DEBUGGING - Startup Path Advancing

== Overview

Complete implementation of the Maiko emulator in Common Lisp (SBCL), following the rewrite specifications in `documentation/specifications/`. The implementation maintains exact compatibility with the C implementation while leveraging Common Lisp's strengths.

== Implementation Statistics

- *Source Files*: 26+ Lisp files
- *Test Files*: 11 test files
- *Opcodes Implemented*: ~191 opcode handlers registered
- *Actual Completeness*: ~30% (sysout loading, Valspace allocation, stack consolidation, 4+ instructions executing)
- *Build System*: ASDF
- *Target Platform*: Linux (SBCL), macOS, Windows (partial)

== Current Status (2026-03-19 03:53)

=== Opcode alignment with Maiko (2026-03-19 03:53)

Laiko now loads `starter.sysout`, enters the startup bytecode, and matches Maiko substantially deeper into the trace than before, but it does #emph[not] yet run to completion.

**2026-03-19 03:53 Update**:
- Implemented `GVAR_` (`0x17`) using Maiko semantics: store cached `TOS` into the atom value cell without changing `TOS`.
- Implemented `LISTP` (`0x03`) using the MDS type table rather than a heuristic.
- Corrected the `JUMPX` family so `JUMPX`, `FJUMPX`, `TJUMPX`, `NFJUMPX`, and `NTJUMPX` use signed 8-bit offsets, while `JUMPXX` uses signed 16-bit offsets.
- Corrected `RETURN` to preserve cached `TOS` and reworked the fast-return path around Maiko's `alink -> PVAR -> FX` model.
- Replaced the `CONTEXTSWITCH` stub with a first real implementation that saves runtime FX state, writes the free-stack-block header, performs a `Midpunt`-style IFPAGE slot exchange, and resumes the selected frame.
- Fixed Laiko's 16-bit VM word access to follow Maiko `GETWORD` semantics on BYTESWAP builds: logical word reads use address XOR `2`, while 32-bit Lisp pointer reads remain sequential.
- Updated dispatch to reload the active bytecode window when a handler resumes execution at an absolute PC outside the currently extracted block.
- Implemented `VAG2` (`0xD1`) using Maiko's cached-TOS and spill-slot stack model.
- Corrected FX word decoding so the first two FX words match the C layout: `alink` is the first word, while `usecount` and the packed flag bits live in the second.
- Corrected the packed FX flag masks so `nopush`, `validnametable`, and `incall` are decoded from the actual packed flag byte rather than from a swapped low-byte interpretation.
- Rewired resumed-frame `PVAR` reads and writes to the real VM-memory PVAR area immediately after the current FX, instead of the obsolete array-based stack model.
- Implemented the non-pop parameter-store family `PVAR_0`-`PVAR_6` and `PVARX_`, matching Maiko `PVARSETMACRO` / `PVARX_` behavior.
- Corrected the `FVAR0`-`FVAR6` family to use Maiko DLword offsets (`0, 2, 4, 6, 8, 10, 12`) rather than a fake closure-environment index.
- Implemented `FVARX` (`0x57`) and ported the unbound-chain lookup path needed by the startup trace: unresolved free-variable slots now scan caller frames via `alink`, choose `fnheader` vs `nametable` according to `validnametable`, and cache the resolved address back into the current frame slot.
- Implemented the indexed non-pop store family needed immediately after that lookup work: `IVARX_` (`0x62`), `FVARX_` (`0x63`), and `PVARX` (`0x4F`).
- Corrected `ACONST` for the actual BIGVM build: it now reads a 32-bit atom operand and advances by 5 bytes, matching Maiko `Get_AtomNo_PCMAC1` / `nextop_atom`.
- Corrected BIGVM `FN0`-`FN4` / `FNX` operand widths to match Maiko: `FN0`-`FN4` are 5-byte instructions and `FNX` is a 6-byte instruction because `Get_AtomNo_PCMAC1` / `Get_AtomNo_PCMAC2` read 32-bit atom operands on BIGVM/BIGATOMS.
- Revalidation showed that the old `0x78` (`MISC1`) frontier at `0x71cad6` was another false decode frontier caused by the wrong `FNx` sizes.
- Current blocker: with the `FNx` widths fixed, startup now stops earlier at `FN0` (`0x08`) / PC `0x60f0af` with `FN0: Undefined function`. The next work is therefore the shared Maiko function-call path for undefined-function / interpreter dispatch, which later also underpins `op_ufn`.

=== ✅ Completed

- ✅ Complete module structure with proper package organization
- ✅ Sysout file loading (BIGVM format, FPtoVP table loading)
- ✅ VM state structure (stack, PC, frame pointers, registers)
- ✅ Dispatch loop with opcode fetching and execution
- ✅ **186+/256 opcodes defined**
- ✅ **Deep startup execution**: loads the sysout, initializes VM state, and executes into the startup control-flow path.
- ✅ **Frame switching frontier advanced**: Laiko now matches Maiko through the first startup `CONTEXTSWITCH` into `faultfxp`.
- ✅ **Later startup arithmetic/path progress**: execution now continues through `VAG2`, through the resumed startup frame, and into a deeper resumed call chain.
- ✅ **Stack System**: Fully consolidated to use virtual memory byte offsets (matching C/Zig).
- ✅ **Graphics**: Basic opcode definitions compiled without warnings (stubbed or partial).

=== ⚠️ Known Issues

- ⚠️ Graphics opcodes are largely stubs or have unverified implementations.
- ⚠️ Subroutine calls are stubs.
- ⚠️ `CONTEXTSWITCH` now has the correct overall control-transfer shape and no longer fails on the false `incall` path, but deeper resumed-frame opcode coverage is still incomplete.
- ⚠️ `RETURN` slow path (`alink & 1`) is not yet implemented.
- ⚠️ REPL interaction is not yet visible because startup parity is not complete.

=== 🔧 In Progress

- Parity testing against C emulator traces.
- Implementing missing opcodes (graphics, I/O).
- Establishing a persistent REPL loop.

=== Critical Fix: VM Initialization & Stack Logic (2026-03-18)

=== Problem

The VM initialization logic in `laiko/src/main.lisp` was incorrectly calculating the initial Frame Pointer (FP) and Stack Pointer (SP) by mixing absolute Lisp addresses with relative stack offsets.

- **Old Logic**:
  - `FP = STK_OFFSET + (currentfxp * 2)` (Incorrect base)
  - `SP = STK_OFFSET + (nextblock * 2)` (Incorrect base)

- **Correct Logic (Maiko Reference)**:
  - `FP = currentfxp` (Relative offset from start of stack space)
  - `SP = stackbase + 2` (Absolute address from `IFPAGE->stackbase`)
  - `TOS = *stackbase` (Top of stack value)

=== Solution

Updated `initialize-vm-from-ifpage` to match Maiko's `initsout.c`:

1. **FP Calculation**: `currentfxp` from IFPAGE is treated as a relative offset.
2. **SP Calculation**: `stackbase` from IFPAGE is treated as an absolute LispPTR.
3. **TOS Initialization**: The value at `stackbase` is loaded as the initial Top-Of-Stack.

This ensures the VM starts execution in the correct stack frame state, preventing immediate crashes or stack corruption upon the first return.

== Critical Fix: Memory Operations (2026-03-20)

=== Problem

The memory access opcodes (`GETBASEBYTE`, `PUTBASEBYTE`, `GETBASE_N`, `GETBASEPTR_N`, `PUTBASE_N`, `PUTBASEPTR_N`) were incorrectly implemented using simple address arithmetic on stack values, returning addresses instead of reading/writing the memory at those addresses.

- `GETBASEPTR_N` was pushing `base + index*4` (address) instead of reading the pointer at that address.
- `GETBASEBYTE` was incorrectly masking the address instead of reading memory.

=== Solution

Implemented proper memory access using the `vm-read-*` and `vm-write-*` family of functions:

1. **Byte Access**: `GETBASEBYTE`/`PUTBASEBYTE` now use `vm-read-byte` / `vm-write-byte`.
2. **Word Access**: `GETBASE_N`/`PUTBASE_N` now use `vm-read-word` / `vm-write-word` (16-bit).
3. **Pointer Access**: `GETBASEPTR_N`/`PUTBASEPTR_N` now use `vm-read-lispptr` / `vm-write-lispptr` (32-bit).

Added `vm-read-word` and `vm-write-word` to `laiko/src/vm/stack.lisp` to support 16-bit memory operations with Little Endian handling.

== Critical Fix: LISTP Uses the MDS Type Table (2026-03-18)

=== Problem

After fixing `GVAR_`, the next startup failure moved to opcode `0x03`. Investigation against the Maiko dispatch loop showed that `0x03` is `LISTP`, not `LISP`.

The important semantic detail is that Maiko does #emph[not] implement `LISTP` as a simple non-NIL test or as a cons-space heuristic. Instead it consults the MDS type table:

#codeblock(lang: "c", [
#define GetTypeEntry(address)      ( GETWORD(MDStypetbl+((address)>>9)) )
#define GetTypeNumber(address)     (GetTypeEntry(address) & 0x7ff)
#define Listp(address)             (GetTypeNumber(address) == TYPE_LISTP)
])

For BIGVM, `MDStypetbl` is initialized from `MDS_OFFSET` in `build_lisp_map()`.

=== Solution

Laiko now implements opcode `0x03` by reading the 16-bit MDS entry for the page containing `TOS` and comparing its low 11 bits to `TYPE_LISTP = 5`.

This matches the C logic exactly:

1. Compute page index with `lispptr >> 9` (512-byte pages)
2. Read the MDS entry from `MDS_OFFSET`
3. Keep `TOS` unchanged only if the type number is `TYPE_LISTP`
4. Otherwise replace `TOS` with `NIL`

=== Debugging note

The trace-name table in `maiko/src/xc.c` had historically labeled `0x03` as `LISP`, but the actual dispatch switch correctly executes `LISTP`. For parity work, the dispatch switch is the authoritative source.

== Critical Fix: RETURN Preserves Cached TOS and Uses `alink` as a PVAR Pointer (2026-03-19 00:58)

=== Problem

After the `LISTP` and `NTJUMPX` fixes, Laiko advanced to a later `RETURN`, but resumed execution in the wrong frame and eventually crashed in `UNBIND`.

Two Maiko-specific details turned out to be critical:

1. `RETURN` does #emph[not] pop the return value first. It preserves cached `TOPOFSTACK` through frame restoration.
2. In the fast path, raw `alink` is #emph[not] a caller-FX pointer. It points to the caller's PVAR area, and the caller FX is recovered with `alink - FRAMESIZE`.

=== C reference

The relevant Maiko logic is:

#codeblock(lang: "c", [
alink = CURRENTFX->alink;
if (alink & 1) slowreturn();

CSTKPTRL = (LispPTR *) IVAR;
returnFX = (struct frameex2 *)
  ((DLword *)(PVARL = (DLword *) NativeAligned2FromStackOffset(alink))
   - FRAMESIZE);
IVARL = (DLword *) NativeAligned2FromStackOffset(GETWORD((DLword *)returnFX - 1));
PCMACL = returnFX->pc + (ByteCode *)(FuncObj = ...);
])

=== Solution

Laiko now mirrors the fast-path structure more closely:

1. `RETURN` reads the return value from cached `vm-tos`.
2. `return-from-function` reads the active FX from VM using the current frame-pointer offset.
3. Raw `alink` is interpreted as the caller PVAR offset.
4. The caller FX is reconstructed at `alink - FRAMESIZE`.
5. The stack spill pointer is restored from the IVAR word immediately preceding the caller FX.
6. Cached `TOS` is preserved as the return value instead of being pushed back into memory immediately.

=== Current limitation

This fix corrected one major source of stack-model divergence, but it did not fully solve the later resume mismatch. The next remaining issue is that `CONTEXTSWITCH` still does not update runtime FX state the way Maiko does, so the frame visible to `RETURN` is still not the one Maiko would see.

== Critical Fix: Frame Switching, BYTESWAP Word Access, and Dynamic Dispatch (2026-03-19 01:40)

=== Problem

Once `RETURN` preserved cached `TOS` correctly, the next authoritative Maiko trace step was a real `CONTEXTSWITCH` into `faultfxp`, not a no-op resume in the current frame.

Laiko was missing three linked behaviors:

1. `CONTEXTSWITCH` was still a stub and could not save the current FX, write an FSB, exchange the IFPAGE slot, and resume the selected frame.
2. 16-bit stack/FX words were being read as plain little-endian words instead of using Maiko's BYTESWAP `GETWORD(base) = *(DLword *)(2 ^ address)` rule.
3. Dispatch assumed a single extracted bytecode block and stopped once frame switching resumed execution at an absolute PC in a different function body.

=== Solution

Laiko now mirrors the C control-flow path much more closely:

1. Added writable FX helpers and mutable IFPAGE FX-slot access so runtime frame state can be saved back into virtual memory.
2. Implemented a first real `CONTEXTSWITCH` that:
   - takes `fxnum` from the low 16 bits of cached `TOS`
   - saves the current FX `pc`
   - sets `nopush`
   - stores the outgoing frame's `nextblock`
   - writes an FSB header
   - performs the `Midpunt`-style slot exchange with the chosen IFPAGE FX slot
   - restores the selected frame using Maiko's `nopush` rule
3. Fixed `vm-read-word` / `vm-write-word` so 16-bit stack and FX fields obey XOR-2 addressing on little-endian hosts.
4. Updated dispatch so an absolute `vm-pc` outside the current bytecode buffer reloads a fresh code window from virtual memory and continues execution there.
5. Implemented `VAG2` as Maiko defines it: combine the previous in-memory spill-slot word as the high 16 bits with cached `TOS` as the low 16 bits, then move the spill-slot pointer back by one LispPTR cell.

=== Result

These fixes moved Laiko past the old post-`RETURN` / `UNBIND` failure. It now follows Maiko through the startup transfer into `faultfxp`, through `VAG2`, and into the later switched-frame startup path.

=== Current limitation

The next live divergence is no longer the first context switch itself. The current blocker is a later switched-frame path where the resumed FX state is still being decoded incorrectly, causing Laiko to treat the target frame as `incall` when the C reference continues.

== Critical Fix: FX packed-word decoding and resumed-frame PVAR access (2026-03-19 02:11)

=== Problem

After the first real `CONTEXTSWITCH` implementation, Laiko advanced into the later switched-frame startup path but then failed in two closely related ways:

1. the resumed FX was falsely reported as `incall`
2. once that was bypassed, resumed `PVAR` access still used the obsolete array-based stack helpers rather than the real PVAR area in VM memory

Both issues came from low-level frame-layout interpretation rather than from the overall control-transfer shape.

=== Solution

Laiko now matches the C frame model more closely:

1. The first two FX words are decoded in the C order:
   - word 0 = `alink`
   - word 1 = packed `usecount` + flag bits
2. The packed FX flag byte now uses the correct bit assignments for `nopush`, `validnametable`, and `incall`.
3. `get-pvar` and `set-pvar` now address the PVAR area in VM memory at `FP + FRAMESIZE`, matching Maiko `PVAR[x]`.
4. The `PVAR_0`-`PVAR_6` and `PVARX_` family are now implemented so cached `TOS` can be written back without a pop, just like `PVARSETMACRO` / `PVARX_` in the C source.

=== Result

These fixes removed the false later `CONTEXTSWITCH returned to incall frame` failure and advanced parity through the resumed startup-frame `PVAR` / `PVAR_` sequence.

=== Current limitation

The next live parity frontier is no longer the first unbound free-variable access or the post-`ACONST` fake decode. After correcting BIGVM `FNx` widths, Laiko now exposes an earlier undefined-function call path at `FN0` (`0x08`) / `0x60f0af`; the later `0x78` / `op_ufn` path remains relevant, but only after the common call machinery is made Maiko-faithful.

== Critical Fix: Free-variable chain resolution and indexed variable opcodes (2026-03-19 02:56)

=== Problem

After the resumed-frame `PVAR` / `PVAR_` fixes, Laiko advanced to `FVARX` (`0x57`) in the deeper startup call chain.

The remaining issue was not just a missing handler:

1. Laiko's `FVAR` helpers still treated free variables like a flat closure array.
2. The C implementation actually addresses `FVAR` slots relative to the current PVAR area in #emph[DLword] units.
3. When a slot is unbound, Maiko does #emph[not] fail immediately. It resolves the variable by scanning caller frames through `alink` and the active name table, then caches the discovered address back into the current frame's FVAR slot.

That meant the whole `FVAR` family was wrong in kind, not just missing `FVARX`.

=== Solution

Laiko now follows the C model much more closely:

1. `FVAR0`-`FVAR6` use DLword offsets `0, 2, 4, 6, 8, 10, 12`.
2. `FVARX` uses the instruction operand as the same DLword offset format.
3. Unbound `FVAR` slots now trigger a caller-frame scan that:
   - starts from the current frame's `alink`
   - reads the next FX from VM memory
   - chooses `fnheader` vs `nametable` according to `validnametable`
   - scans the name table for the target free-variable atom
   - caches either a stack slot address, another free-variable chain pointer, or a top-level atom value-cell address
4. The matching store-side opcodes were added in the same slice:
   - `IVARX_` (`0x62`)
   - `FVARX_` (`0x63`)
   - `PVARX` (`0x4F`)

=== Result

These fixes moved Laiko past the first missing `FVARX` frontier, through several consecutive `FVARX` / `FVARX_` / `PVARX` operations in the resumed startup function, and into the next decode frontier after `ACONST`.

=== Current limitation

The current live mismatch is byte `0x00` at `0x71ca89`. Because this appears immediately after `ACONST`, the next step is to verify whether Laiko is actually reaching a real `opc_unused_0` byte or whether `ACONST` still disagrees with Maiko's `nextop_atom` stepping convention.

== Critical Fix: BIGVM `ACONST` atom width (2026-03-19 03:53)

=== Problem

After the free-variable work, Laiko appeared to hit byte `0x00` immediately after `ACONST`.

That looked at first like a new opcode-coverage gap, but the C reference showed a different story:

1. On BIGATOMS builds, `Get_AtomNo_PCMAC1` reads a pointer-sized atom operand.
2. On BIGVM, `nextop_atom` advances by 5 bytes, not 3.
3. Laiko still implemented `ACONST` as a 3-byte instruction with a 16-bit atom operand.

So the `0x00` frontier was not a real executable opcode. It was a decode drift caused by reading only half of the atom operand.

=== Solution

Laiko `ACONST` now matches the Maiko BIGVM path:

1. instruction length updated from 3 to 5 bytes
2. operand widened from a 16-bit atom index to a 32-bit big-endian atom pointer/index
3. handler updated to use the 32-bit operand reader

=== Result

This removed the false post-`ACONST` `0x00` frontier and advanced the startup trace to the next genuine control-transfer gap.

=== Current limitation

That later `0x78` (`MISC1`) frontier turned out not to be the next real blocker. Once BIGVM `FN0`-`FN4` / `FNX` were corrected to use 32-bit atom operands and 5/6-byte instruction sizes, Laiko stopped earlier in the startup path at `FN0` (`0x08`) / `0x60f0af` with `FN0: Undefined function`. The next parity slice therefore needs the real Maiko undefined-function / interpreter call path before the later `op_ufn` route can be trusted.

=== Critical Fix: GVAR Bounds Check (2026-03-20)

=== Problem

The `GVAR` instruction was pushing `0` instead of `0x140000` (for Atom 522). This was traced to a bug in `laiko/src/data/atom.lisp` where bounds checks compared byte offsets against page counts:

#codeblock(lang: "lisp", [
(when (>= (+ value-cell-offset 4) (length vmem)) ...)
])

`value-cell-offset` is a byte offset (e.g., 131072), while `(length vmem)` is the number of pages (e.g., 256). This caused valid accesses to fail silently.

=== Solution

Fixed the bounds check logic in `read-atom-value`, `write-atom-value`, and `get-defcell` to correctly compare page indices derived from offsets.

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
