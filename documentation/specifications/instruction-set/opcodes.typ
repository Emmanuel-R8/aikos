= Opcode Reference

*Navigation*: README | Instruction Format | Execution Semantics

Complete specification of all 256 bytecode opcodes (0x00-0xFF). Format: `Name (0xXX) [Len] [Ops] Stack: [effect] Exec: [brief]`

== Canonical source

*All opcode byte values must match the C reference*: `maiko/inc/opcodes.h` (enum opcodes). Decimal enum values correspond to byte values (e.g. opc_NIL = 104 implies NIL = 0x68). In C source, constants may be written in octal (e.g. case 0150 for NIL); 0150 octal = 104 decimal = 0x68 hex.

*Important historical note*: when reconstructing opcode names, trust the actual C dispatch implementation in `maiko/src/xc.c` and the opcode enum in `maiko/inc/opcodes.h` over ad hoc trace label tables. For example, opcode `0x03` is `LISTP`; older name tables may misleadingly label it `LISP`.

*Execution note*: several opcodes in the startup path depend on Maiko's cached-TOS and BYTESWAP rules, not just their mnemonic name. In particular, `RETURN`, `CONTEXTSWITCH`, and `VAG2` interact through the spill-slot stack model, and 16-bit stack/FX words on BYTESWAP builds use `GETWORD(base) = *(DLword *)(2 ^ address)`.

== Opcode Categories

- Control Flow & Memory Operations - Control flow, function calls, jumps, variable access
- Data Operations - Cons cells, arrays, types, lists
- Arithmetic & Base Operations - Arithmetic, comparisons, bitwise, base address operations
- Reference Information - Unused opcodes, common misconceptions, length reference, patterns

== Quick Reference

This document provides a high-level overview. For detailed opcode specifications, see the category documents listed above.

=== Control Flow (0x00-0x3F)
- Function calls: FN0-FN4, FNX, APPLYFN, CHECKAPPLY
- Returns: RETURN, SLRETURN
- Jumps: JUMP0-JUMP15, JUMPX, FJUMP0-FJUMP15, FJUMPX, TJUMP0-TJUMP15, TJUMPX, NFJUMPX, NTJUMPX
- Other control: UNWIND, BIND, UNBIND, DUNBIND

`RETURN` preserves cached `TOPOFSTACK` as the function result in the fast path. Raw `alink` identifies the caller PVAR area, so the caller FX is recovered as `alink - FRAMESIZE`.

=== Memory Operations (0x40-0x7F)
- Variable access: IVAR0-IVAR6, IVARX, PVAR0-PVAR6, PVARX, FVAR0-FVAR6, FVARX, GVAR, GVAR\_
- Variable setting: PVAR_0-PVAR_6, PVARX_, PVARSETPOP0-PVARSETPOP6
- Stack operations: POP, POP_N

`CONTEXTSWITCH` (`0x7E`) uses the low 16 bits of cached `TOPOFSTACK` as an IFPAGE FX-slot selector. It saves the current FX, writes a free-stack-block header, exchanges the chosen slot (`Midpunt` semantics), and resumes the selected frame.

`PVAR_0`-`PVAR_6` and `PVARX_` store cached `TOPOFSTACK` into the current frame's PVAR area without popping. The `PVARSETPOP` family performs the same store followed by the normal pop. `PVARX` is the indexed read form of the same PVAR-area access.

The `FVAR` family is resolved relative to the current PVAR base in #emph[DLword] units, not by indexing a separate closure array:

- `FVAR0`-`FVAR6` use offsets `0, 2, 4, 6, 8, 10, 12`
- `FVARX` uses an explicit byte operand in the same DLword-offset space
- `FVARX_` stores cached `TOPOFSTACK` through the same resolved chain without popping

If an `FVAR` slot is still unbound, Maiko resolves it by walking caller frames via `alink` and the active name table, then caches the discovered address back into the current frame slot.

=== Data Operations (0x00-0x3F, 0x80-0xBF)
- Cons operations: CAR, CDR, CONS, RPLACA, RPLACD, CREATECELL, RPLPTR_N
- Array operations: AREF1, AREF2, ASET1, ASET2
- Type operations: NTYPX, TYPEP, DTEST, STRINGP, ARRAYP, CHARACTERP
- List/atom operations: ASSOC, FMEMB, RESTLIST, RPLCONS, LISTGET

=== Arithmetic (0xD0-0xFF)
- Integer arithmetic: IPLUS2, IDIFFERENCE, ITIMES2, IQUO, IREM, IPLUS_N, IDIFFERENCE_N, BOXIPLUS, BOXIDIFFERENCE
- General arithmetic: PLUS2, DIFFERENCE, TIMES2, QUOTIENT
- Floating-point: FPLUS2, FDIFFERENCE, FTIMES2, FQUOTIENT
- Comparisons: EQ, EQL, EQUAL, LESSP, GREATERP, IGREATERP, FGREATERP, LEQ, GEQ, NUMEQUAL, CL_EQUAL
- Bitwise: LOGOR2, LOGAND2, LOGXOR2, LOGNOT, LSH
- Shift: LLSH1, LLSH8, LRSH1, LRSH8

=== Constants (0x67-0x6F)
- ACONST (0x67), NIL (0x68), T (0x69), CONST_0 (0x6A), CONST_1 (0x6B), SIC (0x6C), SNIC (0x6D), SICX (0x6E), GCONST (0x6F). Per maiko/inc/opcodes.h.

=== Base Address Operations (0xC2-0xCE)
- GETBASEBYTE, PUTBASEBYTE, GETBASE_N, GETBASEPTR_N, PUTBASE_N, PUTBASEPTR_N, GETBITS_N_FD, PUTBITS_N_FD

=== Address Manipulation
- ADDBASE, HILOC, LOLOC, BASE_LESSTHAN

`VAG2` (`0xD1`) combines the previous in-memory stack word as the high 16 bits with the low 16 bits of cached `TOPOFSTACK`, then moves the spill-slot pointer back by one LispPTR cell.

=== GC Operations
- GCREF

=== Miscellaneous
- COPY, SWAP, NOP, MAKENUMBER, MYALINK, MYARGCOUNT, STKSCAN

=== Frontier note from startup parity work

Recent startup-path parity work advanced Laiko through resumed-frame `FVARX`, `FVARX_`, and `PVARX`. The next observed frontier in the authoritative C/Laiko comparison is byte `0x00` immediately after `ACONST`. Since `opcodes.h` marks `0x00` as `opc_unused_0`, implementations should verify the surrounding instruction stepping first before assuming that a real executable `0x00` path exists.

=== Implementation guidance: opcode metadata tables

All emulator implementations SHOULD maintain a centralized opcode metadata table that, for each bytecode, records at least:

- The **opcode byte value** (0x00–0xFF), which MUST match `maiko/inc/opcodes.h`.
- The **instruction length** in bytes (including operands).
- The **operand specification** (types and encoding in the instruction stream).
- The **stack effect** (values popped/pushed).
- A **human-readable name and category** (constants, control flow, arithmetic, etc.).

In practice this usually takes the form of:

- A byte-indexed **length table** for advancing the PC.
- A byte-indexed **handler table** for dispatch (byte → function or closure).
- A **metadata map** (name → record) used by documentation, parity tooling, and introspection.

Implementations are free to express this as macros, structs, or data tables, but they SHOULD derive all dispatch and documentation from this single source of truth to avoid divergence across code paths.

For detailed specifications, see:
- Control Flow & Memory Operations
- Data Operations
- Arithmetic & Base Operations
- Reference Information
*CDR Coding*: See Cons Cells for CDR encoding details
