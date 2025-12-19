= Opcode Reference

*Navigation*: README | Instruction Format | Execution Semantics

Complete specification of all 256 bytecode opcodes (0x00-0xFF). Format: `Name (0xXX) [Len] [Ops] Stack: [effect] Exec: [brief]`

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

=== Memory Operations (0x40-0x7F)
- Variable access: IVAR0-IVAR6, IVARX, PVAR0-PVAR6, PVARX, FVAR0-FVAR6, FVARX, GVAR, GVAR_
- Variable setting: PVARSETPOP0-PVARSETPOP6
- Stack operations: POP, POP_N

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
- ACONST, NIL, T, CONST_0, CONST_1, SIC, SNIC, SICX, GCONST

=== Base Address Operations (0xC2-0xCE)
- GETBASEBYTE, PUTBASEBYTE, GETBASE_N, GETBASEPTR_N, PUTBASE_N, PUTBASEPTR_N, GETBITS_N_FD, PUTBITS_N_FD

=== Address Manipulation
- ADDBASE, HILOC, LOLOC, BASE_LESSTHAN

=== GC Operations
- GCREF

=== Miscellaneous
- COPY, SWAP, NOP, MAKENUMBER, MYALINK, MYARGCOUNT, STKSCAN

For detailed specifications, see:
- Control Flow & Memory Operations
- Data Operations
- Arithmetic & Base Operations
- Reference Information
*CDR Coding*: See Cons Cells for CDR encoding details
