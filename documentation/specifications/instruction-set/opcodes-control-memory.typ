= Opcode Reference - Control Flow & Memory Operations

*Navigation*: Opcode Reference | Instruction Format | Execution Semantics

*Last Updated*: 2025-12-18 20:26

Control flow and memory operation opcodes (0x00-0x7F).

== Control Flow (0x00-0x3F)

=== Function Calls
- *FN0 (0x08)* [3] Calls 0-arg function. Format: [opcode][atom_index:2B]. Atom index is 2 bytes (DLword) for non-BIGATOMS, 3-4 bytes for BIGATOMS. Creates frame.
- *FN1 (0x09)* [3] Calls 1-arg function. Format: [opcode][atom_index:2B]. Creates frame.
- *FN2-FN4 (0x0A-0x0C)* [3] Calls 2-4 arg function. Format: [opcode][atom_index:2B]. Creates frame.
- *FNX (0x0D)* [4-5] Variable argument count. Format: [opcode][atom_index:2-3B][arg_count:1B]. Atom index size depends on BIGATOMS setting.
- *APPLYFN (0x0E)* [1] Apply function to arg list on stack.
  - *Purpose*: Apply a function to a list of arguments (Lisp APPLY semantics)
  - *Stack State Before*: [arg_list, function] where arg_list is a cons cell list
  - *Algorithm*:
    1. Pop argument list from stack
    2. Pop function object from stack
    3. Spread list elements onto stack as individual arguments
    4. Count arguments while spreading
    5. Call function with spread arguments (similar to FNX)
  - *C Implementation*: Similar to FNX but handles list spreading
  - *Updated*: 2025-12-18 20:26 - Added documentation and basic implementation structure
- *CHECKAPPLY (0x0F)* [1] Validate apply args before apply.

=== Returns
- *RETURN (0x10)* [1] Pop frame, restore PC, return value on TOS.
- *SLRETURN (0x3F)* [1] Soft return (stack-relative return).
  - *Purpose*: Return from function using stack-relative addressing
  - *C Implementation*: `maiko/src/mvs.c` - handles soft return with different frame handling
  - *Difference from RETURN*: Uses stack-relative addressing for return address, may preserve different stack state
  - *Stack Effect*: [return_value] -> [] (returns to caller)
  - *Updated*: 2025-12-18 20:26 - Documented and implemented basic version

=== Jumps
- *JUMP0-JUMP15 (0x80-0x8F)* [1] Unconditional jump, offset encoded in opcode (0-15). Stack: No effect.
- *FJUMP0-FJUMP15 (0x90-0x9F)* [1] Jump if false (NIL), offset 0-15. Stack: Always pops TOS (pops regardless of condition).
- *TJUMP0-TJUMP15 (0xA0-0xAF)* [1] Jump if true (non-NIL), offset 0-15. Stack: Always pops TOS (pops regardless of condition).
- *JUMPX (0xB0)* [2] Unconditional jump, 8-bit signed offset. Stack: No effect.
  - *Operand*: 1 byte with XOR addressing (addr ^ 3)
  - *Sign-extension*: If byte >= 128, subtract 256 to get signed offset
  - *C Implementation*: `maiko/src/mvs.c` lines 108-113 - uses `GETBYTE((char *)pc + 1)` then sign-extends
  - *CRITICAL*: JUMPX is 8-bit signed, NOT 16-bit. JUMPXX (0xB1) is 16-bit signed.
- *FJUMPX (0xB2)* [3] Jump if false, 8-bit signed offset. Stack: Always pops TOS.
- *TJUMPX (0xB3)* [3] Jump if true, 8-bit signed offset. Stack: Always pops TOS.
- *NFJUMPX (0xB4), NTJUMPX (0xB5)* [3] Negated variants, 8-bit signed offset.

*Critical Stack Behavior for Conditional Jumps*:
- FJUMP variants: Always pop TOS before checking condition. If TOS is NIL, jump; otherwise continue.
- TJUMP variants: Always pop TOS before checking condition. If TOS is non-NIL, jump; otherwise continue.
- This matches C implementation: `FJUMPMACRO(x): if (TOPOFSTACK != 0) { POP; nextop1; } else { CHECK_INTERRUPT; POP; PCMACL += (x); nextop0; }`
- The stack is popped in both branches of the conditional, ensuring TOS is always consumed.

=== Other Control
- *UNWIND (0x07)* [3] Unwind stack to specified frame.
- *BIND (0x11)* [2] Bind variables in PVAR area.
  - *Operands*: byte1 (n1:4, n2:4), byte2 (offset)
  - *Stack*: `[values..., TOS] -> [marker]`
  - *Algorithm*:
    - Calculates `ppvar = (LispPTR *)PVAR + 1 + offset`
    - Pushes `n1` NIL values backwards from `ppvar`
    - If `n2 == 0`: pushes TOS onto stack
    - Otherwise: stores TOS and `n2-1` more values backwards from `ppvar`
    - Sets TOS to marker: `((~(n1 + n2)) << 16) | (offset << 1)`
  - *Marker format*: High 16 bits = `~(n1 + n2)`, low 16 bits = `offset << 1`
- *UNBIND (0x12)* [1] Unbind variables in reverse bind order.
  - *Stack*: `[marker, ...] -> []`
  - *Algorithm*:
    - Walks backwards through stack until finding negative value (marker)
      - *CRITICAL*: Uses SIGNED comparison `((int)*--CSTKPTRL >= 0)`, NOT unsigned
      - Markers have sign bit set (e.g., `0xfffe0002`) but represent negative values when interpreted as signed integers
    - Extracts `num = (~marker) >> 16` and `offset = GetLoWord(marker)`
      - Note: `offset` is used directly (not shifted), calculation is `ppvar = (LispPTR *)((DLword *)PVAR + 2 + offset)`
    - Calculates `ppvar = (LispPTR *)((DLword *)PVAR + 2 + offset)`
    - Restores `num` values to `0xFFFFFFFF` (unbound marker) backwards from `ppvar`
    - After unbinding, `CSTKPTRL` points to the marker
    - *TOPOFSTACK Synchronization*: UNBIND does NOT explicitly update TOPOFSTACK
      - The dispatch loop reads TOPOFSTACK from memory after UNBIND completes
      - This is different from RET which explicitly syncs: `TOPOFSTACK = TopOfStack`
      - See `maiko/inc/inlineC.h:636-685` for implementation details
- *DUNBIND (0x13)* [1] Dynamic unbind.
  - *Stack*: `[marker, ...]` or `[TOS] -> []`
  - *Algorithm*: Similar to UNBIND, but checks TOS first
    - If TOS is negative (marker): uses TOS as marker directly
    - Otherwise: walks backwards to find marker (same as UNBIND)
    - Restores variables and pops marker

== Memory Operations (0x40-0x7F)

=== Variable Access
- *IVAR0-IVAR6 (0x40-0x46)* [1] Push local variable 0-6.
  - Uses LispPTR offset (index * 4 bytes)
  - *C Implementation*: `IVARMACRO(x): PUSH(IVAR[x]);` where `IVAR = ((LispPTR *)IVar)`
  - *IVar Setup*: `IVar = NativeAligned2FromStackOffset(CURRENTFX->nextblock)` = `Stackspace + nextblock`
  - *CRITICAL*: `IVar` is a `DLword*` pointer to stack memory, NOT virtual memory
  - *Access*: `IVAR[x]` reads a `LispPTR` (4 bytes) at offset `x` from `IVar`
  - *Memory Location*: Stack memory (`Stackspace`), not virtual memory
- *IVARX (0x47)* [2] Push indexed local variable.
  - *Stack*: `[] -> [value]`
  - *Operand*: `x` (1B, DLword offset)
  - *CRITICAL*: Uses DLword offset, NOT LispPTR offset
  - *C*: `IVARX(x): PUSH(GetLongWord((DLword *)IVAR + (x)));`
  - *Access*: Reads LispPTR from `(DLword *)IVAR + x` (x is in DLword units)
  - *IVAR Base*: `IVAR = ((LispPTR *)IVar)` where `IVar = Stackspace + nextblock`
  - *Element Size*: Reads 2 DLwords (4 bytes) as LispPTR using `GetLongWord()`
  - *Memory Location*: Stack memory (`Stackspace`), not virtual memory
  - *Byte Order*: Handles big-endian byte order from sysout format
- *PVAR0-PVAR6 (0x48-0x4E)* [2] Push parameter 0-6.
  - Uses LispPTR offset (index * 4 bytes)
- *PVARX (0x4F)* [2] Push indexed parameter.
  - *Stack*: `[] -> [value]`
  - *Operand*: `x` (1B, DLword offset)
  - *CRITICAL*: Uses DLword offset, NOT LispPTR offset
  - *C*: `PVARX(x): PUSH(GetLongWord((DLword *)PVAR + (x)));`
  - *Access*: Reads LispPTR from `(DLword *)PVAR + x` (x is in DLword units)
  - *PVAR Base*: `PVAR` starts after frame header (FRAMESIZE bytes)
  - *Element Size*: Reads 2 DLwords (4 bytes) as LispPTR using `GetLongWord()`
  - *Byte Order*: Handles big-endian byte order from sysout format
- *FVAR0-FVAR6 (0x50-0x56)* [1] Push free variable 0-6.
- *FVARX (0x57)* [2] Push indexed free variable.
- *PVAR_0-PVAR_6 (0x58-0x5E)* [1] Alternative PVAR access.
- *PVARX_ (0x5F)* [2] Alternative indexed PVAR.
  - *Stack*: `[value] -> []`
  - *Operand*: `x` (1B, DLword offset)
  - *CRITICAL*: Uses DLword offset, NOT LispPTR offset
  - *C*: `PVARX_(x): *((LispPTR *)((DLword *)PVAR + (x))) = TOPOFSTACK;`
  - *Access*: Writes LispPTR to `(DLword *)PVAR + x` (x is in DLword units)
  - *Byte Order*: Writes in big-endian byte order for sysout format
- *GVAR (0x60)* [3] Atom index (2B). Push global variable value.
- *ARG0 (0x61)* [1] Push argument 0.
- *IVARX_ (0x62)* [2] Set indexed local variable.
  - *Stack*: `[value] -> []`
  - *Operand*: `x` (1B, DLword offset)
  - *CRITICAL*: Uses DLword offset, NOT LispPTR offset
  - *C*: `IVARX_(x): *((LispPTR *)((DLword *)IVAR + (x))) = TOPOFSTACK;`
  - *Access*: Writes LispPTR to `(DLword *)IVAR + x` (x is in DLword units)
  - *Byte Order*: Writes in big-endian byte order for sysout format
- *GVAR_ (0x63)* [3] Atom index (2B). Set global variable value.
  - *Stack*: `[value] -> []`
  - *Operand*: atom_index (2B)
  - *CRITICAL*: Updates GC refs when setting global variable values
  - Reads old value before writing new value (for GC)
  - Calls `gc_module.deleteReference()` on old value
  - Calls `gc_module.addReference()` on new value
  - Matches C implementation: `FRPLPTR(((struct xpointer *)pslot)->addr, tos)`
  - GC errors are non-fatal (caught and ignored)
  - C: `N_OP_gvarset` in `maiko/src/gvar2.c`
- *ACONST (0x67)* [3] Atom index (2B). Push atom constant.
- *GCONST (0x6F)* [3] Atom index (2B). Push global constant.

=== Variable Setting
- *PVARSETPOP0-PVARSETPOP6 (0x70-0x76)* [1] Set parameter 0-6, pop value.
- *PVARSETPOPX (0x77)* [2] Set indexed parameter, pop value.

=== Stack Operations
- *POP_N (0xC0)* [2] Pop N values (count in operand).
- *PUSH/ADDBASE (0xD0)* [1] Push value onto stack.

== Related Documentation

- Opcode Reference - Complete opcode index
- Data Operations - Data operation opcodes
- Arithmetic & Base Operations - Arithmetic and base operation opcodes
