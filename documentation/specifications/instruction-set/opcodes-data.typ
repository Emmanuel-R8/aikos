= Opcode Reference - Data Operations


Data operation opcodes for cons cells, arrays, types, and lists.

== Data Operations (0x00-0x3F, 0x80-0xBF)

=== Cons Operations
- *CAR* (0x01) [1] TOS = CAR(TOS). Handles CDR_INDIRECT.
- *Validation*: Must check `Listp(TOS)` before accessing. If not a list, trigger UFN (undefined function name) lookup.
- *Special case*: CAR of T (ATOM_T = 1) returns T.
- *CDR_INDIRECT*: If `cdr_code == CDR_INDIRECT`, CAR value points to indirect cell containing actual CAR.
- *CDR* (0x02) [1] TOS = CDR(TOS). Uses CDR coding.
- *Validation*: Must check `Listp(TOS)` before accessing. If not a list, trigger UFN lookup.
- *NIL handling*: CDR of NIL returns NIL (no validation needed for NIL).
- *CONS* (0x1A) [1] Pop CDR, CAR. Push new cons cell. Allocates memory.
- *RPLACA* (0x18) [1] Pop new CAR, cons ptr. Modify CAR. Push cons ptr.
- *RPLACD* (0x19) [1] Pop new CDR, cons ptr. Modify CDR (CDR coding). Push cons ptr.
- *CREATECELL* (0x1F) [1] Pop type code. Allocate cell from DTD. Push address.
- *RPLPTR_N* (0x24) [2] Replace at offset N.
- Operand: offset (1B)
  - Replaces at `base + offset` with new value
  - Updates GC refs: DELREF old value, ADDREF new value (matches C FRPLPTR behavior)
- Returns base on stack
- C: `N_OP_rplptr` in `maiko/src/gvar2.c`

=== Array Operations
- *AREF1* (0xB6) [1] Pop index, array. Push element.
- *Type Check*: Must verify `GetTypeNumber(array_ptr) == TYPE_ONED_ARRAY` (14)
- *Structure*: Accesses `OneDArray` structure at `array_ptr`
    - `base`: Base address (24 bits for non-BIGVM, 28 bits for BIGVM)
    - `offset`: Offset into array (DLword units)
    - `totalsize`: Total array size (DLword units)
- `typenumber`: Element type number (determines element size and encoding) - *Index Validation*:
    - Index must be in `S_POSITIVE` segment (non-negative)
    - Index must be `< totalsize`
- Final index = `index + offset` - *Type Dispatch*: Element access depends on `typenumber`:
    - `38` (TYPE_POINTER): 32-bit elements (LispPTR)
    - `20` (TYPE_SIGNED_16): 16-bit signed integers (DLword, sign-extended)
    - `22` (TYPE_SIGNED_32): 32-bit signed integers (LispPTR, may be boxed as FIXP)
    - `67` (TYPE_CHARACTER): 8-bit character elements (with S_CHARACTER tag)
    - `0` (TYPE_UNSIGNED_1BIT): 1-bit per element (bit array)
    - `3` (TYPE_UNSIGNED_8BIT): 8-bit unsigned elements
- Other types: See C implementation `aref_switch()` in `maiko/src/my.c`
- *C Reference*: `N_OP_aref1` in `maiko/src/arrayops.c`, `AREF1` macro in `maiko/inc/inlineC.h`
- *AREF2* (0xEE) [1] Pop index1, index0, array. Push element (2D).
- Uses `LispArray` structure (multi-dimensional)
- Calculates linear index: `(index0 Dim1) + index1`
- *ASET1* (0xB7) [1] Pop value, index, array. Set element. Push array.
- *Type Check*: Must verify `GetTypeNumber(array_ptr) == TYPE_ONED_ARRAY` (14)
- *Read-only Check*: If `array.readonlyp == 1`, trigger error
- *Index Validation*: Same as AREF1
- *Type Dispatch*: Element write depends on `typenumber`:
    - `38` (TYPE_POINTER): Write full LispPTR value
    - `20` (TYPE_SIGNED_16): Write low 16 bits of value
    - `67` (TYPE_CHARACTER): Write low 8 bits of value
- Other types: See C implementation `aset_switch()` in `maiko/src/my.c`
- *C Reference*: `N_OP_aset1` in `maiko/src/arrayops.c`, `ASET1` macro in `maiko/inc/inlineC.h`
- *ASET2* (0xEF) [1] Pop value, index1, index0, array. Set element. Push array.
- Uses `LispArray` structure (multi-dimensional)

=== Type Operations - *NTYPX* (0x04) [1] TOS = type number of TOS.
- Returns type number (0-2047) from type table: `GetTypeNumber(TOS) = GetTypeEntry(TOS) & 0x7ff`
- Type entry read from: `MDStypetbl + (TOS >> 9)` - *TYPEP* (0x05) [2] Type code (1B). TOS = (GetType(TOS) == code) ? T : NIL.
- Compares type number of TOS with operand type code.
- Uses `GetTypeNumber(TOS)` to get type number.
- *DTEST* (0x06) [3] Atom index (2B). TOS = (TOS has type named by atom_index) ? T : NIL.
- *Implementation*: Walks DTD (Data Type Descriptor) chain starting from `GetDTD(GetTypeNumber(TOS))`.
- Returns ATOM_T (1) if match found, NIL_PTR (0) otherwise.
- *STRINGP* (0xA3) [1] TOS = IsString(TOS) ? T : NIL.
- *ARRAYP* (0xA4) [1] TOS = IsArray(TOS) ? T : NIL.
- *CHARACTERP* (0xA5) [1] TOS = IsCharacter(TOS) ? T : NIL.

*Type Checking Functions*:
- `Listp(address)`: Returns true if `GetTypeNumber(address) == TYPE_LISTP` (5)
- `GetTypeNumber(address)`: Returns type number from type table (low 11 bits of type entry) - `GetTypeEntry(address)`: Returns full 16-bit type entry from `MDStypetbl + (address >> 9)`

*Note*: `FIXP` (0xA0), `SMALLP` (0xA1), and `LISTP` (0xA2) do not exist as separate opcodes. These values are used by `TJUMP0`-`TJUMP2`. Use `TYPEP` with appropriate type codes instead.

=== List/Atom Operations - *ASSOC* (0x16) [1] Association list lookup.
  - Traverses association list (list of (key . value) pairs)
  - Compares keys using EQ (pointer equality)
- Returns matching pair if found, NIL otherwise
- C: `N_OP_assoc` in `maiko/src/vars3.c` - *FMEMB* (0x1C) [1] Fast member test.
  - Tests if item is in list using equality (EQ)
- Returns list starting from item if found, NIL otherwise
- C: `N_OP_fmemb` in `maiko/src/lsthandl.c` - *RESTLIST* (0x23) [2] Rest of list.
- Operand: count (1B)
  - Builds list by consing elements (C implementation uses IVar array)
- Simplified: traverses list count times using CDR
- C: `N_OP_restlist` in `maiko/src/z2.c` - *RPLCONS* (0x26) [1] Replace cons CDR.
  - Replaces CDR of cons cell with new value
- Updates GC refs: DELREF old CDR, ADDREF new CDR
- Returns list (unchanged)
- C: `N_OP_rplcons` in `maiko/src/rplcons.c` - *LISTGET* (0x27) [1] Get element from list by index.
  - Traverses list index times using CDR
  - Returns CAR of current position
  - Returns NIL if index out of bounds
- C: `N_OP_listget` in `maiko/src/lsthandl.c`

== Related Documentation

- Opcode Reference - Complete opcode index
- Control Flow & Memory Operations - Control flow and memory opcodes - Arithmetic & Base Operations - Arithmetic and base operation opcodes
