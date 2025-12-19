= Atom Table Structure

*Navigation*: Data Structures README | Function Calls

*Date*: 2025-12-17 12:29

Complete specification of atom table structure, atom cell layout, and access methods.

== Overview

The atom table (AtomSpace) stores symbol definitions in the Lisp system. Each atom has multiple cells:
- *Pname Cell*: Symbol name (string pointer)
- *Value Cell*: Top-level value binding
- *Definition Cell (DefCell)*: Function definition pointer
- *Property List Cell*: Property list pointer

== Memory Layout

=== BIGVM + BIGATOMS Configuration

*AtomSpace Location*: `Lisp_world + ATOMS_OFFSET`
- *ATOMS_OFFSET*: `0x2c0000` bytes (from `maiko/inc/lispmap.h`)
- *AtomSpace*: Array of atoms, each atom occupies 5 LispPTRs (20 bytes)

*Atom Cell Layout* (per atom, 5 LispPTRs = 20 bytes):
- *LispPTR 0*: Pname cell pointer
- *LispPTR 1*: Value cell pointer  
- *LispPTR 2*: Definition cell pointer (DefCell)
- *LispPTR 3*: Property list cell pointer
- *LispPTR 4*: Package index and flags

*Cell Access Offsets* (from start of atom):
- *PNAME_PTROFF*: 0 (first LispPTR)
- *VALUE_PTROFF*: 1 (second LispPTR)
- *DEFN_PTROFF*: 2 (third LispPTR)
- *PLIST_PTROFF*: 3 (fourth LispPTR)

== Atom Index

*Atom Index*: Integer value identifying an atom in the atom table.

*Source*: 
- For function calls: Read from instruction operand (2 bytes for non-BIGATOMS)
- C: `Get_AtomNo_PCMAC1` reads DLword from `PC + 1`
- Format: Direct atom index (no offset adjustment needed)

*Range*: Valid atom indices start from 0, increasing sequentially.

== Definition Cell (DefCell) Access

=== GetDEFCELL Calculation (BIGVM + BIGATOMS)

*C Implementation*: `maiko/inc/cell.h:394`

#codeblock(lang: "c", [
#define GetDEFCELLlitatom(index) \
    ((LispPTR *)AtomSpace + (5 * (index)) + NEWATOM_DEFN_PTROFF)
])

*Calculation Details*:
- *AtomSpace*: Pointer to start of atom table (`Lisp_world + ATOMS_OFFSET`)
- *Atom offset*: `5 * index` LispPTRs (each atom is 5 LispPTRs)
- *Cell offset*: `NEWATOM_DEFN_PTROFF` (2) LispPTRs from start of atom
- *Total LispPTR offset*: `(5 * index) + 2`
- *Byte offset from AtomSpace*: `((5 * index) + 2) * sizeof(LispPTR)`
- *Total byte offset from Lisp_world*: `ATOMS_OFFSET + ((5 * index) + 2) * 4`

*Example* (index = 10):
- LispPTR offset: `(5 * 10) + 2 = 52` LispPTRs
- Byte offset from AtomSpace: `52 * 4 = 208` bytes (0xd0)
- Total byte offset: `0x2c0000 + 0xd0 = 0x2c00d0`

=== DefCell Structure (BIGVM)

*C Definition*: `maiko/inc/cell.h:170-182`

*Size*: 4 LispPTRs = 16 bytes

*Structure Layout*:
#codeblock(lang: "text", [
LispPTR 0 (bytes 0-3):
  - Bit 31: ccodep (1 = C code function, 0 = Lisp function)
  - Bit 30: fastp (fast function flag)
  - Bits 29-28: argtype (argument type)
  - Bits 27-0: defpointer (28-bit pointer to function definition)

LispPTR 1 (bytes 4-7):
  - nil_PL (skip proplist cell)

LispPTR 2 (bytes 8-11):
  - nilpkg (8 bits): Package index
  - nil2 (4 bits): Padding
  - pseudocodep (1 bit): Pseudo code flag
  - byteswapped (1 bit): Byte-swapped flag
  - nil_last (18 bits): Padding

LispPTR 3 (bytes 12-15):
  - Reserved/unused
])

*C Access Pattern*:
#codeblock(lang: "c", [
defcell_word = *(int *)fn_defcell;  // Read first LispPTR as native int
ccodep = (defcell_word >> 31) & 1;
fastp = (defcell_word >> 30) & 1;
argtype = (defcell_word >> 28) & 3;
defpointer = defcell_word & 0x0FFFFFFF;  // Low 28 bits
])

*Byte Order*: 
- *CRITICAL*: DefCell is stored in *native byte order* in memory
- After sysout loading, pages are byte-swapped to native format
- First LispPTR should be read as *little-endian* on little-endian hosts
- C code reads directly as native `int` (no byte-swapping needed)

== NEWATOM vs LITATOM

*LITATOM*: Traditional atom stored in AtomSpace array
- *Detection*: `(atom_index & SEGMASK) == 0`
- *Access*: Use `GetDEFCELLlitatom(index)` calculation above
- *SEGMASK*: `0x0F000000` (bits 28-31)

*NEWATOM*: Extended atom with pointer-based storage
- *Detection*: `(atom_index & SEGMASK) != 0`
- *Access*: `NativeAligned4FromLAddr(atom_index + NEWATOM_DEFN_OFFSET)`
- *NEWATOM_DEFN_OFFSET*: 8 DLwords = 16 bytes

== Implementation Notes

=== Address Calculation

*CRITICAL*: Pointer arithmetic in C uses element count, not bytes:
- `(LispPTR *)AtomSpace + offset` adds `offset * sizeof(LispPTR)` bytes
- In byte-based implementations, calculate: `ATOMS_OFFSET + (5 * index + 2) * 4`

=== Byte Order Handling

*CRITICAL*: After sysout loading:
1. All memory pages are byte-swapped to native format (C: `word_swap_page()`)
2. DefCell structure is in native byte order
3. Read first LispPTR as native little-endian (not big-endian)
4. C code: `*(int *)ptr` reads as native int (no conversion needed)

=== Invalid DefPointer Handling

*CRITICAL*: `defpointer = 0` indicates atom has no function definition:
- *Detection*: `defpointer & POINTERMASK == 0`
- *C Behavior*: `op_fn_common` checks `GetTypeNumber(defcell->defpointer) == TYPE_COMPILED_CLOSURE`
  - If not a compiled closure, uses `ATOM_INTERPRETER` (interpreter atom)
  - If `defpointer = 0`, should trigger UFN (Undefined Function Name) lookup
- *UFN Lookup*: C code `op_ufn` looks up function in UFN table using opcode byte
- *Implementation Note*: Must check `defpointer != 0` before calling `NativeAligned4FromLAddr()`

=== Bounds Checking

*CRITICAL*: Always validate:
- `defcell_offset + DefCell_size <= virtual_memory.len`
- Atom index is within valid range
- DefCell pointer is valid before dereferencing
- `defpointer != 0` before translating to native address

== Related Documentation

- Function Calls - How function calls use DefCell
- Memory Layout - Overall memory organization
- Sysout Format - Sysout file structure
