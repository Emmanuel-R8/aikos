= Type Checking Specification

*Navigation*: VM Core README | Data Structures | Memory Management

*Date*: 2025-12-17 12:35

Complete specification of type checking mechanisms, including type table access, type number lookup, and type predicates.

== Overview

The VM uses a type table (MDStypetbl) to store type information for all objects in memory. Type checking is essential for:
- Validating object types before operations (e.g., CAR/CDR on lists)
- Determining object layout and access methods
- Implementing type predicates (LISTP, NUMBERP, etc.)

== Type Table Structure

=== MDStypetbl Location

*C Definition*: `maiko/inc/lspglob.h` - `extern DLword *MDStypetbl;`

*Memory Layout*: 
- Type table is indexed by page number: `(address) >> 9`
- Each entry is a DLword (2 bytes)
- Type number is in low 11 bits: `type_entry & 0x7ff`

=== Type Entry Access

*C Implementation*: `maiko/inc/lsptypes.h:611-614`

#codeblock(lang: "c", [
#define GetTypeEntry(address) (GETWORD(MDStypetbl + ((address) >> 9)))
#define GetTypeNumber(address) (GetTypeEntry(address) & 0x7ff)
])

*Calculation*:
- *Page number*: `(address) >> 9` (divide by 512 bytes = 1 page)
- *Type entry offset*: `MDStypetbl + page_number` (DLword offset)
- *Type number*: Low 11 bits of type entry (`& 0x7ff`)

== Type Numbers

*C Definition*: `maiko/inc/lsptypes.h:59-64`

Common type numbers:
- `TYPE_SMALLP = 1` - Small integer (encoded directly)
- `TYPE_FIXP = 2` - Boxed integer (pointer to memory)
- `TYPE_LITATOM = 4` - Literal atom
- `TYPE_LISTP = 5` - Cons cell (list)
- `TYPE_ARRAYP = 6` - Array
- `TYPE_NEWATOM = 21` - New atom (BIGATOMS)

== Type Predicates

=== Listp (List Predicate)

*C Implementation*: `maiko/inc/lsptypes.h:617`

#codeblock(lang: "c", [
#define Listp(address) (GetTypeNumber(address) == TYPE_LISTP)
])

*Behavior*:
- Returns `true` if address points to a cons cell
- Uses type table lookup: `GetTypeNumber(address) == TYPE_LISTP`
- *CRITICAL*: Requires type table access - cannot be implemented without MDStypetbl

*Special Cases*:
- `NIL_PTR (0)`: Not a list (though `(cdr nil) = nil`)
- `ATOM_T (1)`: Not a list (T is an atom)

=== Numberp (Number Predicate)

*C Implementation*: `maiko/inc/lsptypes.h:619`

#codeblock(lang: "c", [
#define Numberp(address) (GetTypeEntry(address) & TT_NUMBERP)
])

*Behavior*:
- Returns `true` if address points to any number type
- Uses type table mask bits: `TT_NUMBERP = 0x1000`

== Address Range Heuristics

*CRITICAL*: When type table is not available, use address range heuristics:

=== Cons Cell Address Range

*Valid Range*: `>= 0x10000` (typically `>= MDS_OFFSET = 0x180000`)

*Reasoning*:
- Cons cells are in MDS (heap) region
- MDS_OFFSET is typically `0x180000` or larger
- Small values (`< 0x10000`) are:
  - Small positive integers (`S_POSITIVE | value`, where `S_POSITIVE = 0xE0000`)
  - Small negative integers (`S_NEGATIVE | value`, where `S_NEGATIVE = 0xF0000`)
  - Characters (`S_CHARACTER | value`)
  - Special values (NIL=0, T=1)

*Implementation Note*: 
- Heuristic: Reject addresses `< 0x10000` as non-list pointers
- This prevents crashes when type table is unavailable
- Full correctness requires type table lookup

=== Small Integer Encoding

*Small Positive Integers*:
- *Base*: `S_POSITIVE = 0xE0000` (from `maiko/inc/lispmap.h:140`)
- *Encoding*: `S_POSITIVE | value` (e.g., value 1 = `0xE0001`)
- *Type*: `TYPE_SMALLP = 1`

*Small Negative Integers*:
- *Base*: `S_NEGATIVE = 0xF0000` (from `maiko/inc/lispmap.h:145`)
- *Encoding*: `S_NEGATIVE | value`
- *Type*: `TYPE_SMALLP = 1`

*Characters*:
- *Base*: `S_CHARACTER = 0x70000` (from `maiko/inc/lispmap.h:149`)
- *Encoding*: `S_CHARACTER | char_code`
- *Type*: `TYPE_CHARACTERP = 9`

== Implementation Notes

=== Without Type Table

*Heuristic Approach* (temporary):
1. Check special values (NIL, T)
2. Reject addresses `< 0x10000` (too small for heap objects)
3. Check alignment (even addresses for cons cells)
4. Check bounds (within virtual memory)

*Limitations*:
- Cannot distinguish between different object types at same address range
- May incorrectly accept non-list pointers as lists
- Should be replaced with full type table lookup when available

=== With Type Table

*Full Implementation*:
1. Calculate page number: `page = address >> 9`
2. Lookup type entry: `type_entry = MDStypetbl[page]`
3. Extract type number: `type_num = type_entry & 0x7ff`
4. Compare: `type_num == TYPE_LISTP`

*Requirements*:
- MDStypetbl must be initialized from sysout
- Type table must be accessible during execution
- Page number calculation must match C implementation

== Related Documentation

- Data Structures - Object structure definitions
- Memory Management - Memory layout and regions
- Cons Cells - Cons cell structure
