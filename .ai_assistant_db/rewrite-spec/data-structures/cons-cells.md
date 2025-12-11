# Cons Cell Specification

**Navigation**: [README](README.md) | [Arrays](arrays.md) | [Function Headers](function-headers.md)

Complete specification of cons cell format, CDR coding, and cons cell operations.

## Overview

Cons cells are the fundamental building blocks of Lisp lists. They use CDR coding to efficiently represent list structure, especially for lists where CDR values are NIL or nearby cells.

**CRITICAL**: Before accessing CAR or CDR, implementations MUST validate that the address points to a valid list (cons cell). This is done using `Listp()` type checking.

## Cons Cell Structure

### Basic Format

```pseudocode
struct ConsCell:
    car_field: LispPTR    // CAR value (32 bits)
    cdr_code: 8 bits      // CDR code (low byte of second word)
    // ... padding ...
```

**Size**: 8 bytes (2 DLwords)
**Alignment**: 4-byte aligned

### Memory Layout

```
Offset  Size    Field
------  ----    -----
0       4       car_field (LispPTR)
4       2       (unused/reserved)
6       1       cdr_code
7       1       (padding)
```

## CDR Coding

CDR coding is a compact representation that avoids storing full CDR pointers when possible.

### CDR Code Values

**Without NEWCDRCODING**:

- **CDR_NIL (128)**: CDR is NIL
- **CDR_INDIRECT (0)**: CDR stored indirectly
- **CDR_ONPAGE (128-255)**: CDR is on same page (offset encoded)
- **CDR_MAXINDIRECT (1-127)**: CDR on different page (offset encoded)

**With NEWCDRCODING**:

- **CDR_NIL (8)**: CDR is NIL
- **CDR_INDIRECT (0)**: CDR stored indirectly
- **CDR_ONPAGE (8-15)**: CDR is on same page (3-bit offset)
- **CDR_MAXINDIRECT (1-7)**: CDR on different page (offset encoded)

### CDR Decoding Algorithm

```pseudocode
function DecodeCDR(cons_cell, cell_address):
    cdr_code = cons_cell.cdr_code

    if cdr_code == CDR_NIL:
        return NIL_PTR

    if cdr_code == CDR_INDIRECT:
        // Indirect encoding: CAR points to indirect cell
        indirect_cell = GetConsCell(cons_cell.car_field)
        return DecodeCDR(indirect_cell, cons_cell.car_field)

    if NEWCDRCODING:
        if cdr_code > CDR_ONPAGE:
            // Same page encoding (3-bit offset)
            offset = (cdr_code & 7) << 1
            return cell_address + offset
        else:
            // Different page encoding
            offset = cdr_code << 1
            return cell_address + offset
    else:
        if cdr_code > CDR_ONPAGE:
            // Same page encoding (7-bit offset)
            offset = (cdr_code & 127) << 1
            return POINTER_PAGEBASE(cell_address) + offset
        else:
            // Different page encoding
            offset = cdr_code << 1
            return POINTER_PAGEBASE(cell_address) + offset
```

### CDR Encoding Algorithm

```pseudocode
function EncodeCDR(cons_cell, cell_address, cdr_value):
    if cdr_value == NIL_PTR:
        cons_cell.cdr_code = CDR_NIL
        return

    // Check if same page
    if NEWCDRCODING:
        if SamePage(cell_address, cdr_value) and (cdr_value > cell_address) and (cdr_value <= cell_address + 14):
            offset = (cdr_value - cell_address) >> 1
            cons_cell.cdr_code = CDR_ONPAGE + offset
            return
    else:
        if SamePage(cell_address, cdr_value):
            offset = (cdr_value & 0xFF) >> 1
            cons_cell.cdr_code = CDR_ONPAGE + offset
            return

    // Check if can use different page encoding
    if CanEncodeDifferentPage(cell_address, cdr_value):
        offset = CalculateOffset(cell_address, cdr_value)
        cons_cell.cdr_code = offset >> 1
        return

    // Must use indirect encoding
    indirect_cell = AllocateIndirectCell()
    indirect_cell.car_field = cdr_value
    cons_cell.car_field = LAddrFromNative(indirect_cell)
    cons_cell.cdr_code = CDR_INDIRECT
```

## Cons Cell Operations

### CAR Operation

```pseudocode
function CAR(cons_cell_address):
    cons_cell = GetConsCell(cons_cell_address)

    if cons_cell.cdr_code == CDR_INDIRECT:
        // CAR stored in indirect cell
        indirect_cell = GetConsCell(cons_cell.car_field)
        return indirect_cell.car_field
    else:
        return cons_cell.car_field
```

### CDR Operation

```pseudocode
function CDR(cons_cell_address):
    cons_cell = GetConsCell(cons_cell_address)
    cdr_code = cons_cell.cdr_code

    if cdr_code == CDR_NIL:
        return NIL_PTR

    if cdr_code == CDR_INDIRECT:
        return CDR(cons_cell.car_field)

    // Decode CDR from code
    return DecodeCDR(cons_cell, cons_cell_address)
```

### CONS Operation

```pseudocode
function CONS(car_value, cdr_value):
    // Allocate new cons cell
    new_cell = AllocateConsCell()
    new_cell_address = LAddrFromNative(new_cell)

    // Set CAR
    new_cell.car_field = car_value

    // Encode CDR
    EncodeCDR(new_cell, new_cell_address, cdr_value)

    // Update GC references
    ADDREF(new_cell_address)
    DELREF(car_value)
    DELREF(cdr_value)

    return new_cell_address
```

### RPLACA Operation

```pseudocode
function RPLACA(cons_cell_address, new_car):
    cons_cell = GetConsCell(cons_cell_address)

    // Update GC references
    old_car = CAR(cons_cell_address)
    DELREF(old_car)
    ADDREF(new_car)

    // Set new CAR
    if cons_cell.cdr_code == CDR_INDIRECT:
        indirect_cell = GetConsCell(cons_cell.car_field)
        indirect_cell.car_field = new_car
    else:
        cons_cell.car_field = new_car

    return cons_cell_address
```

### RPLACD Operation

```pseudocode
function RPLACD(cons_cell_address, new_cdr):
    cons_cell = GetConsCell(cons_cell_address)

    // Update GC references
    old_cdr = CDR(cons_cell_address)
    DELREF(old_cdr)
    ADDREF(new_cdr)

    // Encode new CDR
    EncodeCDR(cons_cell, cons_cell_address, new_cdr)

    return cons_cell_address
```

## Cons Page Organization

### Cons Page Structure

```pseudocode
struct ConsPage:
    count: uint           // Number of free cells
    next_cell: LispPTR   // Next free cell pointer
    // ... cons cells follow ...
```

### Free Cell Management

```pseudocode
struct FreeCons:
    next_free: LispPTR   // Next free cell in chain
    // ... (reused as cons cell when allocated) ...
```

## CDR Coding Examples

### Example 1: Simple List

```
List: (A B C)
Cells:
  Cell1: CAR=A, CDR=Cell2 (same page, offset=2)
         cdr_code = CDR_ONPAGE + 1 = 9 (NEWCDRCODING)

  Cell2: CAR=B, CDR=Cell3 (same page, offset=2)
         cdr_code = CDR_ONPAGE + 1 = 9

  Cell3: CAR=C, CDR=NIL
         cdr_code = CDR_NIL = 8
```

### Example 2: Indirect CDR

```
List: (A . B) where B is far away
Cell1: CAR=A, CDR=indirect
       car_field = IndirectCell1
       cdr_code = CDR_INDIRECT = 0

IndirectCell1: CAR=B, CDR=...
                (normal cons cell)
```

## Related Documentation

- [Memory Management](../memory/) - Cons cell allocation
- [Garbage Collection](../memory/garbage-collection.md) - Cons cell reclamation
- [Instruction Set](../instruction-set/) - CAR/CDR opcodes
