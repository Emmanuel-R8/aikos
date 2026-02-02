# Floating Point Operations

Date: 2026-02-02 16:35

## Overview

Floating point operations in the Interlisp VM provide arithmetic and comparison capabilities for floating point numbers. These operations are critical for scientific computing, graphics, and numerical applications.

## Number Representation

### Small Integers (SMALLP)

Small integers are encoded directly in the LispPTR using segment bits:

- **S_POSITIVE** (0xE0000): Small positive integers (0 to 65535)
- **S_NEGATIVE** (0xF0000): Small negative integers (-65536 to -1)

Encoding: `S_POSITIVE | value` or `S_NEGATIVE | (value & 0xFFFF)`

### Float Cells (TYPE_FLOATP)

Float values are stored in 4-byte cells allocated from the heap. The cell contains the IEEE 754 single-precision float value.

### Fixnum Cells (TYPE_FIXP)

Fixnum values are stored in 4-byte cells allocated from the heap. The cell contains a 32-bit signed integer.

## Core Operations

### Float Arithmetic Operations

These operations pop two float values from the stack, perform the operation, and push the result as a float cell.

#### FPLUS2 (Float Addition)

- **Opcode**: 0x4A
- **Operation**: `result = arg1 + arg2`
- **Stack**: `arg1 arg2 -- result`
- **Error Handling**: Returns FloatingPointError if result is NaN or infinity

#### FDIFFERENCE (Float Subtraction)

- **Opcode**: 0x4B
- **Operation**: `result = arg1 - arg2`
- **Stack**: `arg1 arg2 -- result`
- **Error Handling**: Returns FloatingPointError if result is NaN or infinity

#### FTIMES2 (Float Multiplication)

- **Opcode**: 0x4C
- **Operation**: `result = arg1 * arg2`
- **Stack**: `arg1 arg2 -- result`
- **Error Handling**: Returns FloatingPointError if result is NaN or infinity

#### FQUOTIENT (Float Division)

- **Opcode**: 0x4D
- **Operation**: `result = arg1 / arg2`
- **Stack**: `arg1 arg2 -- result`
- **Error Handling**: Returns FloatingPointError if result is NaN or infinity

#### FGREATERP (Float Greater Than)

- **Opcode**: 0x4E
- **Operation**: `result = (arg1 > arg2) ? T : NIL`
- **Stack**: `arg1 arg2 -- result`
- **Returns**: ATOM_T (1) if arg1 > arg2, NIL_PTR (0) otherwise

### Unboxed Float Operations

These operations work with unboxed float values stored as bit patterns in LispPTR. They are more efficient for numerical computations.

#### UBFLOAT1 (Unbox Float 1)

- **Opcode**: 0x5A
- **Alpha Operations**:
  - **0 (BOX)**: Convert unboxed float to float cell
  - **1 (UNBOX)**: Extract float value from float cell
  - **2 (ABS)**: Absolute value (clear sign bit)
  - **3 (NEGATE)**: Negate (flip sign bit)
  - **4 (UFIX)**: Convert float to fixnum with bounds checking

#### UBFLOAT2 (Unbox Float 2)

- **Opcode**: 0x5B
- **Alpha Operations**:
  - **0 (ADD)**: `result = arg1 + arg2`
  - **1 (SUB)**: `result = arg1 - arg2`
  - **2 (ISUB)**: `result = arg2 - arg1` (reverse order)
  - **3 (MULT)**: `result = arg1 * arg2`
  - **4 (DIV)**: `result = arg1 / arg2`
  - **5 (GT)**: `result = (arg2 > arg1) ? T : NIL`
  - **6 (MAX)**: `result = max(arg1, arg2)`
  - **7 (MIN)**: `result = min(arg1, arg2)`
  - **8 (REM)**: `result = arg1 % arg2`
  - **9 (AREF)**: Array element access

#### UBFLOAT3 (Unbox Float 3)

- **Opcode**: 0x5C
- **Operation**: Polynomial evaluation using Horner's method
- **Formula**: `result = coeff[0] + x * (coeff[1] + x * (coeff[2] + ...))`
- **Stack**: `x n coeff_ptr -- result`

## Type Conversion

### N_MakeFloat

Converts LispPTR to float value:

1. **S_POSITIVE**: Extract low 16 bits as unsigned integer
2. **S_NEGATIVE**: Sign-extend low 16 bits as signed integer
3. **TYPE_FLOATP**: Read float value from memory cell
4. **TYPE_FIXP**: Read integer from memory cell and convert to float

### FPCLEAR

Clears floating point error status. On modern systems, this is a no-op.

### FPTEST

Tests for NaN or infinity:

```c
return !isfinite(result);
```

## Memory Management

### Float Cell Allocation

Float cells are allocated from the heap using the following process:

1. Check if heap has space for 4 bytes (2 DLwords)
2. Clear cell contents to zero (matches C createcell68k behavior)
3. Calculate offset from heap base
4. Advance heap pointer
5. Trigger GC countdown if GC is available
6. Convert offset to LispPTR

### Cell Size

- Float cells: 4 bytes (2 DLwords)
- Fixnum cells: 4 bytes (2 DLwords)

## Error Handling

### FloatingPointError

Returned when a floating point operation produces NaN or infinity. This indicates:

- Division by zero
- Overflow
- Invalid operation (e.g., sqrt(-1))

### Overflow

Returned when a value exceeds the representable range.

## Implementation Notes

### Alignment

Float values must be 4-byte aligned. On systems with strict alignment requirements, use `@alignCast` to assert alignment when casting from byte pointers.

### Endianness

Float values are stored in IEEE 754 single-precision format. On little-endian systems, byte swapping may be required when loading from sysout files.

### GC Integration

Float cell allocation should trigger GC countdown to ensure garbage collection runs periodically.

## Testing Considerations

### Test Cases

1. **Basic Operations**: Test all arithmetic operations with positive, negative, and zero values
2. **Edge Cases**: Test division by zero, overflow, underflow
3. **NaN Propagation**: Verify NaN values propagate correctly
4. **Type Conversion**: Test conversion between SMALLP, FIXP, and FLOATP
5. **Unboxed Operations**: Test all UBFLOAT operations
6. **Polynomial Evaluation**: Test UBFLOAT3 with various polynomials

### Parity Testing

Compare execution traces between C and Zig emulators to ensure identical behavior:

1. Run C emulator with floating point test cases
2. Run Zig emulator with same test cases
3. Compare outputs instruction-by-instruction
4. Investigate any divergences

## References

- C Implementation: `maiko/src/fp.c`, `maiko/src/ubf1.c`, `maiko/src/ubf2.c`, `maiko/src/ubf3.c`
- C Headers: `maiko/inc/my.h`, `maiko/inc/medleyfp.h`, `maiko/inc/lsptypes.h`
- Zig Implementation: `zaiko/src/vm/opcodes/floating_point.zig`, `zaiko/src/vm/opcodes/float_ops.zig`
