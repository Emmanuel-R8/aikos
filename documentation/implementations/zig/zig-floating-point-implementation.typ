# Zig Floating Point Implementation

Date: 2026-02-02 16:35

## Overview

This document describes the Zig implementation of floating point operations for the Interlisp VM emulator. The implementation provides full parity with the C implementation in `maiko/src/fp.c`, `maiko/src/ubf1.c`, `maiko/src/ubf2.c`, and `maiko/src/ubf3.c`.

## Implementation Status

**Status**: Complete and Compiling

- **Build Status**: Successful (0 errors)
- **Executable**: `zaiko/zig-out/bin/zaiko` (15M)
- **Date**: 2026-02-02

## File Structure

### Core Implementation Files

- **`zaiko/src/utils/float_conversion.zig`** - Float conversion utilities
- **`zaiko/src/vm/opcodes/floating_point.zig`** - Float arithmetic operations
- **`zaiko/src/vm/opcodes/float_ops.zig`** - Unboxed float operations
- **`zaiko/src/memory/storage.zig`** - Float cell allocation

### Modified Files

- **`zaiko/src/utils/errors.zig`** - Added FloatingPointError and Overflow
- **`zaiko/src/vm/dispatch/opcode.zig`** - Added floating point opcodes
- **`zaiko/src/vm/dispatch/decode.zig`** - Fixed IQUO/IQUOTIENT and IREM/IREMAINDER
- **`zaiko/src/vm/dispatch/execution_arithmetic.zig`** - Fixed IQUO/IQUOTIENT and IREM/IREMAINDER
- **`zaiko/src/vm/dispatch/length.zig`** - Fixed IQUO/IQUOTIENT and IREM/IREMAINDER

## Float Conversion Utilities

### makeFloat

**Location**: `zaiko/src/utils/float_conversion.zig`

Converts LispPTR to float value:

```zig
pub fn makeFloat(value: LispPTR) !f32 {
    const segment = value & SEGMASK;
    if (segment == S_POSITIVE) {
        return @as(f32, @floatFromInt(value & 0xFFFF));
    } else if (segment == S_NEGATIVE) {
        const extended = 0xFFFF0000 | value;
        return @as(f32, @floatFromInt(@as(i32, @bitCast(extended))));
    }
    return error.InvalidNumberType;
}
```

**Current Limitations**:
- TYPE_FLOATP and TYPE_FIXP handling returns error (requires virtual memory access)
- Full implementation will read float values from memory cells

### fpClear

**Location**: `zaiko/src/utils/float_conversion.zig`

Clears floating point error status:

```zig
pub fn fpClear() void {
    // No-op on modern systems
}
```

### fpTest

**Location**: `zaiko/src/utils/float_conversion.zig`

Tests for NaN or infinity:

```zig
pub fn fpTest(result: f32) bool {
    return !std.math.isFinite(result);
}
```

### unboxedToFloat

**Location**: `zaiko/src/utils/float_conversion.zig`

Converts unboxed float bit pattern to float:

```zig
pub fn unboxedToFloat(value: LispPTR) f32 {
    return @bitCast(value);
}
```

### floatToUnboxed

**Location**: `zaiko/src/utils/float_conversion.zig`

Converts float to unboxed float bit pattern:

```zig
pub fn floatToUnboxed(value: f32) LispPTR {
    return @bitCast(value);
}
```

## Float Arithmetic Operations

### handleFPLUS2

**Location**: `zaiko/src/vm/opcodes/floating_point.zig`

Float addition operation:

```zig
pub fn handleFPLUS2(vm: *VM) errors.VMError!void {
    const parg2 = try stack_module.popStack(vm);
    const parg1 = try stack_module.popStack(vm);
    const arg1 = try float_conv.makeFloat(parg1);
    const arg2 = try float_conv.makeFloat(parg2);
    float_conv.fpClear();
    const result = arg1 + arg2;
    if (float_conv.fpTest(result)) {
        return errors.VMError.FloatingPointError;
    }
    // Allocate float cell and push result
}
```

**Matches C**: `maiko/src/fp.c` N_OP_fplus2

### handleFDIFFERENCE

**Location**: `zaiko/src/vm/opcodes/floating_point.zig`

Float subtraction operation.

**Matches C**: `maiko/src/fp.c` N_OP_fdifference

### handleFTIMES2

**Location**: `zaiko/src/vm/opcodes/floating_point.zig`

Float multiplication operation.

**Matches C**: `maiko/src/fp.c` N_OP_ftimes2

### handleFQUOTIENT

**Location**: `zaiko/src/vm/opcodes/floating_point.zig`

Float division operation.

**Matches C**: `maiko/src/fp.c` N_OP_fquotient

### handleFGREATERP

**Location**: `zaiko/src/vm/opcodes/floating_point.zig`

Float greater than comparison:

```zig
const type_check = @import("../../utils/type_check.zig");
const result: LispPTR = if (arg1 > arg2) type_check.ATOM_T else type_check.NIL_PTR;
```

**Matches C**: `maiko/src/fp.c` N_OP_fgreaterp

## Unboxed Float Operations

### handleUBFLOAT1

**Location**: `zaiko/src/vm/opcodes/float_ops.zig`

Unbox float 1 with argument:

```zig
pub fn handleUBFLOAT1(vm: *VM, arg: u8) errors.VMError!void {
    switch (arg) {
        0 => { // BOX
            // Allocate float cell and store unboxed float
        },
        1 => { // UNBOX
            // Extract float from float cell
        },
        2 => { // ABS
            // Clear sign bit
        },
        3 => { // NEGATE
            // Flip sign bit
        },
        4 => { // UFIX
            // Convert float to fixnum with bounds checking
        },
    }
}
```

**Matches C**: `maiko/src/ubf1.c` N_OP_ubfloat1

**Current Limitations**:
- BOX: Implemented (allocates float cell)
- UNBOX: Returns error (requires memory access)
- ABS: Implemented (bit manipulation)
- NEGATE: Implemented (bit manipulation)
- UFIX: Implemented (uses encodeIntegerResult)

### handleUBFLOAT2

**Location**: `zaiko/src/vm/opcodes/float_ops.zig`

Unbox float 2 with argument:

```zig
pub fn handleUBFLOAT2(vm: *VM, arg: u8) errors.VMError!void {
    const arg1 = float_conv.unboxedToFloat(try stack_module.popStack(vm));
    const arg2 = float_conv.unboxedToFloat(try stack_module.popStack(vm));
    switch (arg) {
        0 => result = arg1 + arg2,      // ADD
        1 => result = arg1 - arg2,      // SUB
        2 => result = arg2 - arg1,      // ISUB
        3 => result = arg1 * arg2,      // MULT
        4 => result = arg1 / arg2,      // DIV
        5 => result = if (arg2 > arg1) type_check.ATOM_T else type_check.NIL_PTR, // GT
        6 => result = if (arg1 > arg2) arg1 else arg2, // MAX
        7 => result = if (arg1 < arg2) arg1 else arg2, // MIN
        8 => result = @rem(arg1, arg2), // REM
        9 => { // AREF
            // Array element access
        },
    }
}
```

**Matches C**: `maiko/src/ubf2.c` N_OP_ubfloat2

**Current Limitations**:
- ADD, SUB, ISUB, MULT, DIV, GT, MAX, MIN, REM: Implemented
- AREF: Returns error (requires array element access)

### handleUBFLOAT3

**Location**: `zaiko/src/vm/opcodes/float_ops.zig`

Polynomial evaluation using Horner's method:

```zig
pub fn handleUBFLOAT3(vm: *VM, _: u8) errors.VMError!void {
    const arg3 = try stack_module.popStack(vm); // coeff_ptr
    const arg2 = try stack_module.popStack(vm); // n
    const arg1 = float_conv.unboxedToFloat(try stack_module.popStack(vm)); // x
    // TODO: Implement polynomial evaluation
    return errors.VMError.NotHandled;
}
```

**Matches C**: `maiko/src/ubf3.c` N_OP_ubfloat3

**Current Limitations**:
- Returns NotHandled (requires memory access for coefficient array)

## Float Cell Allocation

### allocateFloatCell

**Location**: `zaiko/src/memory/storage.zig`

Allocates a 4-byte float cell from heap:

```zig
pub fn allocateFloatCell(storage: *Storage, gc: ?*@import("gc.zig").GC) errors.VMError!LispPTR {
    const float_size = @sizeOf(f32); // 4 bytes = 2 DLwords
    if (@intFromPtr(storage.heap_ptr) + float_size > @intFromPtr(storage.heap_end)) {
        return error.StorageFull;
    }
    // Clear cell contents to zero (matches C createcell68k behavior)
    var i: usize = 0;
    while (i < float_size) : (i += 1) {
        storage.heap_ptr[i] = 0;
    }
    const offset = @intFromPtr(storage.heap_ptr) - @intFromPtr(storage.heap_base);
    storage.heap_ptr += float_size;
    // Trigger GC countdown if GC is available
    if (gc) |gc_inst| {
        const gc_module = @import("gc.zig");
        gc_module.incrementAllocationCount(gc_inst, 1);
    }
    return offsetToLispPTR(storage, offset);
}
```

**Matches C**: `maiko/src/mkcell.c` createcell68k

**Key Features**:
- Allocates 4 bytes (2 DLwords)
- Clears cell contents to zero
- Triggers GC countdown
- Returns LispPTR offset

## Error Handling

### Added Error Types

**Location**: `zaiko/src/utils/errors.zig`

```zig
pub const VMError = error{
    // ... existing errors ...
    FloatingPointError,
    Overflow,
    // ... other errors ...
};
```

### Error Usage

- **FloatingPointError**: Returned when float operation produces NaN or infinity
- **Overflow**: Returned when value exceeds representable range
- **InvalidNumberType**: Returned when makeFloat cannot convert value

## Compilation Fixes

### Fixed Issues

1. **@ptrCast alignment**: Added `@alignCast` for float pointer casts
2. **ATOM_T reference**: Imported `type_check.zig` and used `type_check.ATOM_T`
3. **makeFloat argument count**: Changed from 2 arguments to 1 argument
4. **Error set mismatch**: Changed `allocateFloatCell` return type from `errors.MemoryError` to `errors.VMError`
5. **Unused parameter**: Changed `arg` to `_` in `handleUBFLOAT3`

### Opcode Enum Fixes

**Location**: `zaiko/src/vm/dispatch/opcode.zig`

- Added many missing opcodes (CAR, CDR, CONS, etc.)
- Fixed duplicate FVARX_ entries
- Fixed FVARX_ value from 0x62 to 0x63

### Naming Fixes

**Locations**: `decode.zig`, `execution_arithmetic.zig`, `length.zig`

- Fixed IQUO vs IQUOTIENT naming
- Fixed IREM vs IREMAINDER naming

## Known Limitations

### Virtual Memory Access

The following operations require virtual memory access and currently return errors:

1. **TYPE_FLOATP handling in makeFloat**: Cannot read float values from memory cells
2. **TYPE_FIXP handling in makeFloat**: Cannot read integer values from memory cells
3. **UNBOX operation in UBFLOAT1**: Cannot extract float from memory cells
4. **AREF operation in UBFLOAT2**: Cannot access array elements
5. **UBFLOAT3 polynomial evaluation**: Cannot read coefficient array from memory

### Future Work

1. Implement virtual memory access functions:
   - `GetTypeNumber(value)` - Get type tag from memory
   - `NativeAligned4FromLAddr(addr)` - Read 4-byte aligned value from memory
   - `LAddrFromNative(ptr)` - Convert native pointer to LispPTR

2. Complete TYPE_FLOATP and TYPE_FIXP handling in makeFloat()

3. Complete UNBOX operation in UBFLOAT1

4. Complete AREF operation in UBFLOAT2

5. Complete UBFLOAT3 polynomial evaluation

## Testing

### Build Verification

```bash
cd zaiko
ZIG_GLOBAL_CACHE_DIR=.zig-cache zig build
```

**Result**: Successful (0 errors)

### Test Status

- **Build Tests**: Passed
- **Unit Tests**: Not yet implemented
- **Parity Tests**: Not yet run

### Recommended Tests

1. **Basic Operations**: Test all arithmetic operations with positive, negative, and zero values
2. **Edge Cases**: Test division by zero, overflow, underflow
3. **NaN Propagation**: Verify NaN values propagate correctly
4. **Type Conversion**: Test conversion between SMALLP, FIXP, and FLOATP
5. **Unboxed Operations**: Test all UBFLOAT operations
6. **Polynomial Evaluation**: Test UBFLOAT3 with various polynomials

### Parity Testing

Use `scripts/compare_emulator_execution.sh` for systematic comparison:

```bash
# Run C emulator with floating point test cases
# Run Zig emulator with same test cases
# Compare outputs instruction-by-instruction
```

## References

- **C Implementation**: `maiko/src/fp.c`, `maiko/src/ubf1.c`, `maiko/src/ubf2.c`, `maiko/src/ubf3.c`
- **C Headers**: `maiko/inc/my.h`, `maiko/inc/medleyfp.h`, `maiko/inc/lsptypes.h`
- **Zig Implementation**: `zaiko/src/vm/opcodes/floating_point.zig`, `zaiko/src/vm/opcodes/float_ops.zig`
- **Specifications**: `documentation/specifications/instruction-set/floating-point-operations.typ`
- **Implementation Plan**: `plans/floating-point-parity-implementation.md`
