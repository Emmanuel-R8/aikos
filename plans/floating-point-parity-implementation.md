# Floating Point Operations Parity Implementation Plan

**Date**: 2026-02-02
**Status**: Planning Phase
**Priority**: HIGH (Critical missing functionality)

## Executive Summary

This plan outlines the implementation of floating point operations parity between the C and Zig emulators. The Zig implementation currently has placeholder implementations that perform integer arithmetic instead of proper floating point operations. This is a critical gap that must be addressed to achieve full parity.

## Current State Analysis

### C Implementation (Production Reference)

**Files**:
- [`maiko/src/fp.c`](maiko/src/fp.c) - Core floating point arithmetic
- [`maiko/src/ubf1.c`](maiko/src/ubf1.c) - Unboxed float operations (1 argument)
- [`maiko/src/ubf2.c`](maiko/src/ubf2.c) - Unboxed float operations (2 arguments)
- [`maiko/src/ubf3.c`](maiko/src/ubf3.c) - Polynomial evaluation
- [`maiko/inc/my.h`](maiko/inc/my.h) - N_MakeFloat macro
- [`maiko/inc/medleyfp.h`](maiko/inc/medleyfp.h) - FP error handling
- [`maiko/src/mkcell.c`](maiko/src/mkcell.c) - Cell allocation

**Key Components**:

1. **N_MakeFloat Macro** ([`maiko/inc/my.h:39-58`](maiko/inc/my.h:39-58)):
   ```c
   #define N_MakeFloat(arg, dest, tos) do {
       switch (SEGMASK & (LispPTR)(arg)) {
       case S_POSITIVE:
           (dest) = (float)(0xFFFF & (LispPTR)(arg));
           break;
       case S_NEGATIVE:
           (dest) = (float)((int)(0xFFFF0000 | (LispPTR)(arg)));
           break;
       default:
           switch (GetTypeNumber(arg)) {
             case TYPE_FLOATP:
               (dest) = *((float *)NativeAligned4FromLAddr(arg));
               break;
             case TYPE_FIXP:
               (dest) = (float)(*((int *)NativeAligned4FromLAddr(arg)));
               break;
             default: ERROR_EXIT(tos);
           }
       }
   } while (0)
   ```

2. **FPCLEAR/FPTEST** ([`maiko/inc/medleyfp.h:43-47`](maiko/inc/medleyfp.h:43-47)):
   ```c
   #define FPCLEAR do {} while (0)
   #define FPTEST(result) (!isfinite(result))
   ```

3. **createcell68k** ([`maiko/src/mkcell.c:94-157`](maiko/src/mkcell.c:94-157)):
   - Allocates cell from free list
   - Returns native pointer to allocated cell
   - Clears cell contents to zero

4. **Floating Point Operations** ([`maiko/src/fp.c`](maiko/src/fp.c)):
   - **FPLUS2** (op 350): Float addition
   - **FDIFFERENCE** (op 351): Float subtraction
   - **FTIMES2** (op 352): Float multiplication
   - **FQUOTIENT** (op 353): Float division
   - **FGREATERP** (op 362): Float comparison

5. **UBFLOAT1** ([`maiko/src/ubf1.c`](maiko/src/ubf1.c)):
   - **BOX** (alpha 0): Create float cell from unboxed float
   - **UNBOX** (alpha 1): Extract float from float cell
   - **ABS** (alpha 2): Absolute value (clear sign bit)
   - **NEGATE** (alpha 3): Negate (flip sign bit)
   - **UFIX** (alpha 4): Convert float to fixnum with bounds checking

6. **UBFLOAT2** ([`maiko/src/ubf2.c`](maiko/src/ubf2.c)):
   - **ADD** (alpha 0): `ans = arg1 + arg2`
   - **SUB** (alpha 1): `ans = arg2 - arg1`
   - **ISUB** (alpha 2): `ans = arg1 - arg2`
   - **MULT** (alpha 3): `ans = arg1 * arg2`
   - **DIV** (alpha 4): `ans = arg2 / arg1`
   - **GT** (alpha 5): Return ATOM_T if arg2 > arg1
   - **MAX** (alpha 6): Return larger of arg1, arg2
   - **MIN** (alpha 7): Return smaller of arg1, arg2
   - **REM** (alpha 8): `ans = fmodf(arg2, arg1)`
   - **AREF** (alpha 9): Array element access

7. **UBFLOAT3** ([`maiko/src/ubf3.c`](maiko/src/ubf3.c)):
   - Polynomial evaluation using Horner's method
   - `ans = (ans * val) + *((float *)(++fptr))`

### Zig Implementation (Current State)

**Files**:
- [`zaiko/src/vm/opcodes/float_ops.zig`](zaiko/src/vm/opcodes/float_ops.zig) - UBFLOAT1/2/3 (incomplete)
- [`zaiko/src/vm/opcodes/floating_point.zig`](zaiko/src/vm/opcodes/floating_point.zig) - FPLUS2/3/4/5, FGREATERP (placeholder)

**Current Issues**:

1. **float_ops.zig** ([`zaiko/src/vm/opcodes/float_ops.zig`](zaiko/src/vm/opcodes/float_ops.zig)):
   - UBFLOAT1: Basic implementation with TODO comments
   - UBFLOAT2: Basic implementation with TODO comments
   - UBFLOAT3: Basic implementation with TODO comments

2. **floating_point.zig** ([`zaiko/src/vm/opcodes/floating_point.zig`](zaiko/src/vm/opcodes/floating_point.zig)):
   - FPLUS2: Placeholder - performs integer addition
   - FDIFFERENCE: Placeholder - performs integer subtraction
   - FTIMES2: Placeholder - performs integer multiplication
   - FQUOTIENT: Placeholder - performs integer division
   - FGREATERP: Placeholder - performs integer comparison

**Missing Components**:

1. **N_MakeFloat equivalent**: No function to convert LispPTR to float
2. **createcell68k equivalent**: No function to allocate float cells
3. **FP error handling**: No FPCLEAR/FPTEST equivalent
4. **Memory access for float cells**: No function to read float values from memory

## Implementation Plan

### Phase 1: Foundation Components

#### Task 1.1: Implement N_MakeFloat Equivalent

**File**: [`zaiko/src/utils/types.zig`](zaiko/src/utils/types.zig) or new file [`zaiko/src/utils/float_conversion.zig`](zaiko/src/utils/float_conversion.zig)

**Function Signature**:
```zig
pub fn makeFloat(value: LispPTR, tos: LispPTR) !f32
```

**Implementation**:
```zig
pub fn makeFloat(value: LispPTR, tos: LispPTR) !f32 {
    const segment = value & SEGMASK;

    // Check for SMALLP (small positive or negative)
    if (segment == S_POSITIVE) {
        // Small positive: extract low 16 bits
        return @as(f32, @floatFromInt(value & 0xFFFF));
    } else if (segment == S_NEGATIVE) {
        // Small negative: sign extend low 16 bits
        const extended = 0xFFFF0000 | value;
        return @as(f32, @floatFromInt(@as(i32, @bitCast(extended))));
    }

    // Not SMALLP, check for TYPE_FLOATP or TYPE_FIXP
    // TODO: When virtual memory access is available, check type tag and extract value
    // For now, return error if not SMALLP
    return error.InvalidNumberType;
}
```

**Dependencies**:
- Virtual memory access (for TYPE_FLOATP and TYPE_FIXP)
- Type tag checking (GetTypeNumber equivalent)

**Testing**:
- Test with S_POSITIVE values (0-65535)
- Test with S_NEGATIVE values (-65536 to -1)
- Test with TYPE_FLOATP cells (when VM available)
- Test with TYPE_FIXP cells (when VM available)

#### Task 1.2: Implement createcell68k Equivalent

**File**: [`zaiko/src/memory/storage.zig`](zaiko/src/memory/storage.zig) or [`zaiko/src/memory/storage_enhanced.zig`](zaiko/src/memory/storage_enhanced.zig)

**Function Signature**:
```zig
pub fn allocateFloatCell(storage: *Storage, gc: ?*GC) !LispPTR
```

**Implementation**:
```zig
pub fn allocateFloatCell(storage: *Storage, gc: ?*GC) !LispPTR {
    const float_size = @sizeOf(f32); // 4 bytes = 2 DLwords

    // Allocate from storage
    const cell_ptr = try storage.allocateRaw(float_size, gc);

    // Clear cell contents
    @memset(@as([*]u8, @ptrFromInt(cell_ptr)), 0, float_size);

    // Return LispPTR address
    return storage.offsetToLispPTR(cell_ptr);
}
```

**Dependencies**:
- Storage allocation (allocateRaw or equivalent)
- Offset to LispPTR conversion (offsetToLispPTR)

**Testing**:
- Test allocation succeeds
- Test cell is cleared to zero
- Test GC integration (if GC provided)

#### Task 1.3: Implement FP Error Handling

**File**: [`zaiko/src/utils/float_conversion.zig`](zaiko/src/utils/float_conversion.zig)

**Functions**:
```zig
pub fn fpClear() void {
    // No-op on modern systems (matches C FPCLEAR)
}

pub fn fpTest(result: f32) bool {
    // Check if result is finite (not NaN or infinity)
    return !std.math.isFinite(result);
}
```

**Testing**:
- Test fpClear() compiles and runs
- Test fpTest() with normal values (returns false)
- Test fpTest() with NaN (returns true)
- Test fpTest() with infinity (returns true)

### Phase 2: Core Floating Point Operations

#### Task 2.1: Implement FPLUS2

**File**: [`zaiko/src/vm/opcodes/floating_point.zig`](zaiko/src/vm/opcodes/floating_point.zig)

**Implementation**:
```zig
pub fn handleFPLUS2(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const float_conv = @import("../../utils/float_conversion.zig");

    // Pop two arguments from stack
    const parg2 = try stack_module.popStack(vm);
    const parg1 = try stack_module.popStack(vm);

    // Convert to float using makeFloat
    const arg1 = try float_conv.makeFloat(parg1, parg2);
    const arg2 = try float_conv.makeFloat(parg2, parg2);

    // Clear FP error status
    float_conv.fpClear();

    // Perform addition
    const result = arg1 + arg2;

    // Check for FP errors
    if (float_conv.fpTest(result)) {
        return errors.VMError.FloatingPointError;
    }

    // Allocate float cell and store result
    if (vm.storage) |storage| {
        const gc_module = @import("../../memory/gc.zig");
        const cell_ptr = try storage.allocateFloatCell(storage, vm.gc);
        const float_ptr: *f32 = @ptrFromInt(cell_ptr);
        float_ptr.* = result;

        // Push result onto stack
        try stack_module.pushStack(vm, storage.offsetToLispPTR(cell_ptr));
    } else {
        return errors.VMError.MemoryAccessFailed;
    }
}
```

**Testing**:
- Test with small positive integers
- Test with small negative integers
- Test with float cells (when available)
- Test overflow (should return error)
- Test NaN propagation (should return error)

#### Task 2.2: Implement FDIFFERENCE

**File**: [`zaiko/src/vm/opcodes/floating_point.zig`](zaiko/src/vm/opcodes/floating_point.zig)

**Implementation**: Similar to FPLUS2, but with subtraction:
```zig
const result = arg1 - arg2;
```

**Testing**: Same as FPLUS2

#### Task 2.3: Implement FTIMES2

**File**: [`zaiko/src/vm/opcodes/floating_point.zig`](zaiko/src/vm/opcodes/floating_point.zig)

**Implementation**: Similar to FPLUS2, but with multiplication:
```zig
const result = arg1 * arg2;
```

**Testing**: Same as FPLUS2, plus test for overflow

#### Task 2.4: Implement FQUOTIENT

**File**: [`zaiko/src/vm/opcodes/floating_point.zig`](zaiko/src/vm/opcodes/floating_point.zig)

**Implementation**: Similar to FPLUS2, but with division:
```zig
const result = arg1 / arg2;
```

**Testing**: Same as FPLUS2, plus test for division by zero

#### Task 2.5: Implement FGREATERP

**File**: [`zaiko/src/vm/opcodes/floating_point.zig`](zaiko/src/vm/opcodes/floating_point.zig)

**Implementation**:
```zig
pub fn handleFGREATERP(vm: *VM) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const float_conv = @import("../../utils/float_conversion.zig");
    const types = @import("../../utils/types.zig");

    // Pop two arguments from stack
    const parg2 = try stack_module.popStack(vm);
    const parg1 = try stack_module.popStack(vm);

    // Convert to float using makeFloat
    const arg1 = try float_conv.makeFloat(parg1, parg2);
    const arg2 = try float_conv.makeFloat(parg2, parg2);

    // Compare and return ATOM_T or NIL_PTR
    const result: LispPTR = if (arg1 > arg2) types.ATOM_T else types.NIL_PTR;

    // Push result onto stack
    try stack_module.pushStack(vm, result);
}
```

**Testing**:
- Test with arg1 > arg2 (returns ATOM_T)
- Test with arg1 <= arg2 (returns NIL_PTR)
- Test with equal values (returns NIL_PTR)

### Phase 3: Unboxed Float Operations

#### Task 3.1: Implement UBFLOAT1

**File**: [`zaiko/src/vm/opcodes/float_ops.zig`](zaiko/src/vm/opcodes/float_ops.zig)

**Implementation**:
```zig
pub fn handleUBFLOAT1(vm: *VM, arg: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const types = @import("../../utils/types.zig");

    switch (arg) {
        0 => { // BOX
            // Pop unboxed float from stack
            const unboxed_float = try stack_module.popStack(vm);

            // Allocate float cell
            if (vm.storage) |storage| {
                const gc_module = @import("../../memory/gc.zig");
                const cell_ptr = try storage.allocateFloatCell(storage, vm.gc);
                const float_ptr: *f32 = @ptrFromInt(cell_ptr);
                float_ptr.* = @as(f32, @bitCast(unboxed_float));

                // Push LispPTR onto stack
                try stack_module.pushStack(vm, storage.offsetToLispPTR(cell_ptr));
            } else {
                return errors.VMError.MemoryAccessFailed;
            }
        },
        1 => { // UNBOX
            // Pop float cell from stack
            const float_cell = try stack_module.popStack(vm);

            // Extract float value
            // TODO: Read float from memory when VM available
            // For now, return error
            return errors.VMError.NotHandled;
        },
        2 => { // ABS
            // Pop unboxed float from stack
            const unboxed_float = try stack_module.popStack(vm);

            // Clear sign bit (absolute value)
            const result = 0x7FFFFFFF & unboxed_float;

            // Push result onto stack
            try stack_module.pushStack(vm, result);
        },
        3 => { // NEGATE
            // Pop unboxed float from stack
            const unboxed_float = try stack_module.popStack(vm);

            // Flip sign bit (negate)
            const result = 0x80000000 ^ unboxed_float;

            // Push result onto stack
            try stack_module.pushStack(vm, result);
        },
        4 => { // UFIX
            // Pop unboxed float from stack
            const unboxed_float = try stack_module.popStack(vm);

            // Convert to int with bounds checking
            const temp = @as(f32, @bitCast(unboxed_float));
            const max_fixp: f32 = @floatFromInt(types.MAX_FIXP);
            const min_fixp: f32 = @floatFromInt(types.MIN_FIXP);

            if (temp > max_fixp or temp < min_fixp) {
                return errors.VMError.Overflow;
            }

            const val = @as(i32, @intFromFloat(temp));

            // Encode as SMALLP or FIXP
            const result = try types.encodeIntegerResult(val);

            // Push result onto stack
            try stack_module.pushStack(vm, result);
        },
        else => {
            return errors.VMError.InvalidOpcode;
        }
    }
}
```

**Testing**:
- Test BOX: Create float cell from unboxed float
- Test UNBOX: Extract float from float cell
- Test ABS: Clear sign bit
- Test NEGATE: Flip sign bit
- Test UFIX: Convert float to fixnum with bounds checking

#### Task 3.2: Implement UBFLOAT2

**File**: [`zaiko/src/vm/opcodes/float_ops.zig`](zaiko/src/vm/opcodes/float_ops.zig)

**Implementation**:
```zig
pub fn handleUBFLOAT2(vm: *VM, arg: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const types = @import("../../utils/types.zig");
    const float_conv = @import("../../utils/float_conversion.zig");

    // Pop two unboxed floats from stack
    const a2 = try stack_module.popStack(vm);
    const a1 = try stack_module.popStack(vm);

    // Convert to float (bit-cast from int)
    const arg1 = @as(f32, @bitCast(a1));
    const arg2 = @as(f32, @bitCast(a2));

    // Clear FP error status
    float_conv.fpClear();

    var ans: f32 = undefined;
    var result: LispPTR = undefined;

    switch (arg) {
        0 => { // ADD
            ans = arg1 + arg2;
            result = @as(LispPTR, @bitCast(ans));
        },
        1 => { // SUB
            ans = arg2 - arg1;
            result = @as(LispPTR, @bitCast(ans));
        },
        2 => { // ISUB
            ans = arg1 - arg2;
            result = @as(LispPTR, @bitCast(ans));
        },
        3 => { // MULT
            ans = arg1 * arg2;
            result = @as(LispPTR, @bitCast(ans));
        },
        4 => { // DIV
            ans = arg2 / arg1;
            result = @as(LispPTR, @bitCast(ans));
        },
        5 => { // GT
            result = if (arg2 > arg1) types.ATOM_T else types.NIL_PTR;
        },
        6 => { // MAX
            result = if (arg2 > arg1) a2 else a1;
        },
        7 => { // MIN
            result = if (arg2 > arg1) a1 else a2;
        },
        8 => { // REM
            ans = @rem(arg2, arg1);
            result = @as(LispPTR, @bitCast(ans));
        },
        9 => { // AREF
            // TODO: Implement array element access
            return errors.VMError.NotHandled;
        },
        else => {
            return errors.VMError.InvalidOpcode;
        }
    }

    // Check for FP errors (except for GT, MAX, MIN)
    if (arg < 5 or arg == 8) {
        if (float_conv.fpTest(ans)) {
            return errors.VMError.FloatingPointError;
        }
    }

    // Push result onto stack
    try stack_module.pushStack(vm, result);
}
```

**Testing**:
- Test ADD: arg1 + arg2
- Test SUB: arg2 - arg1
- Test ISUB: arg1 - arg2
- Test MULT: arg1 * arg2
- Test DIV: arg2 / arg1
- Test GT: Return ATOM_T if arg2 > arg1
- Test MAX: Return larger value
- Test MIN: Return smaller value
- Test REM: Remainder operation
- Test AREF: Array element access (when available)

#### Task 3.3: Implement UBFLOAT3

**File**: [`zaiko/src/vm/opcodes/float_ops.zig`](zaiko/src/vm/opcodes/float_ops.zig)

**Implementation**:
```zig
pub fn handleUBFLOAT3(vm: *VM, arg: u8) errors.VMError!void {
    const stack_module = @import("../stack.zig");
    const types = @import("../../utils/types.zig");
    const float_conv = @import("../../utils/float_conversion.zig");

    // Pop arguments from stack
    // arg3 is passed as parameter (unboxed float)
    // arg2 is coefficient array pointer
    // arg1 is degree (must be S_POSITIVE)

    const arg2 = try stack_module.popStack(vm);
    const arg1 = try stack_module.popStack(vm);

    // Extract degree from arg1 (must be S_POSITIVE)
    if ((arg1 & types.SEGMASK) != types.S_POSITIVE) {
        return errors.VMError.InvalidArgument;
    }
    const degree = 0xFFFF & arg1;

    // Extract value from arg3 (bit-cast from int to float)
    const val = @as(f32, @bitCast(@as(i32, arg)));

    // Clear FP error status
    float_conv.fpClear();

    // Get coefficient array pointer
    // TODO: Read float values from memory when VM available
    // For now, return error
    _ = arg2; // Suppress unused warning
    _ = degree; // Suppress unused warning
    _ = val; // Suppress unused warning

    return errors.VMError.NotHandled;

    // Full implementation (when VM available):
    // var ans: f32 = undefined;
    // var fptr: [*]f32 = undefined; // Get from arg2
    // ans = fptr[0];
    // var i: usize = 0;
    // while (i < degree) : (i += 1) {
    //     ans = (ans * val) + fptr[i + 1];
    // }
    //
    // if (float_conv.fpTest(ans)) {
    //     return errors.VMError.FloatingPointError;
    // }
    //
    // const result = @as(LispPTR, @bitCast(ans));
    // try stack_module.pushStack(vm, result);
}
```

**Testing**:
- Test polynomial evaluation with various degrees
- Test with zero degree
- Test with negative coefficients
- Test overflow (should return error)

### Phase 4: Testing and Validation

#### Task 4.1: Unit Tests

**File**: [`zaiko/tests/float_operations_test.zig`](zaiko/tests/float_operations_test.zig)

**Test Cases**:
1. **makeFloat tests**:
   - Test S_POSITIVE conversion
   - Test S_NEGATIVE conversion
   - Test TYPE_FLOATP conversion (when VM available)
   - Test TYPE_FIXP conversion (when VM available)
   - Test invalid type (should return error)

2. **FPLUS2 tests**:
   - Test small positive integers
   - Test small negative integers
   - Test mixed positive/negative
   - Test overflow (should return error)
   - Test NaN propagation (should return error)

3. **FDIFFERENCE tests**:
   - Same as FPLUS2

4. **FTIMES2 tests**:
   - Same as FPLUS2, plus test for overflow

5. **FQUOTIENT tests**:
   - Same as FPLUS2, plus test for division by zero

6. **FGREATERP tests**:
   - Test arg1 > arg2 (returns ATOM_T)
   - Test arg1 <= arg2 (returns NIL_PTR)
   - Test equal values (returns NIL_PTR)

7. **UBFLOAT1 tests**:
   - Test BOX: Create float cell from unboxed float
   - Test UNBOX: Extract float from float cell
   - Test ABS: Clear sign bit
   - Test NEGATE: Flip sign bit
   - Test UFIX: Convert float to fixnum with bounds checking

8. **UBFLOAT2 tests**:
   - Test ADD, SUB, ISUB, MULT, DIV
   - Test GT, MAX, MIN
   - Test REM
   - Test AREF (when available)

9. **UBFLOAT3 tests**:
   - Test polynomial evaluation with various degrees
   - Test with zero degree
   - Test with negative coefficients
   - Test overflow (should return error)

#### Task 4.2: Parity Tests

**Script**: [`scripts/test_float_parity.sh`](scripts/test_float_parity.sh)

**Test Cases**:
1. Run C emulator with floating point test cases
2. Run Zig emulator with same test cases
3. Compare outputs
4. Report any divergences

**Test Cases**:
- Basic arithmetic (addition, subtraction, multiplication, division)
- Comparison operations
- Unboxed operations
- Polynomial evaluation
- Edge cases (overflow, NaN, infinity)

#### Task 4.3: Integration Tests

**Test Cases**:
1. Run full sysout with floating point operations
2. Compare execution traces with C emulator
3. Verify no regressions in existing functionality

### Phase 5: Documentation

#### Task 5.1: Update Specifications

**File**: [`documentation/specifications/instruction-set/floating-point-operations.typ`](documentation/specifications/instruction-set/floating-point-operations.typ)

**Content**:
- Floating point number representation
- Type conversion rules (SMALLP, FIXP, FLOATP)
- Operation semantics for each opcode
- Error handling (NaN, infinity, overflow)
- Implementation notes for Zig

#### Task 5.2: Update Implementation Notes

**File**: [`documentation/implementations/zig-floating-point-implementation.typ`](documentation/implementations/zig-floating-point-implementation.typ)

**Content**:
- Implementation approach
- Key design decisions
- Performance considerations
- Testing strategy
- Known limitations

## Dependencies

### Required Components

1. **Virtual Memory Access**: Required for reading TYPE_FLOATP and TYPE_FIXP cells
   - Status: Partially implemented
   - File: [`zaiko/src/memory/virtual.zig`](zaiko/src/memory/virtual.zig)

2. **Type Tag Checking**: Required for GetTypeNumber equivalent
   - Status: Not implemented
   - Need to implement type tag extraction from LispPTR

3. **Storage Allocation**: Required for allocateFloatCell
   - Status: Implemented
   - File: [`zaiko/src/memory/storage.zig`](zaiko/src/memory/storage.zig)

4. **GC Integration**: Required for allocation tracking
   - Status: Implemented
   - File: [`zaiko/src/memory/gc.zig`](zaiko/src/memory/gc.zig)

### Optional Components

1. **NativeAligned4FromLAddr**: Required for reading float values from memory
   - Status: Not implemented
   - Need to implement address translation

2. **LAddrFromNative**: Required for converting native pointers to LispPTR
   - Status: Partially implemented
   - File: [`zaiko/src/memory/storage.zig`](zaiko/src/memory/storage.zig)

## Risk Assessment

### High Risk

1. **Virtual Memory Access**: Not fully implemented
   - **Mitigation**: Implement stub that returns error for TYPE_FLOATP and TYPE_FIXP
   - **Timeline**: Can be implemented in parallel with floating point operations

2. **Type Tag Checking**: Not implemented
   - **Mitigation**: Implement simple type tag extraction
   - **Timeline**: Can be implemented quickly

### Medium Risk

1. **Performance**: Floating point operations may be slower than C
   - **Mitigation**: Profile and optimize hot paths
   - **Timeline**: Post-implementation optimization

2. **Testing**: Comprehensive testing required
   - **Mitigation**: Start with unit tests, then parity tests
   - **Timeline**: Ongoing throughout implementation

### Low Risk

1. **Documentation**: May be incomplete
   - **Mitigation**: Update documentation as implementation progresses
   - **Timeline**: Post-implementation

## Timeline

### Week 1: Foundation Components
- Day 1-2: Implement makeFloat
- Day 3-4: Implement allocateFloatCell
- Day 5: Implement FP error handling

### Week 2: Core Floating Point Operations
- Day 1-2: Implement FPLUS2
- Day 3-4: Implement FDIFFERENCE, FTIMES2
- Day 5: Implement FQUOTIENT, FGREATERP

### Week 3: Unboxed Float Operations
- Day 1-2: Implement UBFLOAT1
- Day 3-4: Implement UBFLOAT2
- Day 5: Implement UBFLOAT3

### Week 4: Testing and Validation
- Day 1-2: Unit tests
- Day 3-4: Parity tests
- Day 5: Integration tests

### Week 5: Documentation and Polish
- Day 1-2: Update specifications
- Day 3-4: Update implementation notes
- Day 5: Final review and cleanup

## Success Criteria

1. **Functional Completeness**: All floating point opcodes implemented
2. **Parity**: Zig emulator produces same results as C emulator for all floating point operations
3. **Test Coverage**: Unit tests for all operations, parity tests for critical paths
4. **Documentation**: Specifications and implementation notes updated
5. **Performance**: No significant performance regression compared to C

## Next Steps

1. Review and approve this plan
2. Implement foundation components (makeFloat, allocateFloatCell, FP error handling)
3. Implement core floating point operations (FPLUS2, FDIFFERENCE, FTIMES2, FQUOTIENT, FGREATERP)
4. Implement unboxed float operations (UBFLOAT1, UBFLOAT2, UBFLOAT3)
5. Add comprehensive tests
6. Update documentation
7. Run parity tests against C emulator

## References

- [`AGENTS.md`](AGENTS.md) - Project guidelines
- [`maiko/src/fp.c`](maiko/src/fp.c) - C floating point implementation
- [`maiko/src/ubf1.c`](maiko/src/ubf1.c) - C UBFLOAT1 implementation
- [`maiko/src/ubf2.c`](maiko/src/ubf2.c) - C UBFLOAT2 implementation
- [`maiko/src/ubf3.c`](maiko/src/ubf3.c) - C UBFLOAT3 implementation
- [`maiko/inc/my.h`](maiko/inc/my.h) - N_MakeFloat macro
- [`maiko/inc/medleyfp.h`](maiko/inc/medleyfp.h) - FP error handling
- [`maiko/src/mkcell.c`](maiko/src/mkcell.c) - Cell allocation
- [`zaiko/src/vm/opcodes/float_ops.zig`](zaiko/src/vm/opcodes/float_ops.zig) - Zig float ops (incomplete)
- [`zaiko/src/vm/opcodes/floating_point.zig`](zaiko/src/vm/opcodes/floating_point.zig) - Zig floating point (placeholder)
- [`zaiko/src/utils/types.zig`](zaiko/src/utils/types.zig) - Type definitions
- [`zaiko/src/memory/storage.zig`](zaiko/src/memory/storage.zig) - Storage allocation
- [`zaiko/src/memory/gc.zig`](zaiko/src/memory/gc.zig) - Garbage collection
