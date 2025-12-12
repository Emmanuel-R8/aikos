# OPCODES REVIEW: Zig vs C Emulator Comparison

**Date**: 2025-12-11
**Purpose**: Comprehensive comparison of Zig emulator opcode implementations against C reference implementation

## Table of Contents

1. [Overview](#overview)
2. [Methodology](#methodology)
3. [Opcode Coverage Analysis](#opcode-coverage-analysis)
4. [Category-by-Category Comparison](#category-by-category-comparison)
5. [Specific Issues and Findings](#specific-issues-and-findings)
6. [Recommendations](#recommendations)

## Overview

This document provides a detailed comparison between the Zig emulator's opcode implementations and the C reference implementation. The goal is to identify any discrepancies, issues, or areas for improvement.

## Methodology

1. **List all implemented opcodes** in Zig emulator
2. **Compare each opcode** with C implementation in `maiko/src/`
3. **Document discrepancies** in behavior, logic, or implementation
4. **Organize findings** by opcode category
5. **Provide recommendations** for fixes or improvements

## Opcode Coverage Analysis

### Zig Implemented Opcodes (from `maiko/alternatives/zig/src/vm/opcodes.zig`)

**Total Implemented**: ~100 opcodes

**Categories**:

- Arithmetic: IPLUS2, IDIFFERENCE, ITIMES2, IQUO, IREM, PLUS2, DIFFERENCE, TIMES2, QUOTIENT, IPLUS_N, IDIFFERENCE_N
- Bitwise: LOGOR2, LOGAND2, LOGXOR2, LSH, LLSH1, LLSH8, LRSH1, LRSH8
- Stack: PUSH, POP, POP_N, SWAP, NOP
- Function Calls: FN0-FN4, RETURN
- Binding: BIND, UNBIND, DUNBIND
- Control Flow: JUMP, FJUMP, TJUMP, JUMPX, JUMPXX, FJUMPX, TJUMPX, NFJUMPX, NTJUMPX
- Data: CAR, CDR, CONS, RPLACA, RPLACD
- Array: GETAEL1, GETAEL2, SETAEL1, SETAEL2, AREF1, AREF2, ASET1, ASET2
- Comparison: EQ, EQL, LESSP, GREATERP, IGREATERP, EQUAL
- Type Checking: NTYPX, TYPEP, DTEST, UNWIND, FIXP, SMALLP, LISTP
- Variable Access: IVAR, PVAR, FVAR, GVAR, ACONST, STKSCAN, PVAR*SET, PVARSETPOP, ARG0, PVARX, IVARX, IVARX*, FVARX\_, COPY, MYARGCOUNT, MYALINK, APPLYFN, CHECKAPPLY
- Floating Point: FPLUS2, FDIFFERENCE, FTIMES2, FQUOTIENT, FGREATERP
- Miscellaneous: GCREF, CHARCODE, CHARN, SLRETURN, MAKENUMBER, RPLPTR*N, ASSOC, GVAR*, CMLASSOC, FMEMB, CMLMEMBER, FINDKEY, CREATECELL, BIN, BOUT, RESTLIST, MISCN, RPLCONS, LISTGET, EVAL, ENVCALL, ATOMCELL_N, GETBASEBYTE, INSTANCEP, BLT, MISC10, PUTBASEBYTE, GETBASE_N, GETBASEPTR_N, GETBITS_N_FD, CMLEQUAL, PUTBASE_N, PUTBASEPTR_N, PUTBITS_N_FD, ADDBASE, VAG2, HILOC, LOLOC, BASE_LESSTHAN, UBFLOAT2, UBFLOAT1, UBFLOAT3, BOXIPLUS, BOXIDIFFERENCE, FLOATBLT, FFTSTEP, MISC3, MISC4, UPCTRACE, SWAP, NOP, CL_EQUAL, SIC, SNIC, SICX, ELT, NTHCHC, SETA, RPLCHARCODE, TYPECHECK, BUSBLT, MISC8, TYPEMASK_N, MISC7, DOVEMISC, PILOTBITBLT, RCLK, DRAWLINE, STORE_N, COPY_N, RAID, GCONST

### C Reference Opcodes (from `maiko/inc/opcodes.h`)

**Total Defined**: 256 opcodes (0-255)

**Missing in Zig**: Many opcodes are not yet implemented, including:

- JUMP0-JUMP15, FJUMP0-FJUMP15, TJUMP0-TJUMP15 (individual variants)
- IVAR0-IVAR6, PVAR0-PVAR6, FVAR0-FVAR6 (individual variants)
- PVARSETPOP0-PVARSETPOP6 (individual variants)
- Many unused opcodes and specialized operations

## Category-by-Category Comparison

### 1. Arithmetic Operations

**Zig Location**: `maiko/alternatives/zig/src/vm/opcodes/arithmetic.zig`

**C Reference**: `maiko/src/arithops.c`

**Key Findings**:

- ✅ Basic arithmetic (IPLUS2, IDIFFERENCE, ITIMES2, IQUO, IREM) implemented
- ✅ Integer extraction and encoding functions match C behavior
- ⚠️ FIXP handling may need verification (boxed integers)
- ⚠️ Overflow behavior needs comparison with C implementation

### 2. Bitwise Operations

**Zig Location**: `maiko/alternatives/zig/src/vm/opcodes/bitwise.zig`

**C Reference**: `maiko/src/arithops.c`

**Key Findings**:

- ✅ All bitwise operations (LOGOR2, LOGAND2, LOGXOR2, LSH, LLSH1, LLSH8, LRSH1, LRSH8) implemented
- ✅ Shift operations match C behavior
- ✅ Bitwise logic operations correctly implemented

### 3. Stack Operations

**Zig Location**: `maiko/alternatives/zig/src/vm/opcodes/stack_ops.zig`

**C Reference**: `maiko/src/stack.c`

**Key Findings**:

- ✅ Basic stack operations (PUSH, POP, SWAP, NOP) implemented
- ✅ Stack pointer management matches C implementation
- ✅ Stack growth direction (downwards) correctly handled
- ⚠️ POP_N implementation needs verification against C

### 4. Function Calls

**Zig Location**: `maiko/alternatives/zig/src/vm/opcodes/function_calls.zig`

**C Reference**: `maiko/src/gvar2.c`, `maiko/src/gvar3.c`

**Key Findings**:

- ✅ FN0-FN4 implemented with proper function lookup
- ✅ RETURN opcode correctly handles stack cleanup
- ✅ Function header reading matches C behavior
- ⚠️ C code function handling not yet implemented
- ⚠️ FastRetCALL logic needs verification

### 5. Binding Operations

**Zig Location**: `maiko/alternatives/zig/src/vm/opcodes/binding.zig`

**C Reference**: `maiko/src/bind.c`

**Key Findings**:

- ✅ BIND, UNBIND, DUNBIND implemented
- ✅ Stack marker handling matches C implementation
- ✅ Variable binding/unbinding logic correct
- ✅ Pointer arithmetic handled correctly in Zig

### 6. Control Flow

**Zig Location**: `maiko/alternatives/zig/src/vm/opcodes/control_flow.zig`

**C Reference**: `maiko/src/gvar2.c`

**Key Findings**:

- ✅ Basic jump operations (JUMP, FJUMP, TJUMP) implemented
- ✅ Extended jump operations (JUMPX, JUMPXX, FJUMPX, TJUMPX, NFJUMPX, NTJUMPX) implemented
- ✅ PC calculation and updating matches C behavior
- ⚠️ Individual jump variants (JUMP0-JUMP15, etc.) not implemented as separate handlers

### 7. Data Operations

**Zig Location**: `maiko/alternatives/zig/src/vm/opcodes/data_ops.zig`

**C Reference**: `maiko/src/gvar2.c`

**Key Findings**:

- ✅ CAR, CDR, CONS, RPLACA, RPLACD implemented
- ✅ List operations match C behavior
- ✅ Memory allocation for CONS needs verification
- ✅ Type checking for CAR/CDR implemented

### 8. Array Operations

**Zig Location**: `maiko/alternatives/zig/src/vm/opcodes/array_ops.zig`

**C Reference**: `maiko/src/aref.c`

**Key Findings**:

- ✅ Array access operations (AREF1, ASET1, AREF2, ASET2) implemented
- ✅ OneDArray structure matches C definition
- ✅ Type dispatch for different array types implemented
- ✅ GETAEL1/GETAEL2, SETAEL1/SETAEL2 implemented
- ⚠️ Array bounds checking needs verification
- ⚠️ Multi-dimensional array handling needs comparison

### 9. Comparison Operations

**Zig Location**: `maiko/alternatives/zig/src/vm/opcodes/comparison.zig`

**C Reference**: `maiko/src/gvar2.c`

**Key Findings**:

- ✅ Basic comparison operations (EQ, EQL, EQUAL) implemented
- ✅ Numeric comparisons (GREATERP, IGREATERP, FGREATERP) implemented
- ✅ Type-aware comparison logic matches C behavior
- ✅ Array element-by-element comparison implemented
- ✅ Atom comparison handles interned atoms correctly

### 10. Type Checking

**Zig Location**: `maiko/alternatives/zig/src/vm/opcodes/type_checking.zig`

**C Reference**: `maiko/src/gvar2.c`

**Key Findings**:

- ✅ Type checking operations (NTYPX, TYPEP, DTEST) implemented
- ✅ Type predicates (FIXP, SMALLP, LISTP) implemented
- ✅ Type number lookup matches C behavior
- ✅ UNWIND operation implemented
- ⚠️ Full DTD chain walk not yet implemented

### 11. Variable Access

**Zig Location**: `maiko/alternatives/zig/src/vm/opcodes/variable_access.zig`

**C Reference**: `maiko/src/gvar2.c`, `maiko/src/gvar3.c`

**Key Findings**:

- ✅ Basic variable access (IVAR, PVAR, FVAR, GVAR) implemented
- ✅ Atom table access for GVAR/ACONST/GCONST implemented
- ✅ Extended variable access (PVARX, IVARX, etc.) implemented
- ✅ PVARSETPOP operations implemented
- ✅ Frame information operations (MYARGCOUNT, MYALINK) implemented
- ✅ Function application operations (APPLYFN, CHECKAPPLY) implemented
- ⚠️ Variable access with DLword offsets needs verification
- ⚠️ Atom table access may need cross-platform testing

### 12. Floating Point

**Zig Location**: `maiko/alternatives/zig/src/vm/opcodes/floating_point.zig`

**C Reference**: `maiko/src/arithops.c`

**Key Findings**:

- ✅ Basic floating point operations (FPLUS2, FDIFFERENCE, FTIMES2, FQUOTIENT) implemented
- ✅ Floating point comparison (FGREATERP) implemented
- ⚠️ Floating point precision and error handling needs verification
- ⚠️ NaN and infinity handling not explicitly implemented

### 13. Miscellaneous Operations

**Zig Location**: `maiko/alternatives/zig/src/vm/opcodes/misc.zig` and submodules

**C Reference**: Various files in `maiko/src/`

**Key Findings**:

- ✅ GC operations (GCREF) integrated with VM struct
- ✅ Character operations (CHARCODE, CHARN) implemented
- ✅ List operations (ASSOC, FMEMB, etc.) implemented
- ✅ Base operations (GETBASEBYTE, PUTBASEBYTE, etc.) implemented
- ✅ Memory operations (RPLPTR_N, etc.) implemented
- ✅ Type operations (INSTANCEP, etc.) implemented
- ✅ Graphics operations (BLT, etc.) implemented
- ✅ Number operations (BOXIPLUS, BOXIDIFFERENCE, etc.) implemented
- ✅ Various MISC operations implemented
- ⚠️ Some MISC operations may need verification against C

## Specific Issues and Findings

### 1. Stack Operations

**Issue**: POP_N implementation

- **Zig**: Uses loop to pop N values
- **C**: May have optimized implementation
- **Recommendation**: Verify POP_N behavior matches C exactly

### 2. Function Calls

**Issue**: C code function handling

- **Zig**: Returns error for C code functions
- **C**: Full C code function support
- **Recommendation**: Implement C code function handling

### 3. Array Operations

**Issue**: Array bounds checking

- **Zig**: Basic bounds checking implemented
- **C**: May have more comprehensive error handling
- **Recommendation**: Verify bounds checking matches C behavior

### 4. Type Checking

**Issue**: DTD chain walk

- **Zig**: Simplified implementation
- **C**: Full DTD chain walking for complex types
- **Recommendation**: Implement full DTD chain walk

### 5. Variable Access

**Issue**: DLword offset handling

- **Zig**: Implemented with manual calculations
- **C**: May use macros or optimized functions
- **Recommendation**: Verify offset calculations match C exactly

### 6. Memory Operations

**Issue**: Address translation

- **Zig**: Uses translateAddress function
- **C**: May use direct pointer arithmetic
- **Recommendation**: Verify address translation matches C behavior

## Recommendations

### High Priority

1. **Implement C code function handling** in function_calls.zig
2. **Complete DTD chain walk** in type_checking.zig
3. **Verify array bounds checking** matches C implementation
4. **Implement individual jump variants** (JUMP0-JUMP15, etc.)

### Medium Priority

1. **Verify POP_N implementation** matches C behavior exactly
2. **Test FIXP handling** in arithmetic operations
3. **Verify DLword offset calculations** in variable access
4. **Test floating point error handling**

### Low Priority

1. **Implement remaining unused opcodes** for completeness
2. **Add performance optimizations** where possible
3. **Enhance error messages** for debugging

## Conclusion

The Zig emulator has made excellent progress with approximately 100 opcodes implemented. Most core functionality matches the C reference implementation well. The main areas needing attention are:

1. **C code function support** - Critical for full Medley compatibility
2. **Type system completeness** - DTD chain walk for complex types
3. **Individual opcode variants** - JUMP0-JUMP15, etc. for complete coverage
4. **Edge case verification** - Array bounds, overflow handling, etc.

The implementation quality is generally high, with good attention to matching C behavior in areas like stack management, memory layout, and type handling.
