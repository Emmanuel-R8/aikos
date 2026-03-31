# TOPOFSTACK Synchronization Bug Analysis

**Date**: 2026-01-22  
**Priority**: CRITICAL  
**Confidence**: High  
**Status**: Documented, Awaiting Fix

## Executive Summary

A critical TOPOFSTACK synchronization bug has been identified in the UNBIND macro and related functions. This bug causes stale TOPOFSTACK values after binding operations, leading to incorrect stack state, wrong argument passing, and emulator divergence.

## Problem Description

### Root Cause
The UNBIND macro in `maiko/inc/inlineC.h` walks the stack backwards to process binding markers but fails to synchronize TOPOFSTACK with the new stack state.

### Impact
- Stale TOPOFSTACK values in subsequent opcodes
- Incorrect argument passing to functions
- Stack corruption in complex binding scenarios  
- Emulator divergence from reference implementation

## Affected Code Locations

### Primary: UNBIND Macro
**File**: `maiko/inc/inlineC.h` (lines 575-594)
```c
#define UNBIND                                                  \
  do {                                                          \
    int num;                                                    \
    LispPTR *ppvar;                                             \
    int i;                                                      \
    LispPTR value;                                              \
    /* Phase 1: Find BIND marker by walking backwards through stack */ \
    for (; (((int)*--CSTKPTRL) >= 0););                         \
    /* Phase 2: Extract binding info from marker */ \
    value = *CSTKPTR;                                           \
    num = (~value) >> 16;                                       \
    /* Phase 3: Calculate PVAR pointer for variable clearing */ \
    ppvar = (LispPTR *)((DLword *)PVAR + 2 + GetLoWord(value)); \
    /* Phase 4: Clear bound variables to unbound state */ \
    for (i = num; --i >= 0;) {                                  \
      *--ppvar = 0xffffffff;                                    \
    }                                                           \
    nextop1;                                                    \
  } while (0)
```

**Problem**: No TOPOFSTACK synchronization after stack manipulation.

### Secondary: simulate_unbind Function
**File**: `maiko/src/mvs.c` (lines 267-290)
```c
void simulate_unbind(FX2 *frame, int unbind_count, FX2 *returner) {
  // ... stack walking and variable clearing logic ...
  // Missing: TOPOFSTACK synchronization
}
```

**Problem**: Same missing synchronization as UNBIND macro.

## Correct Implementation Pattern

### Reference Implementation: RET Macro
**File**: `maiko/inc/tos1defs.h` (lines 83-88)
```c
#define RET                  \
  do {                       \
    pccache = PC + 1;        \
    StackPtrRestore;         /* CRITICAL: Restores CSTKPTRL from CurrentStackPTR */ \
    TOPOFSTACK = TopOfStack; /* CRITICAL: Syncs TOPOFSTACK from memory */ \
  } while (0)
```

### Key Components
1. **StackPtrRestore**: `CSTKPTRL = (void *)(CurrentStackPTR + 2);`
2. **TopOfStack**: `(MachineState.tosvalue)` - reads from memory
3. **Synchronization**: Ensures both pointer and value are current

## Incorrect Workaround in Zig Implementation

**File**: `zaiko/src/vm/opcodes/binding.zig:192`
```zig
// PHASE 5: Restore environment (TOPOFSTACK)
// TODO: Currently hardcoded to 0x140000 based on C trace analysis
vm_obj.top_of_stack = 0x140000;
```

**Problems**:
- Hardcoded value only works for specific test cases
- Will break with different sysout files
- Not a general solution

## Recommended Fix

### Option 1: Follow RET Pattern (Recommended)
```c
#define UNBIND                                                  \
  do {                                                          \
    int num;                                                    \
    LispPTR *ppvar;                                             \
    int i;                                                      \
    LispPTR value;                                              \
    /* Phase 1: Find BIND marker by walking backwards through stack */ \
    for (; (((int)*--CSTKPTRL) >= 0););                         \
    /* Phase 2: Extract binding info from marker */ \
    value = *CSTKPTR;                                           \
    num = (~value) >> 16;                                       \
    /* Phase 3: Calculate PVAR pointer for variable clearing */ \
    ppvar = (LispPTR *)((DLword *)PVAR + 2 + GetLoWord(value)); \
    /* Phase 4: Clear bound variables to unbound state */ \
    for (i = num; --i >= 0;) {                                  \
      *--ppvar = 0xffffffff;                                    \
    }                                                           \
    /* Phase 5: CRITICAL - Synchronize TOPOFSTACK */ \
    StackPtrRestore;           /* Ensure CSTKPTRL is correct */ \
    TOPOFSTACK = TopOfStack;   /* Read actual top value from memory */ \
    nextop1;                                                    \
  } while (0)
```

### Option 2: Direct Memory Read
```c
    /* Phase 5: CRITICAL - Synchronize TOPOFSTACK */
    TOPOFSTACK = *(CSTKPTRL - 1);  /* Read current top from memory */
```

## Cross-References

### Critical Definitions
- `StackPtrRestore`: `maiko/inc/tos1defs.h:73-74`
- `TopOfStack`: `maiko/inc/lispemul.h:315`
- `TOPOFSTACK`: `maiko/inc/tos1defs.h:40,63`

### Related Functions
- `RET` macro: `maiko/inc/tos1defs.h:83-88` (correct pattern)
- `simulate_unbind()`: `maiko/src/mvs.c:267-290` (same bug)

## Test Cases for Validation

### Basic UNBIND Test
1. Setup: Create variable bindings
2. Execute: UNBIND operation  
3. Verify: TOPOFSTACK reflects new stack state
4. Test: Subsequent operations use correct TOPOFSTACK

### Multiple UNBIND Test
1. Setup: Create nested variable bindings
2. Execute: Multiple UNBIND operations
3. Verify: TOPOFSTACK correctly updated after each
4. Test: Stack consistency maintained

### Integration Test
1. Setup: Function call with bindings
2. Execute: UNBIND followed by function call
3. Verify: Arguments passed correctly
4. Test: No stack corruption

## Implementation Strategy

1. **Phase 1**: Fix UNBIND macro with Option 1 (RET pattern)
2. **Phase 2**: Fix simulate_unbind function
3. **Phase 3**: Update Zig implementation to remove hardcoded value
4. **Phase 4**: Add comprehensive test cases
5. **Phase 5**: Validate parity with C emulator

## Risk Assessment

### High Risk
- Stack corruption in complex scenarios
- Incorrect argument passing to functions
- Emulator divergence from reference

### Mitigation
- Comprehensive testing before commit
- Parity validation against C traces
- Rollback plan with backup files created

## Documentation Updates Required

1. Update `documentation/specifications/instruction-set/opcodes.typ`
2. Update `documentation/implementations/zig-implementation.typ`
3. Add debugging technique to `documentation/CRITICAL_DEBUGGING_TECHNIQUE.typ`

## Files Modified (Documentation Only)

- `maiko/inc/inlineC.h`: Added comprehensive bug documentation
- `maiko/inc/tos1defs.h`: Added correct pattern documentation
- `maiko/src/mvs.c`: Added simulate_unbind bug documentation

## Next Steps

1. Implement UNBIND fix using Option 1 pattern
2. Update Zig implementation to remove hardcoded value
3. Add test cases to prevent regression
4. Update relevant documentation
5. Validate end-to-end execution

---

**Last Updated**: 2026-01-22  
**Status**: Critical bug documented, implementation pending
