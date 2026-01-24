# C Emulator Code Refactoring

**Date**: 2025-01-13  
**Purpose**: Extract tracing code from `xc.c` to reduce file size and improve maintainability

## Overview

The `xc.c` file had grown to 2,705 lines, with approximately 500-800 lines of AUTO tracing code embedded inline within opcode handlers. This refactoring extracts all tracing functionality into separate files to improve code organization and reduce token usage.

## Refactoring Details

### Files Created

1. **`maiko/inc/xc_tracing.h`** (38 lines)
   - Header file with function declarations for all tracing functions
   - All functions marked with `AUTO:` prefix in comments

2. **`maiko/src/xc_tracing.c`** (531 lines)
   - Implementation file containing all tracing function implementations
   - Includes 8 tracing functions:
     - `trace_unbind()` - UNBIND opcode tracing
     - `trace_gvar()` - GVAR opcode tracing
     - `trace_copy()` - COPY opcode tracing
     - `trace_const1()` - CONST_1 opcode tracing
     - `trace_fjump7()` - FJUMP7 opcode tracing
     - `trace_tjump1()` - TJUMP1 opcode tracing
     - `trace_getbaseptr_n()` - GETBASEPTR_N opcode tracing
     - `trace_eq()` - EQ opcode tracing

### Files Modified

1. **`maiko/src/xc.c`**
   - Reduced from 2,705 to 2,201 lines (504 lines removed, 18.6% reduction)
   - Added `#include "xc_tracing.h"`
   - Replaced inline tracing blocks with function calls
   - All tracing code removed, replaced with single-line function calls

2. **`maiko/CMakeLists.txt`**
   - Added `src/xc_tracing.c` to source files list
   - Added `inc/xc_tracing.h` to header files list

## Impact

### Benefits

- **Reduced file size**: Main dispatch file reduced by 504 lines
- **Better organization**: Tracing code isolated and modular
- **Easier maintenance**: Tracing can be modified/removed without touching main dispatch loop
- **Lower token usage**: Reading `xc.c` uses significantly fewer tokens
- **Preserved functionality**: All tracing behavior unchanged

### File Size Comparison

- **Before**: `xc.c` = 2,705 lines
- **After**: 
  - `xc.c` = 2,201 lines
  - `xc_tracing.c` = 531 lines
  - `xc_tracing.h` = 38 lines
  - **Total**: 2,770 lines (slight increase due to function overhead, but main file reduced)

## Implementation Notes

### Function Signatures

All tracing functions accept necessary parameters to avoid accessing local variables/macros:

- `trace_unbind()` - No parameters (uses extern globals)
- `trace_gvar(InstPtr PC, InstPtr pccache)` - PC and pccache passed
- `trace_copy(InstPtr PC, LispPTR tos)` - PC and TopOfStack passed
- `trace_const1(InstPtr PC, LispPTR tos)` - PC and TopOfStack passed
- `trace_fjump7(InstPtr PC, LispPTR tos_before)` - PC and TOS before jump
- `trace_tjump1(InstPtr PC, LispPTR tos_before)` - PC and TOS before jump
- `trace_getbaseptr_n(InstPtr PC, InstPtr pccache, unsigned char operand_byte, LispPTR tos)` - All needed values passed
- `trace_eq(InstPtr PC, LispPTR tos_before, LispPTR pop_tos_1)` - PC and both stack values

### External Variables

The tracing functions access these extern variables:
- `Lisp_world` - Base of virtual memory
- `CurrentStackPTR` - Current stack pointer
- `Stackspace` - Stack base
- `PVAR` - Parameter variable pointer
- `global_debug_instruction_count` - Instruction counter

### Static Flags

Each tracing function uses a static flag to ensure it only traces once:
- `static int unbind_traced = 0;`
- `static int gvar_traced = 0;`
- etc.

This preserves the original behavior of tracing only the first occurrence of each opcode.

## Verification

All 8 tracing functions are correctly called exactly once each in `xc.c`:
- `trace_unbind()` - Called in case 022 (UNBIND)
- `trace_gvar()` - Called in case 0140 (GVAR)
- `trace_copy()` - Called in case 0144 (COPY)
- `trace_const1()` - Called in case 0153 (CONST_1)
- `trace_fjump7()` - Called in case 0225 (FJUMP7)
- `trace_tjump1()` - Called in case 0241 (TJUMP1)
- `trace_getbaseptr_n()` - Called in case 0311 (GETBASEPTR_N)
- `trace_eq()` - Called in case 0360 (EQ)

## Future Considerations

- Tracing code can be easily removed by deleting `xc_tracing.c` and removing function calls
- New tracing functions can be added to `xc_tracing.c` without modifying `xc.c`
- Tracing can be conditionally compiled using preprocessor directives if needed
- All tracing code is marked with `AUTO:` prefix for easy identification

## Related Documentation

- See individual opcode tracing documents:
  - `c-emulator-gvar-tracing.typ`
  - `c-emulator-unbind-tracing.typ`
  - `c-emulator-getbaseptr-tracing.typ`
  - `c-emulator-copy-tracing.typ`
  - `c-emulator-tjump1-tracing.typ`
  - `c-emulator-const1-tracing.typ`
  - `c-emulator-eq-tracing.typ`
  - `c-emulator-fjump7-tracing.typ`
