# Laiko Opcode 0x60 (GVAR) Error Analysis

**Date**: 2026-03-01
**Status**: Analyzed - Root Cause Identified

## Error

```
Error in opcode 0x60: The function LAIKO.DATA:READ-ATOM-VALUE is undefined
```

## Analysis

### What Opcode 0x60 Should Do

- **C Implementation (Maiko)**: Opcode 0x60 = GVAR (opc_GVAR = 96), which fetches the value of a global variable by reading from the atom's value cell
- **Laiko**: Opcode 0x60 correctly defined as GVAR in `laiko/src/vm/op-variable.lisp` line 191

### READ-ATOM-VALUE Function Status

- **Defined in**: `laiko/src/data/atom.lisp` line 113
- **Exported from**: `laiko.data` package (`package.lisp` line 247)
- **Used by**: GVAR handler in `laiko/src/vm/op-variable.lisp` line 205

## Root Cause

**Most Likely**: Package loading issue at runtime

The error indicates that when the GVAR opcode handler executes at runtime, the `laiko.data` package's `read-atom-value` function is not available. This is likely because:

1. The data package files aren't being loaded before the VM dispatch loop starts, OR
2. There's a compilation issue preventing the function from being available in the running image

This is NOT a case of the function not being defined - it exists and is exported correctly. The issue is that at runtime execution, the function isn't accessible to the opcode handler.

## Fix Required

Ensure that `laiko.data` package is fully loaded/available before the VM starts executing bytecode. This may involve:

1. Checking the build/load sequence in `laiko/src/main.lisp`
2. Ensuring all required packages are loaded before `run-dispatch-loop` is called
3. Verifying that `laiko.vm` package has access to `laiko.data` functions

## Files Involved

| File                            | Line    | Purpose                               |
| ------------------------------- | ------- | ------------------------------------- |
| `laiko/src/vm/op-variable.lisp` | 191     | GVAR opcode definition                |
| `laiko/src/vm/op-variable.lisp` | 205     | Calls `laiko.data:read-atom-value`    |
| `laiko/src/data/atom.lisp`      | 113     | `read-atom-value` function definition |
| `laiko/src/package.lisp`        | 247     | Exports `read-atom-value`             |
| `laiko/src/main.lisp`           | 227-266 | `run-dispatch-loop` function          |

## Next Steps

1. Investigate the Laiko build/load sequence to confirm why `laiko.data:read-atom-value` is unavailable at runtime
2. Fix the package loading order or ensure proper package imports
3. Test that GVAR opcode works correctly after the fix
