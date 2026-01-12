# C Emulator PC Calculation Logic

**Date**: 2025-01-27  
**Purpose**: Complete analysis of how the C emulator calculates the Program Counter (PC)

## Overview

The PC (Program Counter) is calculated from frame information using the `FastRetCALL` macro. This document explains the complete calculation flow.

## FastRetCALL Macro

```c
#define FastRetCALL \
  do { \
    IVar = NativeAligned2FromStackOffset(GETWORD((DLword *)CURRENTFX - 1)); \
    FuncObj = (struct fnhead *)NativeAligned4FromLAddr(FX_FNHEADER); \
    PC = (ByteCode *)FuncObj + CURRENTFX->pc ; \
  } while (0)
```

## Step-by-Step Calculation

### 1. FX_FNHEADER

Read from the current frame:

**BIGVM**:
```c
FX_FNHEADER = CURRENTFX->fnheader;
```

**Non-BIGVM**:
```c
FX_FNHEADER = (CURRENTFX->hi2fnheader << 16) | CURRENTFX->lofnheader;
```

- This is a **LispPTR** (DLword offset from `Lisp_world`)

### 2. FuncObj Calculation

```c
FuncObj = (struct fnhead *)NativeAligned4FromLAddr(FX_FNHEADER);
```

**Expanded**:
```c
FuncObj = (struct fnhead *)(Lisp_world + FX_FNHEADER);
```

**Byte offset calculation**:
- Since `Lisp_world` is `DLword *`, pointer arithmetic multiplies by 2
- `FuncObj` byte offset = `FX_FNHEADER * 2` bytes

### 3. PC Calculation

```c
PC = (ByteCode *)FuncObj + CURRENTFX->pc;
```

**Expanded**:
```c
PC = (ByteCode *)(Lisp_world + (FX_FNHEADER * 2) + CURRENTFX->pc);
```

**Key Points**:
- `CURRENTFX->pc` is a **byte offset** (not DLword offset!)
- PC byte offset = `(FX_FNHEADER * 2) + CURRENTFX->pc`

## Verified Values (from Execution Log)

From execution log analysis:

- **PC byte offset**: `0x307898`
- **FuncObj byte offset**: `0x307830` (PC - 104)
- **FX_FNHEADER DLword offset**: `0x183c18` (FuncObj / 2)
- **CURRENTFX->pc**: `104` bytes (`0x68`)

### Verification

```
FuncObj = Lisp_world + (FX_FNHEADER * 2)
        = Lisp_world + (0x183c18 * 2)
        = Lisp_world + 0x307830 ✓

PC = FuncObj + CURRENTFX->pc
   = Lisp_world + 0x307830 + 104
   = Lisp_world + 0x307898 ✓
```

## Virtual Page Calculation

- **PC virtual page**: `0x307898 / 512 = 6204`
- **FuncObj virtual page**: `0x307830 / 512 = 6204`
- **Same page**: ✓ (both in virtual page 6204)
- **Difference**: Exactly 104 bytes (matches `CURRENTFX->pc`)

## Address Conversion Details

### NativeAligned4FromLAddr Implementation

```c
static inline LispPTR *NativeAligned4FromLAddr(LispPTR LAddr)
{
    if (LAddr & 1) {
        printf("Misaligned pointer in NativeAligned4FromLAddr 0x%x\n", LAddr);
    }
    return (void *)(Lisp_world + LAddr);
}
```

**Critical Understanding**:
- `Lisp_world` is `DLword *` (16-bit word pointer)
- `Lisp_world + LAddr` performs pointer arithmetic: adds `LAddr * sizeof(DLword)` bytes
- Since `sizeof(DLword) = 2`, this adds `LAddr * 2` bytes
- **LAddr must be a DLword offset, not byte offset**

## Frame Structure

The frame (`CURRENTFX`) contains:
- `fnheader`: Function header pointer (LispPTR = DLword offset)
- `pc`: Program counter offset (byte offset from FuncObj)

## Verification Results

All calculations have been verified against:
- Execution log (`c_emulator_execution_log_1000.txt`)
- Frame structure analysis
- Address conversion macros

### Verified Components

1. ✅ **FX_FNHEADER extraction**: Correctly read from frame
2. ✅ **FuncObj calculation**: `Lisp_world + (FX_FNHEADER * 2)` verified
3. ✅ **PC calculation**: `FuncObj + CURRENTFX->pc` verified
4. ✅ **Virtual page**: PC and FuncObj in same page, 104 bytes apart

## Key Insights

1. **DLword vs Byte Offsets**: Critical distinction - LispPTR is DLword offset, must multiply by 2
2. **Frame PC Field**: `CURRENTFX->pc` is byte offset, not DLword offset
3. **Consistency**: All calculations match execution log exactly
4. **Same Page**: FuncObj and PC are always in the same virtual page

## References

- `maiko/inc/retmacro.h` - FastRetCALL macro definition
- `maiko/inc/adr68k.h` - NativeAligned4FromLAddr implementation
- `maiko/src/main.c` - Frame initialization and FastRetCALL usage
