# Endianness Analysis Summary

**Date**: 2025-01-27 14:30

## Analysis Approach

### What Was Done

1. **Exhaustive Static Code Analysis**:
   - Read and traced all relevant C header files (`lsptypes.h`, `lispemul.h`, `inlineC.h`, `adr68k.h`, `stack.h`)
   - Read and traced all relevant C source files (`xc.c`, `byteswap.c`, `ldsout.c`)
   - Documented complete macro expansion chains
   - Categorized all 100+ instructions by endianness impact
   - Created comprehensive findings document (1,893 lines)

2. **Debug Statements Added** (for future dynamic verification):
   - Opcode fetch XOR addressing verification (`xc.c:598-608`)
   - Get_DLword value construction verification (`xc.c:1152-1159`)
   - FastRetCALL address translation verification (`retmacro.h:42-57`)
   - GETBASE_N memory access verification (`inlineC.h:246-261`)

### What Requires Dynamic Verification

The static analysis has identified patterns through code reading, but these need to be verified by **running the code**:

1. **XOR Addressing**: Does `base ^ 3` actually read the correct byte?
2. **PC Calculation**: Is `CURRENTFX->pc` a byte offset or DLword offset?
3. **Address Translation**: Does `NativeAligned4FromLAddr()` use DLword arithmetic or byte arithmetic?
4. **Value Construction**: Does manual construction produce correct big-endian values?

## Key Findings (From Static Analysis)

### 1. XOR Addressing Pattern

**Location**: `maiko/inc/lsptypes.h:565, 569`

```c
// BYTESWAP mode
#define GETBYTE(base) (*(unsigned char *)(3 ^ (UNSIGNED)(base)))
#define GETWORD(base) (*(DLword *)(2 ^ (UNSIGNED)(base)))
```

**Finding**: All memory access in BYTESWAP mode uses XOR addressing to compensate for byte-swapped memory layout after 32-bit longword swapping.

**Verification Status**: ⚠️ Pattern identified in code, needs dynamic verification

### 2. Manual Value Construction

**Location**: `maiko/inc/lispemul.h:240, 243-247`

```c
#define Get_DLword(ptr) ((Get_BYTE(ptr) << 8) | Get_BYTE((ptr) + 1))
#define Get_Pointer(ptr) ((Get_BYTE(ptr) << 16) | (Get_BYTE((ptr) + 1) << 8) | Get_BYTE((ptr) + 2))
```

**Finding**: Multi-byte values are constructed manually from bytes, ensuring correct byte order regardless of host endianness.

**Verification Status**: ⚠️ Pattern identified in code, needs dynamic verification

### 3. Two-Stage Memory Access

**Location**: `maiko/inc/inlineC.h:246-251`

```c
#define GETBASE_N(N) \
  TOPOFSTACK = GETWORD((DLword *)NativeAligned2FromLAddr((POINTERMASK & TOPOFSTACK) + (N)));
```

**Finding**: Memory access via LispPTR involves two stages:
1. Address translation: `NativeAligned2FromLAddr()` (pointer arithmetic, no endianness)
2. Data access: `GETWORD()` (XOR addressing in BYTESWAP mode)

**Verification Status**: ⚠️ Pattern identified in code, needs dynamic verification

### 4. 32-bit Longword Swapping

**Location**: `maiko/src/byteswap.c:31-34`

```c
void word_swap_page(void *page, unsigned longwordcount) {
  unsigned int *longpage = (unsigned int *)page;
  for (unsigned int i = 0; i < longwordcount; i++) {
    *(longpage + i) = ntohl(*(longpage + i));
  }
}
```

**Finding**: Pages are swapped as 32-bit longwords using `ntohl()`, not 16-bit DLwords.

**Verification Status**: ✅ Confirmed by code inspection

## Confidence Levels

### High Confidence (Code Inspection Confirms)

- ✅ 32-bit longword swapping (`word_swap_page` implementation is clear)
- ✅ Macro definitions (XOR addressing formulas are explicit)
- ✅ Manual value construction patterns (code is straightforward)

### Medium Confidence (Pattern Identified, Needs Verification)

- ⚠️ XOR addressing correctness (formula is clear, but need to verify it works correctly)
- ⚠️ Address translation behavior (code shows test modifications, need to verify actual behavior)
- ⚠️ PC calculation interpretation (documentation vs code discrepancy)

### Areas of Doubt (Require Dynamic Verification)

- ❓ Does XOR addressing actually produce correct results?
- ❓ Is `CURRENTFX->pc` a byte offset or DLword offset?
- ❓ Does `NativeAligned4FromLAddr()` use DLword arithmetic or byte arithmetic?

## Recommendations

1. **For Implementation**: Proceed with static analysis findings, but implement with verification hooks
2. **For Verification**: Run C emulator with added debug statements to confirm behavior
3. **For Documentation**: Mark findings as "static analysis" vs "dynamically verified"

## Files Created

1. **ENDIANNESS_FINDINGS.md** (1,893 lines): Complete static analysis findings
2. **ENDIANNESS_VERIFICATION_STATUS.md**: Verification status and plan
3. **ENDIANNESS_ANALYSIS_PLAN.md**: Original analysis plan
4. **ENDIANNESS_ANALYSIS_SUMMARY.md** (this file): Summary of approach and findings

## Conclusion

**Static Analysis**: ✅ Complete - Comprehensive documentation based on exhaustive code reading

**Dynamic Verification**: ⚠️ Debug statements added, requires execution to verify actual behavior

**Next Step**: Run C emulator with debug statements to verify findings match actual behavior
