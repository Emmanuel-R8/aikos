# Critical Runtime Issue: Stack/Frame Pointer Initialization Divergence

**Date**: 2025-01-17 19:50
**Priority**: CRITICAL - Blocks actual Zig emulator functionality
**Status**: Issue identified, comparison infrastructure working

## Issue Summary

The Zig emulator, despite having all implementation tasks complete, fails to achieve runtime parity with the C emulator due to incorrect stack and frame pointer initialization.

## Divergence Details

**First Instruction Comparison**:
- **C Emulator**: `PC:0x60f130 POP` with `SP:0x02e88, FP:0x307864`
- **Zig Emulator**: `PC:0x60f130 POP` with `SP:0x002e88, FP:0x002e72`

**Problem**: Zig VM uses wrong stack/frame pointer values, causing all subsequent operations to work on incorrect stack state.

## Root Cause Analysis

**Location**: `zaiko/src/vm/vm_initialization.zig` lines 40-60
**Function**: `initializeVMState()`
**Issue**: Not properly converting IFPAGE DLword offsets to VM stack pointers

**Expected Behavior** (from C emulator):
- Stack Pointer should be: `0x02e88` (from IFPAGE stackbase)
- Frame Pointer should be: `0x307864` (from current frame)

**Actual Behavior** (Zig emulator):
- Stack Pointer: `0x002e88` (wrong calculation)
- Frame Pointer: `0x002e72` (using currentfxp directly)

## Comparison Infrastructure Status

✅ **Working Components**:
1. **C Emulator Trace Generation**: `EMULATOR_MAX_STEPS=N ./maiko/linux.x86_64/ldesdl sysout`
2. **Zig Emulator Trace Generation**: `EMULATOR_MAX_STEPS=N zig build run -- sysout`
3. **Step-wise Comparison**: Both emulators generate comparable traces
4. **Divergence Identification**: First divergence isolated to VM initialization

## Fix Required

**File**: `zaiko/src/vm/vm_initialization.zig`
**Lines**: 40-60 (currentfxp calculation and pointer setting)
**Action**: Properly convert IFPAGE DLword offsets to VM stack pointers to match C emulator behavior

## Verification Plan

1. Fix VM initialization code
2. Run step-wise comparison with `EMULATOR_MAX_STEPS=5`
3. Verify stack/frame pointers match C emulator
4. Continue comparison to identify next divergence
5. Iterate until full runtime parity achieved

## Impact

**Current State**: Zig emulator loads sysout and executes instructions, but on wrong stack state
**Goal**: Achieve actual runtime parity where both emulators produce identical execution traces
**Blocking**: This issue prevents any meaningful testing or validation of Zig emulator correctness

## Documentation Updates Required

- Update `current-state-analysis.md` to reflect actual runtime status
- Document comparison infrastructure as working
- Mark runtime parity as incomplete until this issue is resolved