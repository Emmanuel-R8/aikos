# Next Steps for Emulator Parity Work

**Date**: 2026-01-28 12:34
**Current Status**: Parity verified for 200+ instructions (87.0% complete - 94/108 tasks)

## Immediate Next Steps

### Option 1: Extended Validation (Recommended) 🎯

**Goal**: Identify next divergence point beyond instruction 200

**Actions**:

1. Run extended execution comparison (1000+ instructions)

   ```bash
   cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp
   EMULATOR_MAX_STEPS=1000 ./scripts/compare_emulator_execution.sh medley/internal/loadups/starter.sysout
   ```

2. Analyze divergence point using comparison tools:

   ```bash
   ./scripts/analyze_execution_divergence.py c_emulator_unified_trace.txt zaiko/zig_emulator_execution_log.txt
   ```

3. Follow systematic approach for identified divergence:
   - Understand C implementation
   - Document in maiko/ (if needed)
   - Update Typst documentation
   - Fix Zig implementation

**Expected Outcome**: Identify specific opcode or execution path causing divergence

### Option 2: Address Known Issues

**Priority Issues** (from documentation):

1. **IVAR2: `error.InvalidAddress`** (Medium Priority)

   - **Location**: `zaiko/src/vm/opcodes/variable_access.zig`
   - **Issue**: Needs proper frame access
   - **Systematic Approach**:
     - Review C IVAR implementation in `maiko/src/`
     - Document frame access pattern
     - Update Typst documentation
     - Fix Zig implementation

2. **FN0-FN4: UFN handling** (Medium Priority)

   - **Location**: `zaiko/src/vm/opcodes/function_calls.zig`
   - **Issue**: UFN handling not fully implemented
   - **Systematic Approach**:
     - Review C UFN implementation in `maiko/src/ufn.c`
     - Document UFN call mechanism
     - Update Typst documentation
     - Implement UFN handling in Zig

3. **STKSCAN: Basic stub only** (Low Priority)

   - **Location**: `zaiko/src/vm/opcodes/`
   - **Issue**: Needs full implementation
   - **Systematic Approach**:
     - Review C STKSCAN implementation
     - Document stack scanning algorithm
     - Update Typst documentation
     - Implement full STKSCAN in Zig

4. **MISC2-MISC9: Some variants unimplemented** (Low Priority)
   - **Location**: `zaiko/src/vm/opcodes/misc_ops.zig`
   - **Issue**: Some MISC opcode variants missing
   - **Systematic Approach**:
     - Review C MISC implementations
     - Document each variant
     - Update Typst documentation
     - Implement missing variants

### Option 3: Continue Systematic Documentation

**Goal**: Ensure all critical opcodes are fully documented

**Actions**:

1. Review opcodes with known complexity:

   - RETURN (already verified)
   - UNBIND (already verified)
   - BIND
   - Function calls (FN0-FN4, FNX, APPLYFN)
   - Control flow (JUMP variants, TJUMP, FJUMP)

2. For each opcode:
   - Verify C implementation is documented
   - Verify Typst documentation is consistent
   - Verify Zig implementation matches C

## Recommended Approach

**Start with Option 1 (Extended Validation)** because:

- Current parity is only verified for 200+ instructions
- Full execution involves thousands of instructions
- Need to identify actual divergence points before fixing
- Systematic approach can then be applied to specific issues

**Then proceed with Option 2** for any issues found during extended validation.

## Resources

- **Comparison Script**: `scripts/compare_emulator_execution.sh`
- **Divergence Analysis**: `scripts/analyze_execution_divergence.py`
- **C Implementation**: `maiko/src/` and `maiko/inc/`
- **Documentation**: `documentation/specifications/` and `documentation/implementations/`
- **Current Status**: `reports/CURRENT_STATUS.md`
- **Task Tracking**: `specs/005-zig-completion/tasks.md`

## Success Criteria

- Extended validation completes without crashes
- Divergence points are identified and documented
- Systematic approach is applied to each divergence
- Parity is verified for 1000+ instructions
- All known issues are addressed or documented
