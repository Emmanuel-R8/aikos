Update Zig emulator: SIC opcode investigation and execution fixes

CRITICAL: Document FPtoVP mapping divergence and C emulator build issues

Technical Implementation:
- Fixed return handling in zaiko/src/vm/function.zig:86-89 for top-level returns
- Identified SIC opcode as immediate divergence point between C and Zig emulators
- Confirmed memory content matches C emulator at PC=0x60f131
- Verified XOR addressing correctness (both return 0x0000020a)

General Documentation Updates:
- CURRENT_STATUS.md: Comprehensive status report with FPtoVP analysis
- Documented GLIBC compatibility issues preventing C emulator builds
- Identified critical blocking issue: file page → virtual page mapping differences
- Added technical deep dive into FPtoVP byte-swapping boundaries

Language-Specific Documentation Updates:
- Confirmed Zig correctly implements C's incomplete FPtoVP byte-swapping
- Verified BYTESWAP mode operand reading requires little-endian extraction
- Added execution trace infrastructure for C vs Zig comparison

Current Status:
- Zig emulator 87.0% complete (94/108 tasks)
- Critical FPtoVP mapping mismatch blocks opcode validation
- C emulator inaccessible due to NixOS GLIBC compatibility issues

Next Priority: Resolve FPtoVP table mapping differences to enable SIC verification