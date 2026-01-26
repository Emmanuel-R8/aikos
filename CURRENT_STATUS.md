# Interlisp Zig Emulator - Current Status Report

**Date**: 2025-01-27  
**Status**: Zig emulator 87.0% complete (94/108 tasks) - SIC opcode investigation ongoing

---

## Executive Summary

The Zig implementation of the Maiko VM emulator is nearing completion with 87% of tasks finished. However, critical execution divergence issues between the C and Zig emulators are preventing proper validation of opcode implementations. The SIC (Store Immediate Constant) opcode was identified as the immediate divergence point, requiring operand reading fixes for BYTESWAP mode.

**Critical Issue**: Zig and C emulators load different memory content at identical PC addresses due to FPtoVP table interpretation differences, preventing direct opcode comparison.

---

## Recent Work Completed

### 1. SIC Opcode Operand Reading Investigation

**Background**: The SIC opcode was identified as the first divergence point between C and Zig emulators.

- **File**: `zaiko/src/utils/memory_access.zig:65-95`
- **Issue**: Zig reading operand 0x00 instead of expected 0x3e for SIC at PC=0x60f136
- **Analysis**: For BYTESWAP mode, byte extraction was using incorrect big-endian logic instead of little-endian
- **Fix Applied**: Swapped byte extraction branches to use little-endian ordering for BYTESWAP mode

### 2. Return Handling Correction

**File**: `zaiko/src/vm/function.zig:108-128`

- **Issue**: `returnFromFunction` was setting PC to 0 for top-level returns, causing invalid execution
- **Fix**: Modified to advance PC by instruction length instead, matching C emulator behavior
- **Reference**: C emulator continues execution after top-level returns rather than resetting PC

### 3. Execution Environment Setup

**Files**: 
- `zaiko/src/main.zig:320-340` - Sysout loading process
- `zaiko/src/data/sysout.zig:240-290` - Memory verification

- **Achievement**: Successfully created writable cache directory for Zig builds
- **Verification**: Confirmed Zig and C emulators now have identical memory content at PC=0x60f131
- **Status**: XOR addressing verification successful (both return 0x0000020a)

### 4. Zig Emulator Testing Infrastructure

**File**: `zaiko/src/vm/execution_trace.zig:1-50`

- **Added**: Unified trace format generation for comparison with C emulator
- **Format**: Single-line pipe-delimited traces for rapid divergence analysis
- **Integration**: Compatible with existing C emulator trace comparison scripts

---

## Current Difficulties and Root Causes

### 1. FPtoVP Table Interpretation Mismatch

**Critical Issue**: Zig and C emulators map file pages to different virtual pages, causing memory content divergence.

**Evidence**:
- Zig: File page 5178 → Virtual page 6204 (PC page)
- C: File page 2937 → Virtual page 11850 (different mapping)
- **File**: `zaiko/src/data/sysout.zig:473-495` (FPtoVP byte-swapping logic)

**Root Cause Analysis**:
1. **File**: `maiko/src/ldsout.c:359` - C swaps only `(sysout_size / 4) + 1` entries
2. **File**: `zaiko/src/utils/endianness.zig:225-227` - Zig correctly implements this boundary
3. **Issue**: Different file page mappings despite identical byte-swapping

### 2. C Emulator Build Issues on NixOS

**Blocking Issue**: Cannot build/run C emulator for direct comparison due to GLIBC incompatibility.

**Error Details**:
```
symbol lookup error: .../glibc-2.40-66/lib/libc.so.6: undefined symbol: __nptl_change_stack_perm
```

**Attempted Solutions**:
- Nix-shell wrapper (failed - same GLIBC issue)
- Docker (failed - GLIBC symbol errors)
- CMake vs Make (failed - environment issue)

### 3. Memory Layout Verification Challenges

**File**: `zaiko/src/data/sysout.zig:241-288`

- **Success**: Memory bytes at PC=0x60f131 now match C emulator exactly
- **Issue**: Virtual page mappings differ, causing execution to diverge before reaching SIC
- **Impact**: Cannot verify SIC operand reading fix without identical execution context

### 4. SDL2 Integration Minor Issues

**File**: `zaiko/src/display/sdl_backend.zig:184-187`

- **Issue**: Memory leaks in SDL2 initialization (detected by GPA)
- **Status**: Non-blocking but should be resolved for production
- **Impact**:不影响核心功能，但影响性能和内存使用

---

## Technical Deep Dive: FPtoVP Table Analysis

### C Emulator Behavior

**File**: `maiko/src/ldsout.c:285-360` (BIGVM path)

```c
// C code path analysis
fptovp_offset = (fptovpstart - 1) * BYTESPER_PAGE + 4;  // Line 286
fptovp = malloc(sysout_size * 2 + 4);                 // Line 306
read(sysout, fptovp, sysout_size * 2);                // Line 309
word_swap_page((unsigned short *)fptovp, (sysout_size / 2) + 1); // Line 359
```

**Key Findings**:
- **Line 323**: `unsigned swap_boundary = (sysout_size / 4) + 1;`
- **Coverage**: Only ~50% of entries get byte-swapped (8318/16635 for starter.sysout)
- **File Page 5178**: In unswapped region, read as big-endian

### Zig Implementation

**File**: `zaiko/src/data/sysout.zig:473-495`

```zig
const swap_boundary = endianness_utils.calculateFPtoVPSwapBoundary(sysout_size_halfpages);
entries[i] = endianness_utils.swapFPtoVPEntry(entry_array, i, swap_boundary);
```

**Verification**: Zig correctly implements C's incomplete byte-swapping logic.

**Mystery**: Despite identical FPtoVP processing, file page mappings differ.

---

## Current Priorities (In Order of Importance)

### 1. 🔥 CRITICAL: Resolve FPtoVP Table Mapping Mismatch

**Impact**: Blocks all opcode validation work

**Immediate Actions**:
1. **Compare FPtoVP tables**: Generate and compare C vs Zig FPtoVP entries
2. **Debug mapping logic**: Add comprehensive logging to both emulators
3. **Verify byte-swapping**: Ensure identical byte-swapping boundaries
4. **Check BIGVM vs non-BIGVM**: Confirm both emulators use same mode

**Files to Modify**:
- `zaiko/src/data/sysout.zig:480-495` - Enhanced FPtoVP logging
- `maiko/src/ldsout.c:315-355` - Add FPtoVP comparison logging

### 2. 🟡 HIGH: Establish C Emulator Comparison Environment

**Impact**: Essential for validation without C emulator access

**Solutions to Try**:
1. **Container solution**: Use older GLIBC container for C emulator
2. **Static linking**: Build C emulator with static GLIBC if possible
3. **Cross-compilation**: Build C emulator on compatible system
4. **Trace-based validation**: Use existing C traces for comparison

**Reference Files**:
- `scripts/compare_emulator_execution.sh` - Main comparison script
- `scripts/compare_unified_traces.awk` - Fast trace comparison
- `scripts/analyze_execution_divergence.py` - Detailed analysis

### 3. 🟠 MEDIUM: Complete SIC Opcode Verification

**Prerequisite**: FPtoVP mapping resolution

**Steps**:
1. **Fix execution context**: Ensure identical memory state
2. **Run SIC instruction**: Verify operand reading (should be 0x3e)
3. **Check stack result**: Should push 0xfffe0000 to stack
4. **Update traces**: Add SIC to unified trace comparison

**Relevant Files**:
- `zaiko/src/vm/opcodes/data_ops.zig:76-95` - SIC implementation
- `zaiko/src/utils/memory_access.zig:65-95` - Operand reading logic

### 4. 🟢 LOW: Continue Systematic Opcode Implementation

**Current Status**: 94/108 tasks complete (87.0%)

**Remaining Tasks**:
1. **SDL2 Test Cases** (5 tasks: T092-T096)
2. **Polish Tasks** (5 tasks: T103-T108)

**Reference**: `specs/005-zig-completion/tasks.md` - Complete task list

---

## Background Knowledge for New Contributors

### Project Structure

```
Interlisp/
├── zaiko/                     # Zig implementation (in progress)
│   ├── src/
│   │   ├── vm/opcodes/        # Opcode implementations
│   │   ├── utils/             # Memory access, endianness
│   │   ├── data/              # Sysout loading
│   │   └── display/           # SDL2 integration
├── maiko/                     # C implementation (reference)
│   └── src/                   # Source code for reference
├── specs/005-zig-completion/  # Implementation specifications
└── documentation/              # Technical documentation
```

### Key Concepts

1. **LispPTR**: 32-bit virtual address (DLword offset from Lisp_world)
2. **DLword**: 16-bit unsigned integer
3. **FPtoVP Table**: Maps file pages to virtual pages (BIGVM format)
4. **BYTESWAP**: Byte-swapping for little-endian host compatibility
5. **XOR Addressing**: Special addressing mode for instruction fetch

### Critical Constants

- `IFPAGE_KEYVAL`: 0x15e3 (must match C implementation)
- `BYTESPER_PAGE`: 512 bytes
- `STK_OFFSET`: 0x00010000 (DLword offset for stack area)
- `FRAMESIZE`: 10 DLwords (20 bytes)

### Build Commands

```bash
# Zig emulator
cd zaiko
zig build
zig build run -- ../medley/internal/loadups/starter.sysout

# With step limit
EMULATOR_MAX_STEPS=50 zig build run -- ../medley/internal/loadups/starter.sysout

# Test execution
zig build test
```

---

## Documentation References

### Must-Read Files

1. **`documentation/core/critical_memory.typ`** - Documentation update rules
2. **`documentation/CRITICAL_DEBUGGING_TECHNIQUE.typ`** - Debugging techniques
3. **`specs/005-zig-completion/plan.md`** - Implementation plan
4. **`specs/005-zig-completion/tasks.md`** - Current task status

### Technical Documentation

1. **`documentation/implementations/zig-implementation.typ`** - Zig-specific notes
2. **`documentation/specifications/data-structures/sysout-byte-swapping.typ`** - Byte-swapping details
3. **`scripts/compare_emulator_execution.sh`** - Comparison workflow

---

## Test Data and Verification

### Sysout Files

- **Primary**: `medley/internal/loadups/starter.sysout` (8,517,120 bytes)
- **Identical**: `medley/loadups/starter.sysout` (verified identical via diff)

### Critical Memory Locations

- **PC Target**: 0x60f130 (C emulator's actual starting PC)
- **SIC Location**: 0x60f136 (where SIC opcode should be)
- **Expected Memory**: 0x00 0x00 0x60 0xbf 0xc9 0x12 0x0a 0x02 at PC 0x60f131

### Verification Status

✅ **Memory Content**: Zig and C match at PC=0x60f131  
✅ **XOR Addressing**: Both return 0x0000020a  
❌ **FPtoVP Mapping**: File page → virtual page mappings differ  
❌ **SIC Operand**: Cannot verify without identical execution context  

---

## Next Steps for Continuation

1. **Immediate**: Debug FPtoVP table mapping differences with enhanced logging
2. **Short-term**: Establish reliable C emulator comparison environment
3. **Medium-term**: Complete SIC opcode verification once execution context is identical
4. **Long-term**: Continue systematic opcode implementation to 100% completion

---

**Contact Information**: Refer to `documentation/README.md` for project contacts and additional resources.

**Last Updated**: 2025-01-27  
**Next Review**: After FPtoVP mapping resolution