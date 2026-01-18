# Centralized Memory Management Module Design

**Date**: 2026-01-17 18:00
**Status**: Design Specification
**Purpose**: Centralize scattered memory logic for address translation, endianness, and paging

## Overview

This specification designs a centralized memory management module that consolidates all memory-related operations currently scattered across the codebase. This addresses the recurring problems with understanding address translations, endianness, and memory paging by providing a single source of truth.

## Current Scattered Memory Logic

### Address Translation Issues
- **LispPTR ↔ byte address conversion**: Scattered in `vm_initialization.zig`, `execution_trace.zig`, dispatch files
- **Virtual page calculations**: Duplicated in `execution_trace.zig`, `xc_dispatch_part_02.inc`
- **FPtoVP table lookups**: In `ldsout.c`, `xc_dispatch_part_02.inc`, partially in Zig
- **XOR addressing**: Applied inconsistently across C and Zig implementations

### Endianness Handling
- **Byte-swapping**: Logic in `ldsout.c`, `word_swap_page()`, incomplete in Zig
- **FPtoVP table swapping**: Separate from page swapping, inconsistent boundary detection
- **Memory access patterns**: XOR addressing vs direct access confusion

### Memory Paging
- **File page → virtual page mapping**: Logic in `ldsout.c`, partial in Zig `sysout.zig`
- **Page loading**: Byte-swap boundaries, critical address verification
- **Memory bounds checking**: Scattered validation logic

## Centralized Module Architecture

### Core Components

#### 1. Address Translation Manager
```zig
pub const AddressManager = struct {
    /// Convert LispPTR (DLword offset) to byte offset
    pub fn lispPtrToByte(lisp_ptr: LispPTR) usize {
        return @as(usize, lisp_ptr) * 2;
    }

    /// Convert byte offset to LispPTR (DLword offset)
    pub fn byteToLispPtr(byte_offset: usize) LispPTR {
        return @intCast(byte_offset / 2);
    }

    /// Calculate virtual page for byte offset
    pub fn getVirtualPage(byte_offset: usize) usize {
        return byte_offset / BYTES_PER_PAGE;
    }

    /// Calculate offset within virtual page
    pub fn getPageOffset(byte_offset: usize) usize {
        return byte_offset % BYTES_PER_PAGE;
    }

    /// Calculate virtual address base for page
    pub fn getVirtualAddressBase(vpage: usize) usize {
        return vpage * BYTES_PER_PAGE;
    }
};
```

#### 2. FPtoVP Table Manager
```zig
pub const FPtoVPManager = struct {
    /// Lookup file page for virtual page
    pub fn getFilePageForVirtualPage(fptovp_table: []const u32, vpage: usize) ?u16 {
        for (fptovp_table, 0..) |entry, i| {
            const entry_vpage = entry & 0xFFFF;
            if (entry_vpage == vpage) {
                return @intCast(i);
            }
        }
        return null;
    }

    /// Get page OK flag for file page
    pub fn getPageOK(fptovp_table: []const u32, file_page: usize) u16 {
        if (file_page < fptovp_table.len) {
            return @intCast((fptovp_table[file_page] >> 16) & 0xFFFF);
        }
        return 0;
    }

    /// Calculate file offset for file page
    pub fn getFileOffset(file_page: usize) usize {
        return file_page * BYTES_PER_PAGE;
    }
};
```

#### 3. Endianness Manager
```zig
pub const EndiannessManager = struct {
    /// Determine if file page needs byte swapping
    pub fn needsByteSwap(file_page: usize, sysout_size: usize) bool {
        const swap_boundary = (sysout_size / 4) + 1;
        return file_page < swap_boundary;
    }

    /// Byte-swap 32-bit value (FPtoVP entry)
    pub fn swapU32(value: u32) u32 {
        return ((value & 0xFF) << 24) |
               ((value & 0xFF00) << 8) |
               ((value & 0xFF0000) >> 8) |
               ((value & 0xFF000000) >> 24);
    }

    /// Apply XOR addressing for byte access
    pub fn applyXorAddressing(base_ptr: [*]u8, offset: usize) [*]u8 {
        const base_addr = @intFromPtr(base_ptr);
        const xor_addr = base_addr ^ 3;  // XOR with 3 for byte addressing
        return @ptrFromInt(xor_addr + offset);
    }
};
```

#### 4. Memory Access Manager
```zig
pub const MemoryAccessManager = struct {
    /// Safe memory read with bounds checking
    pub fn readByte(memory: []const u8, offset: usize) ?u8 {
        if (offset < memory.len) {
            return memory[offset];
        }
        return null;
    }

    /// Safe memory read with XOR addressing
    pub fn readByteXor(memory: []const u8, offset: usize) ?u8 {
        if (offset < memory.len) {
            const xor_addr = offset ^ 3;
            if (xor_addr < memory.len) {
                return memory[xor_addr];
            }
        }
        return null;
    }

    /// Read instruction bytes (no XOR, matches C trace format)
    pub fn readInstructionBytes(memory: []const u8, pc: usize, count: usize) []const u8 {
        const end = @min(pc + count, memory.len);
        if (pc < end) {
            return memory[pc..end];
        }
        return &[_]u8{};
    }
};
```

### Integration Points

#### C Emulator Integration
- **xc_dispatch_part_02.inc**: Replace scattered calculations with centralized functions
- **ldsout.c**: Use centralized byte-swapping and FPtoVP logic
- **Unified trace**: Use centralized address calculations

#### Zig Emulator Integration
- **sysout.zig**: Use centralized FPtoVP and byte-swapping
- **execution_trace.zig**: Use centralized address calculations
- **dispatch.zig**: Use centralized memory access patterns

## Documentation Integration

### Memory Operation Patterns
```typst
# Memory Operation Patterns

== Address Translation
- DLword offset (LispPTR) × 2 = byte offset
- byte offset ÷ 512 = virtual page
- byte offset % 512 = page offset

== FPtoVP Lookup
- Search table for matching virtual page
- File page = table index
- File offset = file_page × 512

== Byte Swapping
- Boundary = (sysout_size ÷ 4) + 1
- Pages < boundary: swap 32-bit words
- Pages ≥ boundary: raw bytes

== XOR Addressing
- Base address ⊕ 3 for byte access
- Only for instruction decoding
- Memory traces show raw bytes
```

### Troubleshooting Guide
```typst
# Memory Issue Troubleshooting

== Address Mismatch
1. Verify LispPTR → byte conversion (×2)
2. Check virtual page calculation (÷512)
3. Validate FPtoVP table lookup

== Endianness Problems
1. Check byte swap boundary calculation
2. Verify 32-bit word swapping
3. Compare with C emulator traces

== Page Mapping Issues
1. Confirm FPtoVP table loading
2. Check file page → virtual page mapping
3. Validate memory bounds
```

## Implementation Plan

### Phase 1: Core Module Creation
- Create `zaiko/src/memory/manager.zig`
- Implement address translation functions
- Add basic documentation

### Phase 2: C Integration
- Update `xc_dispatch_part_02.inc` to use centralized functions
- Modify `ldsout.c` for centralized byte-swapping
- Test unified trace generation

### Phase 3: Zig Integration
- Update `sysout.zig` for centralized FPtoVP
- Modify `execution_trace.zig` for centralized calculations
- Ensure unified trace compatibility

### Phase 4: Documentation Update
- Add memory operation patterns to specifications
- Create troubleshooting guide
- Update critical debugging documentation

## Success Criteria

- [ ] Centralized address translation functions implemented
- [ ] Unified byte-swapping logic consolidated
- [ ] FPtoVP table operations centralized
- [ ] Memory access patterns standardized
- [ ] C emulator uses centralized functions
- [ ] Zig emulator uses centralized functions
- [ ] Documentation updated with patterns and troubleshooting
- [ ] Unified traces work with centralized logic

## Benefits

1. **Single Source of Truth**: All memory logic in one place
2. **Consistency**: Same calculations across C and Zig
3. **Maintainability**: Changes affect all uses automatically
4. **Debugging**: Easier to trace memory issues
5. **Documentation**: Clear patterns and troubleshooting guides

## References

- C Memory Implementation: `maiko/src/ldsout.c`, `maiko/src/xc_dispatch_part_02.inc`
- Zig Memory Implementation: `zaiko/src/data/sysout.zig`, `zaiko/src/vm/execution_trace.zig`
- Existing Documentation: `documentation/specifications/memory-management.typ`
- Unified Trace Format: `documentation/implementations/unified-trace-format-specification.typ`