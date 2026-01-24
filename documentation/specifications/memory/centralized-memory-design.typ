= Centralized Memory Management Module Design

*Date*: 2026-01-17 18:00
*Status*: Design Specification
*Purpose*: Centralize scattered memory logic for address translation, endianness, and paging

*Note*: Pseudocode and structure are implementation-agnostic; the Zig snippets are a reference implementation.

== Overview

This specification designs a centralized memory management module that consolidates all memory-related operations currently scattered across the codebase. This addresses the recurring problems with understanding address translations, endianness, and memory paging by providing a single source of truth.

== Current Scattered Memory Logic

=== Address Translation Issues

- *LispPTR ↔ byte address conversion*: Scattered in vm_initialization, execution_trace, dispatch files
- *Virtual page calculations*: Duplicated in execution_trace, xc_dispatch_part_02.inc
- *FPtoVP table lookups*: In `ldsout.c`, `xc_dispatch_part_02.inc`, partially in Zig
- *XOR addressing*: Applied inconsistently across C and Zig implementations

=== Endianness Handling

- *Byte-swapping*: Logic in `ldsout.c`, `word_swap_page()`, incomplete in Zig
- *FPtoVP table swapping*: Separate from page swapping, inconsistent boundary detection
- *Memory access patterns*: XOR addressing vs direct access confusion

=== Memory Paging

- *File page → virtual page mapping*: Logic in `ldsout.c`, partial in Zig `sysout.zig`
- *Page loading*: Byte-swap boundaries, critical address verification
- *Memory bounds checking*: Scattered validation logic

== Centralized Module Architecture

=== Core Components

==== 1. Address Translation Manager

#codeblock(lang: "zig", [
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
])

==== 2. FPtoVP Table Manager

#codeblock(lang: "zig", [
pub const FPtoVPManager = struct {
    pub fn getFilePageForVirtualPage(fptovp_table: []const u32, vpage: usize) ?u16 {
        for (fptovp_table, 0..) |entry, i| {
            const entry_vpage = entry & 0xFFFF;
            if (entry_vpage == vpage) return @intCast(i);
        }
        return null;
    }
    pub fn getPageOK(fptovp_table: []const u32, file_page: usize) u16 {
        if (file_page < fptovp_table.len)
            return @intCast((fptovp_table[file_page] >> 16) & 0xFFFF);
        return 0;
    }
    pub fn getFileOffset(file_page: usize) usize {
        return file_page * BYTES_PER_PAGE;
    }
};
])

==== 3. Endianness Manager

#codeblock(lang: "zig", [
pub const EndiannessManager = struct {
    pub fn needsByteSwap(file_page: usize, sysout_size: usize) bool {
        const swap_boundary = (sysout_size / 4) + 1;
        return file_page < swap_boundary;
    }
    pub fn swapU32(value: u32) u32 { /* 32-bit byte-swap */ }
    pub fn applyXorAddressing(base_ptr: [*]u8, offset: usize) [*]u8 {
        const base_addr = @intFromPtr(base_ptr);
        return @ptrFromInt((base_addr ^ 3) + offset);
    }
};
])

==== 4. Memory Access Manager

#codeblock(lang: "zig", [
pub const MemoryAccessManager = struct {
    pub fn readByte(memory: []const u8, offset: usize) ?u8 { ... }
    pub fn readByteXor(memory: []const u8, offset: usize) ?u8 { ... }
    /// Read instruction bytes (no XOR, matches C trace format)
    pub fn readInstructionBytes(memory: []const u8, pc: usize, count: usize) []const u8 { ... }
};
])

=== Integration Points

- *C*: `xc_dispatch_part_02.inc`, `ldsout.c` — centralized byte-swapping, FPtoVP, address calculations
- *Zig*: `sysout.zig`, `execution_trace.zig`, `dispatch.zig` — FPtoVP, address calculations, memory access

== Memory Operation Patterns

- *Address Translation*: DLword (LispPTR) × 2 = byte offset; byte ÷ 512 = virtual page; byte % 512 = page offset
- *FPtoVP Lookup*: Search table for virtual page; file page = index; file offset = file_page × 512
- *Byte Swapping*: Boundary = (sysout_size ÷ 4) + 1; pages < boundary: swap 32-bit words; ≥ boundary: raw
- *XOR Addressing*: Base ⊕ 3 for byte access; only for instruction decode; traces show raw bytes

== References

- C: `maiko/src/ldsout.c`, `maiko/src/xc_dispatch_part_02.inc`
- Zig: `zaiko/src/data/sysout.zig`, `zaiko/src/vm/execution_trace.zig`
- *Memory*: `../address-translation.typ`, `../../components/memory-management.typ`
- *Trace format*: `../vm-core/trace-and-logging-formats.typ`
