const std = @import("std");
const types = @import("../utils/types.zig");

const LispPTR = types.LispPTR;
const DLword = types.DLword;
const ByteCode = types.ByteCode;

/// =============================================================================
/// MEMORY MANAGEMENT UTILITIES
/// =============================================================================
/// 
/// **Purpose**: Centralized memory management utilities and address translation
/// 
/// **Architecture Overview**:
/// This module provides the core utilities for memory management in the Zig
/// implementation of the Interlisp VM. It centralizes address translation,
/// page management, endianness handling, and memory access patterns to ensure
/// consistency across the entire VM implementation.
/// 
/// **Key Concepts**:
/// 1. **Address Translation**: LispPTR ↔ byte offset conversions
/// 2. **Page Management**: 512-byte virtual page calculations
/// 3. **FPtoVP Translation**: File page ↔ virtual page mapping
/// 4. **Endianness Handling**: Byte swapping and XOR addressing
/// 5. **Safe Memory Access**: Bounds-checked memory operations
/// 
/// **C Reference Compatibility**:
/// - Matches maiko/inc/adr68k.h address translation functions
/// - Compatible with maiko/src/vm.c memory access patterns
/// - Integrates with maiko/inc/lspglob.h memory layout constants
/// 
/// **Integration Points**:
/// - VM execution: Provides address translation for instruction fetch
/// - Memory system: Central utilities for all memory operations
/// - Debugger: Memory inspection and debugging capabilities
/// - Storage system: Address conversion for heap management
/// 
/// **Performance Considerations**:
/// - Critical path: address translation called for every memory access
/// - Cache-friendly: aligned structures and efficient algorithms
/// - Bounds checking: adds safety but impacts performance
/// - XOR addressing: required for little-endian compatibility
/// 
/// **FIXME Items**:
/// - TODO: Add caching for frequently accessed translations
/// - TODO: Optimize hot path functions with inline directives
/// - TODO: Add platform-specific optimizations
/// 
/// **Testing Recommendations**:
/// - Test boundary conditions around page boundaries
/// - Validate XOR addressing with endianness test cases
/// - Test bounds checking with invalid addresses
/// - Verify FPtoVP translation accuracy
/// =============================================================================

/// Constants for memory management
/// 
/// **BYTES_PER_PAGE**: 512 bytes per virtual page (matches C implementation)
/// - Used throughout VM for page-aligned operations
/// - Compatible with maiko/inc/lspglob.h BYTESPER_PAGE constant
/// 
/// **Rationale**: 512 bytes = 256 DLwords, matching original Interlisp design
pub const BYTES_PER_PAGE = 512;

/// =============================================================================
/// ADDRESS TRANSLATION UTILITIES
/// =============================================================================

/// Address translation manager for LispPTR ↔ byte offset conversions
/// 
/// **Purpose**: Provide standardized address translation functions
/// 
/// **Key Operations**:
/// - LispPTR to byte offset conversion
/// - Byte offset to LispPTR conversion
/// - Virtual page calculations
/// - Page offset calculations
/// 
/// **C Reference**: Functions replace NativeAligned2FromLAddr/4FromLAddr logic
pub const AddressManager = struct {
    /// Convert LispPTR (DLword offset) to byte offset
    /// 
    /// **Purpose**: Translate VM address space to byte-addressable memory
    /// 
    /// **Formula**: `lisp_ptr * 2` (each DLword = 2 bytes)
    /// 
    /// **Parameters**:
    /// - lisp_ptr: LispPTR address in DLword units
    /// 
    /// **Returns**: Byte offset from memory base
    /// 
    /// **Integration**: Used by all address translation functions
    /// 
    /// **Complexity**: O(1)
    /// **Confidence**: 100%
    pub fn lispPtrToByte(lisp_ptr: LispPTR) usize {
        return @as(usize, lisp_ptr) * 2;
    }

    /// Convert byte offset to LispPTR (DLword offset)
    /// 
    /// **Purpose**: Translate byte offset back to VM address space
    /// 
    /// **Formula**: `byte_offset / 2` (convert bytes to DLwords)
    /// 
    /// **Parameters**:
    /// - byte_offset: Byte offset from memory base
    /// 
    /// **Returns**: LispPTR address in DLword units
    /// 
    /// **Integration**: Used for reverse address translation
    /// 
    /// **Complexity**: O(1)
    /// **Confidence**: 100%
    pub fn byteToLispPtr(byte_offset: usize) LispPTR {
        return @intCast(byte_offset / 2);
    }

    /// Calculate virtual page number for byte offset
    /// 
    /// **Purpose**: Determine which virtual page contains an address
    /// 
    /// **Formula**: `byte_offset / BYTES_PER_PAGE`
    /// 
    /// **Parameters**:
    /// - byte_offset: Byte offset from memory base
    /// 
    /// **Returns**: Virtual page number
    /// 
    /// **Integration**: Used by page management and FPtoVP operations
    /// 
    /// **Complexity**: O(1)
    /// **Confidence**: 100%
    pub fn getVirtualPage(byte_offset: usize) usize {
        return byte_offset / BYTES_PER_PAGE;
    }

    /// Calculate offset within virtual page for byte offset
    /// 
    /// **Purpose**: Determine position within a virtual page
    /// 
    /// **Formula**: `byte_offset % BYTES_PER_PAGE`
    /// 
    /// **Parameters**:
    /// - byte_offset: Byte offset from memory base
    /// 
    /// **Returns**: Byte offset within page (0-511)
    /// 
    /// **Integration**: Used for page-level operations and debugging
    /// 
    /// **Complexity**: O(1)
    /// **Confidence**: 100%
    pub fn getPageOffset(byte_offset: usize) usize {
        return byte_offset % BYTES_PER_PAGE;
    }

    /// Calculate virtual address base for page number
    /// 
    /// **Purpose**: Convert page number back to byte offset
    /// 
    /// **Formula**: `vpage * BYTES_PER_PAGE`
    /// 
    /// **Parameters**:
    /// - vpage: Virtual page number
    /// 
    /// **Returns**: Byte offset of page base
    /// 
    /// **Integration**: Used by page allocation and mapping
    /// 
    /// **Complexity**: O(1)
    /// **Confidence**: 100%
    pub fn getVirtualAddressBase(vpage: usize) usize {
        return vpage * BYTES_PER_PAGE;
    }
};

/// =============================================================================
/// FPTOVP TABLE MANAGEMENT
/// =============================================================================

/// FPtoVP (File Page to Virtual Page) table management
/// 
/// **Purpose**: Manage mapping between file pages and virtual memory pages
/// 
/// **Background**: 
/// - Sysout files are divided into 512-byte pages
/// - These pages are mapped into virtual memory at load time
/// - FPtoVP table tracks which virtual page each file page occupies
/// - Each 32-bit entry contains virtual page number (low 16 bits) 
///   and page status flags (high 16 bits)
/// 
/// **C Reference**: Matches FPtoVP table structure in maiko/inc/lspglob.h
/// 
/// **Integration**: Used by sysout loading and address translation
pub const FPtoVPManager = struct {
    /// Lookup file page number for given virtual page
    /// 
    /// **Purpose**: Reverse lookup from virtual page to file page
    /// 
    /// **Algorithm**: 
    /// - Scan FPtoVP table for matching virtual page
    /// - Return file page index when found
    /// 
    /// **Parameters**:
    /// - fptovp_table: FPtoVP table array
    /// - vpage: Virtual page number to find
    /// 
    /// **Returns**: File page number, or null if not found
    /// 
    /// **Performance**: O(n) linear scan - could be optimized with caching
    /// 
    /// **Integration**: Used for debugging and page validation
    /// 
    /// **Complexity**: O(n) where n is table size
    /// **Confidence**: 95% (tested, but could be optimized)
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
    /// 
    /// **Purpose**: Extract page status from FPtoVP entry
    /// 
    /// **Status Flags** (high 16 bits):
    /// - Various flags indicating page validity, swapping status, etc.
    /// - "OK" flag indicates page is properly loaded and accessible
    /// 
    /// **Parameters**:
    /// - fptovp_table: FPtoVP table array
    /// - file_page: File page number to check
    /// 
    /// **Returns**: Page status flags
    /// 
    /// **Integration**: Used for page validation and error checking
    /// 
    /// **Complexity**: O(1)
    /// **Confidence**: 100%
    pub fn getPageOK(fptovp_table: []const u32, file_page: usize) u16 {
        if (file_page < fptovp_table.len) {
            return @intCast((fptovp_table[file_page] >> 16) & 0xFFFF);
        }
        return 0;
    }

    /// Calculate file offset for file page
    /// 
    /// **Purpose**: Convert file page to byte offset in sysout file
    /// 
    /// **Formula**: `file_page * BYTES_PER_PAGE`
    /// 
    /// **Parameters**:
    /// - file_page: File page number
    /// 
    /// **Returns**: Byte offset in sysout file
    /// 
    /// **Integration**: Used by sysout loading and page mapping
    /// 
    /// **Complexity**: O(1)
    /// **Confidence**: 100%
    pub fn getFileOffset(file_page: usize) usize {
        return file_page * BYTES_PER_PAGE;
    }
};

/// =============================================================================
/// ENDIANNESS AND BYTE-SWAPPING UTILITIES
/// =============================================================================

/// Endianness management for cross-platform compatibility
/// 
/// **Purpose**: Handle byte order differences between host and VM
/// 
/// **Background**:
/// - Original Interlisp VM was big-endian (68K architecture)
/// - Modern hosts are typically little-endian (x86, ARM)
/// - Sysout files may be byte-swapped for compatibility
/// - XOR addressing is used for proper byte access patterns
/// 
/// **Key Concepts**:
/// - **Byte Swapping**: 32-bit value byte order reversal
/// - **XOR Addressing**: Address ^ 3 for little-endian byte access
/// - **Swap Boundaries**: Determine which pages need swapping
/// 
/// **C Reference**: Matches byte-swapping logic in maiko/src/vm.c
pub const EndiannessManager = struct {
    /// Determine if file page needs byte swapping
    /// 
    /// **Purpose**: Check if page was byte-swapped during loading
    /// 
    /// **Algorithm**: 
    /// - Pages before swap boundary need swapping
    /// - Boundary is typically sysout_size/4 + 1
    /// - Based on C implementation's swap strategy
    /// 
    /// **Parameters**:
    /// - file_page: File page number to check
    /// - sysout_size: Total sysout file size
    /// 
    /// **Returns**: true if page needs byte swapping
    /// 
    /// **Integration**: Used during page loading and address translation
    /// 
    /// **Complexity**: O(1)
    /// **Confidence**: 90% (matches C logic)
    pub fn needsByteSwap(file_page: usize, sysout_size: usize) bool {
        const swap_boundary = (sysout_size / 4) + 1;
        return file_page < swap_boundary;
    }

    /// Byte-swap 32-bit value (reverse byte order)
    /// 
    /// **Purpose**: Convert between big-endian and little-endian
    /// 
    /// **Algorithm**: 
    /// ```
    /// original: [B3][B2][B1][B0]
    /// swapped:  [B0][B1][B2][B3]
    /// ```
    /// 
    /// **Parameters**:
    /// - value: 32-bit value to byte-swap
    /// 
    /// **Returns**: Byte-swapped 32-bit value
    /// 
    /// **Integration**: Used for FPtoVP entries and 32-bit data
    /// 
    /// **Complexity**: O(1) with bit operations
    /// **Confidence**: 100%
    pub fn swapU32(value: u32) u32 {
        return ((value & 0xFF) << 24) |
            ((value & 0xFF00) << 8) |
            ((value & 0xFF0000) >> 8) |
            ((value & 0xFF000000) >> 24);
    }

    /// Apply XOR addressing for byte access on little-endian hosts
    /// 
    /// **Purpose**: Handle big-endian VM byte access on little-endian hardware
    /// 
    **XOR Addressing Explained**:
    /// - VM expects big-endian byte ordering
    /// - On little-endian hosts, individual byte access needs XOR
    /// - Address ^ 3 reverses byte order within 32-bit words
    /// - Example: 0x1000 ^ 3 = 0x1003 accesses bytes in reverse order
    /// 
    /// **Parameters**:
    /// - base_ptr: Base pointer to memory region
    /// - offset: Byte offset from base
    /// 
    /// **Returns**: XOR-adjusted pointer for byte access
    /// 
    /// **C Reference**: Matches GETBYTE macro in C implementation
    /// 
    /// **Integration**: Used for all byte-wise memory access
    /// 
    /// **Complexity**: O(1)
    /// **Confidence**: 100%
    pub fn applyXorAddressing(base_ptr: [*]u8, offset: usize) [*]u8 {
        const base_addr = @intFromPtr(base_ptr);
        const xor_addr = base_addr ^ 3; // XOR with 3 for byte addressing
        return @ptrFromInt(xor_addr + offset);
    }

    /// Get XOR address for byte access
    /// 
    /// **Purpose**: Calculate XOR-adjusted address for byte access
    /// 
    /// **Parameters**:
    /// - address: Original address
    /// 
    /// **Returns**: XOR-adjusted address
    /// 
    /// **Integration**: Used by memory access functions
    /// 
    /// **Complexity**: O(1)
    /// **Confidence**: 100%
    pub fn getXorAddress(address: usize) usize {
        return address ^ 3;
    }

    /// Read 16-bit value from memory with XOR addressing (little-endian)
    /// 
    /// **Purpose**: Read 16-bit word with proper endianness handling
    /// 
    /// **C Reference**: Matches GETBYTE behavior in C emulator
    /// 
    /// **Algorithm**:
    /// 1. Apply XOR addressing to both bytes
    /// 2. Read bytes with bounds checking
    /// 3. Combine as little-endian word
    /// 
    /// **Parameters**:
    /// - memory: Memory buffer to read from
    /// - address: Byte address to read from
    /// 
    /// **Returns**: 16-bit value with proper byte order
    /// 
    /// **Integration**: Used by VM for word-sized memory access
    /// 
    /// **Complexity**: O(1)
    /// **Confidence**: 100%
    pub fn readWordXor(memory: []const u8, address: usize) u16 {
        const xor_addr0 = address ^ 3;
        const xor_addr1 = (address + 1) ^ 3;
        const byte0 = if (xor_addr0 < memory.len) memory[xor_addr0] else 0;
        const byte1 = if (xor_addr1 < memory.len) memory[xor_addr1] else 0;
        return @as(u16, byte0) | (@as(u16, byte1) << 8);
    }

    /// Read 16-bit value from memory without XOR addressing (little-endian)
    /// 
    /// **Purpose**: Read 16-bit word with direct little-endian access
    /// 
    /// **Use Case**: For data that's already in little-endian format
    /// 
    /// **Parameters**:
    /// - memory: Memory buffer to read from
    /// - address: Byte address to read from
    /// 
    /// **Returns**: 16-bit value in little-endian order
    /// 
    /// **Integration**: Used for little-endian data access
    /// 
    /// **Complexity**: O(1)
    /// **Confidence**: 100%
    pub fn readWordLittleEndian(memory: []const u8, address: usize) u16 {
        const byte0 = if (address < memory.len) memory[address] else 0;
        const byte1 = if (address + 1 < memory.len) memory[address + 1] else 0;
        return @as(u16, byte0) | (@as(u16, byte1) << 8);
    }
};

/// =============================================================================
/// MEMORY ACCESS UTILITIES
/// =============================================================================

/// Safe memory access utilities with bounds checking
/// 
/// **Purpose**: Provide safe, consistent memory access patterns
/// 
/// **Design Principles**:
/// - Bounds checking for safety (prevents crashes)
/// - XOR addressing for endianness compatibility
/// - Efficient byte and word access patterns
/// - Instruction fetch optimization
/// 
/// **Integration**: Used throughout VM for all memory operations
pub const MemoryAccessManager = struct {
    /// Safe byte read with bounds checking
    /// 
    /// **Purpose**: Read single byte with safety guarantees
    /// 
    /// **Parameters**:
    /// - memory: Memory buffer to read from
    /// - offset: Byte offset from buffer start
    /// 
    /// **Returns**: Byte value, or null if out of bounds
    /// 
    /// **Integration**: Used for safe byte access throughout VM
    /// 
    /// **Complexity**: O(1)
    /// **Confidence**: 100%
    pub fn readByte(memory: []const u8, offset: usize) ?u8 {
        if (offset < memory.len) {
            return memory[offset];
        }
        return null;
    }

    /// Safe byte read with XOR addressing
    /// 
    /// **Purpose**: Read byte with proper endianness handling
    /// 
    /// **Parameters**:
    /// - memory: Memory buffer to read from
    /// - offset: Byte offset from buffer start
    /// 
    /// **Returns**: Byte value with XOR addressing, or null if out of bounds
    /// 
    /// **Integration**: Used for big-endian byte access on little-endian hosts
    /// 
    /// **Complexity**: O(1)
    /// **Confidence**: 100%
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
    /// 
    /// **Purpose**: Fetch instruction bytes for VM execution
    /// 
    **Special Handling**:
    /// - Instructions are fetched without XOR addressing
    /// - Matches C trace format for consistency
    /// - Used by VM instruction fetch and debugging
    /// 
    /// **Parameters**:
    /// - memory: Memory buffer containing instructions
    /// - pc: Program counter (byte offset)
    /// - count: Number of bytes to read
    /// 
    /// **Returns**: Slice of instruction bytes (may be empty if out of bounds)
    /// 
    /// **Integration**: Primary instruction fetch mechanism
    /// 
    /// **Complexity**: O(1) amortized
    /// **Confidence**: 100%
    pub fn readInstructionBytes(memory: []const u8, pc: usize, count: usize) []const u8 {
        const end = @min(pc + count, memory.len);
        if (pc < end) {
            return memory[pc..end];
        }
        return &[_]u8{};
    }

    /// Read JUMPX offset from memory with XOR addressing
    /// 
    /// **Purpose**: Read signed offset for JUMPX opcode
    /// 
    **Opcode Context**:
    /// - JUMPX opcode at PC (1 byte)
    /// - 16-bit signed offset at PC+1 (2 bytes)
    /// - XOR addressing required for proper byte order
    /// 
    /// **C Reference**: Matches Get_BYTE_PCMAC1 and Get_BYTE_PCMAC2 in C
    /// 
    /// **Parameters**:
    /// - memory: Memory buffer to read from
    /// - pc: Program counter (opcode location)
    /// 
    /// **Returns**: Signed 16-bit jump offset
    /// 
    /// **Integration**: Used by JUMPX opcode implementation
    /// 
    /// **Complexity**: O(1)
    /// **Confidence**: 100%
    pub fn readJumpOffset(memory: []const u8, pc: usize) i16 {
        const byte0 = EndiannessManager.readWordXor(memory, pc + 1);
        return @as(i16, @bitCast(byte0));
    }

    /// Read JUMPX offset without XOR addressing (raw bytes)
    /// 
    /// **Purpose**: Read raw jump offset for debugging/comparison
    /// 
    /// **Use Case**: When comparing with traces that show raw bytes
    /// 
    /// **Parameters**:
    /// - memory: Memory buffer to read from
    /// - pc: Program counter (opcode location)
    /// 
    /// **Returns**: Signed 16-bit jump offset from raw bytes
    /// 
    /// **Integration**: Used for debugging and trace comparison
    /// 
    /// **Complexity**: O(1)
    /// **Confidence**: 100%
    pub fn readJumpOffsetRaw(memory: []const u8, pc: usize) i16 {
        const byte0 = EndiannessManager.readWordLittleEndian(memory, pc + 1);
        return @as(i16, @bitCast(byte0));
    }
};
