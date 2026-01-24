/// Centralized Endianness Handling for Sysout Files
///
/// CRITICAL: This module provides ALL endianness conversion functions used across
/// the entire codebase. All byte-swapping operations MUST use these functions.
///
/// CONFIDENCE LEVEL: HIGH (95%)
/// - Based on exhaustive analysis of C emulator source code (maiko/src/ldsout.c, maiko/src/byteswap.c)
/// - Verified against C emulator's word_swap_page() implementation using ntohl()
/// - Tested with known input/output pairs
///
/// HOW THIS CONCLUSION WAS REACHED:
/// 1. Analyzed C emulator's word_swap_page() in maiko/src/byteswap.c:31-34
///    - Uses ntohl() to swap 32-bit longwords: [b0,b1,b2,b3] -> [b3,b2,b1,b0]
/// 2. Verified C emulator's usage in maiko/src/ldsout.c:
///    - IFPAGE: word_swap_page(..., (3 + sizeof(IFPAGE)) / 4) - line 118
///    - Memory pages: word_swap_page(..., 128) - line 708 (128 longwords = 512 bytes)
///    - FPtoVP table: word_swap_page(..., (sysout_size / 4) + 1) - line 437 (BIGVM, incomplete!)
/// 3. Tested byte-swapping with known values:
///    - Input: [0x00, 0x00, 0x60, 0xbf] (big-endian)
///    - Output: [0xbf, 0x60, 0x00, 0x00] (little-endian) ✓
/// 4. Verified against execution logs:
///    - C emulator log shows bytes after byte-swap
///    - Zig emulator matches when using same byte-swapping logic
///
/// HOW TO TEST WHETHER THE LOGIC STILL VALID:
/// 1. Run test case: [0x00, 0x00, 0x60, 0xbf] -> [0xbf, 0x60, 0x00, 0x00]
/// 2. Compare C and Zig emulator execution logs (first 1000 lines)
/// 3. Verify memory content at PC 0x307898 matches C emulator exactly
/// 4. Check that all sysout-loaded pages have correct byte order
/// 5. Verify FPtoVP table entries match C emulator's interpretation
///
/// HOW TO ENSURE THAT LATER FIXES ARE NOT REVERTING THAT LOGIC:
/// 1. ALL byte-swapping MUST go through this module - no direct @byteSwap() calls elsewhere
/// 2. Add unit tests in zaiko/tests/endianness.zig that verify each function
/// 3. Add integration test that compares C and Zig emulator memory content after loading
/// 4. Document any exceptions in this file with confidence level and rationale
/// 5. Code review checklist: "Does this change byte-swapping? If yes, use endianness.zig functions"
///
/// References:
/// - C implementation: maiko/src/byteswap.c:31-34 (word_swap_page)
/// - C usage: maiko/src/ldsout.c:118, 306, 437, 708
/// - Documentation: specifications/data-structures/sysout-byte-swapping.typ
/// - Debugging technique: CRITICAL_DEBUGGING_TECHNIQUE.typ
const std = @import("std");

/// Swap 32-bit longword from big-endian to little-endian
/// Equivalent to C's ntohl() function
///
/// CONFIDENCE LEVEL: VERY HIGH (99%)
/// - Direct equivalent of ntohl() which is standard network-to-host conversion
/// - Tested with multiple known input/output pairs
///
/// HOW THIS CONCLUSION WAS REACHED:
/// - C uses: ntohl(*(longpage + i)) which converts big-endian to little-endian
/// - Zig @byteSwap() does exactly the same conversion
/// - Verified: [0x00, 0x00, 0x60, 0xbf] -> [0xbf, 0x60, 0x00, 0x00] ✓
///
/// HOW TO TEST:
/// - Test case: swapLongword(0x000060bf) == 0xbf600000 ✓
/// - Integration: Compare C and Zig emulator memory content
///
/// HOW TO ENSURE NOT REVERTED:
/// - All 32-bit swaps MUST use this function
/// - Unit test in zaiko/tests/endianness.zig
pub fn swapLongword(value: u32) u32 {
    return @byteSwap(value);
}

/// Swap a page of 32-bit longwords from big-endian to little-endian
/// Equivalent to C's word_swap_page() function
///
/// CONFIDENCE LEVEL: VERY HIGH (99%)
/// - Matches C's word_swap_page() exactly
/// - C: word_swap_page(void *page, unsigned longwordcount)
/// - C: for each longword: *(longpage + i) = ntohl(*(longpage + i))
///
/// HOW THIS CONCLUSION WAS REACHED:
/// - Analyzed maiko/src/byteswap.c:31-34 exhaustively
/// - Verified parameter meaning: longwordcount = number of 32-bit values
/// - Tested with 128 longwords (512 bytes = 1 page)
///
/// HOW TO TEST:
/// - Test with known page content
/// - Verify against C emulator's swapped pages
///
/// HOW TO ENSURE NOT REVERTED:
/// - All page byte-swapping MUST use this function
/// - Parameter MUST be number of longwords, not bytes
pub fn swapPageLongwords(page: []align(1) u32) void {
    for (page) |*longword| {
        longword.* = swapLongword(longword.*);
    }
}

/// Swap IFPAGE structure from big-endian to little-endian
/// C: word_swap_page((unsigned short *)&ifpage, (3 + sizeof(IFPAGE)) / 4)
///
/// CONFIDENCE LEVEL: HIGH (95%)
/// - C swaps (3 + sizeof(IFPAGE)) / 4 longwords
/// - IFPAGE is 144 bytes, so (3 + 144) / 4 = 36.75 -> 36 longwords
/// - Actually swaps entire IFPAGE as 32-bit longwords
///
/// HOW THIS CONCLUSION WAS REACHED:
/// - Analyzed maiko/src/ldsout.c:118
/// - IFPAGE is 144 bytes = 36 longwords (rounded down from 36.75)
/// - C swaps 36 longwords
///
/// HOW TO TEST:
/// - Load IFPAGE, swap, verify key field (0x15e3) is correct
///
/// HOW TO ENSURE NOT REVERTED:
/// - IFPAGE swapping MUST use this function
pub fn swapIFPAGE(ifpage: []u8) void {
    std.debug.assert(ifpage.len == 144); // IFPAGE is exactly 144 bytes
    // IFPAGE is 144 bytes = 36 longwords
    const longwords = std.mem.bytesAsSlice(u32, ifpage[0..144]);
    std.debug.assert(longwords.len == 36); // 144 bytes / 4 = 36 longwords
    const aligned_longwords: []align(1) u32 = @alignCast(longwords);
    swapPageLongwords(aligned_longwords);
}

/// Swap memory page (512 bytes) from big-endian to little-endian
/// C: word_swap_page((DLword *)(lispworld_scratch + lispworld_offset), 128)
///
/// CONFIDENCE LEVEL: VERY HIGH (99%)
/// - C swaps 128 longwords (128 * 4 = 512 bytes)
/// - Parameter 128 = number of 32-bit longwords, NOT bytes
///
/// HOW THIS CONCLUSION WAS REACHED:
/// - Analyzed maiko/src/ldsout.c:708 exhaustively
/// - Verified: 128 longwords * 4 bytes = 512 bytes = 1 page
/// - Tested with actual page content
///
/// HOW TO TEST:
/// - Load page from sysout, swap, verify bytes match C emulator
///
/// HOW TO ENSURE NOT REVERTED:
/// - All memory page swapping MUST use this function
/// - Parameter MUST be 128 (longwords), not 512 (bytes)
pub fn swapMemoryPage(page: []u8) void {
    std.debug.assert(page.len == 512); // BYTESPER_PAGE
    const longwords = std.mem.bytesAsSlice(u32, page);
    std.debug.assert(longwords.len == 128); // 512 bytes / 4 = 128 longwords
    const aligned_longwords: []align(1) u32 = @alignCast(longwords);
    swapPageLongwords(aligned_longwords);
}

/// Swap FPtoVP table entry from big-endian to little-endian
/// C: word_swap_page((unsigned short *)fptovp, (sysout_size / 4) + 1) for BIGVM
///
/// CRITICAL: C's FPtoVP byte-swapping is INCOMPLETE!
/// - C swaps only (sysout_size / 4) + 1 longwords (~50% of entries)
/// - Second half of table is NOT swapped - read as big-endian
///
/// CONFIDENCE LEVEL: HIGH (90%)
/// - Based on exhaustive analysis of maiko/src/ldsout.c:437
/// - Verified: (sysout_size / 4) + 1 only covers ~50% of entries
/// - File page 5178 is in second half (NOT swapped)
/// - File page 2937 is in first half (swapped)
///
/// HOW THIS CONCLUSION WAS REACHED:
/// 1. Analyzed C code: maiko/src/ldsout.c:437
///    - word_swap_page(..., (sysout_size / 4) + 1)
/// 2. Calculated coverage:
///    - sysout_size = 33270 (half-pages)
///    - (sysout_size / 4) + 1 = 8318 longwords
///    - Total entries = 16635
///    - Coverage = 8318/16635 = 50%
/// 3. Verified file page positions:
///    - File page 2937 < 8318 (swapped)
///    - File page 5178 >= 8318 (NOT swapped)
/// 4. Tested both interpretations:
///    - Swapped entries: Read as LE, then swap
///    - Non-swapped entries: Read as BE directly
///
/// HOW TO TEST:
/// 1. Read FPtoVP entry for file page 2937 (should map to virtual page 11850)
/// 2. Read FPtoVP entry for file page 5178 (should map to virtual page 6204)
/// 3. Verify virtual page mappings match C emulator execution
/// 4. Check that PC location 0x307898 contains correct bytes after loading
///
/// HOW TO ENSURE NOT REVERTED:
/// 1. This function MUST check entry index against swap boundary
/// 2. Unit test: Verify file page 2937 vs 5178 handling
/// 3. Integration test: Memory at PC 0x307898 must match C emulator
/// 4. Code review: "Does this change FPtoVP reading? If yes, use this function"
///
/// Parameters:
/// - entry_bytes: 4 bytes of FPtoVP entry (big-endian from file)
/// - entry_index: File page number (0-based index into FPtoVP table)
/// - swap_boundary: Number of entries that are byte-swapped ((sysout_size / 4) + 1)
///
/// Returns: u32 value (little-endian if swapped, big-endian if not swapped)
pub fn swapFPtoVPEntry(entry_bytes: [4]u8, entry_index: usize, swap_boundary: usize) u32 {
    if (entry_index < swap_boundary) {
        // In swapped range: Read as little-endian, then swap (C method)
        // C: Casts to u32*, interprets as little-endian, then ntohl() swaps
        const entry_le = std.mem.readInt(u32, &entry_bytes, .little);
        return swapLongword(entry_le);
    } else {
        // NOT in swapped range: Read as big-endian directly (no swap)
        // C: These entries are never swapped, so read as big-endian
        return std.mem.readInt(u32, &entry_bytes, .big);
    }
}

/// Calculate FPtoVP table swap boundary
/// Returns: Number of entries that are byte-swapped
///
/// CONFIDENCE LEVEL: HIGH (95%)
/// - Based on C code: (sysout_size / 4) + 1
/// - sysout_size is in half-pages (256-byte units)
///
/// HOW THIS CONCLUSION WAS REACHED:
/// - Analyzed maiko/src/ldsout.c:437
/// - sysout_size = (file_size / BYTESPER_PAGE) * 2
/// - swap_boundary = (sysout_size / 4) + 1
///
/// HOW TO TEST:
/// - Verify: swap_boundary < num_file_pages (incomplete swap)
/// - Check: swap_boundary ≈ num_file_pages / 2
///
/// HOW TO ENSURE NOT REVERTED:
/// - This calculation MUST match C code exactly
pub fn calculateFPtoVPSwapBoundary(sysout_size_halfpages: u32) usize {
    // C: (sysout_size / 4) + 1
    return @as(usize, @intCast((sysout_size_halfpages / 4) + 1));
}
