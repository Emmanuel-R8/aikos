const std = @import("std");
const types = @import("../utils/types.zig");

const LispPTR = types.LispPTR;

/// Memory region offsets (matches C defines)
/// Per data-model.md
pub const MemoryOffsets = struct {
    pub const IFPAGE_OFFSET: u32 = 0x00000000;
    pub const STK_OFFSET: u32 = 0x00010000;
    pub const ATMHT_OFFSET: u32 = 0x00020000;
    pub const ATOMS_OFFSET: u32 = 0x00030000;
    pub const MDS_OFFSET: u32 = 0x00100000;
    pub const DS_OFFSET: u32 = 0x00200000;
    pub const VS_OFFSET: u32 = 0x00300000;
    pub const CS_OFFSET: u32 = 0x00400000;
    pub const SS_OFFSET: u32 = 0x00500000;
    pub const TS_OFFSET: u32 = 0x00600000;
    pub const PS_OFFSET: u32 = 0x00700000;
    pub const ES_OFFSET: u32 = 0x00800000;
    pub const FS_OFFSET: u32 = 0x00900000;
    pub const GS_OFFSET: u32 = 0x00A00000;
    pub const HS_OFFSET: u32 = 0x00B00000;
    pub const IS_OFFSET: u32 = 0x00C00000;
    pub const JS_OFFSET: u32 = 0x00D00000;
    pub const KS_OFFSET: u32 = 0x00E00000;
    pub const LS_OFFSET: u32 = 0x00F00000;
};

/// Page size constants (matches BYTESPER_PAGE = 512 bytes)
pub const PAGE_SIZE: u32 = 512; // 512 bytes per page
pub const PAGE_MASK: u32 = PAGE_SIZE - 1;

/// Get page number from LispPTR (LAddr).
///
/// C reference: `NativeAligned4FromLPage(LPage)` uses `LPage << 8`, i.e. one page = 256 DLwords.
/// Since LispPTR is a DLword offset, the page number is:
///   page = (POINTERMASK & addr) >> 8
pub fn getPageNumber(addr: LispPTR) u32 {
    const masked: u32 = addr & types.POINTERMASK;
    return masked >> 8;
}

/// Get byte offset within page from LispPTR (LAddr).
/// Low 8 bits are DLword offset within a 256-DLword page; convert to bytes by *2.
pub fn getPageOffset(addr: LispPTR) u32 {
    const masked: u32 = addr & types.POINTERMASK;
    const dlword_off: u32 = masked & 0xFF;
    return dlword_off * 2;
}
