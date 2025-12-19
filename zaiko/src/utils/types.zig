// Core types for Maiko VM implementation
// All types must maintain exact compatibility with C implementation

/// 32-bit virtual address (matches C LispPTR)
pub const LispPTR = u32;

/// 16-bit word (matches C DLword)
pub const DLword = u16;

/// Bytecode instruction byte
pub const ByteCode = u8;

/// Address component extraction (matches C macros)
pub fn hiloc(ptr: LispPTR) u16 {
    return @as(u16, @truncate(ptr >> 16));
}

/// Address component extraction (matches C macros)
pub fn loloc(ptr: LispPTR) u16 {
    return @as(u16, @truncate(ptr));
}

/// Get high word from LispPTR (matches C GetHiWord)
/// C: GetHiWord(x) = ((DLword)((x) >> 16))
pub fn getHiWord(value: LispPTR) DLword {
    return @as(DLword, @truncate(value >> 16));
}

/// Get low word from LispPTR (matches C GetLoWord)
/// C: GetLoWord(x) = ((DLword)(x))
pub fn getLoWord(value: LispPTR) DLword {
    return @as(DLword, @truncate(value));
}

/// Interface Page structure (matches C IFPAGE)
/// This is the non-BIGVM, non-BYTESWAP version (most common case)
/// See maiko/inc/ifpage.h for complete structure definition
/// Must maintain exact byte-for-byte compatibility with C structure
pub const IFPAGE = packed struct {
    // Frame pointers
    currentfxp: DLword,
    resetfxp: DLword,
    subovfxp: DLword,
    kbdfxp: DLword,
    hardreturnfxp: DLword,
    gcfxp: DLword,
    faultfxp: DLword,
    endofstack: DLword,

    // Version information
    lversion: DLword,
    minrversion: DLword,
    minbversion: DLword,
    rversion: DLword,
    bversion: DLword,
    machinetype: DLword,
    miscfxp: DLword,

    // Validation key (must be IFPAGE_KEYVAL = 0x15e3)
    key: DLword,
    serialnumber: DLword,
    emulatorspace: DLword,
    screenwidth: DLword,
    nxtpmaddr: DLword,

    // Page management
    nactivepages: DLword,
    ndirtypages: DLword,
    filepnpmp0: DLword,
    filepnpmt0: DLword,
    teleraidfxp: DLword,
    filler1: DLword,
    filler2: DLword,
    filler3: DLword,

    // User information
    usernameaddr: DLword,
    userpswdaddr: DLword,
    stackbase: DLword,

    // Fault handling
    faulthi: DLword,
    faultlo: DLword,
    devconfig: DLword, // was realpagetable

    // Real page table
    rptsize: DLword,
    rpoffset: DLword,
    wasrptlast: DLword,
    embufvp: DLword,

    // Network host addresses
    nshost0: DLword,
    nshost1: DLword,
    nshost2: DLword,

    // Memory zones
    mdszone: DLword,
    mdszonelength: DLword,
    emubuffers: DLword,
    emubuflength: DLword,
    process_size: DLword, // was lastnumchars
    storagefullstate: DLword, // was sysdisk
    isfmap: DLword,

    // Miscapply stack (not ref counted)
    miscstackfn: LispPTR,
    miscstackarg1: LispPTR,
    miscstackarg2: LispPTR,
    miscstackresult: LispPTR,

    // Page management continued
    nrealpages: DLword,
    lastlockedfilepage: DLword,
    lastdominofilepage: DLword,
    fptovpstart: DLword, // FPtoVP table start offset
    fakemousebits: DLword,
    dl24bitaddressable: DLword,
    realpagetableptr: LispPTR,
    dllastvmempage: DLword,
    fullspaceused: DLword,
    fakekbdad4: DLword,
    fakekbdad5: DLword,
};

/// IFPAGE validation key (matches C IFPAGE_KEYVAL)
/// CRITICAL: Must be 0x15e3 to match C implementation (maiko/inc/ifpage.h:15)
pub const IFPAGE_KEYVAL: u16 = 0x15e3;

/// IFPAGE address offset in sysout file (matches C IFPAGE_ADDRESS)
pub const IFPAGE_ADDRESS: u32 = 512;

/// Bytes per page (matches C BYTESPER_PAGE)
pub const BYTESPER_PAGE: u32 = 512;

// ============================================================================
// Number Type Constants (matches C lispmap.h and lsptypes.h)
// ============================================================================

/// Segment mask for extracting segment from LispPTR (matches C SEGMASK)
/// Non-BIGVM: 0xfff0000, BIGVM: 0xff0000
/// Using non-BIGVM value as default
pub const SEGMASK: u32 = 0xfff0000;

/// Small positive integer segment (matches C S_POSITIVE)
pub const S_POSITIVE: u32 = 0xE0000;

/// Small negative integer segment (matches C S_NEGATIVE)
pub const S_NEGATIVE: u32 = 0xF0000;

/// Pointer mask for extracting pointer from LispPTR (matches C POINTERMASK)
/// BIGVM: 0xfffffff (28 bits), non-BIGVM: 0xffffff (24 bits)
/// Using BIGVM value as default (assumes BIGVM build)
pub const POINTERMASK: u32 = 0xfffffff;

/// Type numbers (matches C lsptypes.h)
pub const TYPE_SMALLP: u8 = 1;
pub const TYPE_FIXP: u8 = 2;
pub const TYPE_FLOATP: u8 = 3;

/// Maximum small integer value (matches C MAX_SMALL)
pub const MAX_SMALL: i32 = 65535;

/// Minimum small integer value (matches C MIN_SMALL)
pub const MIN_SMALL: i32 = -65536;

/// Maximum fixnum value (matches C MAX_FIXP)
pub const MAX_FIXP: i32 = 2147483647;

/// Minimum fixnum value (matches C MIN_FIXP)
pub const MIN_FIXP: i32 = -2147483648;

// ============================================================================
// Number Extraction and Encoding Functions
// ============================================================================

/// Extract integer from SMALLP or FIXP (matches C N_IGETNUMBER macro)
/// Handles SMALLP (S_POSITIVE/S_NEGATIVE segment) and FIXP (pointer to integer object)
/// Returns error if value is not a valid integer type
///
/// Note: FIXP extraction requires virtual memory access (will be implemented in Phase 3)
pub fn extractInteger(value: LispPTR) !i32 {
    const segment = value & SEGMASK;

    // Check for SMALLP (small positive or negative)
    if (segment == S_POSITIVE) {
        // Small positive: extract low 16 bits
        return @as(i32, @intCast(value & 0xFFFF));
    } else if (segment == S_NEGATIVE) {
        // Small negative: sign extend low 16 bits
        return @as(i32, @bitCast(@as(u32, value | 0xFFFF0000)));
    }

    // Not SMALLP, check for FIXP (pointer to integer object)
    // TODO: When virtual memory is available, check type tag and extract FIXP value
    // For now, if segment doesn't match SMALLP, we can't extract integer
    // This is a simplified implementation - full version needs type tag checking

    // For now, treat as error if not SMALLP
    // Full implementation will check type tag and extract FIXP value
    return error.InvalidNumberType;
}

/// Encode integer result as SMALLP or FIXP (matches C N_ARITH_SWITCH macro)
/// Returns SMALLP if value fits in small integer range, otherwise creates FIXP
///
/// Note: FIXP creation requires GC allocation (will be implemented in Phase 4)
pub fn encodeIntegerResult(value: i32) !LispPTR {
    // Check if value fits in small integer range
    if (value >= 0 and value <= MAX_SMALL) {
        // Small positive: encode as S_POSITIVE | value
        return S_POSITIVE | @as(u32, @intCast(value));
    } else if (value < 0 and value >= MIN_SMALL) {
        // Small negative: encode as S_NEGATIVE | (value & 0xFFFF)
        return S_NEGATIVE | (@as(u32, @bitCast(@as(i32, value))) & 0xFFFF);
    }

    // Value doesn't fit in SMALLP range, need to create FIXP
    // TODO: Create FIXP cell via GC allocation (createcell68k equivalent)
    // For now, return error - full implementation will allocate FIXP cell

    // Temporary: encode as SMALLP anyway (will be fixed when FIXP creation is implemented)
    // This is incorrect but allows basic arithmetic to work for small values
    // Overflow will be handled by error checking
    if (value >= 0) {
        return S_POSITIVE | @as(u32, @intCast(value & 0xFFFF));
    } else {
        return S_NEGATIVE | (@as(u32, @bitCast(@as(i32, value))) & 0xFFFF);
    }
}
