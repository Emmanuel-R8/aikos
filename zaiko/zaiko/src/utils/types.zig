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