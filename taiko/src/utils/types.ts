// Core types for Taiko VM implementation
// All types must maintain exact compatibility with C implementation

/**
 * 32-bit virtual address (matches C LispPTR)
 * In JavaScript, numbers are safe for 32-bit integers (up to 2^53-1)
 */
export type LispPTR = number;

/**
 * 16-bit word (matches C DLword)
 * Stored as number, but values must be in 0-65535 range
 */
export type DLword = number;

/**
 * 8-bit byte
 */
export type Byte = number;

/**
 * Bytecode instruction byte
 */
export type ByteCode = number;

/**
 * Get high word from LispPTR (matches C GetHiWord)
 * C: GetHiWord(x) = ((DLword)((x) >> 16))
 */
export function getHiWord(value: LispPTR): DLword {
    return (value >> 16) & 0xFFFF;
}

/**
 * Get low word from LispPTR (matches C GetLoWord)
 * C: GetLoWord(x) = ((DLword)(x))
 */
export function getLoWord(value: LispPTR): DLword {
    return value & 0xFFFF;
}

/**
 * Combine two DLwords into a LispPTR (matches C MakeLispPTR)
 */
export function makeLispPTR(hi: DLword, lo: DLword): LispPTR {
    return ((hi & 0xFFFF) << 16) | (lo & 0xFFFF);
}

/**
 * Interface Page structure as parsed directly from the big-endian sysout file.
 * This follows the logical on-disk field order, not Maiko's host-side
 * BYTESWAP in-memory struct layout.
 */
export interface IFPAGE {
    // Frame pointers (file order)
    currentfxp: DLword;
    resetfxp: DLword;
    subovfxp: DLword;
    kbdfxp: DLword;
    hardreturnfxp: DLword;
    gcfxp: DLword;
    faultfxp: DLword;
    endofstack: DLword;

    // Version information
    lversion: DLword;
    minrversion: DLword;
    minbversion: DLword;
    rversion: DLword;
    bversion: DLword;
    machinetype: DLword;

    // Validation key (must be IFPAGE_KEYVAL = 0x15e3)
    miscfxp: DLword;
    key: DLword;
    serialnumber: DLword;
    emulatorspace: DLword;
    screenwidth: DLword;
    nxtpmaddr: DLword;

    // Page management
    nactivepages: DLword;
    ndirtypages: DLword;
    filepnpmp0: DLword;
    filepnpmt0: DLword;
    filler1: DLword;
    teleraidfxp: DLword;
    filler2: DLword;
    filler3: DLword;

    // User information
    usernameaddr: DLword;
    userpswdaddr: DLword;
    stackbase: DLword;
    faulthi: DLword;
    devconfig: DLword;
    faultlo: DLword;
    rptsize: DLword;
    rpoffset: DLword;
    wasrptlast: DLword;
    embufvp: DLword;
    nshost0: DLword;
    nshost1: DLword;
    nshost2: DLword;
    mdszone: DLword;
    mdszonelength: DLword;
    emubuffers: DLword;
    process_size: DLword;
    emubuflength: DLword;
    storagefullstate: DLword;
    isfmap: DLword;

    // Misc apply
    miscstackfn: LispPTR;
    miscstackarg1: LispPTR;
    miscstackarg2: LispPTR;
    miscstackresult: LispPTR;

    // Page management continued
    nrealpages: DLword;
    lastlockedfilepage: DLword;
    lastdominofilepage: DLword;
    fptovpstart: DLword;
    fakemousebits: DLword;
    dl24bitaddressable: DLword;
    realpagetableptr: LispPTR;
    dllastvmempage: DLword;
    fullspaceused: DLword;
    fakekbdad4: DLword;
    fakekbdad5: DLword;
}
