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
 * Interface Page structure (matches C IFPAGE)
 * This is the BYTESWAP version for little-endian machines
 * See maiko/inc/ifpage.h lines 257-328 for the BYTESWAP struct definition
 */
export interface IFPAGE {
    // Frame pointers (BYTESWAP order)
    resetfxp: DLword;
    currentfxp: DLword; // hi word
    kbdfxp: DLword;
    subovfxp: DLword; // hi word
    gcfxp: DLword;
    hardreturnfxp: DLword; // hi word
    endofstack: DLword;
    faultfxp: DLword; // hi word

    // Version information
    minrversion: DLword;
    lversion: DLword; // hi word
    rversion: DLword;
    minbversion: DLword; // hi word
    machinetype: DLword;
    bversion: DLword; // hi word

    // Validation key (must be IFPAGE_KEYVAL = 0x15e3)
    key: DLword;
    miscfxp: DLword; // hi word
    emulatorspace: DLword;
    serialnumber: DLword; // hi word
    nxtpmaddr: DLword;
    screenwidth: DLword; // hi word

    // Page management
    ndirtypages: DLword;
    nactivepages: DLword; // hi word
    filepnpmt0: DLword;
    filepnpmp0: DLword; // hi word
    filler1: DLword;
    teleraidfxp: DLword; // hi word
    filler3: DLword;
    filler2: DLword; // hi word

    // User information
    userpswdaddr: DLword;
    usernameaddr: DLword; // hi word
    faulthi: DLword;
    stackbase: DLword; // hi word
    devconfig: DLword;
    faultlo: DLword; // hi word
    rpoffset: DLword;
    rptsize: DLword; // hi word
    embufvp: DLword;
    wasrptlast: DLword; // hi word
    nshost1: DLword;
    nshost0: DLword; // hi word
    mdszone: DLword;
    nshost2: DLword; // hi word
    emubuffers: DLword;
    mdszonelength: DLword; // hi word
    process_size: DLword;
    emubuflength: DLword; // hi word
    isfmap: DLword;
    storagefullstate: DLword; // hi word

    // Misc apply
    miscstackfn: LispPTR;
    miscstackarg1: LispPTR;
    miscstackarg2: LispPTR;
    miscstackresult: LispPTR;

    // Page management continued
    lastlockedfilepage: DLword;
    nrealpages: DLword; // hi word
    fptovpstart: DLword;
    lastdominofilepage: DLword; // hi word
    dl24bitaddressable: DLword;
    fakemousebits: DLword; // hi word
    realpagetableptr: LispPTR;
    fullspaceused: DLword;
    dllastvmempage: DLword; // hi word
    fakekbdad5: DLword;
    fakekbdad4: DLword; // hi word
}
