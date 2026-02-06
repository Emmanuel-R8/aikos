// Critical constants matching C/Zig implementations
// These must match exactly for compatibility

/**
 * IFPAGE validation key (CRITICAL: Must match C implementation)
 * See maiko/inc/ifpage.h:15
 */
export const IFPAGE_KEYVAL = 0x15e3;

/**
 * IFPAGE address offset from start of sysout file
 * IFPAGE is located at 512 bytes from the start
 */
export const IFPAGE_ADDRESS = 512;

/**
 * Bytes per page (512 bytes = 256 DLwords)
 * Virtual pages are 512-byte pages, not DLword pages
 */
export const BYTESPER_PAGE = 512;

/**
 * DLwords per page
 */
export const DLWORDS_PER_PAGE = 256;

/**
 * Stack offset from Lisp_world (DLword offset)
 * Stack area starts at DLword offset 0x00010000
 */
export const STK_OFFSET = 0x00010000;

/**
 * Frame size in DLwords
 * Each stack frame is 10 DLwords (20 bytes)
 */
export const FRAMESIZE = 10;

/**
 * Frame size in bytes
 */
export const FRAMESIZE_BYTES = 20;

/**
 * NIL pointer (0)
 * Per C: NIL_PTR = 0
 */
export const NIL_PTR = 0;

/**
 * T atom pointer (non-zero, typically 1 in simplified contexts)
 * Per C: ATOM_T
 */
export const ATOM_T = 1;

/**
 * Machine type for Maiko emulator
 * See maiko/inc/ifpage.h:14
 */
export const MACHINETYPE_MAIKO = 3;

/**
 * AtomSpace offset (DLword offset from Lisp_world)
 * Per maiko/inc/lispmap.h
 * BIGVM: 0x2c0000 (44 * 0x10000)
 * Non-BIGVM: 0x80000 (8 * 0x10000)
 * For now, assume BIGVM (most common)
 */
export const ATOMS_OFFSET = 0x2c0000;

/**
 * Defspace offset (DLword offset from Lisp_world)
 * Per maiko/inc/lispmap.h
 * BIGVM: 0xA0000 (10 * 0x10000)
 */
export const DEFS_OFFSET = 0xA0000;

/**
 * Valspace offset (DLword offset from Lisp_world)
 * Per maiko/inc/lispmap.h
 * BIGVM: 0xC0000 (12 * 0x10000)
 */
export const VALS_OFFSET = 0xC0000;
