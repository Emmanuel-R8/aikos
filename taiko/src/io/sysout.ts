// Sysout file loading and parsing
import type { IFPAGE } from '../utils/types';
import { IFPAGE_KEYVAL, IFPAGE_ADDRESS, BYTESPER_PAGE, STK_OFFSET, ATOMS_OFFSET, DEFS_OFFSET, VALS_OFFSET, PLIS_OFFSET, DTD_OFFSET } from '../utils/constants';
import { MemoryManager } from '../vm/memory/manager';
import type { LispPTR } from '../utils/types';

export interface SysoutLoadResult {
    virtualMemory: Uint8Array;
    fptovpTable: Uint32Array;
    ifpage: IFPAGE;
    initialPC: number;
    initialSP: number;
    atomSpaceOffset: number; // Byte offset of AtomSpace in virtual memory
    defSpaceOffset: number; // Byte offset of Defspace in virtual memory
    valSpaceOffset: number; // Byte offset of Valspace in virtual memory
    plistSpaceOffset: number; // Byte offset of PLISTSPACE in virtual memory
    dtdOffset: number; // Byte offset of DTD space in virtual memory
}

/**
 * Parse IFPAGE from sysout file
 * IFPAGE is located at offset 512 bytes from start
 */
function parseIFPAGE(buffer: ArrayBuffer, offset: number): IFPAGE | null {
    const view = new DataView(buffer, offset);

    // Read IFPAGE structure as big-endian (sysout files are big-endian)
    // Per C: word_swap_page() swaps bytes for BYTESWAP builds, but we read directly as big-endian
    // See maiko/inc/ifpage.h lines 257-328
    const ifpage: IFPAGE = {
        resetfxp: view.getUint16(0, false), // big-endian
        currentfxp: view.getUint16(2, false),
        kbdfxp: view.getUint16(4, false),
        subovfxp: view.getUint16(6, false),
        gcfxp: view.getUint16(8, false),
        hardreturnfxp: view.getUint16(10, false),
        endofstack: view.getUint16(12, false),
        faultfxp: view.getUint16(14, false),
        minrversion: view.getUint16(16, false),
        lversion: view.getUint16(18, false),
        rversion: view.getUint16(20, false),
        minbversion: view.getUint16(22, false),
        machinetype: view.getUint16(24, false),
        bversion: view.getUint16(26, false),
        key: view.getUint16(30, false), // big-endian: at offset 30 (0x1E), should read 0x15e3
        miscfxp: view.getUint16(28, false), // miscfxp is at offset 28
        emulatorspace: view.getUint16(32, false),
        serialnumber: view.getUint16(34, false),
        nxtpmaddr: view.getUint16(36, false),
        screenwidth: view.getUint16(38, false),
        ndirtypages: view.getUint16(40, false),
        nactivepages: view.getUint16(42, false),
        filepnpmt0: view.getUint16(44, false),
        filepnpmp0: view.getUint16(46, false),
        filler1: view.getUint16(48, false),
        teleraidfxp: view.getUint16(50, false),
        filler3: view.getUint16(52, false),
        filler2: view.getUint16(54, false),
        userpswdaddr: view.getUint16(56, false),
        usernameaddr: view.getUint16(58, false),
        faulthi: view.getUint16(60, false),
        stackbase: view.getUint16(62, false),
        devconfig: view.getUint16(64, false),
        faultlo: view.getUint16(66, false),
        rpoffset: view.getUint16(68, false),
        rptsize: view.getUint16(70, false),
        embufvp: view.getUint16(72, false),
        wasrptlast: view.getUint16(74, false),
        nshost1: view.getUint16(76, false),
        nshost0: view.getUint16(78, false),
        mdszone: view.getUint16(80, false),
        nshost2: view.getUint16(82, false),
        emubuffers: view.getUint16(84, false),
        mdszonelength: view.getUint16(86, false),
        process_size: view.getUint16(88, false),
        emubuflength: view.getUint16(90, false),
        isfmap: view.getUint16(92, false),
        storagefullstate: view.getUint16(94, false),
        miscstackfn: view.getUint32(96, false), // big-endian
        miscstackarg1: view.getUint32(100, false),
        miscstackarg2: view.getUint32(104, false),
        miscstackresult: view.getUint32(108, false),
        lastlockedfilepage: view.getUint16(112, false),
        nrealpages: view.getUint16(114, false),
        fptovpstart: view.getUint16(116, false),
        lastdominofilepage: view.getUint16(118, false),
        dl24bitaddressable: view.getUint16(120, false),
        fakemousebits: view.getUint16(122, false),
        realpagetableptr: view.getUint32(124, false), // big-endian
        fullspaceused: view.getUint16(128, false),
        dllastvmempage: view.getUint16(130, false),
        fakekbdad5: view.getUint16(132, false),
        fakekbdad4: view.getUint16(134, false),
    };

    // Validate IFPAGE key
    if (ifpage.key !== IFPAGE_KEYVAL) {
        throw new Error(`Invalid IFPAGE key: expected 0x${IFPAGE_KEYVAL.toString(16)}, got 0x${ifpage.key.toString(16)}`);
    }

    return ifpage;
}

/**
 * Load FPtoVP table from sysout file
 * Per C: sysout_loader() reads FPtoVP table from fptovpstart
 *
 * FPtoVP table format:
 * - Location: (fptovpstart - 1) * BYTESPER_PAGE + offset
 * - BIGVM: offset = 4, size = sysout_size * 2 + 4 bytes
 * - Non-BIGVM: offset = 2, size = sysout_size + 2 bytes
 * - Each entry is 32-bit (BIGVM) or 16-bit (non-BIGVM)
 */
function loadFPtoVPTable(buffer: ArrayBuffer, ifpage: IFPAGE, fileSize: number): Uint32Array {
    // Calculate sysout size in "half-pages" (256-byte units)
    // Per C: sysout_size = stat_buf.st_size / BYTESPER_PAGE * 2
    // This gives the number of half-pages (256-byte units)
    const sysoutSize = Math.floor((fileSize / BYTESPER_PAGE) * 2);

    // Get FPtoVP table location from IFPAGE
    // Per C: fptovp_offset = (ifpage.fptovpstart - 1) * BYTESPER_PAGE + offset
    // For BIGVM: offset = 4, for non-BIGVM: offset = 2
    // We'll assume BIGVM format (32-bit entries)
    const fptovpStart = ifpage.fptovpstart;
    const fptovpOffset = (fptovpStart - 1) * BYTESPER_PAGE + 4; // BIGVM format

    // FPtoVP table size: sysout_size * 2 + 4 bytes (BIGVM)
    // Per C BIGVM: fptovp = malloc(sysout_size * 2 + 4)
    // sysout_size is in half-pages, so sysout_size * 2 gives bytes
    // This matches: num_pages * 4 bytes (one 32-bit entry per page)
    const fptovpSize = sysoutSize * 2 + 4;

    // Don't read beyond file bounds
    const maxReadSize = buffer.byteLength - fptovpOffset;
    const actualReadSize = Math.min(fptovpSize, maxReadSize);

    if (actualReadSize < 4) {
        throw new Error(`FPtoVP table too small: offset=${fptovpOffset}, available=${maxReadSize}`);
    }

    // Read FPtoVP table from buffer
    // We need to read as big-endian and then byte-swap
    const fptovpBuffer = buffer.slice(fptovpOffset, fptovpOffset + actualReadSize);
    const numEntries = Math.floor(actualReadSize / 4);
    const fptovpTable = new Uint32Array(numEntries);

    // Read entries as big-endian and byte-swap
    const view = new DataView(buffer, fptovpOffset, actualReadSize);
    for (let i = 0; i < numEntries; i++) {
        // Read as big-endian from sysout file
        const bigEndianValue = view.getUint32(i * 4, false);
        // Swap to little-endian for our use
        fptovpTable[i] = MemoryManager.Endianness.swapU32(bigEndianValue);
    }


    return fptovpTable;
}

/**
 * Load virtual memory from sysout file pages using FPtoVP table
 * Per C: sysout_loader() loads pages using FPtoVP mapping
 *
 * Algorithm:
 *   1. Calculate virtual memory size from process_size
 *   2. For each file page i:
 *      a. Check GETPAGEOK(fptovp, i) != 0xFFFF (page present)
 *      b. Get virtual page: GETFPTOVP(fptovp, i)
 *      c. Read page from file at offset i * BYTESPER_PAGE
 *      d. Write page to virtual memory at vpage * BYTESPER_PAGE
 *      e. Byte-swap page if needed
 */
function loadVirtualMemory(buffer: ArrayBuffer, fptovpTable: Uint32Array, ifpage: IFPAGE): Uint8Array {
    // Calculate virtual memory size from process_size
    // Per C: process_size is in MB, convert to bytes
    // If process_size is 0, use default (64MB for pure sysout)
    let virtualMemorySizeMB = ifpage.process_size;
    if (virtualMemorySizeMB === 0) {
        virtualMemorySizeMB = 64; // DEFAULT_MAX_SYSOUTSIZE
    }
    const virtualMemorySize = virtualMemorySizeMB * 1024 * 1024;
    const virtualMemory = new Uint8Array(virtualMemorySize);

    // Calculate sysout size in "half-pages" (256-byte units)
    // Per C: sysout_size = stat_buf.st_size / BYTESPER_PAGE * 2
    const sysoutSize = Math.floor((buffer.byteLength / BYTESPER_PAGE) * 2);

    // Load pages from sysout file using FPtoVP mapping
    // Per C: for (unsigned i = 0; i < (sysout_size / 2); i++)
    const sysoutBytes = new Uint8Array(buffer);
    let pagesLoaded = 0;

    // Per C: for (unsigned i = 0; i < (sysout_size / 2); i++)
    // sysout_size is in half-pages, so sysout_size / 2 gives number of pages
    const numPages = Math.floor(sysoutSize / 2);

    for (let filePage = 0; filePage < numPages; filePage++) {
        if (filePage >= fptovpTable.length) {
            break; // Beyond FPtoVP table
        }

        const entry = fptovpTable[filePage];

        // GETPAGEOK: Check if page is present (high 16 bits)
        // Per C: GETPAGEOK(fptovp, i) = (fptovp[i] >> 16)
        // 0xFFFF (0177777) means page not present
        const pageOK = (entry >> 16) & 0xFFFF;

        // Skip if page is not present (pageOK == 0xFFFF) or entry is zero (invalid)
        if (pageOK === 0xFFFF || entry === 0) {
            continue; // Skip sparse/invalid pages
        }

        // GETFPTOVP: Get virtual page number (low 16 bits)
        // Per C: GETFPTOVP(fptovp, i) = fptovp[i] (for BIGVM)
        const virtualPage = entry & 0xFFFF;

        // Skip if virtual page is 0 (invalid mapping)
        if (virtualPage === 0) {
            continue;
        }

        // Calculate offsets
        const fileOffset = filePage * BYTESPER_PAGE;
        const virtualOffset = virtualPage * BYTESPER_PAGE;

        // Check bounds
        if (fileOffset + BYTESPER_PAGE > buffer.byteLength) {
            console.error(`Page ${filePage}: File offset ${fileOffset} exceeds file size ${buffer.byteLength}`);
            continue;
        }
        if (virtualOffset + BYTESPER_PAGE > virtualMemory.length) {
            console.error(`Page ${filePage}: Virtual offset ${virtualOffset} exceeds VM size ${virtualMemory.length}`);
            continue;
        }

        // Read page from sysout file
        const pageData = sysoutBytes.slice(fileOffset, fileOffset + BYTESPER_PAGE);

        // Write page to virtual memory
        virtualMemory.set(pageData, virtualOffset);

        // Byte-swap page (sysout is big-endian, we need little-endian)
        // Per C: word_swap_page((DLword *)(lispworld_scratch + lispworld_offset), 128)
        // 128 = BYTESPER_PAGE / 4 (number of 32-bit words per page)
        const pageView = new DataView(virtualMemory.buffer, virtualMemory.byteOffset + virtualOffset, BYTESPER_PAGE);
        for (let wordOffset = 0; wordOffset < BYTESPER_PAGE; wordOffset += 4) {
            const word = pageView.getUint32(wordOffset, false); // Read as big-endian
            pageView.setUint32(wordOffset, MemoryManager.Endianness.swapU32(word), true); // Write as little-endian
        }

        pagesLoaded++;
    }

    console.error(`Loaded ${pagesLoaded} pages into virtual memory (${virtualMemorySizeMB}MB)`);

    return virtualMemory;
}

/**
 * Initialize PC from current frame
 * Per C: start_lisp() -> FastRetCALL -> reads CURRENTFX->fnheader -> gets startpc
 *
 * FastRetCALL logic:
 *   1. FuncObj = NativeAligned4FromLAddr(FX_FNHEADER)
 *   2. PC = (ByteCode *)FuncObj + CURRENTFX->pc
 *
 * NOTE: The stack area (virtual page 256 = STK_OFFSET) is SPARSE in the sysout file,
 * meaning it's not loaded from the FPtoVP table. The stack is initialized at runtime.
 * Therefore, the frame at currentfxp may be all zeros until the stack is initialized.
 * In this case, we return 0 and let the VM initialization (similar to start_lisp())
 * handle PC initialization after the stack is set up.
 */
function initializePC(virtualMemory: Uint8Array, ifpage: IFPAGE): number {
    // CRITICAL: Read current frame and initialize PC from function entry point
    // C: start_lisp() -> FastRetCALL -> reads CURRENTFX->fnheader -> gets startpc
    //
    // Per C: PVar = NativeAligned2FromStackOffset(InterfacePage->currentfxp) + FRAMESIZE
    //        CURRENTFX = ((struct frameex1 *)((DLword *)PVar - FRAMESIZE))
    //        So CURRENTFX = NativeAligned2FromStackOffset(currentfxp)
    //
    // NativeAligned2FromStackOffset returns Stackspace + StackOffset
    // Stackspace points to STK_OFFSET in Lisp address space
    const STK_OFFSET_BYTES = STK_OFFSET * 2; // Convert DLword offset to byte offset
    const currentfxpStackOffset = ifpage.currentfxp; // DLword offset from Stackspace
    const frameOffset = STK_OFFSET_BYTES + (currentfxpStackOffset * 2);

    if (currentfxpStackOffset === 0 || frameOffset + 20 > virtualMemory.length) {
        return 0; // Invalid frame
    }

    // Check if frame is all zeros (stack area is sparse, so frame may not be loaded)
    // Per C: start_lisp() expects CURRENTFX->nextblock to be valid
    // If frame is all zeros, the stack pages are sparse (not in sysout)
    // In this case, we'll return 0 and let the VM initialization handle it
    // (similar to how start_lisp() handles it after stack initialization)
    const isAllZeros = virtualMemory.slice(frameOffset, frameOffset + 20).every(b => b === 0);
    if (isAllZeros) {
        // Stack area is sparse - frame will be initialized at runtime
        // Return 0 to indicate we couldn't initialize PC from frame
        // The VM initialization (similar to start_lisp()) will handle PC setup
        return 0;
    }

    // Read frame structure (FX) - need to handle byte-swapping for big-endian sysout
    // Frame layout per maiko/inc/stack.h (non-BIGVM):
    //   offset 0-1: flags+usecount (DLword)
    //   offset 2-3: alink (DLword)
    //   offset 4-5: lofnheader (DLword) - low 16 bits of function header pointer
    //   offset 6: hi1fnheader (8 bits)
    //   offset 7: hi2fnheader (8 bits) - high 8 bits of function header pointer
    //   offset 8-9: nextblock (DLword)
    //   offset 10-11: pc (DLword) - PC offset within function

    // Read fnheader pointer (non-BIGVM format: lofnheader + hi2fnheader)
    // Note: sysout is big-endian, so we need to read as big-endian
    const view = new DataView(virtualMemory.buffer, virtualMemory.byteOffset + frameOffset);
    const lofnheader = view.getUint16(4, false); // big-endian
    const hi2fnheader = virtualMemory[frameOffset + 7];
    const fnheaderPtr: LispPTR = (hi2fnheader << 16) | lofnheader;

    if (fnheaderPtr === 0) {
        return 0; // Invalid function header pointer
    }

    // Read pc field from frame (offset 10-11)
    const framePc = view.getUint16(10, false); // big-endian

    // Convert fnheader pointer to byte offset
    const fnheaderByteOffset = MemoryManager.Address.lispPtrToByte(fnheaderPtr);

    if (fnheaderByteOffset + 8 > virtualMemory.length) {
        return 0; // Invalid function header
    }

    // Read function header to get startpc (big-endian)
    const fnheaderView = new DataView(virtualMemory.buffer, virtualMemory.byteOffset + fnheaderByteOffset);
    const startpc = fnheaderView.getUint16(6, false); // big-endian

    // Calculate final PC: function header start + startpc + frame pc
    // Per C: PC = (ByteCode *)FuncObj + CURRENTFX->pc
    // FuncObj points to function header, so PC = fnheader_start + startpc + frame_pc
    const finalPC = fnheaderByteOffset + startpc + framePc;

    return finalPC;
}

/**
 * Load sysout file
 */
export async function loadSysout(arrayBuffer: ArrayBuffer): Promise<SysoutLoadResult> {
    // Parse IFPAGE
    const ifpage = parseIFPAGE(arrayBuffer, IFPAGE_ADDRESS);
    if (ifpage === null) {
        throw new Error('Failed to parse IFPAGE');
    }

    // Load FPtoVP table
    const fptovpTable = loadFPtoVPTable(arrayBuffer, ifpage, arrayBuffer.byteLength);

    // Load virtual memory using FPtoVP mapping
    const virtualMemory = loadVirtualMemory(arrayBuffer, fptovpTable, ifpage);

    // Initialize PC from current frame
    const initialPC = initializePC(virtualMemory, ifpage);

    // Initialize SP from IFPAGE
    const initialSP = STK_OFFSET * 2; // Stack base

    // Calculate memory region offsets (convert DLword offsets to byte offsets)
    // Per C: NativeAligned2FromLAddr(ATOMS_OFFSET) = Lisp_world + (ATOMS_OFFSET * 2)
    const atomSpaceOffset = MemoryManager.Address.lispPtrToByte(ATOMS_OFFSET);
    const defSpaceOffset = MemoryManager.Address.lispPtrToByte(DEFS_OFFSET);
    const valSpaceOffset = MemoryManager.Address.lispPtrToByte(VALS_OFFSET);
    const plistSpaceOffset = MemoryManager.Address.lispPtrToByte(PLIS_OFFSET);
    const dtdOffset = MemoryManager.Address.lispPtrToByte(DTD_OFFSET);

    return {
        virtualMemory,
        fptovpTable,
        ifpage,
        initialPC,
        initialSP,
        atomSpaceOffset,
        defSpaceOffset,
        valSpaceOffset,
        plistSpaceOffset,
        dtdOffset,
    };
}
