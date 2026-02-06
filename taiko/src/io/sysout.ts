// Sysout file loading and parsing
import type { IFPAGE } from '../utils/types';
import { IFPAGE_KEYVAL, IFPAGE_ADDRESS, BYTESPER_PAGE, STK_OFFSET, ATOMS_OFFSET, DEFS_OFFSET, VALS_OFFSET } from '../utils/constants';
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
}

/**
 * Parse IFPAGE from sysout file
 * IFPAGE is located at offset 512 bytes from start
 */
function parseIFPAGE(buffer: ArrayBuffer, offset: number): IFPAGE | null {
    const view = new DataView(buffer, offset);

    // Read IFPAGE structure (BYTESWAP version for little-endian)
    // See maiko/inc/ifpage.h lines 257-328
    const ifpage: IFPAGE = {
        resetfxp: view.getUint16(0, true),
        currentfxp: view.getUint16(2, true),
        kbdfxp: view.getUint16(4, true),
        subovfxp: view.getUint16(6, true),
        gcfxp: view.getUint16(8, true),
        hardreturnfxp: view.getUint16(10, true),
        endofstack: view.getUint16(12, true),
        faultfxp: view.getUint16(14, true),
        minrversion: view.getUint16(16, true),
        lversion: view.getUint16(18, true),
        rversion: view.getUint16(20, true),
        minbversion: view.getUint16(22, true),
        machinetype: view.getUint16(24, true),
        bversion: view.getUint16(26, true),
        key: view.getUint16(28, true),
        miscfxp: view.getUint16(30, true),
        emulatorspace: view.getUint16(32, true),
        serialnumber: view.getUint16(34, true),
        nxtpmaddr: view.getUint16(36, true),
        screenwidth: view.getUint16(38, true),
        ndirtypages: view.getUint16(40, true),
        nactivepages: view.getUint16(42, true),
        filepnpmt0: view.getUint16(44, true),
        filepnpmp0: view.getUint16(46, true),
        filler1: view.getUint16(48, true),
        teleraidfxp: view.getUint16(50, true),
        filler3: view.getUint16(52, true),
        filler2: view.getUint16(54, true),
        userpswdaddr: view.getUint16(56, true),
        usernameaddr: view.getUint16(58, true),
        faulthi: view.getUint16(60, true),
        stackbase: view.getUint16(62, true),
        devconfig: view.getUint16(64, true),
        faultlo: view.getUint16(66, true),
        rpoffset: view.getUint16(68, true),
        rptsize: view.getUint16(70, true),
        embufvp: view.getUint16(72, true),
        wasrptlast: view.getUint16(74, true),
        nshost1: view.getUint16(76, true),
        nshost0: view.getUint16(78, true),
        mdszone: view.getUint16(80, true),
        nshost2: view.getUint16(82, true),
        emubuffers: view.getUint16(84, true),
        mdszonelength: view.getUint16(86, true),
        process_size: view.getUint16(88, true),
        emubuflength: view.getUint16(90, true),
        isfmap: view.getUint16(92, true),
        storagefullstate: view.getUint16(94, true),
        miscstackfn: view.getUint32(96, true),
        miscstackarg1: view.getUint32(100, true),
        miscstackarg2: view.getUint32(104, true),
        miscstackresult: view.getUint32(108, true),
        lastlockedfilepage: view.getUint16(112, true),
        nrealpages: view.getUint16(114, true),
        fptovpstart: view.getUint16(116, true),
        lastdominofilepage: view.getUint16(118, true),
        dl24bitaddressable: view.getUint16(120, true),
        fakemousebits: view.getUint16(122, true),
        realpagetableptr: view.getUint32(124, true),
        fullspaceused: view.getUint16(128, true),
        dllastvmempage: view.getUint16(130, true),
        fakekbdad5: view.getUint16(132, true),
        fakekbdad4: view.getUint16(134, true),
    };

    // Validate IFPAGE key
    if (ifpage.key !== IFPAGE_KEYVAL) {
        throw new Error(`Invalid IFPAGE key: expected 0x${IFPAGE_KEYVAL.toString(16)}, got 0x${ifpage.key.toString(16)}`);
    }

    return ifpage;
}

/**
 * Load FPtoVP table from sysout file
 */
function loadFPtoVPTable(buffer: ArrayBuffer, ifpage: IFPAGE, fileSize: number): Uint32Array {
    // FPtoVP table starts after IFPAGE
    // Size depends on number of file pages
    const numFilePages = Math.floor(fileSize / BYTESPER_PAGE);
    const fptovpSize = numFilePages * 4; // Each entry is 32 bits

    const fptovpOffset = IFPAGE_ADDRESS + 144; // IFPAGE is 144 bytes
    const fptovpBuffer = buffer.slice(fptovpOffset, fptovpOffset + fptovpSize);
    const fptovpTable = new Uint32Array(fptovpBuffer);

    // Byte-swap entries if needed (little-endian host)
    for (let i = 0; i < fptovpTable.length; i++) {
        fptovpTable[ i ] = MemoryManager.Endianness.swapU32(fptovpTable[ i ]);
    }

    return fptovpTable;
}

/**
 * Load virtual memory from sysout file pages
 */
function loadVirtualMemory(buffer: ArrayBuffer, fptovpTable: Uint32Array): Uint8Array {
    // Calculate total virtual memory size
    // For now, use a fixed size based on process_size from IFPAGE
    // In production, this would be calculated from FPtoVP table
    const virtualMemorySize = 1024 * 1024; // 1MB default
    const virtualMemory = new Uint8Array(virtualMemorySize);

    // Load pages from sysout file
    // TODO: Implement proper page loading based on FPtoVP table
    // For now, just copy the entire sysout file into virtual memory
    const sysoutBytes = new Uint8Array(buffer);
    const copySize = Math.min(sysoutBytes.length, virtualMemory.length);
    virtualMemory.set(sysoutBytes.slice(0, copySize), 0);

    return virtualMemory;
}

/**
 * Initialize PC from current frame
 * Per C: reads CURRENTFX->fnheader -> gets startpc
 */
function initializePC(virtualMemory: Uint8Array, ifpage: IFPAGE): number {
    // CRITICAL: Read current frame and initialize PC from function entry point
    // C: start_lisp() -> FastRetCALL -> reads CURRENTFX->fnheader -> gets startpc
    const STK_OFFSET_BYTES = STK_OFFSET * 2; // Convert DLword offset to byte offset
    const currentfxpStackOffset = ifpage.currentfxp; // DLword offset from Stackspace
    const frameOffset = STK_OFFSET_BYTES + (currentfxpStackOffset * 2);

    if (frameOffset + 14 > virtualMemory.length) {
        return 0; // Invalid frame
    }

    // Read fnheader from frame (simplified - full implementation would parse frame structure)
    // Frame layout: flags+usecount (2), alink (2), lofnheader (2), hi1fnheader+hi2fnheader (2)
    const lofnheader = virtualMemory[ frameOffset + 4 ] | (virtualMemory[ frameOffset + 5 ] << 8);
    const hi2fnheader = virtualMemory[ frameOffset + 7 ];
    const fnheader: LispPTR = (hi2fnheader << 16) | lofnheader;

    // Read function header to get startpc
    // TODO: Parse function header structure to get startpc
    // For now, return fnheader as PC (simplified)
    return fnheader * 2; // Convert DLword offset to byte offset
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

    // Load virtual memory
    const virtualMemory = loadVirtualMemory(arrayBuffer, fptovpTable);

    // Initialize PC from current frame
    const initialPC = initializePC(virtualMemory, ifpage);

    // Initialize SP from IFPAGE
    const initialSP = STK_OFFSET * 2; // Stack base

    // Calculate memory region offsets (convert DLword offsets to byte offsets)
    // Per C: NativeAligned2FromLAddr(ATOMS_OFFSET) = Lisp_world + (ATOMS_OFFSET * 2)
    const atomSpaceOffset = MemoryManager.Address.lispPtrToByte(ATOMS_OFFSET);
    const defSpaceOffset = MemoryManager.Address.lispPtrToByte(DEFS_OFFSET);
    const valSpaceOffset = MemoryManager.Address.lispPtrToByte(VALS_OFFSET);

    return {
        virtualMemory,
        fptovpTable,
        ifpage,
        initialPC,
        initialSP,
        atomSpaceOffset,
        defSpaceOffset,
        valSpaceOffset,
    };
}
