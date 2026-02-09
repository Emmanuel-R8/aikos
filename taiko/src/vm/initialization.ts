// VM initialization similar to C start_lisp()
// Initializes stack, frame, and PC from sysout state

import type { VM } from './vm';
import type { IFPAGE } from '../utils/types';
import { STK_OFFSET, FRAMESIZE, FRAMESIZE_BYTES, STK_FSB_WORD, STK_GUARD_WORD, BF_MARK, FX_MARK, MINEXTRASTACKWORDS, NIL_PTR } from '../utils/constants';
import { MemoryManager } from './memory/manager';
import { FrameManager } from './memory/frame';

/**
 * Initialize VM state similar to C start_lisp()
 * Per C: main.c:start_lisp() -> initializes stack, frame, and PC
 *
 * Algorithm:
 *   1. Reset TopOfStack and Error_Exit flags
 *   2. Set PVar from currentfxp frame
 *   3. Get nextblock from current frame
 *   4. Verify nextblock points to free stack block (STK_FSB_WORD)
 *   5. Walk free block chain to find end of stack
 *   6. Set CurrentStackPTR
 *   7. FastRetCALL: Initialize IVar, FuncObj, PC from frame
 *
 * @param vm VM instance
 * @param ifpage IFPAGE structure from sysout
 * @returns true if initialization successful, false otherwise
 */
export function initializeVM(vm: VM, ifpage: IFPAGE): boolean {
    if (vm.virtualMemory === null) {
        return false;
    }

    // Step 1: Reset TopOfStack and Error_Exit flags
    // Per C: TopOfStack = 0; Error_Exit = 0;
    vm.topOfStack = NIL_PTR;
    // Error_Exit is not in VM class yet, skip for now

    // Step 2: Set PVar from currentfxp frame
    // Per C: PVar = NativeAligned2FromStackOffset(InterfacePage->currentfxp) + FRAMESIZE
    const STK_OFFSET_BYTES = STK_OFFSET * 2;
    const currentfxpStackOffset = ifpage.currentfxp; // DLword offset from Stackspace
    const frameOffset = STK_OFFSET_BYTES + (currentfxpStackOffset * 2);
    const pvarOffset = frameOffset + FRAMESIZE_BYTES;

    if (frameOffset + FRAMESIZE_BYTES > vm.virtualMemory.length) {
        console.error(`initializeVM: Frame offset ${frameOffset} out of bounds`);
        return false;
    }

    vm.pvar = pvarOffset;

    // Step 3: Get nextblock from current frame
    // Per C: freeptr = next68k = NativeAligned2FromStackOffset(CURRENTFX->nextblock)
    // Read nextblock from frame (offset 8-9 in frame structure)
    const frameView = new DataView(vm.virtualMemory.buffer, vm.virtualMemory.byteOffset + frameOffset);

    // Debug: Print frame contents
    const frameBytes = Array.from(vm.virtualMemory.slice(frameOffset, frameOffset + FRAMESIZE_BYTES))
        .map(b => `0x${b.toString(16).padStart(2, '0')}`)
        .join(' ');
    console.error(`initializeVM: Frame bytes: ${frameBytes}`);

    // Read frame fields
    let flags_usecount = frameView.getUint16(0, false); // big-endian
    let alink = frameView.getUint16(2, false); // big-endian
    let lofnheader = frameView.getUint16(4, false); // big-endian
    let hi2fnheader = vm.virtualMemory[frameOffset + 7];
    let nextblock = frameView.getUint16(8, false); // big-endian
    let pc = frameView.getUint16(10, false); // big-endian

    console.error(`initializeVM: flags_usecount=0x${flags_usecount.toString(16)}, alink=0x${alink.toString(16)}, lofnheader=0x${lofnheader.toString(16)}, hi2fnheader=0x${hi2fnheader.toString(16)}, nextblock=0x${nextblock.toString(16)}, pc=0x${pc.toString(16)}`);

    // Check if frame needs initialization (sparse stack)
    // Critical fields: nextblock must be non-zero for valid frame
    if (nextblock === 0) {
        console.error(`initializeVM: nextblock is 0 - initializing sparse stack area...`);

        // Initialize sparse stack: create frame structure and free stack blocks
        const initSuccess = initializeSparseStack(vm, frameOffset, currentfxpStackOffset);
        if (!initSuccess) {
            return false;
        }

        // Re-read frame fields after initialization
        flags_usecount = frameView.getUint16(0, false); // big-endian
        alink = frameView.getUint16(2, false); // big-endian
        lofnheader = frameView.getUint16(4, false); // big-endian
        hi2fnheader = vm.virtualMemory[frameOffset + 7];
        nextblock = frameView.getUint16(8, false); // big-endian
        pc = frameView.getUint16(10, false); // big-endian

        if (nextblock === 0) {
            console.error(`initializeVM: nextblock still 0 after initialization`);
            return false;
        }
        console.error(`initializeVM: Stack initialized, nextblock=0x${nextblock.toString(16)}`);
    }

    // Convert nextblock (stack offset) to byte offset
    // Per C: NativeAligned2FromStackOffset(stackOffset) = Stackspace + stackOffset
    // Stackspace = STK_OFFSET_BYTES, so byte offset = STK_OFFSET_BYTES + (stackOffset * 2)
    const nextblockByteOffset = STK_OFFSET_BYTES + (nextblock * 2);

    console.error(`initializeVM: nextblock=${nextblock} (0x${nextblock.toString(16)}), nextblockByteOffset=0x${nextblockByteOffset.toString(16)}`);

    if (nextblockByteOffset + 4 > vm.virtualMemory.length) {
        console.error(`initializeVM: nextblock offset ${nextblockByteOffset} out of bounds`);
        return false;
    }

    // Step 4: Verify nextblock points to free stack block (STK_FSB_WORD)
    // Per C: if (GETWORD(next68k) != STK_FSB_WORD) error("Starting Lisp: Next stack block isn't free!");
    // Memory is little-endian (after byte-swapping from sysout), so read as little-endian
    console.error(`initializeVM: Reading FSB at offset 0x${nextblockByteOffset.toString(16)}`);
    const nextblockView = new DataView(vm.virtualMemory.buffer, vm.virtualMemory.byteOffset + nextblockByteOffset);

    // Debug: Check raw bytes
    const rawBytes = Array.from(vm.virtualMemory.slice(nextblockByteOffset, nextblockByteOffset + 4))
        .map(b => `0x${b.toString(16).padStart(2, '0')}`)
        .join(' ');
    console.error(`initializeVM: Raw FSB bytes: ${rawBytes}`);

    // Read as 32-bit value (LispPTR), then extract flagword from high 16 bits
    const fsbValue = nextblockView.getUint32(0, true); // little-endian
    const flagword = (fsbValue >> 16) & 0xFFFF; // Extract high 16 bits
    const size = fsbValue & 0xFFFF; // Extract low 16 bits

    console.error(`initializeVM: Read FSB: fsbValue=0x${fsbValue.toString(16)}, flagword=0x${flagword.toString(16)}, size=${size}`);

    if (flagword !== STK_FSB_WORD) {
        console.error(`initializeVM: Next stack block isn't free: flagword=0x${flagword.toString(16)}, expected STK_FSB_WORD=0x${STK_FSB_WORD.toString(16)}`);
        return false;
    }

    // Step 5: Walk free block chain to find end of stack
    // Per C: while (GETWORD(freeptr) == STK_FSB_WORD) EndSTKP = freeptr = freeptr + GETWORD(freeptr + 1);
    let freeptr = nextblockByteOffset;
    let endSTKP = freeptr;

    while (freeptr + 4 <= vm.virtualMemory.length) {
        const freeptrView = new DataView(vm.virtualMemory.buffer, vm.virtualMemory.byteOffset + freeptr);
        // Read as 32-bit value (LispPTR), then extract flagword and size
        const freeFsbValue = freeptrView.getUint32(0, true); // little-endian
        const freeFlagword = (freeFsbValue >> 16) & 0xFFFF; // Extract high 16 bits

        if (freeFlagword !== STK_FSB_WORD) {
            break; // End of free block chain
        }

        const freeSize = freeFsbValue & 0xFFFF; // Extract low 16 bits (size in DLwords)
        endSTKP = freeptr;
        freeptr = freeptr + (freeSize * 2); // Convert DLwords to bytes
    }

    vm.stackEnd = endSTKP;

    // Step 6: Set CurrentStackPTR
    // Per C: CurrentStackPTR = next68k - 2;
    vm.stackPtr = nextblockByteOffset - 2;
    vm.cstkptrl = vm.stackPtr;

    // Step 7: FastRetCALL - Initialize IVar, FuncObj, PC from frame
    // Per C: FastRetCALL macro (retmacro.h:37-45)
    //   IVar = NativeAligned2FromStackOffset(GETWORD((DLword *)CURRENTFX - 1))
    //   FuncObj = (struct fnhead *)NativeAligned4FromLAddr(FX_FNHEADER)
    //   PC = (ByteCode *)FuncObj + CURRENTFX->pc

    // Get IVar from binding frame before current frame
    // CURRENTFX - 1 points to the binding frame (BF)
    const bfOffset = frameOffset - 4; // BF is 2 DLwords (4 bytes) before FX
    if (bfOffset >= 0 && bfOffset + 2 <= vm.virtualMemory.length) {
        const bfView = new DataView(vm.virtualMemory.buffer, vm.virtualMemory.byteOffset + bfOffset);
        const ivarStackOffset = bfView.getUint16(0, false); // big-endian
        vm.ivar = STK_OFFSET_BYTES + (ivarStackOffset * 2);
    } else {
        vm.ivar = null;
    }

    // Get FuncObj from frame fnheader
    // Per C: FX_FNHEADER = (CURRENTFX->hi2fnheader << 16) | CURRENTFX->lofnheader (non-BIGVM)
    // Note: lofnheader and hi2fnheader already read above
    let fnheaderPtr: number = (hi2fnheader << 16) | lofnheader;

    if (fnheaderPtr === 0) {
        console.error(`initializeVM: fnheaderPtr is 0 - attempting to locate entry point...`);

        // Try to find entry point function
        const entryPoint = locateEntryPoint(vm, ifpage);
        if (entryPoint === null) {
            console.error(`initializeVM: Could not locate entry point function`);
            vm.funcObj = null;
            vm.pc = 0;
            // Still return true since stack is initialized, just PC needs to be set elsewhere
            return true;
        }

        // Update frame with entry point function header
        const entryFnheaderPtr = MemoryManager.Address.byteToLispPtr(entryPoint.fnheaderOffset);
        const entryLofnheader = entryFnheaderPtr & 0xFFFF;
        const entryHi2fnheader = (entryFnheaderPtr >> 16) & 0xFF;

        frameView.setUint16(4, entryLofnheader, false); // big-endian
        vm.virtualMemory[frameOffset + 7] = entryHi2fnheader;
        frameView.setUint16(10, entryPoint.pcOffset, false); // big-endian (PC offset in DLwords)

        // Re-read fnheaderPtr with updated values
        const updatedLofnheader = frameView.getUint16(4, false);
        const updatedHi2fnheader = vm.virtualMemory[frameOffset + 7];
        fnheaderPtr = (updatedHi2fnheader << 16) | updatedLofnheader;

        // Update local variables for use below
        lofnheader = updatedLofnheader;
        hi2fnheader = updatedHi2fnheader;
        pc = entryPoint.pcOffset;

        console.error(`initializeVM: Found entry point: fnheader=0x${entryPoint.fnheaderOffset.toString(16)}, pcOffset=${entryPoint.pcOffset}`);
    }

    // Convert fnheader pointer to byte offset
    const fnheaderByteOffset = MemoryManager.Address.lispPtrToByte(fnheaderPtr);
    vm.funcObj = fnheaderByteOffset;

    // Get PC from frame
    // Per C: PC = (ByteCode *)FuncObj + CURRENTFX->pc
    const framePc = frameView.getUint16(10, false); // big-endian (PC offset in DLwords)

    // Read function header to get startpc
    if (fnheaderByteOffset + 8 > vm.virtualMemory.length) {
        console.error(`initializeVM: Function header offset ${fnheaderByteOffset} out of bounds`);
        return false;
    }

    const fnheaderView = new DataView(vm.virtualMemory.buffer, vm.virtualMemory.byteOffset + fnheaderByteOffset);
    // Read startpc as little-endian (memory is byte-swapped from sysout)
    const startpc = fnheaderView.getUint16(4, true); // little-endian (startpc in DLwords, offset 4-5 in fnheader)

    // Calculate final PC: function header start + startpc + frame pc
    // Per C: PC = (ByteCode *)FuncObj + CURRENTFX->pc
    // FuncObj points to function header, so PC = fnheader_start + startpc + framePc
    // All offsets are in DLwords, convert to bytes
    vm.pc = fnheaderByteOffset + (startpc * 2) + (framePc * 2);

    console.error(`initializeVM: PC calculation: fnheader=0x${fnheaderByteOffset.toString(16)}, startpc=${startpc}, framePc=${framePc}, finalPC=0x${vm.pc.toString(16)}`);

    return true;
}

/**
 * Initialize sparse stack area
 * Creates frame structure and free stack blocks when stack is not loaded from sysout
 *
 * @param vm VM instance
 * @param frameOffset Byte offset of current frame
 * @param currentfxpStackOffset DLword offset of current frame from stack base
 * @returns true if initialization successful
 */
function initializeSparseStack(vm: VM, frameOffset: number, currentfxpStackOffset: number): boolean {
    if (vm.virtualMemory === null) {
        return false;
    }

    const STK_OFFSET_BYTES = STK_OFFSET * 2;

    // Calculate stack area boundaries
    // Stack grows downward (from high addresses to low addresses)
    // Stack base is at STK_OFFSET_BYTES (high address)
    // We need to allocate space below (toward lower addresses)

    // Frame is at frameOffset (STK_OFFSET_BYTES + currentfxp * 2)
    // Free block should be placed after the frame (at lower address, since stack grows down)
    const frameEnd = frameOffset + FRAMESIZE_BYTES;
    const freeBlockStart = frameEnd;

    // Ensure we have enough space for free block header (4 bytes)
    if (freeBlockStart + 4 > vm.virtualMemory.length) {
        console.error(`initializeSparseStack: Not enough space for free block`);
        return false;
    }

    // Calculate free block size (in DLwords)
    // Stack grows downward, so free block extends from freeBlockStart toward lower addresses
    // Use a reasonable size: allocate space downward from the free block start
    // Leave some margin at the end (e.g., 1KB)
    const marginBytes = 1024;
    const availableSpace = freeBlockStart - marginBytes;
    const freeBlockSizeBytes = Math.max(availableSpace - (STK_OFFSET_BYTES - (64 * 1024)), MINEXTRASTACKWORDS * 2);
    const freeBlockSizeDLwords = Math.floor(freeBlockSizeBytes / 2);

    if (freeBlockSizeDLwords < MINEXTRASTACKWORDS) {
        console.error(`initializeSparseStack: Free block too small: ${freeBlockSizeDLwords} DLwords`);
        return false;
    }

    // Step 1: Create binding frame (BF) before current frame
    // BF is 2 DLwords (4 bytes) before FX
    // BF structure: [ivar_offset (DLword), BF_MARK (DLword)]
    const bfOffset = frameOffset - 4;
    if (bfOffset >= 0) {
        const bfView = new DataView(vm.virtualMemory.buffer, vm.virtualMemory.byteOffset + bfOffset);
        // Set ivar_offset to point to free block start (as stack offset)
        const ivarStackOffset = Math.floor((freeBlockStart - STK_OFFSET_BYTES) / 2);
        bfView.setUint16(0, ivarStackOffset, false); // big-endian
        bfView.setUint16(2, BF_MARK, false); // big-endian
    }

    // Step 2: Create current frame (FX)
    // Frame structure per maiko/inc/stack.h (non-BIGVM):
    //   offset 0-1: flags+usecount (DLword)
    //   offset 2-3: alink (DLword)
    //   offset 4-5: lofnheader (DLword)
    //   offset 6: hi1fnheader (8 bits)
    //   offset 7: hi2fnheader (8 bits)
    //   offset 8-9: nextblock (DLword) - points to free stack block
    //   offset 10-11: pc (DLword)
    //   offset 12-13: lonametable (DLword)
    //   offset 14-15: hi1nametable_hi2nametable (16 bits)
    //   offset 16-17: blink (DLword)
    //   offset 18-19: clink (DLword)

    const frameView = new DataView(vm.virtualMemory.buffer, vm.virtualMemory.byteOffset + frameOffset);

    // flags_usecount: flags (3 bits) + usecount (8 bits) = 0 for now
    frameView.setUint16(0, 0, false); // big-endian

    // alink: activation link (0 for now, no previous frame)
    frameView.setUint16(2, 0, false); // big-endian

    // fnheader: function header pointer (0 for now - will need to find entry point)
    frameView.setUint16(4, 0, false); // lofnheader
    vm.virtualMemory[frameOffset + 6] = 0; // hi1fnheader
    vm.virtualMemory[frameOffset + 7] = 0; // hi2fnheader

    // nextblock: points to free stack block (as stack offset)
    const nextblockStackOffset = Math.floor((freeBlockStart - STK_OFFSET_BYTES) / 2);
    frameView.setUint16(8, nextblockStackOffset, false); // big-endian

    // pc: program counter offset (0 for now)
    frameView.setUint16(10, 0, false); // big-endian

    // nametable: (0 for now)
    frameView.setUint16(12, 0, false); // lonametable
    vm.virtualMemory[frameOffset + 14] = 0; // hi1nametable
    vm.virtualMemory[frameOffset + 15] = 0; // hi2nametable

    // blink, clink: binding links (0 for now)
    frameView.setUint16(16, 0, false); // blink
    frameView.setUint16(18, 0, false); // clink

    // Step 3: Create free stack block (FSB)
    // Per C: MAKEFREEBLOCK(ptr68k, size)
    // Structure: [flagword (STK_FSB_WORD), size (DLwords)]
    // Per C: *((LispPTR *)(void *)(ptr68k)) = (STK_FSB_WORD << 16) | ((DLword)(size))
    // This is a 32-bit value: high 16 bits = STK_FSB_WORD, low 16 bits = size
    // Memory is little-endian (after byte-swapping from sysout), so write as little-endian
    const fsbView = new DataView(vm.virtualMemory.buffer, vm.virtualMemory.byteOffset + freeBlockStart);
    const fsbValue = (STK_FSB_WORD << 16) | freeBlockSizeDLwords;
    fsbView.setUint32(0, fsbValue, true); // little-endian (our memory format)

    // Verify the write
    const writtenValue = fsbView.getUint32(0, true); // Read back as little-endian
    const writtenFlagword = (writtenValue >> 16) & 0xFFFF;
    const writtenSize = writtenValue & 0xFFFF;
    console.error(`initializeSparseStack: Wrote FSB: flagword=0x${writtenFlagword.toString(16)}, size=${writtenSize} DLwords`);

    console.error(`initializeSparseStack: Created frame at 0x${frameOffset.toString(16)}, free block at 0x${freeBlockStart.toString(16)} (size=${freeBlockSizeDLwords} DLwords)`);

    return true;
}

/**
 * Locate entry point function for sparse stack initialization
 * Attempts to find a valid function header to use as the entry point
 *
 * @param vm VM instance
 * @param ifpage IFPAGE structure
 * @returns Entry point info (fnheader offset, PC offset) or null if not found
 */
function locateEntryPoint(vm: VM, ifpage: IFPAGE): { fnheaderOffset: number; pcOffset: number } | null {
    if (vm.virtualMemory === null) {
        return null;
    }

    const STK_OFFSET_BYTES = STK_OFFSET * 2;

    // Strategy 1: Check resetfxp frame (might have valid function header)
    // NOTE: Frame data in memory is byte-swapped, so read as little-endian
    const resetfxpStackOffset = ifpage.resetfxp;
    if (resetfxpStackOffset !== 0) {
        const resetfxpFrameOffset = STK_OFFSET_BYTES + (resetfxpStackOffset * 2);
        if (resetfxpFrameOffset + FRAMESIZE_BYTES <= vm.virtualMemory.length) {
            const resetfxpFrameView = new DataView(vm.virtualMemory.buffer, vm.virtualMemory.byteOffset + resetfxpFrameOffset);
            // Read frame fields as little-endian (memory is byte-swapped)
            const resetfxpLofnheader = resetfxpFrameView.getUint16(4, true); // little-endian
            const resetfxpHi2fnheader = vm.virtualMemory[resetfxpFrameOffset + 7];
            const resetfxpFnheaderPtr = (resetfxpHi2fnheader << 16) | resetfxpLofnheader;

            if (resetfxpFnheaderPtr !== 0) {
                const resetfxpFnheaderOffset = MemoryManager.Address.lispPtrToByte(resetfxpFnheaderPtr);
                if (resetfxpFnheaderOffset + 8 <= vm.virtualMemory.length) {
                    // Read function header to get startpc
                    const resetfxpFnheaderView = new DataView(vm.virtualMemory.buffer, vm.virtualMemory.byteOffset + resetfxpFnheaderOffset);
                    const resetfxpStartpc = resetfxpFnheaderView.getUint16(4, true); // little-endian
                    const resetfxpPc = resetfxpFrameView.getUint16(10, true); // little-endian (PC offset within function)
                    console.error(`locateEntryPoint: Found function header in resetfxp frame: 0x${resetfxpFnheaderOffset.toString(16)}, startpc=${resetfxpStartpc}, framePc=${resetfxpPc}`);
                    return { fnheaderOffset: resetfxpFnheaderOffset, pcOffset: resetfxpPc };
                }
            }
        }
    }

    // Strategy 2: Search for function headers in memory
    // Look for valid function header structures (startpc field should be reasonable)
    // Function header structure: na (2 bytes), stkmin (2 bytes), startpc (2 bytes), pv (2 bytes), ...
    // NOTE: Memory has been byte-swapped from big-endian sysout to little-endian
    // So we read function headers as little-endian
    // CRITICAL: Only search in areas that are likely to be loaded (not sparse)
    // Start from a reasonable offset and search in chunks, checking if pages are loaded
    // We'll collect candidates and pick the one with the most consecutive loaded pages
    const searchStart = vm.atomSpaceOffset; // Start from AtomSpace
    const searchEnd = Math.min(vm.virtualMemory.length, searchStart + (20 * 1024 * 1024)); // Search up to 20MB

    interface EntryPointCandidate {
        fnheaderOffset: number;
        pcOffset: number;
        consecutivePages: number;
        startpc: number;
    }

    const candidates: EntryPointCandidate[] = [];

    // Search in page-aligned chunks to skip sparse pages quickly
    for (let pageStart = searchStart; pageStart < searchEnd - 8; pageStart += 512) {
        const pageEnd = Math.min(pageStart + 512, searchEnd);

        // Quick check: skip if entire page is sparse
        const pageBytes = vm.virtualMemory.slice(pageStart, pageEnd);
        if (pageBytes.every(b => b === 0)) {
            continue; // Skip sparse page
        }

        // Search within this page for function headers
        for (let offset = pageStart; offset < pageEnd - 8; offset += 2) {
            // Check if this looks like a function header
            const fnheaderView = new DataView(vm.virtualMemory.buffer, vm.virtualMemory.byteOffset + offset);

            // Read function header fields (little-endian, since memory is byte-swapped)
            const na = fnheaderView.getInt16(0, true); // signed, little-endian
            const stkmin = fnheaderView.getUint16(2, true); // little-endian
            const startpc = fnheaderView.getUint16(4, true); // little-endian
            const pv = fnheaderView.getInt16(6, true); // signed, little-endian

            // Heuristic: valid function header should have:
            // - Reasonable na (-1 to 20) - number of arguments
            // - Reasonable stkmin (0 to 1000) - minimum stack size
            // - Reasonable startpc (1 to 500 DLwords) - MUST be > 0, prefer smaller values
            // - Reasonable pv (-1 to 20) - parameter variable count
            if (na >= -1 && na <= 20 &&
                stkmin >= 0 && stkmin <= 1000 &&
                startpc > 0 && startpc <= 500 &&  // Require startpc > 0, prefer smaller
                pv >= -1 && pv <= 20) {

                // Verify the function header points to valid code
                // Per C: startpc is DLword offset from function header to code
                const codeOffset = offset + (startpc * 2);

                // CRITICAL: Verify code offset is within loaded memory bounds
                // The code offset must be within the virtual memory that was actually loaded
                // We can't check against sysout file size directly, but we can check if the page is loaded
                if (codeOffset < vm.virtualMemory.length && codeOffset > offset) {
                    // Check if code area looks valid (not all zeros, and has reasonable opcode values)
                    const codeBytes = vm.virtualMemory.slice(codeOffset, codeOffset + 10);
                    const isAllZeros = codeBytes.every(b => b === 0);
                    if (!isAllZeros) {
                        // Check if first byte looks like a valid opcode (0-255)
                        // Use XOR addressing to check: opcode = byte ^ 3
                        const rawByte = codeBytes[0];
                        const xorOpcode = rawByte ^ 3;

                        // Opcodes are typically 0-255, but many are < 200
                        // Also check that it's not obviously data (like 0x00, 0xFF patterns)
                        if (xorOpcode > 0 && xorOpcode < 200 && xorOpcode !== 0xFF) {
                            // CRITICAL: Verify the code page is actually loaded (not sparse)
                            const codePageStart = Math.floor(codeOffset / 512) * 512;
                            const codePageEnd = codePageStart + 512;

                            if (codePageEnd <= vm.virtualMemory.length) {
                                const codePageBytes = vm.virtualMemory.slice(codePageStart, codePageEnd);
                                const codePageIsSparse = codePageBytes.every(b => b === 0);

                                if (!codePageIsSparse) {
                                    // CRITICAL: Verify the actual code byte at codeOffset is in a loaded page
                                    // The codeOffset might be in the middle of a page, so check that specific byte
                                    const codePageForOffset = Math.floor(codeOffset / 512) * 512;

                                    // Also check the XOR address page (PC ^ 3)
                                    const xorAddr = codeOffset ^ 3;
                                    const xorPageForOffset = Math.floor(xorAddr / 512) * 512;

                                    // Both pages must be loaded (not sparse)
                                    if (codePageForOffset + 512 <= vm.virtualMemory.length) {
                                        const codePageBytesForOffset = vm.virtualMemory.slice(codePageForOffset, codePageForOffset + 512);
                                        const codePageForOffsetIsSparse = codePageBytesForOffset.every(b => b === 0);

                                        if (codePageForOffsetIsSparse) {
                                            // The actual code byte is in a sparse page - skip this candidate
                                            continue;
                                        }
                                    } else {
                                        // Code page is beyond virtual memory - skip
                                        continue;
                                    }

                                    if (xorPageForOffset + 512 <= vm.virtualMemory.length) {
                                        const xorPageBytesForOffset = vm.virtualMemory.slice(xorPageForOffset, xorPageForOffset + 512);
                                        const xorPageForOffsetIsSparse = xorPageBytesForOffset.every(b => b === 0);

                                        if (xorPageForOffsetIsSparse) {
                                            // The XOR address page is sparse - skip this candidate
                                            continue;
                                        }
                                    } else {
                                        // XOR page is beyond virtual memory - skip
                                        continue;
                                    }

                                    // CRITICAL: Verify the actual bytes at codeOffset and XOR address are non-zero
                                    if (codeOffset >= vm.virtualMemory.length || xorAddr >= vm.virtualMemory.length) {
                                        // Out of bounds - skip
                                        continue;
                                    }

                                    const codeByte = vm.virtualMemory[codeOffset];
                                    const xorByte = vm.virtualMemory[xorAddr];

                                    if (codeByte === 0 || xorByte === 0) {
                                        // Code byte or XOR byte is zero - likely sparse - skip
                                        continue;
                                    }

                                    // Count consecutive loaded pages starting from the page containing codeOffset
                                    // We want to find functions with the most consecutive loaded pages
                                    let consecutiveLoadedPages = 0;
                                    const maxPagesToCheck = 20; // Check up to 20 pages (10KB of code)

                                    // Start counting from the page containing codeOffset
                                    const startPageForCounting = codePageForOffset;

                                    for (let pageOffset = 0; pageOffset < maxPagesToCheck; pageOffset++) {
                                        const checkPageStart = startPageForCounting + (pageOffset * 512);
                                        const checkPageEnd = checkPageStart + 512;

                                        if (checkPageEnd > vm.virtualMemory.length) {
                                            // Beyond virtual memory bounds - stop counting
                                            break;
                                        }

                                        const checkPageBytes = vm.virtualMemory.slice(checkPageStart, checkPageEnd);
                                        const checkPageIsSparse = checkPageBytes.every(b => b === 0);

                                        if (checkPageIsSparse) {
                                            // Hit a sparse page - stop counting
                                            break;
                                        }

                                        consecutiveLoadedPages++;
                                    }

                                    // Collect candidates with at least 3 consecutive loaded pages
                                    // This ensures we have enough code (1.5KB) to execute before hitting sparse pages
                                    // We've already validated that the code byte and XOR byte are non-zero
                                    if (consecutiveLoadedPages >= 3) {
                                        candidates.push({
                                            fnheaderOffset: offset,
                                            pcOffset: 0,
                                            consecutivePages: consecutiveLoadedPages,
                                            startpc: startpc
                                        });
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // If we found candidates, pick the one with the most consecutive loaded pages
    if (candidates.length > 0) {
        // Sort by consecutive pages (descending), then by startpc (ascending - prefer smaller functions)
        candidates.sort((a, b) => {
            if (a.consecutivePages !== b.consecutivePages) {
                return b.consecutivePages - a.consecutivePages; // More pages first
            }
            return a.startpc - b.startpc; // Smaller startpc first
        });

        const best = candidates[0];
        console.error(`locateEntryPoint: Found ${candidates.length} candidate(s), selected best: fnheader=0x${best.fnheaderOffset.toString(16)}, startpc=${best.startpc}, ${best.consecutivePages} consecutive loaded pages`);
        return { fnheaderOffset: best.fnheaderOffset, pcOffset: best.pcOffset };
    }

    console.error(`locateEntryPoint: Could not find entry point function`);
    return null;
}
