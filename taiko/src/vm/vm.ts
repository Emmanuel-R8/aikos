// VM state structure and core VM functionality
import type { LispPTR, DLword } from '../utils/types';
import { STK_OFFSET, FRAMESIZE } from '../utils/constants';
import { MemoryManager } from './memory/manager';

/**
 * Stack frame structure (matches C frameex1)
 * Per maiko/inc/stack.h:81-108
 * Non-BIGVM version (most common)
 */
export interface StackFrame {
    flags_usecount: DLword; // flags (3 bits) + fast (1) + nil2 (1) + incall (1) + validnametable (1) + nopush (1) + usecount (8 bits)
    alink: DLword; // Activation link (Low addr)
    lofnheader: DLword; // Function header pointer (Low addr, 16 bits)
    hi1fnheader_hi2fnheader: number; // hi1fnheader (8 bits) + hi2fnheader (8 bits)
    nextblock: DLword; // Pointer to FreeStackBlock
    pc: DLword; // Program counter (byte offset from FuncObj)
    lonametable: DLword; // NameTable pointer (Low addr, 16 bits)
    hi1nametable_hi2nametable: number; // hi1nametable (8 bits) + hi2nametable (8 bits)
    blink: DLword; // blink pointer (Low addr)
    clink: DLword; // clink pointer (Low addr)
    // Local variables follow in memory after this struct
}

/**
 * VM state structure
 * Manages execution state, stack, and memory
 */
export class VM {
    // Stack management
    stackBase: number; // Byte offset of stack base
    stackPtr: number; // Current stack pointer (byte offset)
    stackEnd: number; // End of stack (byte offset)
    currentFrame: StackFrame | null = null;

    // Memory management
    virtualMemory: Uint8Array | null = null;
    fptovpTable: Uint32Array | null = null;
    atomSpaceOffset: number = 0; // Byte offset of AtomSpace in virtual memory
    defSpaceOffset: number = 0; // Byte offset of Defspace in virtual memory
    valSpaceOffset: number = 0; // Byte offset of Valspace in virtual memory
    plistSpaceOffset: number = 0; // Byte offset of PLISTSPACE in virtual memory
    dtdOffset: number = 0; // Byte offset of DTD space in virtual memory

    // Execution state
    pc: number = 0; // Program counter (byte offset)
    returnPc: number | null = null;
    funcObj: number | null = null; // Current function object (byte offset to function header)
    pvar: number | null = null; // Parameter variable pointer (byte offset)
    ivar: number | null = null; // Instance variable pointer (byte offset)

    // Cached stack top (matches C TopOfStack)
    // CRITICAL: C code uses TopOfStack as a cached value, initially 0
    topOfStack: LispPTR = 0; // Cached top of stack value (initially 0 = NIL)

    // TOS-stack pointer (C: CSTKPTRL in tos1defs.h)
    // Points to where TopOfStack would be stored (cell pointer, 4-byte units)
    cstkptrl: number | null = null; // Byte offset

    // Execution control
    stopRequested: boolean = false;
    running: boolean = false;
    totalInstructionCount: number = 0;
    stepCapActive: boolean = false;
    maxSteps: number | null = null; // EMULATOR_MAX_STEPS

    // Display region (for graphics)
    // Per C: DisplayRegion68k points to display bitmap (DLword array)
    // Each bit represents a pixel (1 = foreground, 0 = background)
    displayRegion: Uint16Array | null = null;
    displayWidth: number = 0;
    displayHeight: number = 0;
    displayRegionOffset: number = 0; // Byte offset in virtual memory

    /**
     * Initialize VM with stack size
     *
     * @param stackSizeBytes Stack size in bytes
     */
    constructor(stackSizeBytes: number = 64 * 1024) {
        // Stack grows down, so base is at the end
        this.stackBase = STK_OFFSET * 2; // Convert DLword offset to byte offset
        this.stackPtr = this.stackBase;
        this.stackEnd = this.stackBase - stackSizeBytes;
    }

    /**
     * Initialize VM state from virtual memory
     *
     * @param virtualMemory Virtual memory array
     * @param fptovpTable FPtoVP table
     * @param atomSpaceOffset Byte offset of AtomSpace
     * @param defSpaceOffset Byte offset of Defspace
     * @param valSpaceOffset Byte offset of Valspace
     * @param plistSpaceOffset Byte offset of PLISTSPACE
     * @param dtdOffset Byte offset of DTD space
     */
    initializeMemory(
        virtualMemory: Uint8Array,
        fptovpTable: Uint32Array,
        atomSpaceOffset: number,
        defSpaceOffset: number,
        valSpaceOffset: number,
        plistSpaceOffset: number,
        dtdOffset: number
    ): void {
        this.virtualMemory = virtualMemory;
        this.fptovpTable = fptovpTable;
        this.atomSpaceOffset = atomSpaceOffset;
        this.defSpaceOffset = defSpaceOffset;
        this.valSpaceOffset = valSpaceOffset;
        this.plistSpaceOffset = plistSpaceOffset;
        this.dtdOffset = dtdOffset;
    }

    /**
     * Initialize display region
     * Per C: init_display2() in maiko/src/initdsp.c
     *
     * @param displayAddr DLword offset of display region in virtual memory
     * @param width Display width in pixels
     * @param height Display height in pixels
     */
    initializeDisplay(displayAddr: number, width: number, height: number): void {
        this.displayWidth = width;
        this.displayHeight = height;
        this.displayRegionOffset = MemoryManager.Address.lispPtrToByte(displayAddr);

        // Create view of display region as Uint16Array
        if (this.virtualMemory && this.displayRegionOffset < this.virtualMemory.length) {
            const displaySize = Math.floor((width * height) / 16); // DLwords needed
            const displayEnd = this.displayRegionOffset + (displaySize * 2);
            if (displayEnd <= this.virtualMemory.length) {
                const displayBuffer = this.virtualMemory.buffer.slice(
                    this.virtualMemory.byteOffset + this.displayRegionOffset,
                    this.virtualMemory.byteOffset + displayEnd
                );
                this.displayRegion = new Uint16Array(displayBuffer);
            }
        }
    }

    /**
     * Get display region as Uint16Array
     * Returns a view of the display bitmap
     */
    getDisplayRegion(): Uint16Array | null {
        return this.displayRegion;
    }

    /**
     * Get current stack pointer offset (DLword offset from stack base)
     * Used for tracing and debugging
     */
    getStackPtrOffset(): number {
        if (this.virtualMemory === null) return 0;
        const stackByteOffset = this.stackPtr;
        const stackBaseByteOffset = this.stackBase;
        const offsetBytes = stackBaseByteOffset - stackByteOffset;
        return Math.floor(offsetBytes / 2); // Convert to DLword offset
    }

    /**
     * Get current frame pointer offset (DLword offset from stack base)
     */
    getFramePtrOffset(): number {
        if (this.currentFrame === null) return 0;
        // Frame pointer is stored in currentFrame, need to calculate from stack
        // This is a simplified version - actual implementation depends on frame structure
        return this.getStackPtrOffset();
    }

    /**
     * Read TopOfStack from memory
     * CRITICAL: Must be called after CSTKPTRL restore to sync TOPOFSTACK
     * Per C: TopOfStack = *(CSTKPTRL - 1)
     */
    readTopOfStackFromMemory(): LispPTR {
        if (this.virtualMemory === null || this.cstkptrl === null) {
            return 0; // NIL
        }

        // CSTKPTRL points to where TopOfStack would be stored
        // TopOfStack is at CSTKPTRL - 1 (previous cell)
        const tosOffset = this.cstkptrl - 4; // 4 bytes = 1 LispPTR cell
        if (tosOffset >= 0 && tosOffset + 4 <= this.virtualMemory.length) {
            const lo = this.virtualMemory[tosOffset] | (this.virtualMemory[tosOffset + 1] << 8);
            const hi = this.virtualMemory[tosOffset + 2] | (this.virtualMemory[tosOffset + 3] << 8);
            return (hi << 16) | lo;
        }
        return 0; // NIL
    }

    /**
     * Initialize CSTKPTRL from CurrentStackPTR
     * Per C: StackPtrRestore() macro
     */
    initCSTKPTRLFromCurrentStackPTR(): void {
        // CurrentStackPTR is the current stack pointer
        // CSTKPTRL = CurrentStackPTR (as cell pointer, 4-byte units)
        this.cstkptrl = this.stackPtr;
    }

    /**
     * Sync TopOfStack from memory after CSTKPTRL restore
     * CRITICAL: Must be called after initCSTKPTRLFromCurrentStackPTR()
     */
    syncTopOfStack(): void {
        this.topOfStack = this.readTopOfStackFromMemory();
    }

    /**
     * Reset VM state
     */
    reset(): void {
        this.pc = 0;
        this.returnPc = null;
        this.stopRequested = false;
        this.running = false;
        this.totalInstructionCount = 0;
        this.topOfStack = 0;
        this.cstkptrl = null;
        this.currentFrame = null;
        this.funcObj = null;
        this.pvar = null;
        this.ivar = null;
        this.stackPtr = this.stackBase;
    }
}
