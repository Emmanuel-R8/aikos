// Frame management for PVAR/IVAR/FVAR access
// Handles stack frame structure and variable access

import type { VM } from '../vm';
import type { LispPTR, DLword } from '../../utils/types';
import { MemoryManager } from './manager';
import { FRAMESIZE, S_POSITIVE } from '../../utils/constants';

/**
 * Stack frame structure (matches C FX structure)
 * Per maiko/inc/stack.h
 */
export interface StackFrame {
    flags_usecount: DLword;
    alink: DLword;
    lofnheader: DLword;
    hi1fnheader_hi2fnheader: number;
    nextblock: DLword;
    pc: DLword;
    lonametable: DLword;
    hi1nametable_hi2nametable: number;
    blink: DLword;
    clink: DLword;
    // Local variables follow after this structure
}

/**
 * Frame manager for accessing frame variables
 * Per maiko/inc/tosfns.h and maiko/inc/inlineC.h
 */
export class FrameManager {
    /**
     * Read parameter variable from current frame
     * Per C: PVARMACRO(x) -> PUSH(PVAR[x])
     * PVAR = (LispPTR *)PVar where PVar = (DLword *)CurrentStackPTR
     *
     * @param vm VM instance
     * @param index Parameter variable index (0-6 or variable)
     * @returns Parameter variable value
     */
    static readPVAR(vm: VM, index: number): LispPTR {
        if (vm.virtualMemory === null || vm.pvar === null) return 0;

        // PVAR points to PVAR area (after frame header)
        // PVAR[index] = *(PVar + index) as LispPTR
        const pvarOffset = vm.pvar + (index * 4); // 4 bytes per LispPTR

        if (pvarOffset + 4 <= vm.virtualMemory.length) {
            return MemoryManager.Access.readLispPTR(vm.virtualMemory, pvarOffset);
        }

        return 0; // NIL if invalid
    }

    /**
     * Write parameter variable to current frame
     * Per C: PVARSETMACRO(x) -> PVAR[x] = TOPOFSTACK
     *
     * @param vm VM instance
     * @param index Parameter variable index
     * @param value Value to write
     */
    static writePVAR(vm: VM, index: number, value: LispPTR): void {
        if (vm.virtualMemory === null || vm.pvar === null) return;

        const pvarOffset = vm.pvar + (index * 4);
        if (pvarOffset + 4 <= vm.virtualMemory.length) {
            MemoryManager.Access.writeLispPTR(vm.virtualMemory, pvarOffset, value);
        }
    }

    /**
     * Read instance variable from current frame
     * Per C: IVARMACRO(x) -> PUSH(IVAR[x])
     * IVAR = (LispPTR *)IVar where IVar = NativeAligned2FromStackOffset(CURRENTFX->nextblock)
     *
     * @param vm VM instance
     * @param index Instance variable index
     * @returns Instance variable value
     */
    static readIVAR(vm: VM, index: number): LispPTR {
        if (vm.virtualMemory === null || vm.ivar === null) return 0;

        // IVAR points to instance variable area (calculated from nextblock)
        // IVAR[index] = *(IVar + index) as LispPTR
        const ivarOffset = vm.ivar + (index * 4); // 4 bytes per LispPTR

        if (ivarOffset + 4 <= vm.virtualMemory.length) {
            return MemoryManager.Access.readLispPTR(vm.virtualMemory, ivarOffset);
        }

        return 0; // NIL if invalid
    }

    /**
     * Write instance variable to current frame
     * Per C: IVARX_(x) -> IVAR[x] = TOPOFSTACK
     *
     * @param vm VM instance
     * @param index Instance variable index
     * @param value Value to write
     */
    static writeIVAR(vm: VM, index: number, value: LispPTR): void {
        if (vm.virtualMemory === null || vm.ivar === null) return;

        const ivarOffset = vm.ivar + (index * 4);
        if (ivarOffset + 4 <= vm.virtualMemory.length) {
            MemoryManager.Access.writeLispPTR(vm.virtualMemory, ivarOffset, value);
        }
    }

    /**
     * Read frame variable from current frame
     * Per C: FVAR(n) -> N_OP_fvarn(n)
     * FVAR uses PVAR area but with name table lookup for unbound variables
     *
     * @param vm VM instance
     * @param index Frame variable index (word offset, not LispPTR offset)
     * @returns Frame variable value
     */
    static readFVAR(vm: VM, index: number): LispPTR {
        if (vm.virtualMemory === null || vm.pvar === null) return 0;

        // FVAR uses PVAR area: chain = (LispPTR *)(PVar + n)
        // n is word offset (DLword offset), so multiply by 2 for bytes
        const fvarOffset = vm.pvar + (index * 2); // 2 bytes per DLword

        // Read FVAR slot (2 DLwords = 4 bytes)
        if (fvarOffset + 4 <= vm.virtualMemory.length) {
            // Check if unbound (LSB of first word)
            const firstWord = MemoryManager.Access.readDLword(vm.virtualMemory, fvarOffset);
            if ((firstWord & 0x8000) !== 0) {
                // Unbound - perform name table lookup
                // Simplified: For now, mark as looked up (full implementation would scan name tables)
                // The full nfvlookup would scan frames through access links
                // For now, return NIL to indicate lookup needed
                return 0; // NIL - indicates unbound variable
            }

            // Bound - extract pointer from FVAR slot
            // Per C: GetLongWord(NativeAligned4FromLAddr(POINTERMASK & (((GETBASEWORD(chain, 1)) << 16) | GETBASEWORD(chain, 0))))
            const highWord = MemoryManager.Access.readDLword(vm.virtualMemory, fvarOffset + 2);
            const lowWord = MemoryManager.Access.readDLword(vm.virtualMemory, fvarOffset);
            const pointer = ((highWord & 0xFFFF) << 16) | (lowWord & 0xFFFF);
            const maskedPointer = pointer & 0x0FFFFFFF; // POINTERMASK

            if (maskedPointer !== 0) {
                const targetOffset = MemoryManager.Address.lispPtrToByte(maskedPointer);
                if (targetOffset + 4 <= vm.virtualMemory.length) {
                    return MemoryManager.Access.readLispPTR(vm.virtualMemory, targetOffset);
                }
            }
        }

        return 0; // NIL if invalid
    }

    /**
     * Read argument 0 from current frame
     * Per C: ARG0 macro
     * ARG0 is typically the first argument passed to the function
     *
     * @param vm VM instance
     * @returns Argument 0 value
     */
    static readARG0(vm: VM): LispPTR {
        // ARG0 is typically stored before PVAR area or in a specific location
        // Per C: Arguments are on stack before the frame
        // For now, simplified: ARG0 might be at IVAR[0] or before frame
        if (vm.ivar !== null) {
            return FrameManager.readIVAR(vm, 0);
        }
        return 0;
    }

    /**
     * Get current frame extension (CURRENTFX)
     * Per C: BCE_CURRENTFX = ((struct frameex2 *)((DLword *)PVAR - FRAMESIZE))
     *
     * @param vm VM instance
     * @returns Byte offset of current frame, or null if invalid
     */
    static getCurrentFX(vm: VM): number | null {
        if (vm.pvar === null) return null;
        // CURRENTFX = PVAR - FRAMESIZE
        return vm.pvar - (FRAMESIZE * 2); // FRAMESIZE DLwords = FRAMESIZE * 2 bytes
    }

    /**
     * Read alink from current frame
     * Per C: CURRENTFX->alink
     *
     * @param vm VM instance
     * @returns Access link value
     */
    static readALink(vm: VM): number {
        const fxOffset = FrameManager.getCurrentFX(vm);
        if (fxOffset === null || vm.virtualMemory === null) return 0;

        // alink is at offset 2 (second DLword) in frame
        if (fxOffset + 4 <= vm.virtualMemory.length) {
            return MemoryManager.Access.readDLword(vm.virtualMemory, fxOffset + 2);
        }
        return 0;
    }

    /**
     * Read blink from current frame
     * Per C: CURRENTFX->blink
     *
     * @param vm VM instance
     * @returns Binding link value
     */
    static readBLink(vm: VM): number {
        const fxOffset = FrameManager.getCurrentFX(vm);
        if (fxOffset === null || vm.virtualMemory === null) return 0;

        // blink is at offset 16 (8th DLword) in frame
        if (fxOffset + 18 <= vm.virtualMemory.length) {
            return MemoryManager.Access.readDLword(vm.virtualMemory, fxOffset + 16);
        }
        return 0;
    }

    /**
     * Calculate argument count from current frame
     * Per C: MYARGCOUNT macro
     *
     * @param vm VM instance
     * @returns Argument count as SMALLP
     */
    static getArgCount(vm: VM): LispPTR {
        if (vm.virtualMemory === null || vm.pvar === null || vm.ivar === null) {
            return 0; // NIL on error
        }

        const fxOffset = FrameManager.getCurrentFX(vm);
        if (fxOffset === null) return 0;

        // Read alink from frame
        const alink = FrameManager.readALink(vm);

        let argNum: number;
        if ((alink & 1) === 0) {
            // Fast case: arg_num = address of word before CURRENTFX (BF marker)
            // CURRENTFX is at fxOffset, so word before is at fxOffset - 4
            argNum = fxOffset - 4;
        } else {
            // Slow case: arg_num = Stackspace + CURRENTFX->blink
            const blink = FrameManager.readBLink(vm);
            argNum = vm.stackBase + (blink * 2); // Convert DLword offset to byte offset
        }

        // Calculate: (arg_num - IVar) >> 2 (divide by 4 to get number of LispPTRs)
        const argCount = (argNum - vm.ivar) >> 2;

        // Return as SMALLP (S_POSITIVE | argCount)
        return (S_POSITIVE | (argCount & 0xFFFF));
    }

    /**
     * Get access link from current frame
     * Per C: MYALINK macro
     *
     * @param vm VM instance
     * @returns Access link as SMALLP
     */
    static getALink(vm: VM): LispPTR {
        const fxOffset = FrameManager.getCurrentFX(vm);
        if (fxOffset === null) return 0;

        const alink = FrameManager.readALink(vm);

        // MYALINK: (((CURRENTFX->alink) & 0xfffe) - FRAMESIZE) | S_POSITIVE
        const maskedAlink = alink & 0xFFFE; // Mask off bit 0
        const alinkValue = maskedAlink - FRAMESIZE; // Subtract FRAMESIZE

        // Return as SMALLP
        return (S_POSITIVE | (alinkValue & 0xFFFF));
    }

    /**
     * Parse frame structure from memory
     *
     * @param vm VM instance
     * @param frameOffset DLword offset of frame
     * @returns Parsed frame structure or null
     */
    static parseFrame(vm: VM, frameOffset: number): StackFrame | null {
        if (vm.virtualMemory === null) return null;

        const byteOffset = MemoryManager.Address.lispPtrToByte(frameOffset);
        if (byteOffset + 20 > vm.virtualMemory.length) return null;

        const frame: StackFrame = {
            flags_usecount: MemoryManager.Access.readDLword(vm.virtualMemory, byteOffset),
            alink: MemoryManager.Access.readDLword(vm.virtualMemory, byteOffset + 2),
            lofnheader: MemoryManager.Access.readDLword(vm.virtualMemory, byteOffset + 4),
            hi1fnheader_hi2fnheader: MemoryManager.Access.readDLword(vm.virtualMemory, byteOffset + 6),
            nextblock: MemoryManager.Access.readDLword(vm.virtualMemory, byteOffset + 8),
            pc: MemoryManager.Access.readDLword(vm.virtualMemory, byteOffset + 10),
            lonametable: MemoryManager.Access.readDLword(vm.virtualMemory, byteOffset + 12),
            hi1nametable_hi2nametable: MemoryManager.Access.readDLword(vm.virtualMemory, byteOffset + 14),
            blink: MemoryManager.Access.readDLword(vm.virtualMemory, byteOffset + 16),
            clink: MemoryManager.Access.readDLword(vm.virtualMemory, byteOffset + 18),
        };

        return frame;
    }
}
