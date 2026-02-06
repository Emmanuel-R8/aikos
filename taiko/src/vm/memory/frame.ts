// Frame management for PVAR/IVAR/FVAR access
// Handles stack frame structure and variable access

import type { VM } from '../vm';
import type { LispPTR, DLword } from '../../utils/types';
import { MemoryManager } from './manager';
import { FRAMESIZE } from '../../utils/constants';

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
 */
export class FrameManager {
    /**
     * Read parameter variable from current frame
     * Per C: PVAR macro in maiko/inc/tosfns.h
     *
     * @param vm VM instance
     * @param index Parameter variable index (0-6 or variable)
     * @returns Parameter variable value
     */
    static readPVAR(vm: VM, index: number): LispPTR {
        if (vm.virtualMemory === null || vm.currentFrame === null) return 0;

        // PVARs are stored after the frame structure
        // Frame is 10 DLwords (20 bytes), PVARs start at offset 20
        const frameOffset = vm.getFramePtrOffset();
        const byteOffset = MemoryManager.Address.lispPtrToByte(frameOffset) + (FRAMESIZE * 2) + (index * 4);

        if (byteOffset + 4 <= vm.virtualMemory.length) {
            return MemoryManager.Access.readLispPTR(vm.virtualMemory, byteOffset);
        }

        return 0;
    }

    /**
     * Read instance variable from current object
     * Per C: IVAR macro in maiko/inc/tosfns.h
     *
     * @param vm VM instance
     * @param index Instance variable index
     * @returns Instance variable value
     */
    static readIVAR(vm: VM, index: number): LispPTR {
        // TODO: Get current object from stack/context
        // IVARs are stored in the object's instance variable array
        // For now, return placeholder
        return 0;
    }

    /**
     * Read frame variable from current frame
     * Per C: FVAR macro in maiko/inc/tosfns.h
     *
     * @param vm VM instance
     * @param index Frame variable index
     * @returns Frame variable value
     */
    static readFVAR(vm: VM, index: number): LispPTR {
        // FVARs are similar to PVARs but accessed differently
        // For now, return placeholder
        return 0;
    }

    /**
     * Read argument 0 from current frame
     * Per C: ARG0 macro
     */
    static readARG0(vm: VM): LispPTR {
        // ARG0 is typically stored before the frame or in a specific location
        // For now, return placeholder
        return 0;
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
