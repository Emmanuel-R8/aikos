// AtomSpace and Valspace management
// Handles global variable lookups (GVAR opcode)

import type { LispPTR } from '../../utils/types';
import { MemoryManager } from './manager';

/**
 * AtomSpace manager
 * Per maiko/inc/lspglob.h
 * Stores atom definitions and their value cells
 */
export class AtomSpaceManager {
    /**
     * Get value cell for atom
     * Per C: GVAR macro in maiko/inc/inlineC.h
     *
     * For non-BIGATOMS: Valspace + (atom_index << 1)
     * For BIGATOMS: AtomSpace + (atom_index * 5) + NEWATOM_VALUE_PTROFF
     *
     * @param memory Virtual memory array
     * @param valSpaceOffset Byte offset of Valspace in virtual memory
     * @param atomIndex Atom index (from GVAR operand)
     * @returns Value cell LispPTR
     */
    static getValueCell(memory: Uint8Array, valSpaceOffset: number, atomIndex: number): LispPTR {
        // Simplified: assume non-BIGATOMS layout
        // Valspace layout: byte offset = valSpaceOffset + atom_index * 4
        // Per C: Valspace + (atom_index << 1) = Valspace + (atom_index * 2) DLwords = (atom_index * 4) bytes
        const valspaceOffset = valSpaceOffset + (atomIndex * 4);

        if (valspaceOffset + 4 <= memory.length) {
            return MemoryManager.Access.readLispPTR(memory, valspaceOffset);
        }

        return 0; // NIL if invalid
    }

    /**
     * Set value cell for atom
     * Per C: GVAR_ opcode in maiko/src/gvar2.c
     *
     * @param memory Virtual memory array
     * @param valSpaceOffset Byte offset of Valspace in virtual memory
     * @param atomIndex Atom index
     * @param value New value
     */
    static setValueCell(memory: Uint8Array, valSpaceOffset: number, atomIndex: number, value: LispPTR): void {
        const valspaceOffset = valSpaceOffset + (atomIndex * 4);

        if (valspaceOffset + 4 <= memory.length) {
            MemoryManager.Access.writeLispPTR(memory, valspaceOffset, value);
        }
    }

    /**
     * Get global variable value cell address
     * Per C: GetVALCELL68k(atom_index)
     *
     * @param memory Virtual memory array
     * @param valSpaceOffset Byte offset of Valspace in virtual memory
     * @param atomIndex Atom index
     * @returns Byte offset of value cell in virtual memory
     */
    static getGlobalVarValueCellAddress(memory: Uint8Array, valSpaceOffset: number, atomIndex: number): number {
        return valSpaceOffset + (atomIndex * 4);
    }
}
