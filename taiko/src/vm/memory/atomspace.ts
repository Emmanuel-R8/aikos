// AtomSpace and Valspace management
// Handles global variable lookups (GVAR opcode)

import type { LispPTR } from '../../utils/types';
import { MemoryManager } from './manager';
import { POINTERMASK, SEGMASK } from '../../utils/constants';

const NEWATOM_VALUE_PTROFF = 1;
const NEWATOM_STRIDE_LISPPTRS = 5;
const DLWORDS_PER_LISPPTR = 2;

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
    static getValueCell(memory: Uint8Array, atomSpaceOffset: number, atomIndex: number): LispPTR {
        let valueCellOffset: number;

        if ((atomIndex & SEGMASK) !== 0) {
            const valueCellPtr = (atomIndex + (NEWATOM_VALUE_PTROFF * DLWORDS_PER_LISPPTR)) & POINTERMASK;
            valueCellOffset = MemoryManager.Address.lispPtrToByte(valueCellPtr);
        } else {
            valueCellOffset = atomSpaceOffset + ((NEWATOM_STRIDE_LISPPTRS * atomIndex) + NEWATOM_VALUE_PTROFF) * 4;
        }

        if (valueCellOffset + 4 <= memory.length) {
            return MemoryManager.Access.readLispPTR(memory, valueCellOffset);
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
    static setValueCell(memory: Uint8Array, atomSpaceOffset: number, atomIndex: number, value: LispPTR): void {
        let valueCellOffset: number;

        if ((atomIndex & SEGMASK) !== 0) {
            const valueCellPtr = (atomIndex + (NEWATOM_VALUE_PTROFF * DLWORDS_PER_LISPPTR)) & POINTERMASK;
            valueCellOffset = MemoryManager.Address.lispPtrToByte(valueCellPtr);
        } else {
            valueCellOffset = atomSpaceOffset + ((NEWATOM_STRIDE_LISPPTRS * atomIndex) + NEWATOM_VALUE_PTROFF) * 4;
        }

        if (valueCellOffset + 4 <= memory.length) {
            MemoryManager.Access.writeLispPTR(memory, valueCellOffset, value);
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
    static getGlobalVarValueCellAddress(memory: Uint8Array, atomSpaceOffset: number, atomIndex: number): number {
        if ((atomIndex & SEGMASK) !== 0) {
            const valueCellPtr = (atomIndex + (NEWATOM_VALUE_PTROFF * DLWORDS_PER_LISPPTR)) & POINTERMASK;
            return MemoryManager.Address.lispPtrToByte(valueCellPtr);
        }

        return atomSpaceOffset + ((NEWATOM_STRIDE_LISPPTRS * atomIndex) + NEWATOM_VALUE_PTROFF) * 4;
    }
}
