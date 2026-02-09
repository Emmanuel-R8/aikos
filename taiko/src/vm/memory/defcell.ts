// Definition cell lookup for function calls
// Per maiko/inc/lispemul.h and maiko/src/bbtsub.c

import type { LispPTR, DLword } from '../../utils/types';
import { MemoryManager } from './manager';
import { NIL_PTR } from '../../utils/constants';

/**
 * Definition cell structure
 * Per maiko/inc/lispemul.h
 */
export interface DefCell {
    defpointer: LispPTR; // Function pointer or closure pointer
    ccodep: number; // C code flag (1 = C function, 0 = Lisp function)
    argtype: number; // Argument type
}

/**
 * Function header structure
 * Per maiko/inc/stack.h:58-79
 */
export interface FunctionHeader {
    stkmin: DLword; // Stack minimum
    na: number; // Number of arguments (signed short)
    pv: number; // Parameter variables count (signed short)
    startpc: DLword; // Start PC (byte offset from function header)
    nil4: number; // Not used
    byteswapped: number; // Code was reswapped
    argtype: number; // Argument type
    framename: LispPTR; // Frame name (index in AtomSpace)
    ntsize: DLword; // Size of NameTable
    nlocals: number; // Number of locals
    fvaroffset: number; // FVAR offset from NameTable
}

/**
 * Definition cell manager
 * Handles lookup of function definitions from atom indices
 */
export class DefCellManager {
    /**
     * Get definition cell for atom index
     * Per C: GetDEFCELL68k(atom_index)
     * For non-BIGATOMS: AtomSpace + (atom_index * 4)
     * For BIGATOMS: AtomSpace + (atom_index * 5) + NEWATOM_DEFN_PTROFF
     *
     * @param memory Virtual memory array
     * @param atomSpaceOffset Byte offset of AtomSpace in virtual memory
     * @param atomIndex Atom index (from GVAR/FN operand)
     * @returns Definition cell or null if invalid
     */
    static getDefCell(memory: Uint8Array, atomSpaceOffset: number, atomIndex: number): DefCell | null {
        // AtomSpace layout: each atom has a 4-byte definition cell
        // defpointer (4 bytes) contains the function pointer
        // For now, simplified layout - actual layout may vary
        // Per C: GetDEFCELL68k(atom_index) = (DefCell *)(AtomSpace + (atom_index * 4))
        const defcellOffset = atomSpaceOffset + (atomIndex * 4);

        if (defcellOffset + 4 <= memory.length) {
            const defpointer = MemoryManager.Access.readLispPTR(memory, defcellOffset);

            // Read ccodep and argtype from defpointer flags
            // Simplified: assume defpointer format includes flags
            // Actual C code checks defcell->ccodep flag
            // Per C: defcell->ccodep is a bit flag in the defpointer
            const ccodep = (defpointer & 0x80000000) ? 1 : 0; // Simplified flag check
            const argtype = (defpointer >> 24) & 0x7F; // Simplified argtype extraction

            return {
                defpointer: defpointer & 0x7FFFFFFF, // Clear flag bits
                ccodep,
                argtype,
            };
        }

        return null;
    }

    /**
     * Check if definition cell is C code
     *
     * @param defcell Definition cell
     * @returns True if C code function
     */
    static isCCode(defcell: DefCell): boolean {
        return defcell.ccodep !== 0;
    }

    /**
     * Get function header pointer from definition cell
     * Per C: NativeAligned4FromLAddr(defcell->defpointer)
     *
     * @param defcell Definition cell
     * @returns Function header LispPTR or 0 if invalid
     */
    static getFunctionHeaderPtr(defcell: DefCell): LispPTR {
        return defcell.defpointer & 0x7FFFFFFF; // Clear high bit flags
    }

    /**
     * Read function header from memory
     * Per C: (struct fnhead *)NativeAligned4FromLAddr(fnheader_ptr)
     *
     * @param memory Virtual memory array
     * @param fnheaderPtr Function header pointer (LispPTR)
     * @returns Function header or null if invalid
     */
    static readFunctionHeader(memory: Uint8Array, fnheaderPtr: LispPTR): FunctionHeader | null {
        const byteOffset = MemoryManager.Address.lispPtrToByte(fnheaderPtr);

        if (byteOffset + 16 > memory.length) {
            return null; // Not enough bytes for function header
        }

        // Read function header fields (little-endian, but sysout may be big-endian)
        // Per maiko/inc/stack.h:58-79
        const stkmin = MemoryManager.Access.readDLword(memory, byteOffset);
        const naRaw = MemoryManager.Access.readDLword(memory, byteOffset + 2);
        const pvRaw = MemoryManager.Access.readDLword(memory, byteOffset + 4);
        const startpc = MemoryManager.Access.readDLword(memory, byteOffset + 6);

        // Read flags byte (byte 8)
        const flagsByte = memory[ byteOffset + 8 ];
        const nil4 = (flagsByte >> 7) & 1;
        const byteswapped = (flagsByte >> 6) & 1;
        const argtype = (flagsByte >> 4) & 3;
        const framenameLow = flagsByte & 0xF;

        // Read framename high bits (bytes 9-11 for BIGVM, bytes 9-10 for non-BIGVM)
        // Assuming BIGVM (28 bits total)
        const framenameMid = (memory[ byteOffset + 9 ] << 16) |
            (memory[ byteOffset + 10 ] << 8) |
            memory[ byteOffset + 11 ];
        const framename: LispPTR = (framenameLow << 24) | framenameMid;

        const ntsize = MemoryManager.Access.readDLword(memory, byteOffset + 12);
        const nlocals = memory[ byteOffset + 14 ];
        const fvaroffset = memory[ byteOffset + 15 ];

        // Convert signed na and pv
        const na = new Int16Array([ naRaw ])[ 0 ]; // Convert to signed
        const pv = new Int16Array([ pvRaw ])[ 0 ]; // Convert to signed

        return {
            stkmin,
            na,
            pv,
            startpc,
            nil4,
            byteswapped,
            argtype,
            framename,
            ntsize,
            nlocals,
            fvaroffset,
        };
    }
}
