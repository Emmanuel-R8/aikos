// Address translation utilities
// See specifications/memory/address-translation.typ
import type { LispPTR } from '../../utils/types';
import { BYTESPER_PAGE } from '../../utils/constants';

/**
 * Address translation utilities
 * Converts between LispPTR (DLword offset) and byte offsets
 */
export class AddressManager {
    /**
     * Convert LispPTR (DLword offset) to byte offset
     * Formula: lisp_ptr * 2 (each DLword = 2 bytes)
     */
    static lispPtrToByte(lisp_ptr: LispPTR): number {
        return lisp_ptr * 2;
    }

    /**
     * Convert byte offset to LispPTR (DLword offset)
     * Formula: byte_offset / 2 (convert bytes to DLwords)
     */
    static byteToLispPtr(byte_offset: number): LispPTR {
        return Math.floor(byte_offset / 2);
    }

    /**
     * Convert a stack-space offset in DLwords to an absolute byte offset.
     * Per C: NativeAligned2FromStackOffset(StackOffset) = Stackspace + StackOffset.
     */
    static stackOffsetToByte(stack_base_byte_offset: number, stack_offset: number): number {
        return stack_base_byte_offset + (stack_offset * 2);
    }

    /**
     * Convert an absolute byte offset within Stackspace to a DLword stack offset.
     * Per C: StackOffsetFromNative(SAddr) = ((DLword *)SAddr) - Stackspace.
     */
    static byteToStackOffset(stack_base_byte_offset: number, byte_offset: number): number {
        return Math.floor((byte_offset - stack_base_byte_offset) / 2);
    }

    /**
     * Calculate virtual page number for byte offset
     * Formula: byte_offset / BYTES_PER_PAGE
     */
    static getVirtualPage(byte_offset: number): number {
        return Math.floor(byte_offset / BYTESPER_PAGE);
    }

    /**
     * Calculate offset within virtual page for byte offset
     * Formula: byte_offset % BYTES_PER_PAGE
     */
    static getPageOffset(byte_offset: number): number {
        return byte_offset % BYTESPER_PAGE;
    }

    /**
     * Calculate virtual address base for page number
     * Formula: vpage * BYTES_PER_PAGE
     */
    static getVirtualAddressBase(vpage: number): number {
        return vpage * BYTESPER_PAGE;
    }
}
