// Type checking utilities for Lisp values
// Per maiko/inc/lsptypes.h and maiko/src/typetbl.c

import type { LispPTR } from '../../utils/types';
import { S_POSITIVE, S_NEGATIVE, NIL_PTR, POINTERMASK } from '../../utils/constants';
import { MemoryManager } from './manager';

/**
 * Type number constants
 * Per maiko/inc/lsptypes.h:61-66
 */
export const TYPE_SMALLP = 1;     // Small integer (immediate)
export const TYPE_FIXP = 2;       // Fixnum (bignum)
export const TYPE_FLOATP = 3;     // Floating point number
export const TYPE_LISTP = 5;      // List (cons cell)

/**
 * Check if value is an immediate (SMALLP, character, etc.)
 * Per C: IF_IMMEDIATE macro
 *
 * Immediate values have high bits set (S_POSITIVE, S_NEGATIVE, etc.)
 */
export function isImmediate(value: LispPTR): boolean {
    // SMALLP positive: top 3 bits set (S_POSITIVE)
    // SMALLP negative: top 2 bits set (S_NEGATIVE)
    // Characters and other immediates also have high bits set
    return (value & 0xC0000000) !== 0 && (value & POINTERMASK) === value;
}

/**
 * Get type number of a Lisp value
 * Per C: GetTypeNumber in maiko/src/typetbl.c
 *
 * Simplified version - for now, only handles:
 * - SMALLP (immediate integers)
 * - FIXP (bignum fixnums)
 * - FLOATP (floating point)
 * - LISTP (cons cells)
 *
 * TODO: Full implementation would check memory structures
 */
export function getTypeNumber(value: LispPTR, vm: { virtualMemory: Uint8Array | null } | null = null): number {
    // NIL is a special case
    if (value === NIL_PTR) {
        return TYPE_LISTP; // NIL is a list
    }

    // Check for SMALLP (immediate integers)
    if ((value & S_POSITIVE) === S_POSITIVE) {
        return TYPE_SMALLP;
    }
    if ((value & S_NEGATIVE) === S_NEGATIVE) {
        return TYPE_SMALLP;
    }

    // For non-immediate values, we'd need to check memory
    // For now, assume they're pointers to typed objects
    // This is a simplified implementation
    // TODO: Read type from memory structure

    // If it's a valid pointer (within POINTERMASK), it could be:
    // - FIXP (bignum)
    // - FLOATP
    // - LISTP (cons cell)
    // - Other types

    // For now, return a default type
    // Full implementation would read the type from memory
    return TYPE_LISTP; // Default to list for now
}

/**
 * Check if value is a number (SMALLP, FIXP, or FLOATP)
 * Per C: Numberp macro
 */
export function isNumber(value: LispPTR, vm: { virtualMemory: Uint8Array | null } | null = null): boolean {
    const type = getTypeNumber(value, vm);
    return type === TYPE_SMALLP || type === TYPE_FIXP || type === TYPE_FLOATP;
}

/**
 * Get FIXP value from memory
 * Per C: FIXP_VALUE macro
 *
 * FIXP values are stored in memory as 32-bit integers
 */
export function getFIXPValue(value: LispPTR, vm: { virtualMemory: Uint8Array | null }): number | null {
    if (!vm.virtualMemory) {
        return null;
    }

    const offset = MemoryManager.Address.lispPtrToByte(value);
    if (offset + 4 > vm.virtualMemory.length) {
        return null;
    }

    const view = new DataView(vm.virtualMemory.buffer, vm.virtualMemory.byteOffset + offset);
    return view.getInt32(0, true); // little-endian
}

/**
 * Get FLOATP value from memory
 * Per C: FLOATP_VALUE macro
 *
 * FLOATP values are stored in memory as 64-bit floats
 */
export function getFLOATPValue(value: LispPTR, vm: { virtualMemory: Uint8Array | null }): number | null {
    if (!vm.virtualMemory) {
        return null;
    }

    const offset = MemoryManager.Address.lispPtrToByte(value);
    if (offset + 8 > vm.virtualMemory.length) {
        return null;
    }

    const view = new DataView(vm.virtualMemory.buffer, vm.virtualMemory.byteOffset + offset);
    return view.getFloat64(0, true); // little-endian
}
