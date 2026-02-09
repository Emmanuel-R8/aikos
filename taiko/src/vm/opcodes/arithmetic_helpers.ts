// Arithmetic operation helpers
// Provides arithmetic operations for opcodes

import type { VM } from '../vm';
import type { LispPTR } from '../../utils/types';
import { popStack, pushStack } from './stack_helpers';

/**
 * Check if value is a small integer (SMALLP)
 * Per C: SMALLP macro
 */
function isSmallP(value: LispPTR): boolean {
    // SMALLP: high 3 bits are 111 (0xE00000)
    return (value & 0xE00000) === 0xE00000;
}

/**
 * Extract integer value from SMALLP
 */
function smallPToInt(value: LispPTR): number {
    // Extract low 20 bits and sign-extend
    const low20 = value & 0xFFFFF;
    // Sign extend from bit 19
    if (low20 & 0x80000) {
        return low20 | 0xFFF00000; // Sign extend negative
    }
    return low20;
}

/**
 * Convert integer to SMALLP
 */
function intToSmallP(value: number): LispPTR {
    return 0xE00000 | (value & 0xFFFFF);
}

/**
 * Add two values (PLUS2)
 * Per C: N_OP_plus2 in maiko/src/arithops.c
 */
export function plus2(vm: VM): LispPTR {
    const b = popStack(vm);
    const a = vm.topOfStack;

    // TODO: Handle different number types (fixnum, bignum, float)
    // For now, assume both are SMALLP
    if (isSmallP(a) && isSmallP(b)) {
        const aInt = smallPToInt(a);
        const bInt = smallPToInt(b);
        const result = aInt + bInt;
        vm.topOfStack = intToSmallP(result);
    } else {
        // Placeholder: return sum of raw values
        vm.topOfStack = (a + b) & 0xFFFFFF;
    }

    return vm.topOfStack;
}

/**
 * Subtract two values (DIFFERENCE)
 * Per C: N_OP_difference in maiko/src/arithops.c
 */
export function difference(vm: VM): LispPTR {
    const b = popStack(vm);
    const a = vm.topOfStack;

    if (isSmallP(a) && isSmallP(b)) {
        const aInt = smallPToInt(a);
        const bInt = smallPToInt(b);
        const result = aInt - bInt;
        vm.topOfStack = intToSmallP(result);
    } else {
        vm.topOfStack = (a - b) & 0xFFFFFF;
    }

    return vm.topOfStack;
}

/**
 * Multiply two values (TIMES2)
 * Per C: N_OP_times2 in maiko/src/arithops.c
 */
export function times2(vm: VM): LispPTR {
    const b = popStack(vm);
    const a = vm.topOfStack;

    if (isSmallP(a) && isSmallP(b)) {
        const aInt = smallPToInt(a);
        const bInt = smallPToInt(b);
        const result = aInt * bInt;
        vm.topOfStack = intToSmallP(result);
    } else {
        vm.topOfStack = (a * b) & 0xFFFFFF;
    }

    return vm.topOfStack;
}

/**
 * Divide two values (QUOTIENT)
 * Per C: N_OP_quotient in maiko/src/arithops.c
 */
export function quotient(vm: VM): LispPTR {
    const b = popStack(vm);
    const a = vm.topOfStack;

    if (isSmallP(a) && isSmallP(b)) {
        const aInt = smallPToInt(a);
        const bInt = smallPToInt(b);
        if (bInt === 0) {
            vm.topOfStack = 0; // NIL on divide by zero
        } else {
            const result = Math.floor(aInt / bInt);
            vm.topOfStack = intToSmallP(result);
        }
    } else {
        vm.topOfStack = 0; // NIL
    }

    return vm.topOfStack;
}

/**
 * Integer addition (IPLUS2)
 */
export function iplus2(vm: VM): LispPTR {
    return plus2(vm); // Same as PLUS2 for integers
}

/**
 * Integer subtraction (IDIFFERENCE)
 */
export function idifference(vm: VM): LispPTR {
    return difference(vm); // Same as DIFFERENCE for integers
}

/**
 * Integer multiplication (ITIMES2)
 */
export function itimes2(vm: VM): LispPTR {
    return times2(vm); // Same as TIMES2 for integers
}
