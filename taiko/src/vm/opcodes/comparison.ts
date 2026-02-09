// Comparison operation opcodes
import type { VM } from '../vm';
import type { Instruction } from '../dispatch/opcode';
import { Opcode } from '../dispatch/opcode';
import { registerOpcodeHandler } from './index';
import { popStack, pushStack } from './stack_helpers';
import { NIL_PTR, ATOM_T, S_POSITIVE, S_NEGATIVE } from '../../utils/constants';
import { isImmediate, getTypeNumber, isNumber, getFIXPValue, getFLOATPValue, TYPE_FIXP, TYPE_FLOATP, TYPE_LISTP } from '../memory/typecheck';
import { isSmallP, smallPToInt } from './arithmetic_helpers';
import type { LispPTR } from '../../utils/types';
import { MemoryManager } from '../memory/manager';

/**
 * EQ opcode handler
 * Strict pointer comparison (equivalent to C's ==)
 * Per C: inline EQ in maiko/src/eqf.c (opcode 0360)
 *
 * EQ returns T if both values are the same pointer, NIL otherwise
 * This is simpler than EQL - it does no type conversions or value comparisons
 *
 * Stack: [arg1, arg2] -> [result]
 */
function handleEQ(vm: VM, instruction: Instruction): number | null {
    const arg2 = popStack(vm);
    const arg1 = popStack(vm);

    // Strict pointer comparison - same pointer means equal
    pushStack(vm, arg1 === arg2 ? ATOM_T : NIL_PTR);
    return null;
}

/**
 * EQL opcode handler
 * Compares two values using EQL semantics (Common Lisp EQL)
 * Per C: N_OP_eqlop in maiko/src/eqf.c
 *
 * EQL returns T if:
 * - Both values are EQ (same pointer)
 * - Both are numbers of the same type and equal value
 * - Both are characters with same code
 *
 * Returns NIL otherwise
 */
function handleEQL(vm: VM, instruction: Instruction): number | null {
    const arg2 = popStack(vm);
    const arg1 = popStack(vm);

    // If same pointer, they're equal
    if (arg1 === arg2) {
        pushStack(vm, ATOM_T);
        return null;
    }

    // Per C: IF_IMMEDIATE(arg1, return (NIL), return (NIL));
    // If either is immediate and they're not the same pointer, they're not equal
    if (isImmediate(arg1) || isImmediate(arg2)) {
        // For SMALLP, we can compare values directly
        if (isSmallP(arg1) && isSmallP(arg2)) {
            const val1 = smallPToInt(arg1);
            const val2 = smallPToInt(arg2);
            pushStack(vm, val1 === val2 ? ATOM_T : NIL_PTR);
            return null;
        }
        // Other immediates (characters, etc.) - if not same pointer, not equal
        pushStack(vm, NIL_PTR);
        return null;
    }

    // Get type numbers - if different types, not equal
    const type1 = getTypeNumber(arg1, vm);
    const type2 = getTypeNumber(arg2, vm);

    if (type1 !== type2) {
        pushStack(vm, NIL_PTR);
        return null;
    }

    // Both are same type - compare values based on type
    switch (type1) {
        case TYPE_FIXP: {
            // Both are fixnums - compare values
            const val1 = getFIXPValue(arg1, vm);
            const val2 = getFIXPValue(arg2, vm);
            if (val1 !== null && val2 !== null && val1 === val2) {
                pushStack(vm, ATOM_T);
                return null;
            }
            pushStack(vm, NIL_PTR);
            return null;
        }

        case TYPE_FLOATP: {
            // Both are floats - compare values
            const val1 = getFLOATPValue(arg1, vm);
            const val2 = getFLOATPValue(arg2, vm);
            if (val1 !== null && val2 !== null && val1 === val2) {
                pushStack(vm, ATOM_T);
                return null;
            }
            pushStack(vm, NIL_PTR);
            return null;
        }

        default:
            // For other types, if not same pointer, not equal
            // Per C: if (Numberp(arg1)) { ERROR_EXIT(arg2); } else return (NIL);
            if (isNumber(arg1, vm)) {
                // This shouldn't happen if types match, but handle it
                pushStack(vm, NIL_PTR);
                return null;
            }
            pushStack(vm, NIL_PTR);
            return null;
    }
}

/**
 * Recursive EQUAL comparison helper
 * Compares two values recursively (for lists, arrays, etc.)
 * Uses EQL semantics at the leaves
 */
function equalRecursive(vm: VM, arg1: LispPTR, arg2: LispPTR, depth: number = 0): boolean {
    // Prevent infinite recursion (circular lists)
    if (depth > 100) {
        return false; // Too deep, assume not equal
    }

    // If same pointer, they're equal
    if (arg1 === arg2) {
        return true;
    }

    // Handle immediate values (SMALLP, characters)
    if (isImmediate(arg1) || isImmediate(arg2)) {
        // For SMALLP, compare values
        if (isSmallP(arg1) && isSmallP(arg2)) {
            return smallPToInt(arg1) === smallPToInt(arg2);
        }
        // Other immediates - if not same pointer, not equal
        return false;
    }

    // Get type numbers
    const type1 = getTypeNumber(arg1, vm);
    const type2 = getTypeNumber(arg2, vm);

    // If different types, not equal (except for numbers which can be compared)
    if (type1 !== type2) {
        // Numbers can be compared even if different types (e.g., FIXP vs FLOATP)
        if (isNumber(arg1, vm) && isNumber(arg2, vm)) {
            // Compare numeric values (would need conversion logic)
            // For now, if types differ, not equal
            return false;
        }
        return false;
    }

    // Both are same type - handle based on type
    switch (type1) {
        case TYPE_FIXP: {
            const val1 = getFIXPValue(arg1, vm);
            const val2 = getFIXPValue(arg2, vm);
            return val1 !== null && val2 !== null && val1 === val2;
        }

        case TYPE_FLOATP: {
            const val1 = getFLOATPValue(arg1, vm);
            const val2 = getFLOATPValue(arg2, vm);
            return val1 !== null && val2 !== null && val1 === val2;
        }

        case TYPE_LISTP: {
            // Recursive comparison for lists
            // Compare CAR and CDR recursively
            const car1 = getCAR(vm, arg1);
            const car2 = getCAR(vm, arg2);

            if (!equalRecursive(vm, car1, car2, depth + 1)) {
                return false;
            }

            const cdr1 = getCDR(vm, arg1);
            const cdr2 = getCDR(vm, arg2);

            return equalRecursive(vm, cdr1, cdr2, depth + 1);
        }

        default:
            // For other types, use EQL semantics (pointer comparison)
            return false;
    }
}

/**
 * CMLEQUAL opcode handler
 * Common Lisp EQUAL comparison (recursive)
 * Per C: N_OP_clequal in maiko/src/eqf.c
 *
 * CL:EQUAL is a recursive comparison that uses EQL at the leaves
 * It recursively compares lists, arrays, and other structures
 */
function handleCMLEQUAL(vm: VM, instruction: Instruction): number | null {
    const arg2 = popStack(vm);
    const arg1 = popStack(vm);

    // Use recursive comparison
    const result = equalRecursive(vm, arg1, arg2);
    pushStack(vm, result ? ATOM_T : NIL_PTR);
    return null;
}

/**
 * RECLAIMCELL opcode handler
 * Reclaims a memory cell (GC operation)
 * Per C: RECLAIMCELL macro in maiko/inc/inlineC.h
 *
 * Currently a no-op (just advances PC)
 * Full implementation would reclaim the cell on TOPOFSTACK
 */
function handleRECLAIMCELL(vm: VM, instruction: Instruction): number | null {
    // Per C: RECLAIMCELL is a no-op in most configurations
    // It just advances to next instruction
    // Full implementation would:
    // - Pop cell from stack
    // - Reclaim it to free list
    // - Update GC statistics
    return null;
}

/**
 * Get CAR of cons cell (helper function)
 * Re-exported from list.ts for use in comparison.ts
 */
function getCAR(vm: VM, listPtr: LispPTR): LispPTR {
    if (vm.virtualMemory === null) return 0;
    if (listPtr === 0) return 0; // NIL

    const byteOffset = MemoryManager.Address.lispPtrToByte(listPtr);
    if (byteOffset + 4 > vm.virtualMemory.length) return 0;

    // Read CAR field (first 4 bytes of cons cell)
    return MemoryManager.Access.readLispPTR(vm.virtualMemory, byteOffset);
}

/**
 * Get CDR of cons cell (helper function)
 * Re-exported from list.ts for use in comparison.ts
 * Handles CDR coding
 */
function getCDR(vm: VM, listPtr: LispPTR): LispPTR {
    if (vm.virtualMemory === null) return 0;
    if (listPtr === 0) return 0; // NIL

    const byteOffset = MemoryManager.Address.lispPtrToByte(listPtr);
    if (byteOffset + 5 > vm.virtualMemory.length) return 0;

    // Read CDR code (byte at offset 4)
    const cdrCode = vm.virtualMemory[ byteOffset + 4 ];
    const CDR_NIL = 0x80;
    const CDR_ONPAGE = 0x80;
    const CDR_INDIRECT = 0xFE;

    if (cdrCode === CDR_NIL || cdrCode === 0) {
        return 0; // NIL
    } else if ((cdrCode & CDR_ONPAGE) !== 0) {
        // CDR_ONPAGE: CDR is on same page
        const pageBase = MemoryManager.Address.getVirtualAddressBase(
            MemoryManager.Address.getVirtualPage(byteOffset)
        );
        const cdrOffset = ((cdrCode & 0x7F) << 1);
        const cdrPtr = MemoryManager.Address.byteToLispPtr(pageBase + cdrOffset);
        return cdrPtr;
    } else if (cdrCode === CDR_INDIRECT) {
        // CDR_INDIRECT: CDR is stored in next cell
        const nextCellOffset = byteOffset + 8; // Next cons cell (8 bytes)
        if (nextCellOffset + 4 > vm.virtualMemory.length) return 0;
        return MemoryManager.Access.readLispPTR(vm.virtualMemory, nextCellOffset);
    } else {
        // Direct CDR coding (low 7 bits encode offset)
        const pageBase = MemoryManager.Address.getVirtualAddressBase(
            MemoryManager.Address.getVirtualPage(byteOffset)
        );
        const cdrOffset = (cdrCode << 1);
        const cdrPtr = MemoryManager.Address.byteToLispPtr(pageBase + cdrOffset);
        return cdrPtr;
    }
}

/**
 * NTHCHC opcode handler
 * NTH with CDR coding (nth cdr chain)
 * Gets the nth element of a list by following CDR chain n times, then returns CAR
 *
 * Algorithm:
 * 1. Pop index (n) from stack
 * 2. Pop list from stack
 * 3. Follow CDR chain n times
 * 4. Push CAR of the nth cell (or NIL if list is too short)
 */
function handleNTHCHC(vm: VM, instruction: Instruction): number | null {
    // Pop index (n) from stack
    const index = popStack(vm);

    // Pop list from stack
    let list = popStack(vm);

    // Extract numeric index from LispPTR
    // For now, assume index is SMALLP
    let n: number;
    if ((index & 0xE0000000) === 0xE0000000) {
        // SMALLP - extract low 16 bits
        n = index & 0xFFFF;
    } else {
        // Not a small integer - treat as 0
        n = 0;
    }

    // Follow CDR chain n times
    for (let i = 0; i < n; i++) {
        if (list === NIL_PTR) {
            // List is too short - return NIL
            pushStack(vm, NIL_PTR);
            return null;
        }

        // Get CDR
        list = getCDR(vm, list);
    }

    // If we reached NIL before n iterations, return NIL
    if (list === NIL_PTR) {
        pushStack(vm, NIL_PTR);
        return null;
    }

    // Get CAR of the nth cell
    const car = getCAR(vm, list);
    pushStack(vm, car);
    return null;
}

// Register handlers
registerOpcodeHandler(Opcode.EQ, handleEQ);
registerOpcodeHandler(Opcode.EQL, handleEQL);
registerOpcodeHandler(Opcode.CMLEQUAL, handleCMLEQUAL);
registerOpcodeHandler(Opcode.RECLAIMCELL, handleRECLAIMCELL);
registerOpcodeHandler(Opcode.NTHCHC, handleNTHCHC);
