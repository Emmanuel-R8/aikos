// Stack operation helpers
// Provides push/pop functionality for opcodes

import type { VM } from '../vm';
import type { LispPTR } from '../../utils/types';
import { MemoryManager } from '../memory/manager';

/**
 * Push value onto stack
 * Per C: PUSH macro in maiko/inc/tosfns.h
 *
 * @param vm VM instance
 * @param value Value to push (LispPTR)
 */
export function pushStack(vm: VM, value: LispPTR): void {
    if (vm.virtualMemory === null) return;

    // Update TopOfStack cache
    vm.topOfStack = value;

    // Update CSTKPTRL to point to new stack position
    if (vm.cstkptrl === null) {
        vm.cstkptrl = vm.stackPtr;
    } else {
        vm.cstkptrl += 4; // Move forward one LispPTR (4 bytes)
    }

    // Write value to memory at CSTKPTRL
    if (vm.cstkptrl >= 0 && vm.cstkptrl + 4 <= vm.virtualMemory.length) {
        MemoryManager.Access.writeLispPTR(vm.virtualMemory, vm.cstkptrl, value);
    }

    // Update stack pointer
    vm.stackPtr = vm.cstkptrl;
}

/**
 * Pop value from stack
 * Per C: POP macro in maiko/inc/tosfns.h
 *
 * @param vm VM instance
 * @returns Popped value (LispPTR)
 */
export function popStack(vm: VM): LispPTR {
    if (vm.virtualMemory === null) return 0;

    // Read current TopOfStack
    const value = vm.topOfStack;

    // Move CSTKPTRL back one position
    if (vm.cstkptrl !== null && vm.cstkptrl >= 4) {
        vm.cstkptrl -= 4; // Move back one LispPTR (4 bytes)

        // Read new TopOfStack from memory
        if (vm.cstkptrl >= 0 && vm.cstkptrl + 4 <= vm.virtualMemory.length) {
            vm.topOfStack = MemoryManager.Access.readLispPTR(vm.virtualMemory, vm.cstkptrl);
        } else {
            vm.topOfStack = 0; // NIL
        }
    } else {
        vm.topOfStack = 0; // NIL
    }

    // Update stack pointer
    vm.stackPtr = vm.cstkptrl ?? vm.stackBase;

    return value;
}

/**
 * Get value at stack offset (without popping)
 *
 * @param vm VM instance
 * @param offset Offset from current stack (0 = top, 1 = second, etc.)
 * @returns Value at offset
 */
export function peekStack(vm: VM, offset: number): LispPTR {
    if (vm.virtualMemory === null || vm.cstkptrl === null) return 0;

    const peekOffset = vm.cstkptrl - (offset * 4);
    if (peekOffset >= 0 && peekOffset + 4 <= vm.virtualMemory.length) {
        return MemoryManager.Access.readLispPTR(vm.virtualMemory, peekOffset);
    }

    return 0; // NIL
}
