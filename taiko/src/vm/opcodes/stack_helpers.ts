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

    // Stack grows DOWN from stackBase.
    // When empty, CSTKPTRL is null and stackPtr == stackBase.
    if (vm.cstkptrl === null) {
        // First push: place value at stackBase - 4
        vm.cstkptrl = vm.stackBase - 4;
    } else {
        // Subsequent pushes: move down one cell (4 bytes)
        vm.cstkptrl -= 4;
    }

    // Write value to memory at CSTKPTRL
    if (vm.cstkptrl >= 0 && vm.cstkptrl + 4 <= vm.virtualMemory.length) {
        MemoryManager.Access.writeLispPTR(vm.virtualMemory, vm.cstkptrl, value);
    }

    // Update cached TopOfStack and stack pointer
    vm.topOfStack = value;
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

    // Empty stack
    if (vm.cstkptrl === null) {
        vm.topOfStack = 0;
        vm.stackPtr = vm.stackBase;
        return 0;
    }

    // Read value at current CSTKPTRL
    let value = 0;
    if (vm.cstkptrl >= 0 && vm.cstkptrl + 4 <= vm.virtualMemory.length) {
        value = MemoryManager.Access.readLispPTR(vm.virtualMemory, vm.cstkptrl);
    }

    // If this was the last element (at stackBase - 4), clear the stack
    if (vm.cstkptrl === vm.stackBase - 4) {
        vm.cstkptrl = null;
        vm.stackPtr = vm.stackBase;
        vm.topOfStack = 0;
    } else {
        // Move CSTKPTRL up one cell (toward stackBase)
        vm.cstkptrl += 4;
        vm.stackPtr = vm.cstkptrl;

        // Update TopOfStack to new top element
        if (vm.cstkptrl >= 0 && vm.cstkptrl + 4 <= vm.virtualMemory.length) {
            vm.topOfStack = MemoryManager.Access.readLispPTR(vm.virtualMemory, vm.cstkptrl);
        } else {
            vm.topOfStack = 0;
        }
    }

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
