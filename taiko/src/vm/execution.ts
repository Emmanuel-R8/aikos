// Main execution loop and instruction dispatch
import type { VM } from './vm';
import { decodeInstructionFromMemory } from './dispatch/decoder';
import type { Instruction } from './dispatch/opcode';
import { routeOpcode } from './opcodes/index';

// Import opcode handlers to register them
import './opcodes/constants';
import './opcodes/stack';
import './opcodes/arithmetic';
import './opcodes/control';
import './opcodes/memory';
import './opcodes/floating';
import './opcodes/graphics';
import './opcodes/variables';
import './opcodes/list';
import './opcodes/copy';
import './opcodes/binding';
import './opcodes/function_calls';

/**
 * Execute a single instruction
 * Returns jump offset if instruction is a jump, null otherwise
 */
export function executeInstruction(vm: VM, instruction: Instruction): number | null {
    // CRITICAL: Sync CSTKPTRL and TOPOFSTACK before each instruction
    // Per C: StackPtrRestore() macro restores CSTKPTRL from CurrentStackPTR
    vm.initCSTKPTRLFromCurrentStackPTR();
    // CRITICAL: TOPOFSTACK must be read from memory after restoring CSTKPTRL
    vm.syncTopOfStack();

    // TODO: Route to opcode handler
    // For now, just advance PC
    const jumpOffset = executeOpcode(vm, instruction);

    return jumpOffset;
}

/**
 * Execute opcode handler
 * Routes to registered opcode handlers
 */
function executeOpcode(vm: VM, instruction: Instruction): number | null {
    // Route to handler
    const jumpOffset = routeOpcode(vm, instruction);

    // Handle optimized jump opcodes if no handler registered
    if (jumpOffset === null) {
        const opcode = instruction.opcode;

        // Handle jump opcodes
        if (opcode >= 0x80 && opcode <= 0x8F) {
            // JUMP0-JUMP15: offset encoded in opcode (0-15)
            return (opcode - 0x80) + 1; // +1 because offset is 1-based
        }
        if (opcode >= 0x90 && opcode <= 0x9F) {
            // FJUMP0-FJUMP15: conditional jump if false
            if (vm.topOfStack === 0) { // NIL = false
                return (opcode - 0x90) + 1;
            }
            return null;
        }
        if (opcode >= 0xA0 && opcode <= 0xAF) {
            // TJUMP0-TJUMP15: conditional jump if true
            if (vm.topOfStack !== 0) { // Non-NIL = true
                return (opcode - 0xA0) + 1;
            }
            return null;
        }
    }

    return jumpOffset;
}

/**
 * Execute one step of the VM
 * Returns true if execution should continue, false if it should stop
 */
export function executeStep(vm: VM): boolean {
    if (vm.virtualMemory === null) {
        return false;
    }

    // Check step cap
    if (vm.maxSteps !== null && vm.totalInstructionCount >= vm.maxSteps) {
        vm.stopRequested = true;
        return false;
    }

    // Check stop flag
    if (vm.stopRequested) {
        return false;
    }

    // Decode instruction at PC
    const instruction = decodeInstructionFromMemory(vm, vm.pc);
    if (instruction === null) {
        // Invalid instruction or end of memory
        vm.stopRequested = true;
        return false;
    }

    // Execute instruction
    const jumpOffset = executeInstruction(vm, instruction);

    // Update PC
    if (jumpOffset !== null) {
        const newPc = vm.pc + jumpOffset;
        if (newPc < 0 || newPc >= vm.virtualMemory.length) {
            vm.stopRequested = true;
            return false;
        }
        vm.pc = newPc;
        // If jump offset is 0, still advance by instruction length
        if (jumpOffset === 0) {
            vm.pc += instruction.length;
        }
    } else {
        vm.pc += instruction.length;
    }

    vm.totalInstructionCount++;

    return true;
}

/**
 * Run VM until stop or step cap
 */
export function runVM(vm: VM): void {
    vm.running = true;
    vm.stopRequested = false;

    while (!vm.stopRequested && executeStep(vm)) {
        // Continue execution
    }

    vm.running = false;
}
