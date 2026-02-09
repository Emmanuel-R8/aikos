// Main execution loop and instruction dispatch
import type { VM } from './vm';
import { decodeInstructionFromMemory } from './dispatch/decoder';
import type { Instruction } from './dispatch/opcode';
import { routeOpcode } from './opcodes/index';
import type { ExecutionTrace } from './trace';

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
import './opcodes/comparison';
import './opcodes/memory_ops';
import './opcodes/misc';
import './opcodes/type_checking';

/**
 * Execute a single instruction
 * Returns jump offset if instruction is a jump, null otherwise
 */
export function executeInstruction(vm: VM, instruction: Instruction): number | null {
    // Route to opcode handler and execute it
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
 *
 * @param vm VM instance
 * @param tracer Optional execution tracer (for parity testing)
 */
export function executeStep(vm: VM, tracer?: ExecutionTrace): boolean {
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

    // CRITICAL: Sync CSTKPTRL and TOPOFSTACK before each instruction
    vm.initCSTKPTRLFromCurrentStackPTR();
    vm.syncTopOfStack();

    // Decode instruction at PC
    const instruction = decodeInstructionFromMemory(vm, vm.pc);
    if (instruction === null) {
        // Invalid instruction or end of memory
        vm.stopRequested = true;
        return false;
    }

    // Log instruction before execution (if tracer provided)
    if (tracer) {
        tracer.logInstruction(vm, instruction, false);
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
