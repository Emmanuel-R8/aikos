// Control flow opcodes
import type { VM } from '../vm';
import type { Instruction } from '../dispatch/opcode';
import { Opcode } from '../dispatch/opcode';
import { registerOpcodeHandler } from './index';
import { MemoryManager } from '../memory/manager';

/**
 * JUMPX opcode handler
 * Unconditional jump with 2-byte signed offset
 */
function handleJUMPX(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;
    if (instruction.operands.length < 2) return null;

    const offset = MemoryManager.Access.readJumpOffset(vm.virtualMemory, vm.pc);
    return offset;
}

/**
 * FJUMPX opcode handler
 * Conditional jump if false
 */
function handleFJUMPX(vm: VM, instruction: Instruction): number | null {
    // TODO: Check TopOfStack for false condition
    if (vm.topOfStack === 0) { // NIL = false
        const offset = MemoryManager.Access.readJumpOffset(vm.virtualMemory!, vm.pc);
        return offset;
    }
    return null;
}

/**
 * TJUMPX opcode handler
 * Conditional jump if true
 */
function handleTJUMPX(vm: VM, instruction: Instruction): number | null {
    // TODO: Check TopOfStack for true condition
    if (vm.topOfStack !== 0) { // Non-NIL = true
        const offset = MemoryManager.Access.readJumpOffset(vm.virtualMemory!, vm.pc);
        return offset;
    }
    return null;
}

// Function call handlers are now in function_calls.ts
// Import them to register handlers
import './function_calls';

// Register jump handlers
registerOpcodeHandler(Opcode.JUMPX, handleJUMPX);
registerOpcodeHandler(Opcode.FJUMPX, handleFJUMPX);
registerOpcodeHandler(Opcode.TJUMPX, handleTJUMPX);
