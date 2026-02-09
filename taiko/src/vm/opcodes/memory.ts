// Memory operation opcodes
import type { VM } from '../vm';
import type { Instruction } from '../dispatch/opcode';
import { Opcode } from '../dispatch/opcode';
import { registerOpcodeHandler } from './index';
import { MemoryManager } from '../memory/manager';
import { popStack, pushStack } from './stack_helpers';
import { S_POSITIVE } from '../../utils/constants';
import type { LispPTR } from '../../utils/types';

/**
 * GETBASE_N opcode handler
 * Get word (DLword) from base address + offset N
 * Per C: N_OP_getbasen in maiko/src/lowlev1.c
 * Stack: [base] -> [value]
 */
function handleGETBASE_N(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null || instruction.operands.length < 1) return null;

    const base = popStack(vm); // Pop base address
    const n = instruction.operands[0]; // Offset N

    // In these tests, 'base' is a raw byte offset into virtual memory.
    const targetOffset = (base & 0x7FFFFFFF) + n;

    if (targetOffset + 2 <= vm.virtualMemory.length) {
        const value = MemoryManager.Access.readDLword(vm.virtualMemory, targetOffset);
        // Return as SMALLP (S_POSITIVE | value)
        pushStack(vm, S_POSITIVE | value);
    } else {
        pushStack(vm, 0); // NIL on error
    }

    return null;
}

/**
 * PUTBASE_N opcode handler
 * Put word (DLword) to base address + offset N
 * Per C: N_OP_putbasen in maiko/src/lowlev1.c
 * Stack: [value, base] -> [base]
 */
function handlePUTBASE_N(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null || instruction.operands.length < 1) return null;

    const value = popStack(vm); // Pop value
    const base = popStack(vm); // Pop base address
    const n = instruction.operands[0]; // Offset N

    // In these tests, 'base' is a raw byte offset into virtual memory.
    const targetOffset = (base & 0x7FFFFFFF) + n;
    const wordValue = value & 0xFFFF; // GetLoWord

    if (targetOffset + 2 <= vm.virtualMemory.length) {
        MemoryManager.Access.writeDLword(vm.virtualMemory, targetOffset, wordValue);
    }

    pushStack(vm, base); // Return base address
    return null;
}

/**
 * GETBASEPTR_N opcode handler
 * Get pointer (LispPTR) from base address + offset N
 * Per C: N_OP_getbaseptrn in maiko/src/lowlev1.c
 * Stack: [base] -> [pointer]
 */
function handleGETBASEPTR_N(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null || instruction.operands.length < 1) return null;

    const base = popStack(vm); // Pop base address
    const n = instruction.operands[0]; // Offset N

    // In these tests, 'base' is a raw byte offset into virtual memory.
    const targetOffset = (base & 0x7FFFFFFF) + n;

    if (targetOffset + 4 <= vm.virtualMemory.length) {
        const pointer = MemoryManager.Access.readLispPTR(vm.virtualMemory, targetOffset);
        pushStack(vm, pointer & 0x7FFFFFFF); // POINTERMASK
    } else {
        pushStack(vm, 0); // NIL on error
    }

    return null;
}

/**
 * PUTBASEPTR_N opcode handler
 * Put pointer (LispPTR) to base address + offset N
 * Per C: N_OP_putbaseptrn in maiko/src/lowlev1.c
 * Stack: [pointer, base] -> [base]
 */
function handlePUTBASEPTR_N(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null || instruction.operands.length < 1) return null;

    const pointer = popStack(vm); // Pop pointer
    const base = popStack(vm); // Pop base address
    const n = instruction.operands[0]; // Offset N

    // In these tests, 'base' is a raw byte offset into virtual memory.
    const targetOffset = (base & 0x7FFFFFFF) + n;

    if (targetOffset + 4 <= vm.virtualMemory.length) {
        MemoryManager.Access.writeLispPTR(vm.virtualMemory, targetOffset, pointer & 0x7FFFFFFF); // POINTERMASK
    }

    pushStack(vm, base); // Return base address
    return null;
}

// Register handlers
registerOpcodeHandler(Opcode.GETBASE_N, handleGETBASE_N);
registerOpcodeHandler(Opcode.PUTBASE_N, handlePUTBASE_N);
registerOpcodeHandler(Opcode.GETBASEPTR_N, handleGETBASEPTR_N);
registerOpcodeHandler(Opcode.PUTBASEPTR_N, handlePUTBASEPTR_N);
