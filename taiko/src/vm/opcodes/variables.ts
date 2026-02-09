// Variable access opcodes
import type { VM } from '../vm';
import type { Instruction } from '../dispatch/opcode';
import { Opcode } from '../dispatch/opcode';
import { registerOpcodeHandler } from './index';
import { MemoryManager } from '../memory/manager';
import { AtomSpace } from '../memory/atomspace';
import { FrameManager } from '../memory/frame';
import type { LispPTR } from '../../utils/types';

/**
 * GVAR opcode handler
 * Get global variable value
 * Per C: GVAR(Get_AtomNo_PCMAC1) macro
 *
 * GVAR reads atom number from instruction operand (PC+1 with XOR addressing)
 * Looks up atom's GVAR slot and pushes value onto stack
 */
function handleGVAR(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;

    // Read atom number from operand (PC+1 with XOR addressing)
    // C: Get_AtomNo_PCMAC1 = Get_AtomNo(PCMAC + 1)
    // Get_AtomNo reads byte with XOR addressing
    const atomNoByte = MemoryManager.Access.readByteXor(vm.virtualMemory, vm.pc + 1);
    if (atomNoByte === null) return null;

    const atomIndex = atomNoByte;

    // Look up atom's GVAR slot from AtomSpace/Valspace
    // C: pslot = (LispPTR *)Valspace + atom_index;
    const value = AtomSpaceManager.getValueCell(vm.virtualMemory, vm.valSpaceOffset, atomIndex);
    pushStack(vm, value);

    return null; // No jump
}

/**
 * ARG0 opcode handler
 * Push argument 0 onto stack
 */
function handleARG0(vm: VM, instruction: Instruction): number | null {
    const arg0 = FrameManager.readARG0(vm);
    vm.topOfStack = arg0;
    return null;
}

/**
 * IVAR0-IVAR6 opcode handlers
 * Push instance variable onto stack
 */
function handleIVAR0(vm: VM, instruction: Instruction): number | null {
    vm.topOfStack = FrameManager.readIVAR(vm, 0);
    return null;
}

function handleIVAR1(vm: VM, instruction: Instruction): number | null {
    vm.topOfStack = FrameManager.readIVAR(vm, 1);
    return null;
}

function handleIVAR2(vm: VM, instruction: Instruction): number | null {
    vm.topOfStack = FrameManager.readIVAR(vm, 2);
    return null;
}

function handleIVAR3(vm: VM, instruction: Instruction): number | null {
    vm.topOfStack = FrameManager.readIVAR(vm, 3);
    return null;
}

function handleIVAR4(vm: VM, instruction: Instruction): number | null {
    vm.topOfStack = FrameManager.readIVAR(vm, 4);
    return null;
}

function handleIVAR5(vm: VM, instruction: Instruction): number | null {
    vm.topOfStack = FrameManager.readIVAR(vm, 5);
    return null;
}

function handleIVAR6(vm: VM, instruction: Instruction): number | null {
    vm.topOfStack = FrameManager.readIVAR(vm, 6);
    return null;
}

/**
 * IVARX opcode handler
 * Push indexed instance variable onto stack
 */
function handleIVARX(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;
    const index = MemoryManager.Access.readByteXor(vm.virtualMemory, vm.pc + 1);
    if (index === null) return null;
    vm.topOfStack = FrameManager.readIVAR(vm, index);
    return null;
}

/**
 * PVAR0-PVAR6 opcode handlers
 * Push parameter variable onto stack
 */
function handlePVAR0(vm: VM, instruction: Instruction): number | null {
    vm.topOfStack = FrameManager.readPVAR(vm, 0);
    return null;
}

function handlePVAR1(vm: VM, instruction: Instruction): number | null {
    vm.topOfStack = FrameManager.readPVAR(vm, 1);
    return null;
}

function handlePVAR2(vm: VM, instruction: Instruction): number | null {
    vm.topOfStack = FrameManager.readPVAR(vm, 2);
    return null;
}

function handlePVAR3(vm: VM, instruction: Instruction): number | null {
    vm.topOfStack = FrameManager.readPVAR(vm, 3);
    return null;
}

function handlePVAR4(vm: VM, instruction: Instruction): number | null {
    vm.topOfStack = FrameManager.readPVAR(vm, 4);
    return null;
}

function handlePVAR5(vm: VM, instruction: Instruction): number | null {
    vm.topOfStack = FrameManager.readPVAR(vm, 5);
    return null;
}

function handlePVAR6(vm: VM, instruction: Instruction): number | null {
    vm.topOfStack = FrameManager.readPVAR(vm, 6);
    return null;
}

/**
 * PVARX opcode handler
 * Push indexed parameter variable onto stack
 */
function handlePVARX(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;
    const index = MemoryManager.Access.readByteXor(vm.virtualMemory, vm.pc + 1);
    if (index === null) return null;
    vm.topOfStack = FrameManager.readPVAR(vm, index);
    return null;
}

// Register handlers
registerOpcodeHandler(Opcode.GVAR, handleGVAR);
registerOpcodeHandler(Opcode.ARG0, handleARG0);
registerOpcodeHandler(Opcode.IVAR0, handleIVAR0);
registerOpcodeHandler(Opcode.IVAR1, handleIVAR1);
registerOpcodeHandler(Opcode.IVAR2, handleIVAR2);
registerOpcodeHandler(Opcode.IVAR3, handleIVAR3);
registerOpcodeHandler(Opcode.IVAR4, handleIVAR4);
registerOpcodeHandler(Opcode.IVAR5, handleIVAR5);
registerOpcodeHandler(Opcode.IVAR6, handleIVAR6);
registerOpcodeHandler(Opcode.IVARX, handleIVARX);
registerOpcodeHandler(Opcode.PVAR0, handlePVAR0);
registerOpcodeHandler(Opcode.PVAR1, handlePVAR1);
registerOpcodeHandler(Opcode.PVAR2, handlePVAR2);
registerOpcodeHandler(Opcode.PVAR3, handlePVAR3);
registerOpcodeHandler(Opcode.PVAR4, handlePVAR4);
registerOpcodeHandler(Opcode.PVAR5, handlePVAR5);
registerOpcodeHandler(Opcode.PVAR6, handlePVAR6);
registerOpcodeHandler(Opcode.PVARX, handlePVARX);
