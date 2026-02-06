// Graphics operation opcodes (stub)
import type { VM } from '../vm';
import type { Instruction } from '../dispatch/opcode';
import { Opcode } from '../dispatch/opcode';
import { registerOpcodeHandler } from './index';

/**
 * BLT opcode handler (stub)
 * BitBLT operation
 */
function handleBLT(vm: VM, instruction: Instruction): number | null {
    // TODO: Implement BitBLT
    return null;
}

/**
 * BUSBLT opcode handler (stub)
 * Bus BitBLT operation
 */
function handleBUSBLT(vm: VM, instruction: Instruction): number | null {
    // TODO: Implement Bus BitBLT
    return null;
}

/**
 * DRAWLINE opcode handler (stub)
 */
function handleDRAWLINE(vm: VM, instruction: Instruction): number | null {
    // TODO: Implement line drawing
    return null;
}

// Register handlers
registerOpcodeHandler(Opcode.BLT, handleBLT);
registerOpcodeHandler(Opcode.BUSBLT, handleBUSBLT);
registerOpcodeHandler(Opcode.DRAWLINE, handleDRAWLINE);
