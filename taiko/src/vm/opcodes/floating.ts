// Floating point operation opcodes (stub)
import type { VM } from '../vm';
import type { Instruction } from '../dispatch/opcode';
import { Opcode } from '../dispatch/opcode';
import { registerOpcodeHandler } from './index';

/**
 * FPLUS2 opcode handler (stub)
 */
function handleFPLUS2(vm: VM, instruction: Instruction): number | null {
    // TODO: Implement floating point addition
    return null;
}

/**
 * FDIFFERENCE opcode handler (stub)
 */
function handleFDIFFERENCE(vm: VM, instruction: Instruction): number | null {
    // TODO: Implement floating point subtraction
    return null;
}

/**
 * FTIMES2 opcode handler (stub)
 */
function handleFTIMES2(vm: VM, instruction: Instruction): number | null {
    // TODO: Implement floating point multiplication
    return null;
}

/**
 * FQUOTIENT opcode handler (stub)
 */
function handleFQUOTIENT(vm: VM, instruction: Instruction): number | null {
    // TODO: Implement floating point division
    return null;
}

// Register handlers
registerOpcodeHandler(Opcode.FPLUS2, handleFPLUS2);
registerOpcodeHandler(Opcode.FDIFFERENCE, handleFDIFFERENCE);
registerOpcodeHandler(Opcode.FTIMES2, handleFTIMES2);
registerOpcodeHandler(Opcode.FQUOTIENT, handleFQUOTIENT);

/**
 * UBFLOAT3 opcode handler
 * Unboxed floating point 3-word operation
 * Per C: UBFLOAT3 in maiko/src/xc.c
 */
function handleUBFLOAT3(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;
    // Placeholder: unboxed float operation
    console.warn('UBFLOAT3: Placeholder - no-op');
    return null;
}

registerOpcodeHandler(Opcode.UBFLOAT3, handleUBFLOAT3);
