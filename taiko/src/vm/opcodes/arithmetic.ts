// Arithmetic operation opcodes
import type { VM } from '../vm';
import type { Instruction } from '../dispatch/opcode';
import { Opcode } from '../dispatch/opcode';
import { registerOpcodeHandler } from './index';
import { plus2, difference, times2, quotient, iplus2, idifference, itimes2 } from './arithmetic_helpers';

/**
 * PLUS2 opcode handler
 * Adds two numbers
 * Per C: N_OP_plus2 in maiko/src/arithops.c
 */
function handlePLUS2(vm: VM, instruction: Instruction): number | null {
    plus2(vm);
    return null;
}

/**
 * DIFFERENCE opcode handler
 * Subtracts two numbers
 * Per C: N_OP_difference in maiko/src/arithops.c
 */
function handleDIFFERENCE(vm: VM, instruction: Instruction): number | null {
    difference(vm);
    return null;
}

/**
 * TIMES2 opcode handler
 * Multiplies two numbers
 * Per C: N_OP_times2 in maiko/src/arithops.c
 */
function handleTIMES2(vm: VM, instruction: Instruction): number | null {
    times2(vm);
    return null;
}

/**
 * QUOTIENT opcode handler
 * Divides two numbers
 * Per C: N_OP_quotient in maiko/src/arithops.c
 */
function handleQUOTIENT(vm: VM, instruction: Instruction): number | null {
    quotient(vm);
    return null;
}

/**
 * IPLUS2 opcode handler
 * Integer addition
 * Per C: N_OP_iplus2 in maiko/src/arithops.c
 */
function handleIPLUS2(vm: VM, instruction: Instruction): number | null {
    iplus2(vm);
    return null;
}

/**
 * IDIFFERENCE opcode handler
 * Integer subtraction
 * Per C: N_OP_idifference in maiko/src/arithops.c
 */
function handleIDIFFERENCE(vm: VM, instruction: Instruction): number | null {
    idifference(vm);
    return null;
}

/**
 * ITIMES2 opcode handler
 * Integer multiplication
 * Per C: N_OP_itimes2 in maiko/src/arithops.c
 */
function handleITIMES2(vm: VM, instruction: Instruction): number | null {
    itimes2(vm);
    return null;
}

// Register handlers
registerOpcodeHandler(Opcode.PLUS2, handlePLUS2);
registerOpcodeHandler(Opcode.DIFFERENCE, handleDIFFERENCE);
registerOpcodeHandler(Opcode.TIMES2, handleTIMES2);
registerOpcodeHandler(Opcode.QUOTIENT, handleQUOTIENT);
registerOpcodeHandler(Opcode.IPLUS2, handleIPLUS2);
registerOpcodeHandler(Opcode.IDIFFERENCE, handleIDIFFERENCE);
registerOpcodeHandler(Opcode.ITIMES2, handleITIMES2);
