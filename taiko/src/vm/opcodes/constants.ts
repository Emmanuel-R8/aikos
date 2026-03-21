// Constant opcodes
import type { VM } from '../vm';
import type { Instruction } from '../dispatch/opcode';
import { Opcode } from '../dispatch/opcode';
import { registerOpcodeHandler } from './index';
import { pushStack } from './stack_helpers';
import { S_NEGATIVE, S_POSITIVE } from '../../utils/constants';

/**
 * NIL opcode handler
 * Push NIL (0) onto stack
 * Per C: PUSH(NIL_PTR) macro
 */
function handleNIL(vm: VM, instruction: Instruction): number | null {
    pushStack(vm, 0); // NIL
    return null;
}

/**
 * T opcode handler
 * Push T (non-zero) onto stack
 * Per C: PUSH(ATOM_T) macro
 */
function handleT(vm: VM, instruction: Instruction): number | null {
    pushStack(vm, 1); // T (simplified - actual T atom value would be different)
    return null;
}

/**
 * CONST_0 opcode handler
 * Push constant 0 onto stack
 */
function handleCONST_0(vm: VM, instruction: Instruction): number | null {
    pushStack(vm, 0);
    return null;
}

/**
 * CONST_1 opcode handler
 * Push constant 1 onto stack
 */
function handleCONST_1(vm: VM, instruction: Instruction): number | null {
    pushStack(vm, 1);
    return null;
}

/**
 * ACONST opcode handler
 * Push atom constant onto stack
 * Per C: ACONST(Get_AtomNo_PCMAC1) macro
 */
function handleACONST(vm: VM, instruction: Instruction): number | null {
    if (instruction.operands.length < 1) return null;

    // Push atom as SMALLP (S_POSITIVE | atom_index)
    // Per C: PUSH(S_POSITIVE | Get_AtomNo_PCMAC1)
    const atomValue = S_POSITIVE | instruction.operands[0];
    pushStack(vm, atomValue);

    return null;
}

function handleSIC(vm: VM, instruction: Instruction): number | null {
    if (instruction.operands.length < 1) return null;
    pushStack(vm, S_POSITIVE | instruction.operands[0]);
    return null;
}

function handleSNIC(vm: VM, instruction: Instruction): number | null {
    if (instruction.operands.length < 1) return null;
    pushStack(vm, S_NEGATIVE | 0xff00 | instruction.operands[0]);
    return null;
}

function handleSICX(vm: VM, instruction: Instruction): number | null {
    if (instruction.operands.length < 2) return null;
    const value = (instruction.operands[0] << 8) | instruction.operands[1];
    pushStack(vm, S_POSITIVE | value);
    return null;
}

// Register handlers
registerOpcodeHandler(Opcode.NIL, handleNIL);
registerOpcodeHandler(Opcode.T, handleT);
registerOpcodeHandler(Opcode.CONST_0, handleCONST_0);
registerOpcodeHandler(Opcode.CONST_1, handleCONST_1);
registerOpcodeHandler(Opcode.ACONST, handleACONST);
registerOpcodeHandler(Opcode.SIC, handleSIC);
registerOpcodeHandler(Opcode.SNIC, handleSNIC);
registerOpcodeHandler(Opcode.SICX, handleSICX);
