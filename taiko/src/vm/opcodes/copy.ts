// COPY opcode handler
// Copies top of stack

import type { VM } from '../vm';
import type { Instruction } from '../dispatch/opcode';
import { Opcode } from '../dispatch/opcode';
import { registerOpcodeHandler } from './index';
import { pushStack } from './stack_helpers';

/**
 * COPY opcode handler
 * Copy top of stack
 * Per C: COPY macro in maiko/inc/tosfns.h
 * Stack: [value] -> [value, value]
 */
function handleCOPY(vm: VM, instruction: Instruction): number | null {
    const value = vm.topOfStack;
    pushStack(vm, value); // Push copy
    return null;
}

// Register handler
registerOpcodeHandler(Opcode.COPY, handleCOPY);
