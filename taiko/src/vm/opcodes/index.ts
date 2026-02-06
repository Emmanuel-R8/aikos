// Opcode registry and dispatch mapping
import type { VM } from '../vm';
import type { Instruction } from '../dispatch/opcode';
import { Opcode } from '../dispatch/opcode';

/**
 * Opcode handler function type
 * Returns jump offset if instruction is a jump, null otherwise
 */
export type OpcodeHandler = (vm: VM, instruction: Instruction) => number | null;

/**
 * Opcode handler registry
 */
const opcodeHandlers: Map<Opcode, OpcodeHandler> = new Map();

/**
 * Register opcode handler
 */
export function registerOpcodeHandler(opcode: Opcode, handler: OpcodeHandler): void {
    opcodeHandlers.set(opcode, handler);
}

/**
 * Get opcode handler
 */
export function getOpcodeHandler(opcode: Opcode): OpcodeHandler | null {
    return opcodeHandlers.get(opcode) ?? null;
}

/**
 * Route opcode to handler
 */
export function routeOpcode(vm: VM, instruction: Instruction): number | null {
    const handler = getOpcodeHandler(instruction.opcode);
    if (handler) {
        return handler(vm, instruction);
    }
    // Unknown opcode - just advance PC
    return null;
}
