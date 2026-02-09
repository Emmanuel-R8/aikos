// Binding opcodes (BIND, UNBIND, DUNBIND)
// Per maiko/src/binds.c

import type { VM } from '../vm';
import type { Instruction } from '../dispatch/opcode';
import { Opcode } from '../dispatch/opcode';
import { registerOpcodeHandler } from './index';
import { popStack, pushStack } from './stack_helpers';
import { NIL_PTR } from '../../utils/constants';
import { MemoryManager } from '../memory/manager';

/**
 * BIND opcode handler
 * Bind variables in PVAR area
 * Per maiko/src/binds.c:N_OP_bind
 *
 * Operands: byte1 (n1:4, n2:4), byte2 (offset)
 * Stack: [values..., TOS] -> [marker]
 *
 * Algorithm:
 * 1. Bind n1 variables to NIL in PVAR area
 * 2. Bind n2 variables to values from evaluation stack
 * 3. Push binding marker with encoded metadata
 */
function handleBIND(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;
    if (instruction.operands.length < 2) return null;

    const byte1 = instruction.operands[0];
    const byte2 = instruction.operands[1];

    // Decode operands: n1 (high 4 bits), n2 (low 4 bits)
    const n1 = (byte1 >> 4) & 0xF;
    const n2 = byte1 & 0xF;

    // Get PVAR base address
    // C: ppvar = (LispPTR *)PVar + 1 + byte2;
    // PVar points to base of PVAR area, +1 for display/closure slot
    // For now, simplified: assume PVAR is at stackPtr + FRAMESIZE
    const pvarBase = vm.stackPtr + (10 * 2); // FRAMESIZE = 10 DLwords = 20 bytes
    const pvarOffset = byte2; // Offset in LispPTR units (4 bytes each)

    // Calculate ppvar pointer (going backwards from this point)
    const ppvarOffset = pvarBase + ((1 + pvarOffset) * 4);

    // Bind n1 variables to NIL
    for (let i = 0; i < n1; i++) {
        const slotOffset = ppvarOffset - ((i + 1) * 4);
        if (slotOffset >= 0 && slotOffset + 4 <= vm.virtualMemory.length) {
            MemoryManager.Access.writeLispPTR(vm.virtualMemory, slotOffset, NIL_PTR);
        }
    }

    // Handle n2 variable bindings from stack
    if (n2 === 0) {
        // Push TOS to evaluation stack
        // C: *stack_pointer++ = tos;
        pushStack(vm, vm.topOfStack);
    } else {
        // Bind first value to TopOfStack
        const firstSlotOffset = ppvarOffset - ((n1 + 1) * 4);
        if (firstSlotOffset >= 0 && firstSlotOffset + 4 <= vm.virtualMemory.length) {
            MemoryManager.Access.writeLispPTR(vm.virtualMemory, firstSlotOffset, vm.topOfStack);
        }

        // Bind remaining values from stack
        for (let i = 1; i < n2; i++) {
            const value = popStack(vm);
            const slotOffset = ppvarOffset - ((n1 + i + 1) * 4);
            if (slotOffset >= 0 && slotOffset + 4 <= vm.virtualMemory.length) {
                MemoryManager.Access.writeLispPTR(vm.virtualMemory, slotOffset, value);
            }
        }
    }

    // Push binding marker
    // Marker format: ((~(n1 + n2)) << 16) | (byte2 << 1)
    const total = n1 + n2;
    const marker: number = ((~total & 0xFFFF) << 16) | ((byte2 << 1) & 0xFFFF);
    pushStack(vm, marker);

    return null;
}

/**
 * UNBIND opcode handler
 * Unbind variables from previous BIND operation
 * Per maiko/src/binds.c:N_OP_unbind
 *
 * Algorithm:
 * 1. Walk backwards through stack to find BIND marker (MSB set in upper word)
 * 2. Extract binding information from marker
 * 3. Restore PVAR slots to unbound state (0xFFFFFFFF)
 */
function handleUNBIND(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null || vm.cstkptrl === null) return null;

    // CRITICAL: Walk backwards through stack to find BIND marker
    // C: for (; !(*--stack_pointer & 0x80000000););
    // Marker has MSB (0x80000000) set in upper word
    let markerFound = false;
    let markerValue = 0;
    let searchOffset = vm.cstkptrl;

    // Search for marker (scan backwards)
    for (let i = 0; i < 1000; i++) { // Safety limit
        searchOffset -= 4; // Decrement by 4 bytes (LispPTR size)
        if (searchOffset < vm.stackEnd || searchOffset >= vm.stackBase) {
            break; // Out of bounds
        }

        const value = MemoryManager.Access.readLispPTR(vm.virtualMemory, searchOffset);
        if ((value & 0x80000000) !== 0) {
            // Found marker (MSB set)
            markerFound = true;
            markerValue = value;
            vm.cstkptrl = searchOffset; // Update CSTKPTRL to marker position
            break;
        }
    }

    if (!markerFound) {
        console.warn('UNBIND: Marker not found');
        return null;
    }

    // Extract marker information
    // C: num = (DLword) ~(value >> 16);
    const num = (~(markerValue >> 16)) & 0xFFFF;
    const offset = (markerValue & 0xFFFF) >> 1; // Extract offset (was << 1)

    // Setup PVAR pointer for restoration
    // C: ppvar = (LispPTR *)(PVar + 2 + GetLoWord(value));
    const pvarBase = vm.stackPtr + (10 * 2); // FRAMESIZE = 10 DLwords
    const ppvarOffset = pvarBase + ((2 + offset) * 4);

    // Restore PVAR slots to unbound state (0xFFFFFFFF)
    // C: for (i = 0; i < num; i++) { *--ppvar = 0xffffffff; }
    const unboundValue = 0xFFFFFFFF;
    for (let i = 0; i < num; i++) {
        const slotOffset = ppvarOffset - ((i + 1) * 4);
        if (slotOffset >= 0 && slotOffset + 4 <= vm.virtualMemory.length) {
            MemoryManager.Access.writeLispPTR(vm.virtualMemory, slotOffset, unboundValue);
        }
    }

    // CRITICAL: Sync TOPOFSTACK after UNBIND
    // UNBIND may have changed CSTKPTRL, so we need to re-read TOPOFSTACK
    vm.syncTopOfStack();

    return null;
}

/**
 * DUNBIND opcode handler (stub)
 * Conditional unbinding operation
 */
function handleDUNBIND(vm: VM, instruction: Instruction): number | null {
    // TODO: Implement DUNBIND
    console.warn(`DUNBIND (0x${Opcode.DUNBIND.toString(16)}) not implemented.`);
    return null;
}

// Register handlers
registerOpcodeHandler(Opcode.BIND, handleBIND);
registerOpcodeHandler(Opcode.UNBIND, handleUNBIND);
registerOpcodeHandler(Opcode.DUNBIND, handleDUNBIND);
