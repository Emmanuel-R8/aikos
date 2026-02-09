// Variable access opcodes
import type { VM } from '../vm';
import type { Instruction } from '../dispatch/opcode';
import { Opcode } from '../dispatch/opcode';
import { registerOpcodeHandler } from './index';
import { MemoryManager } from '../memory/manager';
import { AtomSpaceManager } from '../memory/atomspace';
import { FrameManager } from '../memory/frame';
import { pushStack, popStack } from './stack_helpers';
import type { LispPTR } from '../../utils/types';
import { S_POSITIVE } from '../../utils/constants';

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
 * Per C: ARG0 macro
 */
function handleARG0(vm: VM, instruction: Instruction): number | null {
    const arg0 = FrameManager.readARG0(vm);
    pushStack(vm, arg0);
    return null;
}

/**
 * IVAR0-IVAR6 opcode handlers
 * Push instance variable onto stack
 * Per C: IVARMACRO(x) -> PUSH(IVAR[x])
 */
function handleIVAR(vm: VM, instruction: Instruction, index: number): number | null {
    const value = FrameManager.readIVAR(vm, index);
    pushStack(vm, value);
    return null;
}

/**
 * IVARX opcode handler
 * Push indexed instance variable onto stack
 * Per C: IVARX(x) -> PUSH(IVAR[x])
 */
function handleIVARX(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null || instruction.operands.length < 1) return null;
    const index = instruction.operands[0];
    const value = FrameManager.readIVAR(vm, index);
    pushStack(vm, value);
    return null;
}

/**
 * PVAR0-PVAR6 opcode handlers
 * Push parameter variable onto stack
 * Per C: PVARMACRO(x) -> PUSH(PVAR[x])
 */
function handlePVAR(vm: VM, instruction: Instruction, index: number): number | null {
    const value = FrameManager.readPVAR(vm, index);
    pushStack(vm, value);
    return null;
}

/**
 * PVARX opcode handler
 * Push indexed parameter variable onto stack
 * Per C: PVARX(x) -> PUSH(PVAR[x])
 */
function handlePVARX(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null || instruction.operands.length < 1) return null;
    const index = instruction.operands[0];
    const value = FrameManager.readPVAR(vm, index);
    pushStack(vm, value);
    return null;
}

/**
 * FVAR0-FVAR6, FVARX opcode handlers
 * Push frame variable onto stack
 * Per C: FVAR(n) -> N_OP_fvarn(n)
 */
function handleFVAR(vm: VM, instruction: Instruction, index: number): number | null {
    // FVAR index is word offset (DLword offset), not LispPTR offset
    const value = FrameManager.readFVAR(vm, index);
    pushStack(vm, value);
    return null;
}

function handleFVARX(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null || instruction.operands.length < 1) return null;
    const index = instruction.operands[0];
    return handleFVAR(vm, instruction, index);
}

/**
 * PVARSETPOP0-PVARSETPOP6 opcode handlers
 * Set parameter variable and pop from stack
 * Per C: PVARSETPOPMACRO(x) -> PVAR[x] = TOPOFSTACK; POP
 */
function handlePVARSETPOP(vm: VM, instruction: Instruction, index: number): number | null {
    const value = popStack(vm);
    FrameManager.writePVAR(vm, index, value);
    return null;
}

/**
 * PVARX_ opcode handler
 * Set indexed parameter variable
 * Per C: PVARX_(x) -> PVAR[x] = TOPOFSTACK
 */
function handlePVARX_(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null || instruction.operands.length < 1) return null;
    const index = instruction.operands[0];
    const value = popStack(vm);
    FrameManager.writePVAR(vm, index, value);
    return null;
}

/**
 * IVARX_ opcode handler
 * Set indexed instance variable
 * Per C: IVARX_(x) -> IVAR[x] = TOPOFSTACK
 */
function handleIVARX_(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null || instruction.operands.length < 1) return null;
    const index = instruction.operands[0];
    const value = popStack(vm);
    FrameManager.writeIVAR(vm, index, value);
    return null;
}

/**
 * FVARX_ opcode handler
 * Set indexed frame variable
 * Per C: FVARX_(n) -> N_OP_fvar_(TOPOFSTACK, n)
 *
 * Note: Full name table lookup not yet implemented - simplified version
 */
function handleFVARX_(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null || instruction.operands.length < 1) return null;

    const value = popStack(vm); // Pop value to set
    const index = instruction.operands[0]; // FVAR index

    if (vm.pvar === null) {
        pushStack(vm, value);
        return null;
    }

    // FVAR slot offset
    const fvarSlotOffset = vm.pvar + (index * 2); // 2 bytes per DLword

    // Read FVAR slot to get target address
    if (fvarSlotOffset + 4 <= vm.virtualMemory.length) {
        // Check if unbound - if so, name table lookup would be needed
        const firstWord = MemoryManager.Access.readDLword(vm.virtualMemory, fvarSlotOffset);
        if ((firstWord & 0x8000) !== 0) {
            // Unbound - simplified: can't set unbound variable without lookup
            console.warn(`FVARX_: Attempting to set unbound FVAR ${index} - name table lookup needed`);
            pushStack(vm, value);
            return null;
        }

        // Extract target address from FVAR slot
        const highWord = MemoryManager.Access.readDLword(vm.virtualMemory, fvarSlotOffset + 2);
        const lowWord = MemoryManager.Access.readDLword(vm.virtualMemory, fvarSlotOffset);
        const pointer = ((highWord & 0xFFFF) << 16) | (lowWord & 0xFFFF);
        const maskedPointer = pointer & 0x0FFFFFFF; // POINTERMASK

        if (maskedPointer !== 0) {
            const targetOffset = MemoryManager.Address.lispPtrToByte(maskedPointer);
            if (targetOffset + 4 <= vm.virtualMemory.length) {
                // Write value to target address
                MemoryManager.Access.writeLispPTR(vm.virtualMemory, targetOffset, value);
            }
        }
    }

    pushStack(vm, value); // Return value
    return null;
}

/**
 * PVAR_0-PVAR_6 opcode handlers
 * Push parameter variable (alternative encoding)
 * Per C: Similar to PVAR but may use different encoding
 */
function handlePVAR_(vm: VM, instruction: Instruction, index: number): number | null {
    // Same as PVAR for now
    return handlePVAR(vm, instruction, index);
}

/**
 * MYARGCOUNT opcode handler
 * Push argument count onto stack
 * Per C: MYARGCOUNT macro in maiko/inc/inlineC.h
 */
function handleMYARGCOUNT(vm: VM, instruction: Instruction): number | null {
    const argCount = FrameManager.getArgCount(vm);
    pushStack(vm, argCount);
    return null;
}

/**
 * MYALINK opcode handler
 * Push access link onto stack
 * Per C: MYALINK macro in maiko/inc/inlineC.h
 */
function handleMYALINK(vm: VM, instruction: Instruction): number | null {
    const alink = FrameManager.getALink(vm);
    pushStack(vm, alink);
    return null;
}

/**
 * GVAR_ opcode handler
 * Set global variable value
 * Per C: N_OP_gvar_ in maiko/src/gvar2.c
 *
 * Stack: [value] -> []
 * Operand: atom_index (1 byte)
 *
 * Algorithm:
 * 1. Pop value from stack
 * 2. Read atom_index from operand
 * 3. Write value to atom's GVAR slot
 * 4. Update GC references (DELREF old value, ADDREF new value)
 *
 * Note: GC reference counting is simplified for now
 */
function handleGVAR_(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;
    if (instruction.operands.length < 1) return null;

    // Pop value from stack
    const value = popStack(vm);

    // Read atom_index from operand (1 byte)
    const atomIndex = instruction.operands[0];

    // Get old value before writing (for GC reference counting)
    const oldValue = AtomSpaceManager.getValueCell(vm.virtualMemory, vm.valSpaceOffset, atomIndex);

    // TODO: Update GC references (DELREF old value, ADDREF new value)
    // C: FRPLPTR(((struct xpointer *)pslot)->addr, tos);
    // This does: DELREF(old_value), ADDREF(new_value), then set value
    // For now, just write the value

    // Write value to atom's GVAR slot
    AtomSpaceManager.setValueCell(vm.virtualMemory, vm.valSpaceOffset, atomIndex, value);

    return null;
}

// Register handlers
registerOpcodeHandler(Opcode.GVAR, handleGVAR);
registerOpcodeHandler(Opcode.GVAR_, handleGVAR_);
registerOpcodeHandler(Opcode.ARG0, handleARG0);

// IVAR handlers (0x40-0x47)
for (let i = 0; i <= 6; i++) {
    registerOpcodeHandler(Opcode.IVAR0 + i, (vm, instr) => handleIVAR(vm, instr, i));
}
registerOpcodeHandler(Opcode.IVARX, handleIVARX);

// PVAR handlers (0x48-0x4F)
for (let i = 0; i <= 6; i++) {
    registerOpcodeHandler(Opcode.PVAR0 + i, (vm, instr) => handlePVAR(vm, instr, i));
}
registerOpcodeHandler(Opcode.PVARX, handlePVARX);

// FVAR handlers (0x50-0x57)
// Note: FVAR indices are word offsets: 0, 2, 4, 6, 8, 10, 12
const fvarIndices = [0, 2, 4, 6, 8, 10, 12];
for (let i = 0; i <= 6; i++) {
    registerOpcodeHandler(Opcode.FVAR0 + i, (vm, instr) => handleFVAR(vm, instr, fvarIndices[i]));
}
registerOpcodeHandler(Opcode.FVARX, handleFVARX);

// PVARSETPOP handlers (0xB8-0xBE)
for (let i = 0; i <= 6; i++) {
    registerOpcodeHandler(Opcode.PVARSETPOP0 + i, (vm, instr) => handlePVARSETPOP(vm, instr, i));
}

// Variable set handlers
registerOpcodeHandler(Opcode.PVARX_, handlePVARX_);
registerOpcodeHandler(Opcode.IVARX_, handleIVARX_);
registerOpcodeHandler(Opcode.FVARX_, handleFVARX_);

// PVAR_ handlers (0x58-0x5E)
for (let i = 0; i <= 6; i++) {
    registerOpcodeHandler(Opcode.PVAR_0 + i, (vm, instr) => handlePVAR_(vm, instr, i));
}

// MYARGCOUNT and MYALINK handlers
registerOpcodeHandler(Opcode.MYARGCOUNT, handleMYARGCOUNT);
registerOpcodeHandler(Opcode.MYALINK, handleMYALINK);

/**
 * FINDKEY opcode handler
 * Find key in function argument list (IVAR)
 * Per C: N_OP_findkey in maiko/src/findkey.c
 *
 * Stack: [key] -> [arg_nth or NIL]
 * Operand: byte (offset from IVAR)
 *
 * Algorithm:
 * 1. Pop key from stack
 * 2. Read byte offset from operand
 * 3. Search IVAR starting at IVar + ((byte * 2) - 2) up to find_end
 * 4. If key found, return S_POSITIVE | arg_nth
 * 5. Otherwise, return NIL_PTR
 *
 * Note: Simplified implementation - full version requires frame type detection
 */
function handleFINDKEY(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null || vm.ivar === null) return null;

    // Stack: [key]
    const key = popStack(vm);

    // Read byte offset from operand (PC+1 with XOR addressing)
    const byteOffset = MemoryManager.Access.readByteXor(vm.virtualMemory, vm.pc + 1);
    if (byteOffset === null) {
        pushStack(vm, 0); // NIL on error
        return null;
    }

    const byte = byteOffset;

    // C: Determine find_end based on frame type
    // For now, simplified: search from IVar + ((byte * 2) - 2) to end of IVAR area
    // C: arg_nth = byte + 1
    let argNth = byte + 1;

    // C: Start search at IVar + ((byte * 2) - 2)
    // IVar is a byte offset, so we need to convert to LispPTR offset
    // Each LispPTR is 4 bytes, so (byte * 2) DLwords = (byte * 2) * 2 bytes = byte * 4 bytes
    // But we subtract 2 DLwords = 4 bytes, so: startOffset = ivar + (byte * 4) - 4
    const startOffset = vm.ivar + (byte * 4) - 4;

    // Simplified: search up to a reasonable limit (e.g., 100 LispPTRs = 400 bytes)
    const maxSearch = 400; // bytes
    const endOffset = Math.min(startOffset + maxSearch, vm.virtualMemory.length - 4);

    // C: for (ptr = ...; find_end >= ptr; ptr += 2, arg_nth += 2)
    // Search through IVAR looking for matching key
    for (let offset = startOffset; offset < endOffset; offset += 4, argNth += 2) {
        const value = MemoryManager.Access.readLispPTR(vm.virtualMemory, offset);
        if (value === key) {
            // Key found - return S_POSITIVE | arg_nth
            const result = S_POSITIVE | argNth;
            pushStack(vm, result);
            return null;
        }
    }

    // Not found - return NIL
    pushStack(vm, 0);
    return null;
}

registerOpcodeHandler(Opcode.FINDKEY, handleFINDKEY);
