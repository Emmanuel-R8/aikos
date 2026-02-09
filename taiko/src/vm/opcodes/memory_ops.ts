// Memory operation opcodes (RPLPTR, GCREF, etc.)
import type { VM } from '../vm';
import type { Instruction } from '../dispatch/opcode';
import { Opcode } from '../dispatch/opcode';
import { registerOpcodeHandler } from './index';
import { popStack, pushStack } from './stack_helpers';
import { MemoryManager } from '../memory/manager';
import { POINTERMASK } from '../../utils/constants';

/**
 * RPLPTR_N opcode handler
 * Replace pointer field at base + offset
 * Per C: N_OP_rplptr in maiko/src/gvar2.c
 *
 * Stack: [newValue, base] -> [base]
 * Operand: alpha (word offset, 1 byte)
 *
 * Algorithm:
 * 1. Pop newValue from stack
 * 2. Pop base from stack
 * 3. Read alpha (word offset) from instruction operand
 * 4. Calculate slot address = base + (alpha * 2) bytes
 * 5. Write newValue to slot (with GC reference counting)
 * 6. Push base back on stack
 */
function handleRPLPTR_N(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;
    if (instruction.operands.length < 1) return null;

    // Pop newValue and base from stack
    const newValue = popStack(vm);
    const base = popStack(vm);

    // Get alpha (word offset) from operand
    // alpha is a byte, representing word offset
    const alpha = instruction.operands[0];

    // Calculate slot address: base + alpha (in LispPTR/DLword units)
    // Per C: pslot = (struct xpointer *)NativeAligned4FromLAddr(tos_m_1 + alpha);
    // NativeAligned4FromLAddr does: (POINTERMASK & addr) then converts to native pointer
    // alpha is a word offset, so we add it to base (both in DLword/LispPTR units)
    // Then mask and convert to bytes

    // Check if base is valid (not NIL and within POINTERMASK)
    if (base === 0 || (base & ~POINTERMASK) !== 0) {
        // Invalid base address - might be NIL or tagged value
        // For now, just push base back and continue
        pushStack(vm, base);
        return null;
    }

    const slotLispPtr = (base + alpha) & POINTERMASK;
    const slotByteOffset = MemoryManager.Address.lispPtrToByte(slotLispPtr);

    // Check bounds
    if (slotByteOffset < 0 || slotByteOffset + 4 > vm.virtualMemory.length) {
        // Out of bounds - push base back and continue (don't crash)
        pushStack(vm, base);
        return null;
    }

    // TODO: GC reference counting (FRPLPTR)
    // For now, just write the value directly
    // Per C: FRPLPTR(pslot->addr, tos);
    // This should:
    // 1. DELREF the old value at pslot->addr
    // 2. ADDREF the new value (tos)
    // 3. Write tos to pslot->addr

    // Write new value to slot
    MemoryManager.Access.writeLispPTR(vm.virtualMemory, slotByteOffset, newValue);

    // Push base back on stack
    pushStack(vm, base);
    return null;
}

/**
 * GCREF opcode handler
 * Garbage collection reference counting
 * Per C: OP_gcref in maiko/src/gc.c
 *
 * Stack: [slot_address] -> [slot_address or 0]
 * Operand: alpha (1 byte) - ADDREF (0), DELREF (1), or STKREF (2)
 *
 * Algorithm:
 * 1. Get slot address from TopOfStack
 * 2. Read alpha from operand
 * 3. Update GC reference count:
 *    - ADDREF (0): Increment reference count
 *    - DELREF (1): Decrement reference count
 *    - STKREF (2): Mark as stack reference
 * 4. If stk=0 and refcnt=0, leave TopOfStack unchanged
 *    Otherwise, replace TopOfStack with 0
 *
 * Note: Full GC implementation required. For now, placeholder.
 */
function handleGCREF(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;
    if (instruction.operands.length < 1) return null;

    // Read alpha from operand (ADDREF=0, DELREF=1, STKREF=2)
    const alpha = instruction.operands[0];

    // TODO: Implement full GC reference counting
    // This requires:
    // 1. GC hash table (HashMainTable) for reference counts
    // 2. GCLOOKUPV function to update reference counts
    // 3. Checking if slot is on stack (stk flag)
    // 4. Updating TopOfStack based on refcnt and stk
    //
    // For now, simplified: just leave TopOfStack unchanged
    // Full implementation would:
    // - Call GCLOOKUPV(slot_address, alpha, slot_address)
    // - Check if stk=0 and refcnt=0
    // - If so, leave TopOfStack unchanged
    // - Otherwise, set TopOfStack to 0

    return null;
}

// Register handlers
registerOpcodeHandler(Opcode.RPLPTR_N, handleRPLPTR_N);
registerOpcodeHandler(Opcode.GCREF, handleGCREF);
