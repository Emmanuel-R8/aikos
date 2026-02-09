// Type checking opcodes (NTYPX, TYPEP, DTEST, UNWIND)
import type { VM } from '../vm';
import type { Instruction } from '../dispatch/opcode';
import { Opcode } from '../dispatch/opcode';
import { registerOpcodeHandler } from './index';
import { NIL_PTR, ATOM_T } from '../../utils/constants';
import { getTypeNumber } from '../memory/typecheck';

/**
 * NTYPX opcode handler
 * Get type number of TOS
 * Per C: NTYPX macro in maiko/inc/inlineC.h
 *
 * Stack: [value] -> [typeNumber]
 *
 * Algorithm:
 * 1. Get type number of TOS using GetTypeNumber()
 * 2. Replace TOS with type number
 *
 * Type number is 0-2047 from type table:
 * GetTypeNumber(TOS) = GetTypeEntry(TOS) & 0x7ff
 */
function handleNTYPX(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;

    const value = vm.topOfStack;
    const typeNumber = getTypeNumber(value, vm);

    // Replace TOS with type number
    vm.topOfStack = typeNumber;
    return null;
}

/**
 * TYPEP opcode handler
 * Type predicate - check if TOS matches type code
 * Per C: TYPEP(n) macro in maiko/inc/inlineC.h
 *
 * Stack: [value] -> [value or NIL]
 * Operand: type_code (1 byte)
 *
 * Algorithm:
 * 1. Get type number of TOS using GetTypeNumber()
 * 2. Compare with operand type_code
 * 3. If types match: TOS remains unchanged
 * 4. If types don't match: TOS = NIL_PTR
 *
 * C Implementation:
 * if ((DLword)GetTypeNumber(TOPOFSTACK) != (n)) TOPOFSTACK = NIL_PTR;
 */
function handleTYPEP(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;
    if (instruction.operands.length < 1) return null;

    const typeCode = instruction.operands[ 0 ];
    const value = vm.topOfStack;
    const typeNumber = getTypeNumber(value, vm);

    // Compare type number with type code
    // If they don't match, set TOS to NIL_PTR
    if (typeNumber !== typeCode) {
        vm.topOfStack = NIL_PTR;
    }
    // If they match, TOS remains unchanged

    return null;
}

/**
 * DTEST opcode handler
 * Test if TOS has type named by atom_index
 * Per C: DTEST macro in maiko/inc/inlineC.h
 *
 * Stack: [value] -> [T or NIL]
 * Operand: atom_index (2 bytes)
 *
 * Algorithm:
 * 1. Get atom at atom_index
 * 2. Check if TOS has type named by that atom
 * 3. Push T if match, NIL otherwise
 *
 * Note: Full implementation requires atom lookup and type name comparison
 * For now, simplified placeholder
 */
function handleDTEST(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;
    if (instruction.operands.length < 2) return null;

    // Read atom_index as 2-byte value (little-endian)
    const atomIndex = instruction.operands[ 0 ] | (instruction.operands[ 1 ] << 8);

    // TODO: Implement full DTEST logic
    // This requires:
    // 1. Looking up atom at atom_index
    // 2. Getting type name from atom
    // 3. Comparing TOS type with atom's type name
    //
    // For now, simplified: just return NIL
    vm.topOfStack = NIL_PTR;
    return null;
}

/**
 * UNWIND opcode handler
 * Stack unwinding operation
 * Per C: UNWIND macro in maiko/inc/inlineC.h
 *
 * Stack: [arg1, arg2] -> []
 * Operands: 2 bytes (unwind parameters)
 *
 * Note: Full implementation requires proper stack unwinding logic
 * For now, simplified placeholder
 */
function handleUNWIND(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;

    // TODO: Implement full UNWIND logic
    // This requires:
    // 1. Reading unwind parameters from operands
    // 2. Unwinding stack frames
    // 3. Restoring bindings
    // 4. Jumping to unwind target
    //
    // For now, simplified: just advance PC
    return null;
}

// Register handlers
registerOpcodeHandler(Opcode.NTYPX, handleNTYPX);
registerOpcodeHandler(Opcode.TYPEP, handleTYPEP);
registerOpcodeHandler(Opcode.DTEST, handleDTEST);
registerOpcodeHandler(Opcode.UNWIND, handleUNWIND);
