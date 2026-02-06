// List operation opcodes
import type { VM } from '../vm';
import type { Instruction } from '../dispatch/opcode';
import { Opcode } from '../dispatch/opcode';
import { registerOpcodeHandler } from './index';
import { MemoryManager } from '../memory/manager';
import type { LispPTR } from '../../utils/types';

/**
 * ConsCell structure
 * Per maiko/inc/cell.h
 */
interface ConsCell {
    car_field: LispPTR;  // CAR field (32-bit LispPTR)
    cdr_code: number;    // CDR code (8 bits)
}

/**
 * CDR code constants
 */
const CDR_NIL = 0;
const CDR_ONPAGE = 0x80;
const CDR_INDIRECT = 0xFE;
const CDR_MAXINDIRECT = 0xFD;

/**
 * Check if value is a list (cons cell)
 */
function isList(vm: VM, ptr: LispPTR): boolean {
    if (ptr === 0) return false; // NIL is not a list for this check
    if (vm.virtualMemory === null) return false;

    // Check if pointer is valid and points to a cons cell
    // Simplified check - full implementation would verify type tag
    const byteOffset = MemoryManager.Address.lispPtrToByte(ptr);
    return byteOffset < vm.virtualMemory.length;
}

/**
 * Get CAR of cons cell
 * Per C: car() function in maiko/src/car-cdr.c
 */
function getCAR(vm: VM, listPtr: LispPTR): LispPTR {
    if (vm.virtualMemory === null) return 0;
    if (listPtr === 0) return 0; // NIL

    const byteOffset = MemoryManager.Address.lispPtrToByte(listPtr);
    if (byteOffset + 4 > vm.virtualMemory.length) return 0;

    // Read CAR field (first 4 bytes of cons cell)
    return MemoryManager.Access.readLispPTR(vm.virtualMemory, byteOffset);
}

/**
 * Get CDR of cons cell
 * Per C: cdr() function in maiko/src/car-cdr.c
 * Handles CDR coding
 */
function getCDR(vm: VM, listPtr: LispPTR): LispPTR {
    if (vm.virtualMemory === null) return 0;
    if (listPtr === 0) return 0; // NIL

    const byteOffset = MemoryManager.Address.lispPtrToByte(listPtr);
    if (byteOffset + 5 > vm.virtualMemory.length) return 0;

    // Read CDR code (byte at offset 4)
    const cdrCode = vm.virtualMemory[byteOffset + 4];

    if (cdrCode === CDR_NIL) {
        return 0; // NIL
    } else if ((cdrCode & CDR_ONPAGE) !== 0) {
        // CDR_ONPAGE: CDR is on same page
        const pageBase = MemoryManager.Address.getVirtualAddressBase(
            MemoryManager.Address.getVirtualPage(byteOffset)
        );
        const cdrOffset = ((cdrCode & 0x7F) << 1);
        const cdrPtr = MemoryManager.Address.byteToLispPtr(pageBase + cdrOffset);
        return cdrPtr;
    } else if (cdrCode === CDR_INDIRECT) {
        // CDR_INDIRECT: CDR is stored in next cell
        const nextCellOffset = byteOffset + 8; // Next cons cell (8 bytes)
        if (nextCellOffset + 4 > vm.virtualMemory.length) return 0;
        return MemoryManager.Access.readLispPTR(vm.virtualMemory, nextCellOffset);
    } else {
        // Direct CDR coding (low 7 bits encode offset)
        const pageBase = MemoryManager.Address.getVirtualAddressBase(
            MemoryManager.Address.getVirtualPage(byteOffset)
        );
        const cdrOffset = (cdrCode << 1);
        const cdrPtr = MemoryManager.Address.byteToLispPtr(pageBase + cdrOffset);
        return cdrPtr;
    }
}

/**
 * CAR opcode handler
 * Get CAR (first element) of cons cell
 * Per C: OPCAR macro in maiko/inc/tosfns.h
 */
function handleCAR(vm: VM, instruction: Instruction): number | null {
    const listPtr = vm.topOfStack;
    const car = getCAR(vm, listPtr);
    vm.topOfStack = car;
    return null;
}

/**
 * CDR opcode handler
 * Get CDR (rest) of cons cell
 * Per C: OPCDR macro in maiko/inc/tosfns.h
 */
function handleCDR(vm: VM, instruction: Instruction): number | null {
    const listPtr = vm.topOfStack;
    const cdr = getCDR(vm, listPtr);
    vm.topOfStack = cdr;
    return null;
}

/**
 * CONS opcode handler
 * Create new cons cell
 * Per C: N_OP_cons in maiko/src/conspage.c
 * Stack: [car, cdr] -> [cons_cell]
 */
function handleCONS(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;

    // Pop CDR and CAR from stack
    const cdr = vm.topOfStack;
    // TODO: Need to pop second value - for now, use placeholder
    const car = 0; // Placeholder - need proper stack management

    // TODO: Allocate new cons cell from cons page
    // For now, create a placeholder
    // Full implementation would:
    // 1. Check if cons_cdr == NIL (use free cell with CDR_NIL)
    // 2. Otherwise allocate new cell and set CDR code
    // 3. Handle CDR coding (ONPAGE, INDIRECT, etc.)

    const newConsCell = 0; // Placeholder
    vm.topOfStack = newConsCell;

    return null;
}

/**
 * RPLACA opcode handler
 * Replace CAR of cons cell
 * Per C: N_OP_rplaca in maiko/src/car-cdr.c
 */
function handleRPLACA(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;

    const newCar = vm.topOfStack;
    // TODO: Pop cons cell from stack
    const consCell = 0; // Placeholder

    const byteOffset = MemoryManager.Address.lispPtrToByte(consCell);
    if (byteOffset + 4 <= vm.virtualMemory.length) {
        MemoryManager.Access.writeLispPTR(vm.virtualMemory, byteOffset, newCar);
    }

    vm.topOfStack = consCell; // Return cons cell
    return null;
}

/**
 * RPLACD opcode handler
 * Replace CDR of cons cell
 * Per C: N_OP_rplacd in maiko/src/car-cdr.c
 */
function handleRPLACD(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;

    const newCdr = vm.topOfStack;
    // TODO: Pop cons cell from stack
    const consCell = 0; // Placeholder

    // TODO: Update CDR code based on newCdr value
    // This is complex - involves CDR coding logic

    vm.topOfStack = consCell; // Return cons cell
    return null;
}

/**
 * LISTP opcode handler
 * Check if value is a list
 */
function handleLISTP(vm: VM, instruction: Instruction): number | null {
    const value = vm.topOfStack;
    const isListValue = isList(vm, value);
    vm.topOfStack = isListValue ? 1 : 0; // T or NIL
    return null;
}

// Register handlers
registerOpcodeHandler(Opcode.CAR, handleCAR);
registerOpcodeHandler(Opcode.CDR, handleCDR);
registerOpcodeHandler(Opcode.CONS, handleCONS);
registerOpcodeHandler(Opcode.RPLACA, handleRPLACA);
registerOpcodeHandler(Opcode.RPLACD, handleRPLACD);
registerOpcodeHandler(Opcode.TYPEP, handleLISTP); // TYPEP is similar
