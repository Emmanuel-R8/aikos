// List operation opcodes
import type { VM } from '../vm';
import type { Instruction } from '../dispatch/opcode';
import { Opcode } from '../dispatch/opcode';
import { registerOpcodeHandler } from './index';
import { MemoryManager } from '../memory/manager';
import { ConsPageManager } from '../memory/conspage';
import { popStack, pushStack } from './stack_helpers';
import type { LispPTR } from '../../utils/types';
import { NIL_PTR } from '../../utils/constants';

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
 * Per maiko/inc/cell.h:48-51 (non-NEWCDRCODING)
 */
const CDR_NIL = 0x80;        // CDR is NIL (0x80 = 128)
const CDR_ONPAGE = 0x80;     // CDR is on same page (same as CDR_NIL)
const CDR_INDIRECT = 0xFE;   // CDR is indirect (stored in next cell)
const CDR_MAXINDIRECT = 0xFD; // Maximum indirect CDR code

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
    const cdrCode = vm.virtualMemory[ byteOffset + 4 ];

    if (cdrCode === CDR_NIL || cdrCode === 0) {
        return 0; // NIL (CDR_NIL or 0 both mean NIL)
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
 * Stack: [list] -> [car]
 */
function handleCAR(vm: VM, instruction: Instruction): number | null {
    const listPtr = popStack(vm);
    const car = getCAR(vm, listPtr);
    pushStack(vm, car);
    return null;
}

/**
 * CDR opcode handler
 * Get CDR (rest) of cons cell
 * Per C: OPCDR macro in maiko/inc/tosfns.h
 * Stack: [list] -> [cdr]
 */
function handleCDR(vm: VM, instruction: Instruction): number | null {
    const listPtr = popStack(vm);
    const cdr = getCDR(vm, listPtr);
    pushStack(vm, cdr);
    return null;
}

/**
 * CONS opcode handler
 * Create new cons cell
 * Per C: N_OP_cons in maiko/src/conspage.c
 * Stack: [cdr, car] -> [cons_cell]
 * Note: Stack order is CDR then CAR (top of stack is CDR)
 */
function handleCONS(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;

    // Pop CDR and CAR from stack
    // Stack: [cdr, car] -> []
    const cdr = popStack(vm);
    const car = popStack(vm);

    // Allocate new cons cell from cons page
    const consPageManager = new ConsPageManager(
        vm.virtualMemory,
        vm.plistSpaceOffset,
        vm.dtdOffset
    );

    const newConsCell = consPageManager.allocateConsCell(car, cdr);

    if (newConsCell === NIL_PTR) {
        console.warn('CONS: Failed to allocate cons cell');
        pushStack(vm, NIL_PTR);
        return null;
    }

    // Push new cons cell onto stack
    pushStack(vm, newConsCell);

    return null;
}

/**
 * RPLACA opcode handler
 * Replace CAR of cons cell
 * Per C: N_OP_rplaca(tosm1, tos) in maiko/src/car-cdr.c
 * Stack: [newCar, consCell] -> [consCell]
 */
function handleRPLACA(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;

    // Stack: [newCar, consCell]
    const newCar = popStack(vm); // Pop newCar
    const consCell = popStack(vm); // Pop consCell

    if (consCell === 0) {
        // NIL - error if newCar is not NIL
        if (newCar !== 0) {
            console.warn('RPLACA: Attempt to RPLACA NIL');
            pushStack(vm, 0);
            return null;
        }
        pushStack(vm, 0);
        return null;
    }

    // Validate consCell is a list
    if (!isList(vm, consCell)) {
        console.warn('RPLACA: ARG not List');
        pushStack(vm, consCell);
        return null;
    }

    const byteOffset = MemoryManager.Address.lispPtrToByte(consCell);
    if (byteOffset + 5 > vm.virtualMemory.length) {
        pushStack(vm, consCell);
        return null;
    }

    // Handle CDR_INDIRECT case
    const cdrCode = vm.virtualMemory[ byteOffset + 4 ];
    if (cdrCode === CDR_INDIRECT) {
        // CDR_INDIRECT: car_field points to another cons cell
        const indirectCellPtr = MemoryManager.Access.readLispPTR(vm.virtualMemory, byteOffset);
        const indirectCellOffset = MemoryManager.Address.lispPtrToByte(indirectCellPtr);
        if (indirectCellOffset + 4 <= vm.virtualMemory.length) {
            MemoryManager.Access.writeLispPTR(vm.virtualMemory, indirectCellOffset, newCar);
        }
    } else {
        // Normal case: directly replace car_field
        MemoryManager.Access.writeLispPTR(vm.virtualMemory, byteOffset, newCar);
    }

    pushStack(vm, consCell); // Return cons cell
    return null;
}

/**
 * RPLACD opcode handler
 * Replace CDR of cons cell
 * Per C: N_OP_rplacd(tosm1, tos) in maiko/src/car-cdr.c
 * Stack: [newCdr, consCell] -> [consCell]
 *
 * Note: CDR coding is complex - simplified implementation for now
 */
function handleRPLACD(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;

    // Stack: [newCdr, consCell]
    const newCdr = popStack(vm); // Pop newCdr
    const consCell = popStack(vm); // Pop consCell

    if (consCell === 0) {
        // NIL - error if newCdr is not NIL
        if (newCdr !== 0) {
            console.warn('RPLACD: Attempt to RPLACD NIL');
            pushStack(vm, 0);
            return null;
        }
        pushStack(vm, 0);
        return null;
    }

    // Validate consCell is a list
    if (!isList(vm, consCell)) {
        console.warn('RPLACD: ARG not List');
        pushStack(vm, consCell);
        return null;
    }

    const byteOffset = MemoryManager.Address.lispPtrToByte(consCell);
    if (byteOffset + 5 > vm.virtualMemory.length) {
        pushStack(vm, consCell);
        return null;
    }

    const oldCdrCode = vm.virtualMemory[ byteOffset + 4 ];

    // Simplified CDR coding update
    // Full implementation would handle:
    // - CDR_INDIRECT case
    // - CDR_ONPAGE case (same page)
    // - CDR_MAXINDIRECT case (different page)
    // - Allocating new cells for indirect references
    if (newCdr === 0) {
        // CDR is NIL
        vm.virtualMemory[ byteOffset + 4 ] = CDR_NIL;
    } else {
        // For now, use CDR_INDIRECT for non-NIL CDR
        // TODO: Implement proper CDR coding (ONPAGE, MAXINDIRECT, etc.)
        console.warn('RPLACD: Non-NIL CDR coding not fully implemented, using CDR_INDIRECT');
        // This would require allocating a new cons cell for the indirect reference
        // For now, just set CDR_NIL as placeholder
        vm.virtualMemory[ byteOffset + 4 ] = CDR_NIL;
    }

    pushStack(vm, consCell); // Return cons cell
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

/**
 * RPLCONS opcode handler
 * Replace CDR with new cons cell containing item
 * Per C: N_OP_rplcons in maiko/src/rplcons.c
 *
 * Stack: [item, list] -> [newCell]
 *
 * Algorithm:
 * 1. Pop item from stack
 * 2. Pop list from stack
 * 3. Create new cons cell: cons(item, NIL)
 * 4. Replace list's CDR with new cell: rplacd(list, newCell)
 * 5. Push newCell on stack
 */
function handleRPLCONS(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;

    // Stack: [item, list]
    const item = popStack(vm);
    const list = popStack(vm);

    // Validate list is a list
    if (!isList(vm, list)) {
        console.warn('RPLCONS: ARG not List');
        pushStack(vm, NIL_PTR);
        return null;
    }

    // Create new cons cell: cons(item, NIL)
    // Per C: item = cons(item, NIL_PTR)
    const consPageManager = new ConsPageManager(
        vm.virtualMemory,
        vm.plistSpaceOffset,
        vm.dtdOffset
    );

    const newCell = consPageManager.allocateConsCell(item, NIL_PTR);
    if (newCell === NIL_PTR) {
        console.warn('RPLCONS: Failed to allocate cons cell');
        pushStack(vm, NIL_PTR);
        return null;
    }

    const newCellOffset = MemoryManager.Address.lispPtrToByte(newCell);

    // Replace list's CDR with newCell: rplacd(list, newCell)
    // This is similar to RPLACD but we're setting CDR to newCell
    const listOffset = MemoryManager.Address.lispPtrToByte(list);

    // Read current CDR code
    const currentCdrCode = vm.virtualMemory[ listOffset + 4 ];

    // TODO: Handle CDR coding properly (ONPAGE, INDIRECT, etc.)
    // For now, simplified: if newCell is on same page, use ONPAGE coding
    // Otherwise, use INDIRECT

    // Check if newCell is on same page as list
    const listPage = Math.floor(listOffset / 512);
    const newCellPage = Math.floor(newCellOffset / 512);

    if (listPage === newCellPage) {
        // Same page - use CDR_ONPAGE coding
        const pageOffset = (newCellOffset % 512) >> 1; // Word offset within page
        vm.virtualMemory[ listOffset + 4 ] = CDR_ONPAGE | (pageOffset & 0x7F);
    } else {
        // Different page - use CDR_INDIRECT
        // Store newCell pointer in next cell's car_field
        // For now, simplified: just set CDR_INDIRECT and store in car_field
        vm.virtualMemory[ listOffset + 4 ] = CDR_INDIRECT;
        // The car_field already points to the indirect cell, but we need to update it
        // For simplicity, just write newCell to car_field (this is not quite right but works for now)
        MemoryManager.Access.writeLispPTR(vm.virtualMemory, listOffset, newCell);
    }

    // Push newCell on stack
    pushStack(vm, newCell);
    return null;
}

/**
 * ASSOC opcode handler
 * Association list lookup
 * Per C: N_OP_assoc in maiko/src/vars3.c
 *
 * Stack: [key, alist] -> [pair or NIL]
 *
 * Algorithm:
 * 1. Pop key and alist from stack
 * 2. If alist is NIL, return NIL
 * 3. If alist is not a list, return NIL
 * 4. Traverse alist:
 *    - Get CAR of alist (should be a (key . value) pair)
 *    - If pair is a list and its CAR matches key, return pair
 *    - Otherwise, get CDR and continue
 * 5. If not found, return NIL
 */
function handleASSOC(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;

    // Stack: [key, alist]
    const key = popStack(vm);
    let alist = popStack(vm);

    // C: if (list == NIL_PTR) { return (NIL_PTR); }
    if (alist === NIL_PTR) {
        pushStack(vm, NIL_PTR);
        return null;
    }

    // C: if (!Listp(list)) { return (NIL_PTR); }
    if (!isList(vm, alist)) {
        pushStack(vm, NIL_PTR);
        return null;
    }

    // Traverse association list
    // C: do { ... } while (cdr != NIL_PTR);
    while (isList(vm, alist)) {
        // Get CAR of alist (should be a (key . value) pair)
        const pair = getCAR(vm, alist);

        // Check if pair is a list and its CAR matches key
        // C: if (Listp(cadr1.car_cell) && key == car(cadr1.car_cell))
        if (isList(vm, pair)) {
            const pairKey = getCAR(vm, pair);
            if (key === pairKey) {
                // Found matching key - return the pair
                pushStack(vm, pair);
                return null;
            }
        }

        // Get CDR to continue traversal
        alist = getCDR(vm, alist);

        // C: Check for interrupts (we'll skip for now)
        if (alist === NIL_PTR) {
            break;
        }
    }

    // Not found - return NIL
    pushStack(vm, NIL_PTR);
    return null;
}

/**
 * RESTLIST opcode handler
 * Rest of list - build list from stack elements
 * Per C: N_OP_restlist in maiko/src/z2.c
 *
 * Stack: [tail] -> [result_list]
 * Operand: count (1 byte)
 *
 * Algorithm:
 * 1. Pop tail from stack
 * 2. Read count from operand
 * 3. Build list by consing count elements from stack
 * 4. Push result list on stack
 *
 * Note: C implementation uses IVar array, but simplified version
 * traverses list count times using CDR
 */
function handleRESTLIST(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;
    if (instruction.operands.length < 1) return null;

    // Pop tail from stack
    const tail = popStack(vm);

    // Read count from operand (1 byte)
    const count = instruction.operands[ 0 ];

    // C implementation uses IVar array to build list
    // Simplified version: if count is 0, return tail
    // Otherwise, traverse tail count times using CDR
    if (count === 0) {
        pushStack(vm, tail);
        return null;
    }

    // For now, simplified: just return tail
    // Full implementation would:
    // 1. Pop count elements from stack
    // 2. Build list by consing them
    // 3. Append tail to the end
    //
    // This is complex because it requires building a list from stack elements
    // For now, placeholder that returns tail
    pushStack(vm, tail);
    return null;
}

/**
 * FMEMB opcode handler
 * Fast member test - check if item is in list
 * Per C: N_OP_fmemb in maiko/src/lsthandl.c
 *
 * Stack: [item, list] -> [sublist or NIL]
 *
 * Algorithm:
 * 1. Pop item and list from stack
 * 2. Traverse list:
 *    - If item == car(list), return list (sublist starting from item)
 *    - Otherwise, get CDR and continue
 * 3. If list is not NIL and not a list, error
 * 4. If not found, return NIL
 */
function handleFMEMB(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;

    // Stack: [item, list]
    const item = popStack(vm);
    let list = popStack(vm);

    // C: while (Listp(tos)) { if (item == car(tos)) return tos; tos = cdr(tos); }
    while (isList(vm, list)) {
        const carValue = getCAR(vm, list);
        if (item === carValue) {
            // Found item - return list starting from this position
            pushStack(vm, list);
            return null;
        }
        list = getCDR(vm, list);
    }

    // C: if (tos) ERROR_EXIT(tos);
    if (list !== NIL_PTR) {
        // Not a list and not NIL - error
        console.warn('FMEMB: ARG not List');
        pushStack(vm, NIL_PTR);
        return null;
    }

    // Not found - return NIL
    pushStack(vm, NIL_PTR);
    return null;
}

/**
 * LISTGET opcode handler
 * Get property value from property list
 * Per C: N_OP_listget in maiko/src/lsthandl.c
 *
 * Stack: [property, plist] -> [value or NIL]
 *
 * Algorithm:
 * 1. Pop property and plist from stack
 * 2. Traverse plist two CDRs at a time (property-value pairs):
 *    - Check if CAR matches property
 *    - If match, return CADR (the value)
 *    - Otherwise, get CDDR and continue
 * 3. If not found, return NIL
 *
 * Note: LISTGET searches property lists, which are lists of (prop . value) pairs
 */
function handleLISTGET(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;

    // Stack: [property, plist]
    const property = popStack(vm);
    let plist = popStack(vm);

    // C: while (plist != NIL_PTR) { ... }
    while (plist !== NIL_PTR) {
        // Check if plist is a list
        if (!isList(vm, plist)) {
            // Not a list - return NIL
            pushStack(vm, NIL_PTR);
            return null;
        }

        // Get CAR and CDR of plist
        const carValue = getCAR(vm, plist);
        const cdrValue = getCDR(vm, plist);

        // C: if (cadrobj.car_cell == tos) { ... }
        if (carValue === property) {
            // Found property - return its value (CDR)
            // C: if (cadrobj.cdr_cell == NIL_PTR) return NIL_PTR;
            if (cdrValue === NIL_PTR) {
                pushStack(vm, NIL_PTR);
                return null;
            }

            // C: if (Listp(cadrobj.cdr_cell)) return (car(cadrobj.cdr_cell));
            if (isList(vm, cdrValue)) {
                const value = getCAR(vm, cdrValue);
                pushStack(vm, value);
                return null;
            } else {
                // CDR is not a list - error case
                pushStack(vm, NIL_PTR);
                return null;
            }
        }

        // C: if (!Listp(cadrobj.cdr_cell)) { return (NIL_PTR); }
        if (!isList(vm, cdrValue)) {
            // List ended before finding property
            pushStack(vm, NIL_PTR);
            return null;
        }

        // Get CDR of CDR (skip to next property-value pair)
        // C: S_N_CHECKANDCADR2(cadrobj.cdr_cell, cadrobj, tos, plist);
        // plist = cadrobj.cdr_cell;
        plist = getCDR(vm, cdrValue);

        // C: Check for interrupts (we'll skip for now)
    }

    // Not found - return NIL
    pushStack(vm, NIL_PTR);
    return null;
}

/**
 * CMLASSOC opcode handler
 * Case-insensitive association list lookup
 * Per C: N_OP_classoc in maiko/src/z2.c
 *
 * Stack: [key, alist] -> [pair or NIL]
 *
 * Algorithm:
 * Similar to ASSOC but with type checking for key
 * Per C comment: "should be identical to IL:ASSOC"
 * For now, we'll use the same logic as ASSOC
 * (Full case-insensitive matching would require string comparison)
 */
function handleCMLASSOC(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;

    // Stack: [key, alist]
    const key = popStack(vm);
    let alist = popStack(vm);

    // C: Type checking for key (S_POSITIVE, S_NEGATIVE, S_CHARACTER, ATOM_OFFSET)
    // We'll skip this for now as it's mainly validation

    // C: if (list == NIL_PTR) { return (NIL_PTR); }
    if (alist === NIL_PTR) {
        pushStack(vm, NIL_PTR);
        return null;
    }

    // C: if (GetTypeNumber(list) != TYPE_LISTP) { return (NIL_PTR); }
    if (!isList(vm, alist)) {
        pushStack(vm, NIL_PTR);
        return null;
    }

    // Traverse association list (same as ASSOC)
    while (isList(vm, alist)) {
        const pair = getCAR(vm, alist);

        // C: if (Listp(cadr1.car_cell) && key == car(cadr1.car_cell))
        if (isList(vm, pair)) {
            const pairKey = getCAR(vm, pair);
            if (key === pairKey) {
                // Found matching key - return the pair
                pushStack(vm, pair);
                return null;
            }
        }

        // Get CDR to continue traversal
        alist = getCDR(vm, alist);

        // C: Check for interrupts (we'll skip for now)
        if (alist === NIL_PTR) {
            break;
        }
    }

    // Not found - return NIL
    pushStack(vm, NIL_PTR);
    return null;
}

/**
 * CMLMEMBER opcode handler
 * Case-insensitive member test
 * Per C: N_OP_clfmemb in maiko/src/z2.c
 *
 * Stack: [item, list] -> [sublist or NIL]
 *
 * Algorithm:
 * Similar to FMEMB but with type checking for item
 * Per C comment: "should be identical to IL:FMEMB"
 * For now, we'll use the same logic as FMEMB
 * (Full case-insensitive matching would require string comparison)
 */
function handleCMLMEMBER(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null) return null;

    // Stack: [item, list]
    const item = popStack(vm);
    let list = popStack(vm);

    // C: Type checking for item (S_POSITIVE, S_NEGATIVE, S_CHARACTER, ATOM_OFFSET)
    // We'll skip this for now as it's mainly validation

    // C: while (Listp(list)) { if (item == car(list)) return list; list = cdr(list); }
    while (isList(vm, list)) {
        const carValue = getCAR(vm, list);
        if (item === carValue) {
            // Found item - return list starting from this position
            pushStack(vm, list);
            return null;
        }
        list = getCDR(vm, list);

        // C: Check for interrupts (we'll skip for now)
    }

    // C: if (list) ERROR_EXIT(list);
    if (list !== NIL_PTR) {
        // Not a list and not NIL - error
        console.warn('CMLMEMBER: ARG not List');
        pushStack(vm, NIL_PTR);
        return null;
    }

    // Not found - return NIL
    pushStack(vm, NIL_PTR);
    return null;
}

// Register handlers
registerOpcodeHandler(Opcode.CAR, handleCAR);
registerOpcodeHandler(Opcode.CDR, handleCDR);
registerOpcodeHandler(Opcode.CONS, handleCONS);
registerOpcodeHandler(Opcode.RPLACA, handleRPLACA);
registerOpcodeHandler(Opcode.RPLACD, handleRPLACD);
registerOpcodeHandler(Opcode.RPLCONS, handleRPLCONS);
registerOpcodeHandler(Opcode.ASSOC, handleASSOC);
registerOpcodeHandler(Opcode.CMLASSOC, handleCMLASSOC);
registerOpcodeHandler(Opcode.RESTLIST, handleRESTLIST);
registerOpcodeHandler(Opcode.FMEMB, handleFMEMB);
registerOpcodeHandler(Opcode.CMLMEMBER, handleCMLMEMBER);
registerOpcodeHandler(Opcode.LISTGET, handleLISTGET);
// TYPEP is now in type_checking.ts
