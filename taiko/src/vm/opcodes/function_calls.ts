// Function call opcodes (FN0-FNX, RETURN)
// Per maiko/inc/tosfns.h and maiko/src/bbtsub.c

import type { VM } from '../vm';
import type { Instruction } from '../dispatch/opcode';
import { Opcode } from '../dispatch/opcode';
import { registerOpcodeHandler } from './index';
import { MemoryManager } from '../memory/manager';
import { DefCellManager, FunctionHeader } from '../memory/defcell';
import { pushStack, popStack } from './stack_helpers';
import { NIL_PTR, FRAMESIZE } from '../../utils/constants';

// Constants for frame markers
const BF_MARK = 0x80000000; // Binding frame marker
const FX_MARK = 0x40000000; // Frame marker
const BF_MARK32 = 0x80000000; // 32-bit binding frame marker
const UNBOUND_VALUE = 0xFFFFFFFF; // Unbound variable marker

/**
 * Common function call handler
 * Per C: OPFN macro in maiko/inc/tosfns.h
 *
 * @param vm VM instance
 * @param instruction Instruction
 * @param argCount Number of arguments (0-4 for FN0-FN4, variable for FNX)
 * @returns Jump offset (null for normal execution)
 */
function handleFN(vm: VM, instruction: Instruction, argCount: number): number | null {
    if (vm.virtualMemory === null) return null;

    // Get atom index from instruction operand
    // C: Get_AtomNo_PCMAC1 = Get_DLword_PCMAC1
    // For FN0-FN4: atom index is at PC+1 (1 byte operand)
    // For FNX: atom index is at PC+2 (after num_args byte)
    let atomIndex: number;
    if (instruction.opcode === Opcode.FNX) {
        if (instruction.operands.length < 2) return null;
        argCount = instruction.operands[0]; // First operand is num_args
        atomIndex = instruction.operands[1]; // Second operand is atom index
    } else {
        if (instruction.operands.length < 1) return null;
        atomIndex = instruction.operands[0];
    }

    // Lookup definition cell
    // C: defcell = GetDEFCELL68k(atom_index)
    const defcell = DefCellManager.getDefCell(vm.virtualMemory, vm.atomSpaceOffset, atomIndex);

    if (!defcell) {
        console.warn(`FN: Definition cell not found for atom index ${atomIndex}`);
        return null;
    }

    // Check if C code function
    // C: if (!(fn_defcell->ccodep))
    if (DefCellManager.isCCode(defcell)) {
        // C code function - TODO: Handle C code functions
        console.warn(`FN: C code functions not yet supported for atom index ${atomIndex}`);
        return null;
    }

    // Get function header
    // C: LOCFNCELL = (struct fnhead *)NativeAligned4FromLAddr(defcell->defpointer)
    const fnheaderPtr = DefCellManager.getFunctionHeaderPtr(defcell);
    if (fnheaderPtr === 0) {
        console.warn(`FN: Function header pointer is 0 for atom index ${atomIndex}`);
        return null;
    }

    const fnheader = DefCellManager.readFunctionHeader(vm.virtualMemory, fnheaderPtr);
    if (!fnheader) {
        console.warn(`FN: Failed to read function header at 0x${fnheaderPtr.toString(16)}`);
        return null;
    }

    // Save current PC in frame
    // C: CURRENTFX->pc = ((UNSIGNED)PC - (UNSIGNED)FuncObj) + FN_OPCODE_SIZE
    const instructionSize = instruction.length;
    const savedPc = vm.pc - (vm.funcObj || 0) + instructionSize;

    // Calculate nextblock (IVAR base)
    // C: IVARL = (DLword *)(CSTKPTR - (argcount) + 1)
    // C: CURRENTFX->nextblock = StackOffsetFromNative(IVARL)
    const ivarOffset = vm.stackPtr - (argCount * 4) + 4; // +4 for TOS
    const nextblock = (ivarOffset - vm.stackBase) / 2; // Convert to DLword offset

    // Push TOS (save current top of stack)
    // C: HARD_PUSH(TOPOFSTACK)
    pushStack(vm, vm.topOfStack);

    // Handle spread arguments
    // C: if (LOCFNCELL->na >= 0) { /* Spread type */ }
    if (fnheader.na >= 0) {
        const restArgs = argCount - fnheader.na;
        if (restArgs < 0) {
            // Not enough arguments - push NIL for missing args
            for (let i = 0; i < -restArgs; i++) {
                pushStack(vm, NIL_PTR);
            }
        } else if (restArgs > 0) {
            // Too many arguments - adjust stack pointer
            vm.stackPtr -= (restArgs * 4);
            if (vm.cstkptrl !== null) {
                vm.cstkptrl -= restArgs;
            }
        }
    }

    // Set up BF (Binding Frame) marker
    // C: HARD_PUSH(BF_MARK32 | NEXTBLOCK)
    const bfMarker = BF_MARK32 | (nextblock & 0xFFFF);
    pushStack(vm, bfMarker);

    // Set up FX (Frame) marker
    // C: *((LispPTR *)CSTKPTR) = (FX_MARK << 16) | (StackOffsetFromNative(PVAR))
    const pvarOffset = (vm.stackPtr - vm.stackBase) / 2; // DLword offset
    const fxMarker = (FX_MARK << 16) | (pvarOffset & 0xFFFF);
    pushStack(vm, fxMarker);

    // Write function header pointer to frame
    // C: ((struct frameex2 *)CSTKPTR)->fnheader = SWAP_FNHEAD(defcell_word)
    // For now, simplified: store fnheader pointer in frame
    const frameOffset = vm.stackPtr;
    MemoryManager.Access.writeLispPTR(vm.virtualMemory, frameOffset, fnheaderPtr);

    // Update CSTKPTRL and PVAR
    // C: CSTKPTRL = (LispPTR *)(((DLword *)CSTKPTR) + FRAMESIZE)
    // C: PVARL = (DLword *)CSTKPTR
    vm.stackPtr += (FRAMESIZE * 2); // FRAMESIZE DLwords = FRAMESIZE * 2 bytes
    vm.cstkptrl = vm.stackPtr;
    vm.pvar = vm.stackPtr;

    // Initialize PVAR slots with unbound values
    // C: for (int pv = LOCFNCELL->pv; pv >= 0; pv--) { HARD_PUSH(unboundval); HARD_PUSH(unboundval); }
    if (fnheader.pv >= 0) {
        for (let pv = fnheader.pv; pv >= 0; pv--) {
            pushStack(vm, UNBOUND_VALUE);
            pushStack(vm, UNBOUND_VALUE);
        }
    }

    // Push closure environment (NIL for now)
    // C: HARD_PUSH(closure_env)
    pushStack(vm, NIL_PTR);

    // Update PC to function start
    // C: PCMACL = (ByteCode *)LOCFNCELL + LOCFNCELL->startpc + 1
    const fnheaderByteOffset = MemoryManager.Address.lispPtrToByte(fnheaderPtr);
    vm.pc = fnheaderByteOffset + fnheader.startpc + 1;
    vm.funcObj = fnheaderByteOffset;

    // Sync TOPOFSTACK
    vm.syncTopOfStack();

    return null; // No jump offset - PC already updated
}

/**
 * FN0-FN4 handlers
 */
function handleFN0(vm: VM, instruction: Instruction): number | null {
    return handleFN(vm, instruction, 0);
}

function handleFN1(vm: VM, instruction: Instruction): number | null {
    return handleFN(vm, instruction, 1);
}

function handleFN2(vm: VM, instruction: Instruction): number | null {
    return handleFN(vm, instruction, 2);
}

function handleFN3(vm: VM, instruction: Instruction): number | null {
    return handleFN(vm, instruction, 3);
}

function handleFN4(vm: VM, instruction: Instruction): number | null {
    return handleFN(vm, instruction, 4);
}

function handleFNX(vm: VM, instruction: Instruction): number | null {
    return handleFN(vm, instruction, -1); // -1 indicates variable args
}

/**
 * RETURN opcode handler
 * Return from function call
 * Per C: OPRETURN macro in maiko/inc/tosret.h
 */
function handleRETURN(vm: VM, instruction: Instruction): number | null {
    if (vm.virtualMemory === null || vm.cstkptrl === null) return null;

    // Get return value from TOPOFSTACK
    const returnValue = vm.topOfStack;

    // Find FX marker on stack
    // C: Walks backwards to find FX marker
    let fxFound = false;
    let fxOffset = vm.cstkptrl;

    // Search for FX marker (FX_MARK in upper word)
    for (let i = 0; i < 1000; i++) {
        fxOffset -= 4;
        if (fxOffset < vm.stackEnd || fxOffset >= vm.stackBase) {
            break;
        }

        const value = MemoryManager.Access.readLispPTR(vm.virtualMemory, fxOffset);
        if ((value & 0x40000000) !== 0) { // FX_MARK
            fxFound = true;
            break;
        }
    }

    if (!fxFound) {
        console.warn('RETURN: FX marker not found');
        return null;
    }

    // Read function header pointer from frame
    // C: FuncObj = (struct fnhead *)NativeAligned4FromLAddr(FX_FNHEADER)
    const fnheaderPtr = MemoryManager.Access.readLispPTR(vm.virtualMemory, fxOffset);
    const fnheader = DefCellManager.readFunctionHeader(vm.virtualMemory, fnheaderPtr & 0x7FFFFFFF);

    if (!fnheader) {
        console.warn('RETURN: Failed to read function header');
        return null;
    }

    // Restore PC from frame
    // C: PC = FuncObj + returnFX->pc
    const framePcOffset = fxOffset + 10; // PC is at offset 10 (5 DLwords) in frame
    const savedPc = MemoryManager.Access.readDLword(vm.virtualMemory, framePcOffset);
    const fnheaderByteOffset = MemoryManager.Address.lispPtrToByte(fnheaderPtr & 0x7FFFFFFF);
    vm.pc = fnheaderByteOffset + savedPc;
    vm.funcObj = fnheaderByteOffset;

    // Restore stack pointer (pop frame and BF)
    // C: Restore CSTKPTRL to previous frame
    // Simplified: restore to position before FX marker
    vm.stackPtr = fxOffset;
    vm.cstkptrl = fxOffset;

    // Set return value as TOPOFSTACK
    vm.topOfStack = returnValue;

    // Sync TOPOFSTACK
    vm.syncTopOfStack();

    return null; // No jump offset - PC already updated
}

// Register handlers
registerOpcodeHandler(Opcode.FN0, handleFN0);
registerOpcodeHandler(Opcode.FN1, handleFN1);
registerOpcodeHandler(Opcode.FN2, handleFN2);
registerOpcodeHandler(Opcode.FN3, handleFN3);
registerOpcodeHandler(Opcode.FN4, handleFN4);
registerOpcodeHandler(Opcode.FNX, handleFNX);
registerOpcodeHandler(Opcode.RETURN, handleRETURN);
