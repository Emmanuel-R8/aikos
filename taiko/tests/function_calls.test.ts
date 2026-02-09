// Function call tests
import { describe, test, expect, beforeEach } from 'bun:test';
import { VM } from '../src/vm/vm';
import { executeStep } from '../src/vm/execution';
import { MemoryManager } from '../src/vm/memory/manager';
import { DefCellManager } from '../src/vm/memory/defcell';
import { Opcode } from '../src/vm/dispatch/opcode';
import { NIL_PTR, FRAMESIZE_BYTES, ATOMS_OFFSET, DEFS_OFFSET } from '../src/utils/constants';

describe('Function Calls', () => {
    let vm: VM;
    let memory: Uint8Array;

    beforeEach(() => {
        vm = new VM(64 * 1024);
        memory = new Uint8Array(1024 * 1024);
        const fptovpTable = new Uint32Array(100);

        vm.initializeMemory(
            memory,
            fptovpTable,
            MemoryManager.Address.lispPtrToByte(ATOMS_OFFSET),
            MemoryManager.Address.lispPtrToByte(DEFS_OFFSET),
            0x30000, 0x40000, 0x50000
        );

        vm.pc = 0x1000;
        vm.stackPtr = vm.stackBase;
        vm.cstkptrl = vm.stackBase;
    });

    test.skip('FN0 calls function with 0 arguments', () => {
        // Set up function header
        const fnheaderOffset = 0x5000;
        MemoryManager.Access.writeDLword(memory, fnheaderOffset + 0, 0); // stkmin
        MemoryManager.Access.writeDLword(memory, fnheaderOffset + 2, 0); // na (0 args)
        MemoryManager.Access.writeDLword(memory, fnheaderOffset + 4, 0); // pv
        MemoryManager.Access.writeDLword(memory, fnheaderOffset + 6, 0); // startpc
        memory[ fnheaderOffset + 8 ] = 0; // flags

        // Set up defcell pointing to function header
        const atomIndex = 0;
        const defcellOffset = vm.defSpaceOffset + (atomIndex * 4);
        const fnheaderPtr = MemoryManager.Address.byteToLispPtr(fnheaderOffset);
        MemoryManager.Access.writeLispPTR(memory, defcellOffset, fnheaderPtr);

        // Set up FN0 instruction
        memory[ vm.pc ] = Opcode.FN0;
        memory[ vm.pc + 1 ] = atomIndex;

        executeStep(vm);

        // Should have set up frame and updated PC
        expect(vm.funcObj).not.toBeNull();
        expect(vm.pvar).not.toBeNull();
        expect(vm.pc).toBe(fnheaderOffset);
    });

    test.skip('RETURN restores frame state', () => {
        // Set up a frame on the stack
        const fxOffset = vm.stackBase - FRAMESIZE_BYTES - 4;
        const savedPc = 0x50;
        const fnheaderPtr = 0x5000;
        const nextblock = 0x100;

        // Write FX marker
        const fxMarker = (0x40000000) | ((fxOffset - vm.stackBase) / 2);
        MemoryManager.Access.writeLispPTR(memory, fxOffset - 4, fxMarker);

        // Write BF marker
        const bfMarker = 0x80000000 | nextblock;
        MemoryManager.Access.writeLispPTR(memory, fxOffset - 8, bfMarker);

        // Write frame structure
        MemoryManager.Access.writeDLword(memory, fxOffset + 0, 0); // flags
        MemoryManager.Access.writeDLword(memory, fxOffset + 2, 0x1234); // alink
        MemoryManager.Access.writeDLword(memory, fxOffset + 4, fnheaderPtr & 0xFFFF); // lofnheader
        MemoryManager.Access.writeDLword(memory, fxOffset + 6, (fnheaderPtr >> 16) & 0xFFFF); // hifnheader
        MemoryManager.Access.writeDLword(memory, fxOffset + 8, nextblock); // nextblock
        MemoryManager.Access.writeDLword(memory, fxOffset + 10, savedPc); // pc

        // Set up VM state
        vm.stackPtr = fxOffset;
        vm.cstkptrl = fxOffset;
        vm.topOfStack = 0x12345678; // Return value
        vm.pc = 0x2000; // Current PC (will be restored)

        // Execute RETURN
        memory[ vm.pc ] = Opcode.RETURN;
        executeStep(vm);

        // Should restore PC and stack pointer
        expect(vm.pc).toBe(fnheaderPtr + savedPc);
        expect(vm.topOfStack).toBe(0x12345678);
    });
});
