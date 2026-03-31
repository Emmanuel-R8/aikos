// Function call tests
import { describe, test, expect, beforeEach } from 'bun:test';
import { VM } from '../src/vm/vm';
import { executeInstruction } from '../src/vm/execution';
import { MemoryManager } from '../src/vm/memory/manager';
import { Opcode, type Instruction, getInstructionLength } from '../src/vm/dispatch/opcode';
import { FRAMESIZE_BYTES } from '../src/utils/constants';

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
            0x1000,
            0x2000,
            0x3000,
            0x4000,
            0x5000
        );

        vm.pc = 0x1000;
        vm.stackPtr = vm.stackBase;
        vm.cstkptrl = vm.stackBase;
    });

    function runOpcode(opcode: Opcode, operands: number[] = []): number | null {
        const instruction: Instruction = {
            opcode,
            operands: new Uint8Array(operands),
            length: getInstructionLength(opcode),
        };
        return executeInstruction(vm, instruction);
    }

    test('FN0 calls function with 0 arguments', () => {
        // Set up function header
        const fnheaderOffset = 0x6000;
        MemoryManager.Access.writeDLword(memory, fnheaderOffset + 0, 0); // stkmin
        MemoryManager.Access.writeDLword(memory, fnheaderOffset + 2, 0); // na (0 args)
        MemoryManager.Access.writeDLword(memory, fnheaderOffset + 4, 0xFFFF); // pv = -1
        MemoryManager.Access.writeDLword(memory, fnheaderOffset + 6, 0); // startpc
        memory[ fnheaderOffset + 8 ] = 0; // flags

        // Set up defcell pointing to function header
        const atomIndex = 0;
        const defcellOffset = vm.atomSpaceOffset + (atomIndex * 4);
        const fnheaderPtr = MemoryManager.Address.byteToLispPtr(fnheaderOffset);
        MemoryManager.Access.writeLispPTR(memory, defcellOffset, fnheaderPtr);

        runOpcode(Opcode.FN0, [ atomIndex ]);

        // Should have set up frame and updated PC
        expect(vm.funcObj).toBe(fnheaderOffset);
        expect(vm.currentFrameOffset).not.toBeNull();
        expect(vm.pvar).toBe((vm.currentFrameOffset ?? 0) + FRAMESIZE_BYTES);
        expect(vm.pc).toBe(fnheaderOffset + 1);
    });

    test('RETURN restores frame state', () => {
        // Set up a frame on the stack
        const fxOffset = vm.stackBase + 0x40;
        const savedPc = 0x50;
        const fnheaderOffset = 0x7000;
        const fnheaderPtr = MemoryManager.Address.byteToLispPtr(fnheaderOffset);
        const nextblock = 0x100;
        const returnValue = 0x12345678;

        // Minimal function header so RETURN can validate FuncObj.
        MemoryManager.Access.writeDLword(memory, fnheaderOffset + 0, 0);
        MemoryManager.Access.writeDLword(memory, fnheaderOffset + 2, 0);
        MemoryManager.Access.writeDLword(memory, fnheaderOffset + 4, 0xFFFF);
        MemoryManager.Access.writeDLword(memory, fnheaderOffset + 6, 0);

        // Write BF marker / nextblock slot immediately before FX.
        const bfMarker = 0x80000000 | nextblock;
        MemoryManager.Access.writeLispPTR(memory, fxOffset - 4, bfMarker);

        // Write frame structure
        MemoryManager.Access.writeDLword(memory, fxOffset + 0, 0); // flags
        MemoryManager.Access.writeDLword(memory, fxOffset + 2, 0x1234); // alink
        MemoryManager.Access.writeDLword(memory, fxOffset + 4, fnheaderPtr & 0xFFFF); // lofnheader
        MemoryManager.Access.writeDLword(memory, fxOffset + 6, (fnheaderPtr >> 16) & 0xFFFF); // hifnheader
        MemoryManager.Access.writeDLword(memory, fxOffset + 8, nextblock); // nextblock
        MemoryManager.Access.writeDLword(memory, fxOffset + 10, savedPc); // pc

        // Set up VM state
        vm.currentFrameOffset = fxOffset;
        vm.currentFrame = null;
        vm.pvar = fxOffset + FRAMESIZE_BYTES;
        vm.stackPtr = vm.pvar;
        vm.cstkptrl = vm.pvar;
        vm.topOfStack = returnValue;
        vm.pc = 0x2000; // Current PC (will be restored)
        vm.funcObj = 0x6800;

        runOpcode(Opcode.RETURN);

        // Should restore PC and stack pointer
        expect(vm.pc).toBe(fnheaderOffset + savedPc);
        expect(vm.funcObj).toBe(fnheaderOffset);
        expect(vm.ivar).toBe(MemoryManager.Address.stackOffsetToByte(vm.stackBase, nextblock));
        expect(vm.pvar).toBe(fxOffset + FRAMESIZE_BYTES);
        expect(vm.currentFrameOffset).toBe(fxOffset);
        expect(vm.topOfStack).toBe(returnValue);
    });
});
