// Opcode execution tests
import { describe, test, expect, beforeEach } from 'bun:test';
import { VM } from '../src/vm/vm';
import { executeStep, executeInstruction } from '../src/vm/execution';
import { MemoryManager } from '../src/vm/memory/manager';
import { Opcode, type Instruction, getInstructionLength } from '../src/vm/dispatch/opcode';
import { NIL_PTR, ATOM_T, S_POSITIVE, FRAMESIZE_BYTES } from '../src/utils/constants';

describe('Opcode Execution', () => {
    let vm: VM;
    let memory: Uint8Array;

    beforeEach(() => {
        vm = new VM(64 * 1024); // 64KB stack
        memory = new Uint8Array(1024 * 1024); // 1MB virtual memory
        const fptovpTable = new Uint32Array(100);

        // Initialize memory with test offsets
        vm.initializeMemory(
            memory,
            fptovpTable,
            0x10000, // atomSpaceOffset
            0x20000, // defSpaceOffset
            0x30000, // valSpaceOffset
            0x40000, // plistSpaceOffset
            0x50000  // dtdOffset
        );
    });

    // Helper to run a single opcode with explicit operands, bypassing the byte-level decoder.
    function runOpcode(vm: VM, opcode: Opcode, operands: number[] = []): number | null {
        const length = getInstructionLength(opcode);
        const instruction: Instruction = { opcode, operands, length };
        const jumpOffset = executeInstruction(vm, instruction);
        vm.pc += length;
        return jumpOffset;
    }

    describe('Stack Operations', () => {
        test('NIL pushes NIL onto stack', () => {
            vm.pc = 0x1000;
            runOpcode(vm, Opcode.NIL);

            expect(vm.topOfStack).toBe(NIL_PTR);
            expect(vm.pc).toBe(0x1001);
        });

        test('T pushes T atom onto stack', () => {
            vm.pc = 0x1000;
            runOpcode(vm, Opcode.T);

            expect(vm.topOfStack).toBe(ATOM_T);
            expect(vm.pc).toBe(0x1001);
        });

        test('POP removes top of stack', () => {
            vm.pc = 0x1000;
            vm.topOfStack = 0x12345678;
            vm.stackPtr = vm.stackBase - 4;
            vm.cstkptrl = vm.stackPtr;
            MemoryManager.Access.writeLispPTR(memory, vm.stackPtr, 0x12345678);
            runOpcode(vm, Opcode.POP);

            expect(vm.stackPtr).toBe(vm.stackBase);
            expect(vm.pc).toBe(0x1001);
        });
    });

    describe('Arithmetic Operations', () => {
        test('PLUS2 adds two numbers', () => {
            vm.pc = 0x1000;
            vm.stackPtr = vm.stackBase - 8;
            vm.cstkptrl = vm.stackPtr;
            MemoryManager.Access.writeLispPTR(memory, vm.stackPtr, S_POSITIVE | 10);
            MemoryManager.Access.writeLispPTR(memory, vm.stackPtr + 4, S_POSITIVE | 20);
            vm.topOfStack = S_POSITIVE | 20;
            runOpcode(vm, Opcode.PLUS2);

            expect(vm.topOfStack & 0xFFFF).toBe(30);
        });

        test('DIFFERENCE subtracts two numbers', () => {
            vm.pc = 0x1000;
            vm.stackPtr = vm.stackBase - 8;
            vm.cstkptrl = vm.stackPtr;
            MemoryManager.Access.writeLispPTR(memory, vm.stackPtr, S_POSITIVE | 20);
            MemoryManager.Access.writeLispPTR(memory, vm.stackPtr + 4, S_POSITIVE | 10);
            vm.topOfStack = S_POSITIVE | 10;
            runOpcode(vm, Opcode.DIFFERENCE);

            expect(vm.topOfStack & 0xFFFF).toBe(10);
        });
    });

    describe('List Operations', () => {
        test('CAR gets first element of cons cell', () => {
            // Create a cons cell with CAR = 0x1234
            const consCellOffset = 0x5000;
            MemoryManager.Access.writeLispPTR(memory, consCellOffset, 0x1234);
            memory[ consCellOffset + 4 ] = 0x80; // CDR_NIL

            const consCellPtr = MemoryManager.Address.byteToLispPtr(consCellOffset);
            vm.pc = 0x1000;
            // Seed stack with consCellPtr on top
            vm.stackPtr = vm.stackBase - 4;
            vm.cstkptrl = vm.stackPtr;
            MemoryManager.Access.writeLispPTR(memory, vm.stackPtr, consCellPtr);
            vm.topOfStack = consCellPtr;
            runOpcode(vm, Opcode.CAR);

            expect(vm.topOfStack).toBe(0x1234);
        });

        test('CDR gets rest of cons cell', () => {
            // Create a cons cell with CDR_NIL
            const consCellOffset = 0x5000;
            MemoryManager.Access.writeLispPTR(memory, consCellOffset, 0x1234);
            memory[ consCellOffset + 4 ] = 0x80; // CDR_NIL

            const consCellPtr = MemoryManager.Address.byteToLispPtr(consCellOffset);
            vm.pc = 0x1000;
            // Seed stack with consCellPtr on top
            vm.stackPtr = vm.stackBase - 4;
            vm.cstkptrl = vm.stackPtr;
            MemoryManager.Access.writeLispPTR(memory, vm.stackPtr, consCellPtr);
            vm.topOfStack = consCellPtr;
            runOpcode(vm, Opcode.CDR);

            expect(vm.topOfStack).toBe(NIL_PTR);
        });

        test('CONS creates new cons cell', () => {
            vm.pc = 0x1000;
            vm.stackPtr = vm.stackBase - 8;
            vm.cstkptrl = vm.stackPtr;
            MemoryManager.Access.writeLispPTR(memory, vm.stackPtr, NIL_PTR); // CDR
            MemoryManager.Access.writeLispPTR(memory, vm.stackPtr + 4, 0x1234); // CAR
            vm.topOfStack = NIL_PTR;
            runOpcode(vm, Opcode.CONS);

            // Should have allocated a cons cell
            expect(vm.topOfStack).not.toBe(NIL_PTR);
            expect(vm.topOfStack).not.toBe(0x1234);
        });
    });

    describe('Control Flow', () => {
        test('JUMPX jumps to offset', () => {
            vm.pc = 0x1000;
            const jumpOffset = runOpcode(vm, Opcode.JUMPX, [ 0x1234 ]);

            expect(jumpOffset).toBe(0x1234);
        });

        test('FJUMPX jumps if false (NIL)', () => {
            vm.pc = 0x1000;
            vm.topOfStack = NIL_PTR;
            const jumpOffset = runOpcode(vm, Opcode.FJUMPX, [ 0x1234 ]);

            expect(jumpOffset).toBe(0x1234);
        });

        test('FJUMPX does not jump if true (non-NIL)', () => {
            vm.pc = 0x1000;
            vm.topOfStack = ATOM_T;
            const jumpOffset = runOpcode(vm, Opcode.FJUMPX, [ 0x1234 ]);

            expect(jumpOffset).toBeNull();
        });

        test('TJUMPX jumps if true (non-NIL)', () => {
            vm.pc = 0x1000;
            vm.topOfStack = ATOM_T;
            const jumpOffset = runOpcode(vm, Opcode.TJUMPX, [ 0x1234 ]);

            expect(jumpOffset).toBe(0x1234);
        });

        test('TJUMPX does not jump if false (NIL)', () => {
            vm.pc = 0x1000;
            vm.topOfStack = NIL_PTR;
            const jumpOffset = runOpcode(vm, Opcode.TJUMPX, [ 0x1234 ]);

            expect(jumpOffset).toBeNull();
        });
    });

    describe('Memory Operations', () => {
        test('GETBASE_N reads word from base + offset', () => {
            vm.pc = 0x1000;
            const base = 0x5000;
            const offset = 4;
            const value = 0x1234;

            vm.stackPtr = vm.stackBase - 4;
            vm.cstkptrl = vm.stackPtr;
            MemoryManager.Access.writeLispPTR(memory, vm.stackPtr, base);
            vm.topOfStack = base;
            MemoryManager.Access.writeDLword(memory, base + offset, value);

            runOpcode(vm, Opcode.GETBASE_N, [ offset ]);

            expect(vm.topOfStack & 0xFFFF).toBe(value);
            expect((vm.topOfStack & S_POSITIVE) !== 0).toBe(true);
        });

        test('PUTBASE_N writes word to base + offset', () => {
            vm.pc = 0x1000;
            const base = 0x5000;
            const offset = 4;
            const value = S_POSITIVE | 0x5678;

            vm.stackPtr = vm.stackBase - 8;
            vm.cstkptrl = vm.stackPtr;
            MemoryManager.Access.writeLispPTR(memory, vm.stackPtr, value);
            MemoryManager.Access.writeLispPTR(memory, vm.stackPtr + 4, base);
            vm.topOfStack = value;

            runOpcode(vm, Opcode.PUTBASE_N, [ offset ]);

            const written = MemoryManager.Access.readDLword(memory, base + offset);
            expect(written).toBe(0x5678);
            expect(vm.topOfStack).toBe(base);
        });
    });
});
