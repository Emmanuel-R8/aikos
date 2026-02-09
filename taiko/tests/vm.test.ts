// VM core tests
import { describe, test, expect, beforeEach } from 'bun:test';
import { VM } from '../src/vm/vm';
import { MemoryManager } from '../src/vm/memory/manager';
import { STK_OFFSET, FRAMESIZE_BYTES, NIL_PTR } from '../src/utils/constants';

describe('VM', () => {
    let vm: VM;

    beforeEach(() => {
        vm = new VM();
    });

    test('initializes with default stack size', () => {
        expect(vm.pc).toBe(0);
        expect(vm.topOfStack).toBe(0);
        expect(vm.running).toBe(false);
        expect(vm.stackBase).toBe(STK_OFFSET * 2);
        expect(vm.stackPtr).toBe(STK_OFFSET * 2);
    });

    test('initializes memory with all offsets', () => {
        const virtualMemory = new Uint8Array(1024);
        const fptovpTable = new Uint32Array(10);
        const atomSpaceOffset = 0x1000;
        const defSpaceOffset = 0x2000;
        const valSpaceOffset = 0x3000;
        const plistSpaceOffset = 0x4000;
        const dtdOffset = 0x5000;

        vm.initializeMemory(
            virtualMemory,
            fptovpTable,
            atomSpaceOffset,
            defSpaceOffset,
            valSpaceOffset,
            plistSpaceOffset,
            dtdOffset
        );

        expect(vm.virtualMemory).toBe(virtualMemory);
        expect(vm.fptovpTable).toBe(fptovpTable);
        expect(vm.atomSpaceOffset).toBe(atomSpaceOffset);
        expect(vm.defSpaceOffset).toBe(defSpaceOffset);
        expect(vm.valSpaceOffset).toBe(valSpaceOffset);
        expect(vm.plistSpaceOffset).toBe(plistSpaceOffset);
        expect(vm.dtdOffset).toBe(dtdOffset);
    });

    test('resets state correctly', () => {
        vm.pc = 100;
        vm.topOfStack = 42;
        vm.running = true;
        vm.funcObj = 0x1000;
        vm.pvar = 0x2000;
        vm.ivar = 0x3000;
        vm.returnPc = 50;
        vm.cstkptrl = 0x4000;
        vm.totalInstructionCount = 1000;

        vm.reset();

        expect(vm.pc).toBe(0);
        expect(vm.topOfStack).toBe(0);
        expect(vm.running).toBe(false);
        expect(vm.funcObj).toBeNull();
        expect(vm.pvar).toBeNull();
        expect(vm.ivar).toBeNull();
        expect(vm.returnPc).toBeNull();
        expect(vm.cstkptrl).toBeNull();
        expect(vm.totalInstructionCount).toBe(0);
    });

    test('syncs top of stack from memory', () => {
        const virtualMemory = new Uint8Array(1024);
        const fptovpTable = new Uint32Array(10);
        vm.initializeMemory(virtualMemory, fptovpTable, 0, 0, 0, 0, 0);

        vm.stackPtr = 0x100;
        vm.cstkptrl = 0x104; // CSTKPTRL points to next slot
        const testValue = 0x12345678;
        MemoryManager.Access.writeLispPTR(virtualMemory, 0x100, testValue);

        vm.syncTopOfStack();

        expect(vm.topOfStack).toBe(testValue);
    });

    test('initializes display region', () => {
        const virtualMemory = new Uint8Array(1024 * 1024); // 1MB for display
        const fptovpTable = new Uint32Array(10);
        vm.initializeMemory(virtualMemory, fptovpTable, 0, 0, 0, 0, 0);

        const displayAddr = 0x2000; // DLword offset
        const width = 1024;
        const height = 768;

        vm.initializeDisplay(displayAddr, width, height);

        expect(vm.displayRegion).not.toBeNull();
        expect(vm.displayWidth).toBe(width);
        expect(vm.displayHeight).toBe(height);
        expect(vm.displayRegionOffset).toBe(MemoryManager.Address.lispPtrToByte(displayAddr));
    });
});
