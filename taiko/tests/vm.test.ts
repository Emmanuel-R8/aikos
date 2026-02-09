// VM core tests
import { describe, test, expect } from 'bun:test';
import { VM } from '../src/vm/vm';

describe('VM', () => {
    test('initializes with default stack size', () => {
        const vm = new VM();
        expect(vm.pc).toBe(0);
        expect(vm.topOfStack).toBe(0);
        expect(vm.running).toBe(false);
    });

    test('initializes memory', () => {
        const vm = new VM();
        const virtualMemory = new Uint8Array(1024);
        const fptovpTable = new Uint32Array(10);
        vm.initializeMemory(virtualMemory, fptovpTable);
        expect(vm.virtualMemory).toBe(virtualMemory);
        expect(vm.fptovpTable).toBe(fptovpTable);
    });

    test('resets state', () => {
        const vm = new VM();
        vm.pc = 100;
        vm.topOfStack = 42;
        vm.running = true;
        vm.reset();
        expect(vm.pc).toBe(0);
        expect(vm.topOfStack).toBe(0);
        expect(vm.running).toBe(false);
    });
});
