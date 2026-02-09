// Frame variable access tests
import { describe, test, expect, beforeEach } from 'bun:test';
import { VM } from '../src/vm/vm';
import { FrameManager } from '../src/vm/memory/frame';
import { MemoryManager } from '../src/vm/memory/manager';
import { FRAMESIZE_BYTES, NIL_PTR, S_POSITIVE } from '../src/utils/constants';

describe('Frame Variable Access', () => {
    let vm: VM;
    let memory: Uint8Array;

    beforeEach(() => {
        vm = new VM(64 * 1024);
        memory = new Uint8Array(1024 * 1024);
        const fptovpTable = new Uint32Array(100);

        vm.initializeMemory(
            memory,
            fptovpTable,
            0x10000, 0x20000, 0x30000, 0x40000, 0x50000
        );

        // Set up a test frame
        vm.stackPtr = vm.stackBase - FRAMESIZE_BYTES - 40; // Frame + some PVARs
        vm.pvar = vm.stackPtr;
        vm.cstkptrl = vm.stackPtr;
    });

    describe('PVAR Access', () => {
        test('reads PVAR0', () => {
            const testValue = 0x12345678;
            MemoryManager.Access.writeLispPTR(memory, vm.pvar! + 0, testValue);

            const value = FrameManager.readPVAR(vm, 0);
            expect(value).toBe(testValue);
        });

        test('reads PVAR1', () => {
            const testValue = 0xABCDEF00 >>> 0; // Force unsigned
            MemoryManager.Access.writeLispPTR(memory, vm.pvar! + 4, testValue);

            const value = FrameManager.readPVAR(vm, 1);
            expect(value >>> 0).toBe(testValue);
        });

        test('writes PVAR0', () => {
            const testValue = 0x12345678;
            FrameManager.writePVAR(vm, 0, testValue);

            const read = MemoryManager.Access.readLispPTR(memory, vm.pvar! + 0);
            expect(read).toBe(testValue);
        });
    });

    describe('IVAR Access', () => {
        beforeEach(() => {
            vm.ivar = vm.stackBase - 100; // Set IVAR pointer
        });

        test('reads IVAR0', () => {
            const testValue = 0x12345678;
            MemoryManager.Access.writeLispPTR(memory, vm.ivar! + 0, testValue);

            const value = FrameManager.readIVAR(vm, 0);
            expect(value).toBe(testValue);
        });

        test('writes IVAR0', () => {
            const testValue = 0xABCDEF00 >>> 0; // Force unsigned
            FrameManager.writeIVAR(vm, 0, testValue);

            const read = MemoryManager.Access.readLispPTR(memory, vm.ivar! + 0);
            expect(read >>> 0).toBe(testValue);
        });
    });

    describe('FVAR Access', () => {
        test('reads bound FVAR (simplified encoding)', () => {
            const fvarIndex = 0; // Word offset
            const targetValue = 0x12345678;
            const targetOffset = 0x6000;
            const targetLispPtr = MemoryManager.Address.byteToLispPtr(targetOffset);

            // Simplified FVAR slot format: store LispPTR directly in two DLwords
            const fvarSlotOffset = vm.pvar! + (fvarIndex * 2);
            MemoryManager.Access.writeDLword(memory, fvarSlotOffset, targetLispPtr & 0xFFFF);
            MemoryManager.Access.writeDLword(memory, fvarSlotOffset + 2, (targetLispPtr >> 16) & 0xFFFF);

            // Write value at target
            MemoryManager.Access.writeLispPTR(memory, targetOffset, targetValue);

            const value = FrameManager.readFVAR(vm, fvarIndex);
            expect(value).toBe(targetValue);
        });

        test('detects unbound FVAR', () => {
            const fvarIndex = 0;
            const fvarSlotOffset = vm.pvar! + (fvarIndex * 2);

            // Mark as unbound (LSB set)
            MemoryManager.Access.writeDLword(memory, fvarSlotOffset, 0x8000);

            const value = FrameManager.readFVAR(vm, fvarIndex);
            expect(value).toBe(0); // Returns NIL for unbound (simplified)
        });
    });

    describe('Frame Information', () => {
        beforeEach(() => {
            // Set up frame structure
            const fxOffset = vm.pvar! - FRAMESIZE_BYTES;
            MemoryManager.Access.writeDLword(memory, fxOffset + 2, 0x1234); // alink
            MemoryManager.Access.writeDLword(memory, fxOffset + 16, 0x5678); // blink
        });

        test('gets current FX offset', () => {
            const fxOffset = FrameManager.getCurrentFX(vm);
            expect(fxOffset).toBe(vm.pvar! - FRAMESIZE_BYTES);
        });

        test('reads alink from frame', () => {
            const alink = FrameManager.readALink(vm);
            expect(alink).toBe(0x1234);
        });

        test('reads blink from frame', () => {
            const blink = FrameManager.readBLink(vm);
            expect(blink).toBe(0x5678);
        });

        test('calculates argument count', () => {
            // Set up IVAR to calculate arg count
            vm.ivar = vm.stackBase - 20;
            const argCount = FrameManager.getArgCount(vm);

            // Should return SMALLP
            expect((argCount & S_POSITIVE) !== 0).toBe(true);
        });

        test('gets access link', () => {
            const alink = FrameManager.getALink(vm);

            // Should return SMALLP
            expect((alink & S_POSITIVE) !== 0).toBe(true);
        });
    });
});
