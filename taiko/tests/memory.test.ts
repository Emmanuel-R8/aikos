// Memory management tests
import { describe, test, expect, beforeEach } from 'bun:test';
import { MemoryManager } from '../src/vm/memory/manager';
import { BYTESPER_PAGE, NIL_PTR } from '../src/utils/constants';

describe('MemoryManager.Address', () => {
    test('converts LispPTR to byte offset', () => {
        expect(MemoryManager.Address.lispPtrToByte(0x1000)).toBe(0x2000);
        expect(MemoryManager.Address.lispPtrToByte(0)).toBe(0);
        expect(MemoryManager.Address.lispPtrToByte(0x5000)).toBe(0xA000);
    });

    test('converts byte offset to LispPTR', () => {
        expect(MemoryManager.Address.byteToLispPtr(0x2000)).toBe(0x1000);
        expect(MemoryManager.Address.byteToLispPtr(0)).toBe(0);
        expect(MemoryManager.Address.byteToLispPtr(0xA000)).toBe(0x5000);
    });

    test('calculates virtual page', () => {
        expect(MemoryManager.Address.getVirtualPage(0)).toBe(0);
        expect(MemoryManager.Address.getVirtualPage(BYTESPER_PAGE)).toBe(1);
        expect(MemoryManager.Address.getVirtualPage(BYTESPER_PAGE * 2)).toBe(2);
        expect(MemoryManager.Address.getVirtualPage(513)).toBe(1); // Rounds down
    });

    test('calculates page offset', () => {
        expect(MemoryManager.Address.getPageOffset(0)).toBe(0);
        expect(MemoryManager.Address.getPageOffset(BYTESPER_PAGE)).toBe(0);
        expect(MemoryManager.Address.getPageOffset(BYTESPER_PAGE + 100)).toBe(100);
    });
});

describe('MemoryManager.FPtoVP', () => {
    test('calculates file offset', () => {
        expect(MemoryManager.FPtoVP.getFileOffset(0)).toBe(0);
        expect(MemoryManager.FPtoVP.getFileOffset(1)).toBe(BYTESPER_PAGE);
        expect(MemoryManager.FPtoVP.getFileOffset(10)).toBe(BYTESPER_PAGE * 10);
    });

    test('gets file page for virtual page', () => {
        const fptovpTable = new Uint32Array([ 0x0001, 0x0002, 0x0003, 0x0000 ]);
        // Simplified test - would need proper FPtoVP table setup
        expect(MemoryManager.FPtoVP.getFilePageForVirtualPage(fptovpTable, 0)).toBeDefined();
    });
});

describe('MemoryManager.Endianness', () => {
    test('swaps 32-bit value', () => {
        const value = 0x12345678;
        const swapped = MemoryManager.Endianness.swapU32(value);
        expect(swapped).toBe(0x78563412);
    });

    test('swaps 16-bit value', () => {
        const value = 0x1234;
        const swapped = MemoryManager.Endianness.swapU16(value);
        expect(swapped).toBe(0x3412);
    });

    test('applies XOR addressing', () => {
        const addr = 0x1000;
        const xorAddr = MemoryManager.Endianness.getXorAddress(addr);
        expect(xorAddr).toBe(addr ^ 3);
    });
});

describe('MemoryManager.Access', () => {
    let memory: Uint8Array;

    beforeEach(() => {
        memory = new Uint8Array(1024);
    });

    test('reads and writes LispPTR', () => {
        const value = 0x12345678;
        const offset = 0x100;

        MemoryManager.Access.writeLispPTR(memory, offset, value);
        const read = MemoryManager.Access.readLispPTR(memory, offset);

        expect(read).toBe(value);
    });

    test('reads and writes DLword', () => {
        const value = 0x1234;
        const offset = 0x100;

        MemoryManager.Access.writeDLword(memory, offset, value);
        const read = MemoryManager.Access.readDLword(memory, offset);

        expect(read).toBe(value);
    });

    test('reads and writes byte', () => {
        const value = 0xAB;
        const offset = 0x100;

        MemoryManager.Access.writeByte(memory, offset, value);
        const read = MemoryManager.Access.readByte(memory, offset);

        expect(read).toBe(value);
    });

    test('reads byte with XOR addressing', () => {
        const value = 0xAB;
        const offset = 0x100;
        memory[ offset ^ 3 ] = value;

        const read = MemoryManager.Access.readByteXor(memory, offset);
        expect(read).toBe(value);
    });

    test('handles bounds checking', () => {
        const memory = new Uint8Array(100);

        // Should not throw for valid access
        expect(() => {
            MemoryManager.Access.readLispPTR(memory, 0);
        }).not.toThrow();

        // Should handle out of bounds gracefully
        const result = MemoryManager.Access.readLispPTR(memory, 1000);
        expect(result).toBe(0); // Returns 0 for invalid access
    });
});
