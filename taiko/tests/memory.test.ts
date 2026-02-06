// Memory management tests
import { describe, test, expect } from 'bun:test';
import { AddressManager } from '../src/vm/memory/address';
import { FPtoVPManager } from '../src/vm/memory/fptovp';
import { EndiannessManager } from '../src/vm/memory/endianness';

describe('AddressManager', () => {
    test('converts LispPTR to byte offset', () => {
        expect(AddressManager.lispPtrToByte(0x1000)).toBe(0x2000);
        expect(AddressManager.lispPtrToByte(0)).toBe(0);
    });

    test('converts byte offset to LispPTR', () => {
        expect(AddressManager.byteToLispPtr(0x2000)).toBe(0x1000);
        expect(AddressManager.byteToLispPtr(0)).toBe(0);
    });

    test('calculates virtual page', () => {
        expect(AddressManager.getVirtualPage(512)).toBe(1);
        expect(AddressManager.getVirtualPage(1024)).toBe(2);
    });
});

describe('FPtoVPManager', () => {
    test('calculates file offset', () => {
        expect(FPtoVPManager.getFileOffset(0)).toBe(0);
        expect(FPtoVPManager.getFileOffset(1)).toBe(512);
        expect(FPtoVPManager.getFileOffset(10)).toBe(5120);
    });
});

describe('EndiannessManager', () => {
    test('swaps 32-bit value', () => {
        const value = 0x12345678;
        const swapped = EndiannessManager.swapU32(value);
        expect(swapped).toBe(0x78563412);
    });

    test('swaps 16-bit value', () => {
        const value = 0x1234;
        const swapped = EndiannessManager.swapU16(value);
        expect(swapped).toBe(0x3412);
    });
});
