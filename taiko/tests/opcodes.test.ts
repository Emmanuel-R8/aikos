// Opcode tests
import { describe, test, expect } from 'bun:test';
import { Opcode, getInstructionLength } from '../src/vm/dispatch/opcode';

describe('Opcode', () => {
    test('has correct opcode values', () => {
        expect(Opcode.NIL).toBe(0x68);
        expect(Opcode.T).toBe(0x69);
        expect(Opcode.POP).toBe(0xBF);
    });

    test('getInstructionLength returns correct lengths', () => {
        expect(getInstructionLength(Opcode.NIL)).toBe(1);
        expect(getInstructionLength(Opcode.JUMPX)).toBe(3);
        expect(getInstructionLength(Opcode.JUMPXX)).toBe(5);
    });
});
