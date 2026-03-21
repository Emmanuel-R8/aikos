// Sysout loading tests
import { describe, test, expect } from 'bun:test';
import { loadSysout } from '../src/io/sysout';
import { decodeInstructionFromMemory } from '../src/vm/dispatch/decoder';
import { Opcode } from '../src/vm/dispatch/opcode';
import { executeStep } from '../src/vm/execution';
import { initializeVM } from '../src/vm/initialization';
import { VM } from '../src/vm/vm';
import { IFPAGE_KEYVAL, IFPAGE_ADDRESS, BYTESPER_PAGE, ATOMS_OFFSET, DEFS_OFFSET, VALS_OFFSET, PLIS_OFFSET, DTD_OFFSET } from '../src/utils/constants';
import { MemoryManager } from '../src/vm/memory/manager';
import { existsSync } from 'fs';
import { resolve } from 'path';

describe('Sysout Loading', () => {
    function writeWordBE(view: DataView, offset: number, value: number): void {
        view.setUint16(offset, value & 0xFFFF, false);
    }

    function writeDWordBE(view: DataView, offset: number, value: number): void {
        view.setUint32(offset, value >>> 0, false);
    }

    function writeMinimalIFPage(buffer: ArrayBuffer, overrides: Record<number, number> = {}): void {
        const view = new DataView(buffer, IFPAGE_ADDRESS);

        writeWordBE(view, 30, IFPAGE_KEYVAL); // key
        writeWordBE(view, 88, 1); // process_size (1 MB)
        writeWordBE(view, 118, 3); // fptovpstart => page 3, after IFPAGE

        for (const [offset, value] of Object.entries(overrides)) {
            writeWordBE(view, Number(offset), value);
        }
    }

    test('creates minimal valid sysout buffer', async () => {
        const buffer = new ArrayBuffer(2048);
        writeMinimalIFPage(buffer);

        await expect(loadSysout(buffer)).resolves.toBeDefined();
    });

    test('validates IFPAGE keyval', async () => {
        const buffer = new ArrayBuffer(2048);
        writeMinimalIFPage(buffer, { 30: 0x1234 });

        await expect(loadSysout(buffer)).rejects.toThrow(/Invalid IFPAGE key/);
    });

    test('loads virtual memory from FPtoVP table', async () => {
        const buffer = new ArrayBuffer(4096);
        writeMinimalIFPage(buffer, { 118: 5 });
        const bufferView = new DataView(buffer);

        // FPtoVP table starts at page 5 offset 4 to avoid overlapping test pages.
        const fptovpOffset = ((5 - 1) * BYTESPER_PAGE) + 4;
        // Encode entries directly in file order: pageOK=0, virtual page=1/2.
        writeDWordBE(bufferView, fptovpOffset + (1 * 4), 0x00000001);
        writeDWordBE(bufferView, fptovpOffset + (2 * 4), 0x00000002);

        // Write big-endian 32-bit words into file pages 1 and 2.
        writeDWordBE(bufferView, (1 * BYTESPER_PAGE) + 8, 0x11223344);
        writeDWordBE(bufferView, (2 * BYTESPER_PAGE) + 12, 0x55667788);

        const result = await loadSysout(buffer);

        expect(Array.from(result.virtualMemory.slice((1 * BYTESPER_PAGE) + 8, (1 * BYTESPER_PAGE) + 12))).toEqual([
            0x44, 0x33, 0x22, 0x11,
        ]);
        expect(Array.from(result.virtualMemory.slice((2 * BYTESPER_PAGE) + 12, (2 * BYTESPER_PAGE) + 16))).toEqual([
            0x88, 0x77, 0x66, 0x55,
        ]);
    });

    test('returns correct memory region offsets', async () => {
        const buffer = new ArrayBuffer(2048);
        writeMinimalIFPage(buffer);

        const result = await loadSysout(buffer);

        expect(result.atomSpaceOffset).toBe(MemoryManager.Address.lispPtrToByte(ATOMS_OFFSET));
        expect(result.defSpaceOffset).toBe(MemoryManager.Address.lispPtrToByte(DEFS_OFFSET));
        expect(result.valSpaceOffset).toBe(MemoryManager.Address.lispPtrToByte(VALS_OFFSET));
        expect(result.plistSpaceOffset).toBe(MemoryManager.Address.lispPtrToByte(PLIS_OFFSET));
        expect(result.dtdOffset).toBe(MemoryManager.Address.lispPtrToByte(DTD_OFFSET));
    });

    test('parses IFPAGE fields in file order', async () => {
        const buffer = new ArrayBuffer(2048);
        writeMinimalIFPage(buffer, {
            0: 0x1234,
            2: 0x5678,
            4: 0x9abc,
            6: 0xdef0,
            40: 0x0102,
            42: 0x0304,
            44: 0x0506,
            46: 0x0708,
            56: 0x1111,
            58: 0x2222,
            60: 0x3333,
            62: 0x4444,
            92: 0x5555,
            94: 0x6666,
            112: 0x7777,
            114: 0x8888,
            116: 0x9999,
            118: 3,
        });

        const result = await loadSysout(buffer);

        expect(result.ifpage.currentfxp).toBe(0x1234);
        expect(result.ifpage.resetfxp).toBe(0x5678);
        expect(result.ifpage.subovfxp).toBe(0x9abc);
        expect(result.ifpage.kbdfxp).toBe(0xdef0);
        expect(result.ifpage.nactivepages).toBe(0x0102);
        expect(result.ifpage.ndirtypages).toBe(0x0304);
        expect(result.ifpage.filepnpmp0).toBe(0x0506);
        expect(result.ifpage.filepnpmt0).toBe(0x0708);
        expect(result.ifpage.usernameaddr).toBe(0x1111);
        expect(result.ifpage.userpswdaddr).toBe(0x2222);
        expect(result.ifpage.stackbase).toBe(0x3333);
        expect(result.ifpage.faulthi).toBe(0x4444);
        expect(result.ifpage.storagefullstate).toBe(0x5555);
        expect(result.ifpage.isfmap).toBe(0x6666);
        expect(result.ifpage.nrealpages).toBe(0x7777);
        expect(result.ifpage.lastlockedfilepage).toBe(0x8888);
        expect(result.ifpage.lastdominofilepage).toBe(0x9999);
        expect(result.ifpage.fptovpstart).toBe(3);
    });

    test('loads real starter.sysout (if present)', async () => {
        // Resolve absolute path to Medley starter.sysout from taiko/ directory
        const sysoutPath = resolve(process.cwd(), '../medley/internal/loadups/starter.sysout');

        if (!existsSync(sysoutPath)) {
            // Allow running tests even when medley submodule or sysout is absent
            console.warn(`starter.sysout not found at ${sysoutPath}, skipping real-sysout integration test.`);
            return;
        }

        const arrayBuffer = await Bun.file(sysoutPath).arrayBuffer();

        const result = await loadSysout(arrayBuffer);

        expect(result.virtualMemory.length).toBeGreaterThan(0);
        expect(result.fptovpTable.length).toBeGreaterThan(0);
        expect(result.initialSP).toBeGreaterThan(0);

        const vm = new VM();
        vm.initializeMemory(
            result.virtualMemory,
            result.fptovpTable,
            result.atomSpaceOffset,
            result.defSpaceOffset,
            result.valSpaceOffset,
            result.plistSpaceOffset,
            result.dtdOffset,
        );

        expect(initializeVM(vm, result.ifpage)).toBe(true);
        expect(vm.pc).toBe(0x60F130);
        expect(vm.funcObj).toBe(0x60F0C8);
        expect(vm.currentFrameOffset).not.toBeNull();

        const instruction = decodeInstructionFromMemory(vm, vm.pc);
        expect(instruction).not.toBeNull();
        expect(instruction?.opcode).toBe(Opcode.POP);

        expect(executeStep(vm)).toBe(true);
        expect(vm.pc).toBe(0x60F131);

        expect(executeStep(vm)).toBe(true);
        expect(vm.pc).toBe(0x60F136);
        expect(decodeInstructionFromMemory(vm, vm.pc)?.opcode).toBe(Opcode.UNBIND);

        expect(executeStep(vm)).toBe(true);
        expect(vm.pc).toBe(0x60F137);
    });
});
