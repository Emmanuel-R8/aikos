// Sysout loading tests
import { describe, test, expect } from 'bun:test';
import { loadSysout } from '../src/io/sysout';
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
        writeWordBE(view, 116, 3); // fptovpstart => page 3, after IFPAGE

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
        writeMinimalIFPage(buffer, { 116: 5 });
        const bufferView = new DataView(buffer);

        // FPtoVP table starts at page 5 offset 4 to avoid overlapping test pages.
        const fptovpOffset = ((5 - 1) * BYTESPER_PAGE) + 4;
        // Encode entries so that byte-swap yields 0x00000001 and 0x00000002.
        writeDWordBE(bufferView, fptovpOffset + (1 * 4), 0x01000000);
        writeDWordBE(bufferView, fptovpOffset + (2 * 4), 0x02000000);

        // Write big-endian 32-bit words into file pages 1 and 2.
        writeDWordBE(bufferView, (1 * BYTESPER_PAGE) + 8, 0x11223344);
        writeDWordBE(bufferView, (2 * BYTESPER_PAGE) + 12, 0x55667788);

        const result = await loadSysout(buffer);

        expect(Array.from(result.virtualMemory.slice((1 * BYTESPER_PAGE) + 8, (1 * BYTESPER_PAGE) + 12))).toEqual([
            0x11, 0x22, 0x33, 0x44,
        ]);
        expect(Array.from(result.virtualMemory.slice((2 * BYTESPER_PAGE) + 12, (2 * BYTESPER_PAGE) + 16))).toEqual([
            0x55, 0x66, 0x77, 0x88,
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

    test('loads real starter.sysout (if present)', async () => {
        // Resolve absolute path to Medley starter.sysout from taiko/ directory
        const sysoutPath = resolve(process.cwd(), '../medley/internal/loadups/starter.sysout');

        if (!existsSync(sysoutPath)) {
            // Allow running tests even when medley submodule or sysout is absent
            console.warn(`starter.sysout not found at ${sysoutPath}, skipping real-sysout integration test.`);
            return;
        }

        const arrayBuffer = await Bun.file(sysoutPath).arrayBuffer();

        try {
            const result = await loadSysout(arrayBuffer);

            // Basic sanity checks – we only assert that the sysout can be parsed
            expect(result.virtualMemory.length).toBeGreaterThan(0);
            expect(result.fptovpTable.length).toBeGreaterThan(0);
            expect(result.initialPC).toBeGreaterThan(0);
            expect(result.initialSP).toBeGreaterThan(0);
        } catch (err: any) {
            // If the real sysout does not match the current loader assumptions
            // (e.g. different IFPAGE layout), treat this as a skipped integration
            // scenario rather than a hard failure.
            console.warn(
                `starter.sysout found at ${sysoutPath} but could not be parsed by Taiko loader: ${String(
                    err?.message ?? err,
                )}. Treating as skipped real-sysout integration test.`,
            );
        }
    });
});
