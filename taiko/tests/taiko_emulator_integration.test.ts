// TaikoEmulator integration test with a minimal synthetic sysout
import { describe, test, expect } from 'bun:test';
import { TaikoEmulator } from '../src/main';
import { IFPAGE_KEYVAL, IFPAGE_ADDRESS, BYTESPER_PAGE } from '../src/utils/constants';
import { MemoryManager } from '../src/vm/memory/manager';

// This integration test requires a DOM environment (for HTMLCanvasElement).
// In non-DOM environments (e.g. bun test in Node), we skip it safely.
describe('TaikoEmulator + sysout integration', () => {
  if (typeof document === 'undefined') {
    test.skip('requires DOM (document) to create canvas', () => {});
    return;
  }

  test('loads a minimal synthetic sysout without errors', async () => {
    // Create a small buffer large enough for IFPAGE + FPtoVP + 1 page
    const buffer = new ArrayBuffer(BYTESPER_PAGE * 4);
    const view = new Uint8Array(buffer);

    // Write minimal IFPAGE at standard offset
    const ifpageOffset = IFPAGE_ADDRESS;
    MemoryManager.Access.writeDLword(view, ifpageOffset + 0, IFPAGE_KEYVAL); // keyval
    // process_size = 2 pages (small but non-zero)
    MemoryManager.Access.writeDLword(view, ifpageOffset + 8, 2);

    // Write a trivial FPtoVP table right after IFPAGE header
    const fptovpOffset = ifpageOffset + 16;
    // Map file page 0 -> vpage 1, file page 1 -> vpage 2
    MemoryManager.Access.writeDLword(view, fptovpOffset + 0, 1);
    MemoryManager.Access.writeDLword(view, fptovpOffset + 2, 2);

    // Synthetic display / misc fields can stay at zero

    // Create a dummy canvas for the emulator (offscreen)
    const canvas = Object.assign(document.createElement('canvas'), {
      width: 1024,
      height: 768,
    }) as HTMLCanvasElement;

    const emulator = new TaikoEmulator(canvas, { maxSteps: 10 });

    await expect(async () => {
      await emulator.loadSysout(buffer);
    }).resolves.not.toThrow();
  });
});
