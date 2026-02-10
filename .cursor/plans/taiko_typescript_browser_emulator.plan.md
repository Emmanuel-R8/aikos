---
name: Taiko TypeScript Browser Emulator
overview: Implement a TypeScript version of the Interlisp emulator (Taiko) that runs in web browsers, using Bun for development/build tooling and WebGL for display. The implementation will target full parity with the C implementation (94.5% opcode coverage) and be structured as a standalone web application.
todos:
  - id: setup-project
    content: Create taiko/ directory structure, package.json, tsconfig.json, bunfig.toml, and initial web files (index.html, app.ts)
    status: completed
  - id: core-types
    content: Implement src/utils/types.ts and src/utils/constants.ts with LispPTR, DLword types and critical constants (IFPAGE_KEYVAL, etc.)
    status: completed
    dependencies:
      - setup-project
  - id: memory-system
    content: Implement centralized memory management (manager.ts, address.ts, fptovp.ts, endianness.ts) matching C/Zig implementations
    status: completed
    dependencies:
      - core-types
  - id: vm-core
    content: Implement VM core (vm.ts) and execution loop (execution.ts) with proper CSTKPTRL/TOPOFSTACK synchronization
    status: completed
    dependencies:
      - memory-system
  - id: sysout-loader
    content: Implement sysout file loading (sysout.ts) with IFPAGE parsing, FPtoVP table loading, and VM initialization
    status: completed
    dependencies:
      - vm-core
  - id: opcode-dispatch
    content: Implement opcode dispatch system (opcode.ts, decoder.ts, opcodes/index.ts) and basic opcode handlers
    status: completed
    dependencies:
      - vm-core
  - id: opcodes-stack-arithmetic
    content: Implement stack and arithmetic opcodes (stack.ts, arithmetic.ts) - priority opcodes for basic execution
    status: completed
    dependencies:
      - opcode-dispatch
  - id: opcodes-control-memory
    content: Implement control flow and memory opcodes (control.ts, memory.ts) - essential for program execution
    status: completed
    dependencies:
      - opcode-dispatch
  - id: webgl-display
    content: Implement WebGL display system (webgl.ts, bitblt.ts) for rendering display region to canvas
    status: completed
    dependencies:
      - opcodes-stack-arithmetic
  - id: browser-events
    content: Implement browser event handling (events.ts) for keyboard/mouse input translation to Lisp events
    status: completed
    dependencies:
      - webgl-display
  - id: trace-system
    content: Implement unified trace format logging (trace.ts) matching C/Zig trace format for parity testing
    status: completed
    dependencies:
      - opcodes-control-memory
  - id: remaining-opcodes
    content: Implement remaining opcodes to reach 94.5% coverage (242/256) - floating point, graphics, I/O operations
    status: completed
    dependencies:
      - opcodes-control-memory
      - trace-system
  - id: testing
    content: Create test suite (vm.test.ts, opcodes.test.ts, memory.test.ts) and trace comparison tools
    status: completed
    dependencies:
      - trace-system
  - id: browser-ui
    content: Enhance browser UI with file input, controls, trace export, and performance metrics display
    status: completed
    dependencies:
      - browser-events
      - sysout-loader
  - id: documentation
    content: Update documentation following critical-memory.typ rules - specifications and TypeScript implementation notes
    status: in_progress
    dependencies:
      - testing
---

# Taiko TypeScript Browser Emulator Implementation Plan

## Overview

Create a new TypeScript implementation of the Interlisp emulator called **Taiko** in the `taiko/` directory. The emulator will run entirely in web browsers, use Bun for development/build tooling, and target full parity with the C implementation (94.5% opcode coverage, 242/256 opcodes).

## Project Structure

```
taiko/
├── src/
│   ├── main.ts                    # Browser entry point, file input handling
│   ├── vm/
│   │   ├── vm.ts                  # Core VM state and initialization
│   │   ├── execution.ts           # Main dispatch loop
│   │   ├── memory/
│   │   │   ├── manager.ts         # Centralized memory management
│   │   │   ├── address.ts         # Address translation (LispPTR ↔ bytes)
│   │   │   ├── fptovp.ts          # File page to virtual page mapping
│   │   │   └── endianness.ts      # Byte-swapping logic
│   │   ├── opcodes/
│   │   │   ├── index.ts           # Opcode registry
│   │   │   ├── stack.ts           # Stack operations
│   │   │   ├── arithmetic.ts      # Arithmetic operations
│   │   │   ├── control.ts         # Control flow
│   │   │   ├── memory.ts          # Memory operations
│   │   │   ├── floating.ts        # Floating point (stub initially)
│   │   │   └── graphics.ts         # Graphics operations
│   │   ├── dispatch/
│   │   │   ├── opcode.ts          # Opcode definitions and types
│   │   │   └── decoder.ts          # Instruction decoding
│   │   └── trace.ts               # Execution tracing (unified format)
│   ├── display/
│   │   ├── webgl.ts               # WebGL renderer
│   │   ├── bitblt.ts              # BitBLT operations
│   │   └── events.ts              # Browser event handling (keyboard, mouse)
│   ├── io/
│   │   ├── sysout.ts              # Sysout file loading/parsing
│   │   └── file.ts                # File system operations (browser limitations)
│   └── utils/
│       ├── types.ts               # TypeScript type definitions
│       └── constants.ts           # Critical constants (IFPAGE_KEYVAL, etc.)
├── tests/
│   ├── vm.test.ts                 # VM core tests
│   ├── opcodes.test.ts            # Opcode tests
│   └── memory.test.ts             # Memory management tests
├── web/
│   ├── index.html                 # Main HTML page
│   ├── styles.css                 # UI styling
│   └── app.ts                     # Browser app initialization
├── bun.lockb                      # Bun lockfile
├── package.json                   # Dependencies and scripts
├── tsconfig.json                  # TypeScript configuration
├── bunfig.toml                    # Bun configuration
└── README.md                      # Taiko documentation
```

## Implementation Phases

### Phase 1: Project Setup and Core Infrastructure

**Files to create:**

- `taiko/package.json` - Bun project configuration, dependencies
- `taiko/tsconfig.json` - TypeScript config targeting browser ES2020+
- `taiko/bunfig.toml` - Bun build configuration
- `taiko/src/utils/types.ts` - Core type definitions (LispPTR, DLword, etc.)
- `taiko/src/utils/constants.ts` - Critical constants from C/Zig implementations
- `taiko/web/index.html` - Basic HTML structure with file input
- `taiko/web/app.ts` - Browser app entry point

**Key decisions:**

- Use Bun's native TypeScript support (no transpilation step needed)
- Target ES2020+ for browser compatibility
- Use WebGL 2.0 for graphics rendering
- Implement file loading via File API (drag-drop or file input)

### Phase 2: Memory Management System

**Files to create:**

- `taiko/src/vm/memory/manager.ts` - Centralized memory manager (mirror `zaiko/src/memory/manager.zig`)
- `taiko/src/vm/memory/address.ts` - LispPTR ↔ byte conversions
- `taiko/src/vm/memory/fptovp.ts` - FPtoVP table management (512-byte pages)
- `taiko/src/vm/memory/endianness.ts` - Byte-swapping for little-endian hosts

**Critical requirements:**

- Match C implementation's memory layout exactly
- Support 512-byte virtual pages (not DLword pages)
- Handle byte-swapping for sysout files loaded on little-endian systems
- Implement bounds checking for all memory accesses
- Use TypedArray (Uint16Array for DLwords, Uint8Array for bytes) for performance

### Phase 3: VM Core and Execution Loop

**Files to create:**

- `taiko/src/vm/vm.ts` - VM state structure and initialization
- `taiko/src/vm/dispatch/opcode.ts` - Opcode enum and type definitions
- `taiko/src/vm/dispatch/decoder.ts` - Instruction decoding
- `taiko/src/vm/execution.ts` - Main dispatch loop with CSTKPTRL/TOPOFSTACK sync

**Critical gotchas to handle:**

- PC is in bytes, not DLwords (when indexing virtual_memory)
- CSTKPTRL/TOPOFSTACK must be synced from memory after restore (not cached)
- Stack grows DOWN
- Frame size is 10 DLwords (20 bytes)

### Phase 4: Sysout Loading and Initialization

**Files to create:**

- `taiko/src/io/sysout.ts` - Sysout file parser
  - Read IFPAGE at offset 512 bytes
  - Validate IFPAGE_KEYVAL (0x15e3)
  - Load FPtoVP table
  - Initialize virtual memory from file pages
  - Extract SP/FP from IFPAGE for VM initialization

**Browser-specific considerations:**

- Use FileReader API or fetch() for loading sysout files
- Handle ArrayBuffer conversion
- Support drag-and-drop file loading in UI

### Phase 5: Opcode Implementation (Priority Order)

**Files to create:**

- `taiko/src/vm/opcodes/index.ts` - Opcode registry and dispatch mapping
- `taiko/src/vm/opcodes/stack.ts` - Stack operations (PUSH, POP, etc.)
- `taiko/src/vm/opcodes/arithmetic.ts` - Arithmetic (ADD, SUB, MUL, etc.)
- `taiko/src/vm/opcodes/control.ts` - Control flow (CALL, RETURN, BRANCH, etc.)
- `taiko/src/vm/opcodes/memory.ts` - Memory operations (GET, PUT, etc.)
- `taiko/src/vm/opcodes/floating.ts` - Floating point (stub initially, implement later)
- `taiko/src/vm/opcodes/graphics.ts` - Graphics operations

**Implementation strategy:**

- Cross-reference C implementation (`maiko/src/`) for each opcode
- Use Zig implementation (`zaiko/src/vm/opcodes/`) as secondary reference
- Implement opcodes in batches, testing after each batch
- Maintain unified trace format for parity testing

### Phase 6: Display System (WebGL)

**Files to create:**

- `taiko/src/display/webgl.ts` - WebGL 2.0 renderer
  - Initialize WebGL context
  - Create shaders for bit-to-pixel conversion
  - Render display region (DLword array) to canvas
  - Handle foreground/background colors
- `taiko/src/display/bitblt.ts` - BitBLT operations
  - Convert bit array to pixel data
  - Handle coordinate translation (window → display)
- `taiko/src/display/events.ts` - Browser event handling
  - Keyboard events → Lisp keycodes (74-entry keymap)
  - Mouse events → display coordinates
  - Window resize handling

**WebGL considerations:**

- Use texture-based rendering for performance
- Implement pixel-perfect scaling
- Support fullscreen mode
- Handle WebGL context loss/recovery

### Phase 7: Execution Tracing and Debugging

**Files to create:**

- `taiko/src/vm/trace.ts` - Unified trace format logger
  - Match format: `LINE#|PC|INSTRUCTION|OPCODE|OPERANDS|REGISTERS|FLAGS|SP_FP|STACK_SUMMARY|MEMORY_CONTEXT|FP_VP_FO_VA|BS_MEM|NOTES`
  - Support `EMULATOR_MAX_STEPS` environment variable
  - Export traces for comparison with C/Zig

**Integration:**

- Add trace logging to dispatch loop
- Support trace export/download in browser UI
- Enable/disable tracing via UI toggle

### Phase 8: Testing and Parity Validation

**Files to create:**

- `taiko/tests/vm.test.ts` - VM initialization and basic execution
- `taiko/tests/opcodes.test.ts` - Individual opcode tests
- `taiko/tests/memory.test.ts` - Memory management tests
- `taiko/scripts/compare_traces.ts` - Trace comparison tool (Bun script)

**Testing strategy:**

- Use Bun's built-in test runner
- Create test fixtures from known-good C traces
- Implement parity comparison against C emulator traces
- Test with `starter.sysout` and other loadups

### Phase 9: Browser UI and Integration

**Files to create:**

- `taiko/web/styles.css` - Modern, responsive UI styling
- Enhance `taiko/web/index.html` with:
  - File input/drag-drop area
  - Canvas for display
  - Control panel (start/stop/reset)
  - Trace export button
  - Performance metrics display

**Features:**

- Drag-and-drop sysout file loading
- Real-time execution controls
- Display canvas with proper scaling
- Error message display
- Loading progress indicator

## Technical Decisions

### Type System

- Use TypeScript strict mode
- Define types matching C/Zig structures:
  - `type LispPTR = number` (32-bit, but JS numbers are safe for 32-bit ints)
  - `type DLword = number` (16-bit, use Uint16Array)
  - `type Byte = number` (8-bit, use Uint8Array)

### Memory Representation

- Use `ArrayBuffer` and `TypedArray` views for memory:
  - `Uint16Array` for DLword-addressable memory
  - `Uint8Array` for byte-addressable memory
  - Single ArrayBuffer backing both views for consistency

### Performance Optimizations

- Use Bun's native performance (faster than Node.js)
- Minimize allocations in hot paths (dispatch loop)
- Cache frequently accessed values (with proper sync points)
- Use WebGL for efficient graphics rendering
- Consider Web Workers for heavy computation (future enhancement)

### Browser Limitations

- No direct file system access (use File API)
- No native threading (use Web Workers if needed)
- Memory limits (handle large sysout files gracefully)
- WebGL context loss handling

## Dependencies

**Runtime (browser):**

- None (vanilla TypeScript/Web APIs)

**Development:**

- `bun` - Runtime and build tool
- `@types/web` - Web API type definitions (optional, Bun includes many)

**Testing:**

- Bun's built-in test framework

## Build Configuration

**Bun build:**

- Bundle TypeScript directly (no transpilation step)
- Output ES modules for browser
- Minify for production builds
- Source maps for debugging

**Development workflow:**

- `bun run dev` - Start dev server with hot reload
- `bun test` - Run test suite
- `bun run build` - Build for production
- `bun run serve` - Serve built files locally

## Documentation Updates

Following `documentation/core/critical-memory.typ`:

- Update `documentation/specifications/` with emulator-independent findings
- Create `documentation/implementations/typescript-implementation.typ` for TypeScript-specific details
- Document browser-specific adaptations
- Record performance characteristics vs C/Zig implementations

## Success Criteria

1. ✅ Loads and executes `starter.sysout` successfully
2. ✅ Displays graphics output via WebGL
3. ✅ Handles keyboard/mouse input
4. ✅ Achieves execution trace parity with C emulator (for implemented opcodes)
5. ✅ All 242 opcodes from C implementation ported and tested
6. ✅ Runs smoothly in modern browsers (Chrome, Firefox, Safari, Edge)
7. ✅ Performance is acceptable for interactive use (target: <100ms frame time)

## Future Enhancements (Out of Scope)

- Web Workers for parallel execution
- Save/restore state functionality
- Debugger UI with step-through
- Multiple sysout file support
- Network file loading
- Service Worker for offline support
