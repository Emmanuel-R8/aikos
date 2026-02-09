# Taiko - TypeScript Browser Emulator

TypeScript implementation of the Interlisp emulator that runs entirely in web browsers.

## Overview

Taiko is a browser-based emulator for the Interlisp system, targeting full parity with the C implementation (94.5% opcode coverage, 242/256 opcodes). It uses WebGL for graphics rendering and Bun for development/build tooling.

## Features

- Runs entirely in the browser (no server required)
- WebGL-based graphics rendering
- Drag-and-drop sysout file loading
- Execution trace export for parity testing
- Full opcode coverage matching C implementation

## Development

```bash
# Install dependencies (Bun handles this automatically)
bun install

# Run development server
bun run dev

# Run tests
bun test

# Build for production
bun run build

# Serve built files
bun run serve
```

## Usage

1. Open `web/index.html` in a browser or run `bun run dev`
2. Load a sysout file (e.g., `medley/internal/loadups/starter.sysout`)
3. The emulator will initialize and begin execution
4. Use the UI controls to start/stop/reset execution

## Architecture

See `documentation/implementations/typescript-implementation.typ` for detailed architecture documentation.

## License

Same as the Interlisp project.
