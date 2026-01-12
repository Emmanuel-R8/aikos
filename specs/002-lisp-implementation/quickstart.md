# Quickstart Guide: Maiko Emulator in Common Lisp

**Feature**: 002-lisp-implementation
**Date**: 2025-12-04

## Prerequisites

- **SBCL**: Steel Bank Common Lisp (version 2.0.0 or later)
- **SDL3**: SDL3 development libraries
- **Quicklisp** (optional): For dependency management

### Installing Prerequisites

**Linux (Ubuntu/Debian)**:
```bash
sudo apt-get install sbcl libsdl3-dev
```

**macOS**:
```bash
brew install sbcl sdl3
```

**Quicklisp** (recommended):
```bash
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --quit
```

## Building the Project

### Using ASDF

```bash
cd alternatives/lisp
sbcl --load laiko.asd \
     --eval "(asdf:load-system :laiko)" \
     --quit
```

### Using Build Script

```bash
cd alternatives/lisp
./build.sh
```

## Running the Emulator

### Basic Usage

```bash
cd alternatives/lisp
./run.sh path/to/sysout.file
```

### Command-Line Options

```
Usage: laiko [options] <sysout-file>

Options:
  --stack-size <size>     Stack size in DLwords (default: 32768)
  --memory-size <mb>      Memory size in MB (default: 16)
  --screen-width <width>  Display width (default: 1024)
  --screen-height <height> Display height (default: 768)
  --help                  Show this help message
```

### Example

```bash
./run.sh ../test-sysouts/basic.sysout \
  --stack-size 65536 \
  --memory-size 32 \
  --screen-width 1920 \
  --screen-height 1080
```

## Development Setup

### Loading in REPL

```lisp
(load "laiko.asd")
(asdf:load-system :laiko)

(in-package :laiko.main)

;; Create VM
(defvar *vm* (laiko.vm:make-vm 32768))

;; Load sysout
(defvar *sysout* (laiko.data:load-sysout "path/to/sysout.file"))

;; Run emulator
(laiko.main:run-emulator "path/to/sysout.file" nil)
```

### Running Tests

```bash
cd alternatives/lisp
sbcl --load laiko.asd \
     --eval "(asdf:test-system :laiko)" \
     --quit
```

## Project Structure

```
alternatives/lisp/
├── laiko.asd          # ASDF system definition
├── README.md               # Project documentation
├── build.sh                # Build script
├── run.sh                  # Run script
├── src/
│   ├── package.lisp        # Package definitions
│   ├── main.lisp           # Entry point
│   ├── vm/                 # VM core
│   ├── memory/             # Memory management
│   ├── data/               # Data structures
│   ├── display/            # Display subsystem
│   ├── io/                 # I/O subsystem
│   └── utils/              # Utilities
└── tests/                  # Test suite
```

## Common Tasks

### Loading a Sysout File

```lisp
(laiko.data:load-sysout "path/to/sysout.file")
```

### Creating a VM

```lisp
(laiko.vm:make-vm 32768)  ; 32KB stack
```

### Executing Bytecode

```lisp
(let ((vm (laiko.vm:make-vm 32768))
      (code (make-array 10 :element-type 'laiko.utils:bytecode
                        :initial-contents '(#xD8 #xD9 #xBF ...))))
  (laiko.vm:dispatch vm code))
```

### Allocating Memory

```lisp
(let ((storage (laiko.memory:make-storage (* 16 1024 1024))))
  (laiko.memory:allocate-cons-cell storage))
```

## Troubleshooting

### SDL Initialization Fails

**Error**: `display-error: SDL initialization failed`

**Solution**: Ensure SDL3 libraries are installed and accessible:
```bash
# Linux
sudo apt-get install libsdl3-dev

# macOS
brew install sdl3
```

### Sysout File Load Fails

**Error**: `sysout-load-failed: Invalid sysout file`

**Solution**:
- Verify sysout file is from compatible Maiko version
- Check file permissions and path
- Validate sysout file format

### Stack Overflow

**Error**: `stack-overflow: Stack overflow in function call`

**Solution**: Increase stack size:
```bash
./run.sh sysout.file --stack-size 65536
```

### Memory Allocation Fails

**Error**: `memory-error: Storage full`

**Solution**:
- Increase memory size: `--memory-size 32`
- Check if GC is running correctly
- Verify sysout file isn't corrupted

## Next Steps

- Read [Implementation Plan](plan.md) for architecture details
- Review [Data Model](data-model.md) for data structures
- Check [Interface Contracts](contracts/) for API specifications
- See [Tasks](tasks.md) for implementation roadmap

## Getting Help

- Review rewrite documentation in `documentation/rewrite-spec/`
- Check test cases in `tests/` directory
- Compare with C implementation in `maiko/src/` directory
