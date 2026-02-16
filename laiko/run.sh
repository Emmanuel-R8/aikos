#!/usr/bin/env bash
# Run script for Maiko Lisp implementation

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Check if SBCL is available
if ! command -v sbcl &>/dev/null; then
	echo "Error: SBCL not found. Please install SBCL."
	exit 1
fi

# Use load-emulator.lisp approach to avoid ASDF cache permission issues
# Load all modules and run main
sbcl --noinform \
	--eval "(require :asdf)" \
	--eval "(load \"src/package.lisp\")" \
	--eval "(load \"src/utils/types.lisp\")" \
	--eval "(load \"src/utils/errors.lisp\")" \
	--eval "(load \"src/data/cons.lisp\")" \
	--eval "(load \"src/data/array.lisp\")" \
	--eval "(load \"src/data/function-header.lisp\")" \
	--eval "(load \"src/data/sysout-utils.lisp\")" \
	--eval "(load \"src/data/bytecode.lisp\")" \
	--eval "(load \"src/data/frame-extension.lisp\")" \
	--eval "(load \"src/data/memory-access.lisp\")" \
	--eval "(load \"src/data/atom.lisp\")" \
	--eval "(load \"src/data/defcell.lisp\")" \
	--eval "(load \"src/data/sysout.lisp\")" \
	--eval "(load \"src/memory/storage.lisp\")" \
	--eval "(load \"src/memory/gc.lisp\")" \
	--eval "(load \"src/memory/virtual.lisp\")" \
	--eval "(load \"src/memory/layout.lisp\")" \
	--eval "(load \"src/utils/address.lisp\")" \
	--eval "(load \"src/vm/stack.lisp\")" \
	--eval "(load \"src/vm/interrupt.lisp\")" \
	--eval "(load \"src/vm/trace.lisp\")" \
	--eval "(load \"src/vm/dispatch.lisp\")" \
	--eval "(load \"src/vm/op-stack.lisp\")" \
	--eval "(load \"src/vm/op-arithmetic.lisp\")" \
	--eval "(load \"src/vm/op-list.lisp\")" \
	--eval "(load \"src/vm/op-comparison.lisp\")" \
	--eval "(load \"src/vm/op-variable.lisp\")" \
	--eval "(load \"src/vm/op-control.lisp\")" \
	--eval "(load \"src/vm/op-memory.lisp\")" \
	--eval "(load \"src/vm/op-logic.lisp\")" \
	--eval "(load \"src/vm/op-const.lisp\")" \
	--eval "(load \"src/vm/op-misc.lisp\")" \
	--eval "(load \"src/vm/op-graphics.lisp\")" \
	--eval "(load \"src/vm/opcodes-main.lisp\")" \
	--eval "(load \"src/vm/function.lisp\")" \
	--eval "(load \"src/main.lisp\")" \
	--eval "(maiko-lisp:main)" \
	"$@"
