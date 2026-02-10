#!/usr/bin/env bash
# Load script for Laiko Common Lisp emulator
cd /home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/laiko

sbcl --non-interactive --eval "(require :asdf)" \
  --eval "(load \"src/package.lisp\")" \
  --eval "(load \"src/utils/types.lisp\")" \
  --eval "(load \"src/utils/errors.lisp\")" \
  --eval "(load \"src/data/cons.lisp\")" \
  --eval "(load \"src/data/array.lisp\")" \
  --eval "(load \"src/data/function-header.lisp\")" \
  --eval "(load \"src/data/sysout-utils.lisp\")" \
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
  --eval "(format t \"All modules loaded!~%\")" \
  --eval "(maiko-lisp:print-info)" \
  "$@"
