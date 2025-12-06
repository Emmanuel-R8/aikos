# Data Model: Maiko Emulator in Common Lisp

**Feature**: 002-lisp-implementation
**Date**: 2025-12-04
**Status**: Complete

## Overview

This document defines the Common Lisp data structures that match the C implementation exactly, ensuring compatibility with sysout files and bytecode execution.

## Core Types

### LispPTR

```lisp
(deftype lisp-ptr () '(unsigned-byte 32))
```

32-bit unsigned integer representing a Lisp virtual address.

### DLword

```lisp
(deftype dlword () '(unsigned-byte 16))
```

16-bit unsigned integer (double-length word).

### Bytecode

```lisp
(deftype bytecode () '(unsigned-byte 8))
```

8-bit bytecode instruction.

## VM Core Structures

### VM State

```lisp
(defstruct (vm (:conc-name vm-))
  "VM state structure"
  (current-frame nil :type (or null stack-frame))
  (stack nil :type (simple-array dlword (*)))
  (stack-ptr 0 :type (integer 0 *))
  (stack-size 0 :type (integer 0 *))
  (storage nil :type (or null storage))
  (virtual-memory nil :type (or null virtual-memory))
  (interrupt-state nil :type (or null interrupt-state))
  (pc 0 :type lisp-ptr)
  (return-pc nil :type (or null lisp-ptr)))
```

### Stack Frame (FX)

```lisp
(defstruct (stack-frame (:conc-name fx-))
  "Stack frame structure (matches C FX)"
  (next-block 0 :type dlword)
  (link 0 :type lisp-ptr)
  (fn-header 0 :type lisp-ptr)
  (pc-offset 0 :type dlword))
```

### Interrupt State (INTSTAT)

```lisp
(defstruct (interrupt-state (:conc-name int-))
  "Interrupt state structure (matches C INTSTAT)"
  (log-file-io nil :type boolean)
  (ether-interrupt nil :type boolean)
  (io-interrupt nil :type boolean)
  (gc-disabled nil :type boolean)
  (vmem-full nil :type boolean)
  (stack-overflow nil :type boolean)
  (storage-full nil :type boolean)
  (waiting-interrupt nil :type boolean)
  (int-char-code 0 :type dlword))
```

## Memory Structures

### Storage

```lisp
(defstruct (storage (:conc-name storage-))
  "Storage structure"
  (heap nil :type (simple-array (unsigned-byte 8) (*)))
  (free-list nil :type list)
  (size 0 :type (integer 0 *)))
```

### Virtual Memory

```lisp
(defstruct (virtual-memory (:conc-name vmem-))
  "Virtual memory structure"
  (pages nil :type (simple-array (simple-array (unsigned-byte 8) (256)) (*)))
  (fptovp-table nil :type (simple-array lisp-ptr (*)))
  (page-count 0 :type (integer 0 *)))
```

### GC Hash Entry

```lisp
(defstruct (gc-hash-entry (:conc-name gche-))
  "GC hash table entry"
  (object-ptr 0 :type lisp-ptr)
  (ref-count 0 :type (integer 0 *))
  (next nil :type (or null gc-hash-entry)))
```

### GC State

```lisp
(defstruct (gc (:conc-name gc-))
  "Garbage collection state"
  (hash-table nil :type hash-table)
  (overflow-table nil :type hash-table)
  (scan-stack nil :type list))
```

## Data Structures

### Cons Cell

```lisp
(defstruct (cons-cell (:conc-name cons-))
  "Cons cell structure (matches C ConsCell)"
  (car-field 0 :type lisp-ptr)
  (cdr-code 0 :type (unsigned-byte 8)))
```

**Memory Layout**:
- Offset 0-3: `car-field` (LispPTR, 32 bits)
- Offset 4-5: Reserved (16 bits)
- Offset 6: `cdr-code` (8 bits)
- Offset 7: Padding (8 bits)
- **Total Size**: 8 bytes (2 DLwords)

**CDR Coding Constants**:
```lisp
(defconstant +cdr-nil+ 128 "CDR is NIL")
(defconstant +cdr-indirect+ 0 "CDR stored indirectly")
(defconstant +cdr-onpage-min+ 128 "Minimum on-page CDR code")
(defconstant +cdr-onpage-max+ 255 "Maximum on-page CDR code")
```

### Array Header (OneDArray)

```lisp
(defstruct (array-header (:conc-name array-))
  "One-dimensional array header"
  (base 0 :type lisp-ptr)
  (offset 0 :type dlword)
  (typenumber 0 :type (unsigned-byte 8))
  (extendablep nil :type boolean)
  (fillpointerp nil :type boolean)
  (displacedp nil :type boolean)
  (ajustablep nil :type boolean)
  (stringp nil :type boolean)
  (bitp nil :type boolean)
  (indirectp nil :type boolean)
  (readonlyp nil :type boolean)
  (totalsize 0 :type (integer 0 *))
  (fillpointer 0 :type (integer 0 *)))
```

### Function Header (FNHEAD)

```lisp
(defstruct (function-header (:conc-name fn-))
  "Function header structure (matches C FNHEAD)"
  (stkmin 0 :type dlword)
  (na 0 :type (signed-byte 16))
  (pv 0 :type (signed-byte 16))
  (startpc 0 :type dlword)
  (byteswapped nil :type boolean)
  (argtype 0 :type (unsigned-byte 2))
  (framename 0 :type (unsigned-byte 24))
  (ntsize 0 :type dlword)
  (nlocals 0 :type (unsigned-byte 8))
  (fvaroffset 0 :type (unsigned-byte 8)))
```

## Sysout File Structures

### IFPAGE (Interface Page)

```lisp
(defstruct (ifpage (:conc-name ifpage-))
  "Interface page structure (matches C IFPAGE)"
  (version 0 :type (unsigned-byte 32))
  (keyval 0 :type (unsigned-byte 32))
  (fptovp-start 0 :type lisp-ptr)
  (nactive-pages 0 :type (unsigned-byte 32))
  (stack-base 0 :type lisp-ptr)
  (end-of-stack 0 :type lisp-ptr)
  (current-fxp 0 :type lisp-ptr)
  (storage-full-state 0 :type (unsigned-byte 32)))
```

**Validation Constant**:
```lisp
(defconstant +sysout-keyval+ #x12345678
  "Expected keyval for sysout file validation")
```

## Display Structures

### Display Interface

```lisp
(defstruct (display-interface (:conc-name display-))
  "Display interface structure"
  (window nil :type (or null t))
  (renderer nil :type (or null t))
  (width 0 :type (integer 1 *))
  (height 0 :type (integer 1 *))
  (buffer nil :type (simple-array (unsigned-byte 8) (*))))
```

## I/O Structures

### Keyboard Event

```lisp
(defstruct (keyboard-event (:conc-name kbd-))
  "Keyboard event structure"
  (keycode 0 :type (unsigned-byte 32))
  (modifiers 0 :type (unsigned-byte 16))
  (pressed-p nil :type boolean))
```

### Mouse Event

```lisp
(defstruct (mouse-event (:conc-name mouse-))
  "Mouse event structure"
  (x 0 :type (integer 0 *))
  (y 0 :type (integer 0 *))
  (buttons 0 :type (unsigned-byte 8))
  (pressed-p nil :type boolean))
```

## Memory Region Offsets

```lisp
(defconstant +ifpage-offset+ 0
  "Interface page offset")
(defconstant +stack-offset+ #x10000
  "Stack space offset")
(defconstant +atom-ht-offset+ #x20000
  "Atom hash table offset")
(defconstant +atoms-offset+ #x30000
  "Atom space offset")
(defconstant +plist-offset+ #x40000
  "Property list space offset")
(defconstant +mds-offset+ #x50000
  "MDS (heap) space offset")
(defconstant +display-offset+ #x60000
  "Display region offset")
```

## Type Predicates

```lisp
(defun is-small-integer (ptr)
  "Check if LispPTR is a small integer"
  (declare (type lisp-ptr ptr))
  (= (logand ptr #x80000000) #x80000000))

(defun is-fixnum (ptr)
  "Check if LispPTR is a fixnum"
  (declare (type lisp-ptr ptr))
  (= (logand ptr #x80000001) #x80000001))

(defun is-list (ptr)
  "Check if LispPTR is a list (cons cell)"
  (declare (type lisp-ptr ptr))
  (= (logand ptr #x80000003) #x80000003))
```

## Address Translation

```lisp
(defun hiloc (ptr)
  "Get high 16 bits of LispPTR"
  (declare (type lisp-ptr ptr))
  (ash ptr -16))

(defun loloc (ptr)
  "Get low 16 bits of LispPTR"
  (declare (type lisp-ptr ptr))
  (logand ptr #xFFFF))
```

## Validation Rules

### Cons Cell Validation

- `car-field` must be valid LispPTR
- `cdr-code` must be in range 0-255
- CDR coding must be valid (NIL, INDIRECT, or on-page offset)

### Array Header Validation

- `base` must be valid LispPTR
- `totalsize` must be positive
- `fillpointer` must be <= `totalsize`
- Type flags must be consistent

### Function Header Validation

- `stkmin` must be positive
- `na` can be negative (spread function)
- `startpc` must be valid code offset
- `nlocals` must be non-negative

### Sysout File Validation

- `keyval` must match `+sysout-keyval+`
- `version` must be compatible
- `fptovp-start` must be valid file offset
- `nactive-pages` must be reasonable

## State Transitions

### VM State

- **Initialized**: VM created, stack allocated
- **Running**: Dispatch loop active, executing bytecode
- **Interrupted**: Interrupt pending, handling interrupt
- **Halted**: VM stopped, cleanup required

### GC State

- **Idle**: No GC in progress
- **Scanning**: Scanning hash table for references
- **Reclaiming**: Reclaiming unreferenced objects
- **Coordination**: Coordinating with Common Lisp GC

## Relationships

- **VM** → **Stack Frame**: One current frame, linked list of frames
- **VM** → **Storage**: One storage instance
- **VM** → **Virtual Memory**: One virtual memory instance
- **VM** → **Interrupt State**: One interrupt state
- **Storage** → **Cons Cell**: Many cons cells allocated
- **GC** → **GC Hash Entry**: Hash table of object references
- **Display Interface** → **Display Buffer**: One buffer for rendering
