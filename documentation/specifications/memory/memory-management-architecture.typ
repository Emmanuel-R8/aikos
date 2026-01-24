# Memory Management System Architecture
# Maiko C Implementation - Part 3: Memory Management

Date: 2026-01-23 10:30
Author: Systematic Documentation Analysis

## Overview

The Maiko C implementation contains a sophisticated memory management system that implements a mark-and-sweep garbage collector with reference counting, cons cell optimization, and virtual memory management. This analysis documents the complete memory management architecture, including garbage collection algorithms, memory allocation strategies, and integration points.

## Memory Management Architecture

### Core Components

```
Memory Management Subsystem
├── Garbage Collection Engine
│   ├── Marking & Scanning (gcscan.c, gcmain3.c)
│   ├── Object Reclamation (gcfinal.c, gcrcell.c)
│   ├── Reference Management (gcoflow.c, gcdata.h)
│   └── Collection Control (gcr.c, gc.c)
├── Memory Allocation System
│   ├── Cons Cell Management (conspage.c, cell.h)
│   ├── Page Allocation (allocmds.c)
│   ├── Array Block Management (gcfinal.c)
│   └── Type-Specific Allocators
├── Virtual Memory Integration
│   ├── Page Tables (lispmap.h)
│   ├── Address Translation (adr68k.h)
│   └── Storage Management (storagedefs.h)
└── Data Structure Definitions
    ├── Core Types (lsptypes.h)
    ├── Cell Layouts (cell.h)
    └── GC Tables (gcdata.h)
```

## 1. Garbage Collection Engine

### 1.1 Marking and Scanning System

**File: `gcmain3.c` - Main GC Controller**

**Confidence Level: HIGH**

The main garbage collection controller implements the core marking and scanning algorithms:

```c
LispPTR gcmapscan(void) {
    // Phase 1: Mark reachable objects from hash tables
    // Walks HTmain and HTcoll tables to find reclaimable objects
    // Reclaims objects with zero stack reference count
}

LispPTR gcmapunscan(void) {
    // Phase 2: Clean up stack references after marking
    // Removes STKREF bits and prepares for next collection cycle
}

LispPTR gcscanstack(void) {
    // Phase 3: Scan entire execution stack for references
    // Walks all stack frames and marks reachable objects
    // Handles different frame types (FX, FSB, GUARD, basic)
}
```

**Key Algorithms:**

- **Hash Table Scanning**: Efficient linear scan of HTmain entries with collision handling
- **Stack Frame Traversal**: Recursive walk of stack frames with type-specific handling
- **Reference Count Integration**: Links GC with reference counting system for precise reclamation

**Performance Considerations:**
- O(n) complexity where n = number of hash table entries
- Stack scanning proportional to stack depth
- Collision resolution via linked lists in HTcoll table

### 1.2 Hash Table Scanning Functions

**File: `gcscan.c` - Scanning Primitives**

**Confidence Level: HIGH**

Low-level scanning primitives for hash table operations:

```c
int gcscan1(int probe) {
    // Find entries with zero count or collision bits
    // Returns next reclaimable entry or -1 if none
    // Used during initial marking phase
}

int gcscan2(int probe) {
    // Find entries with stack reference bits set
    // Returns next stack-referenced entry or -1 if none
    // Used during stack reference cleanup
}
```

**Algorithm Details:**
- Linear backward scan from probe position
- Efficient collision detection and handling
- Integration with reference counting system

## 2. Memory Allocation System

### 2.1 Cons Cell Management

**File: `conspage.c` - Cons Page Allocation**

**Confidence Level: VERY HIGH**

Implements efficient cons cell allocation with spatial locality:

```c
LispPTR GetConsCell(void) {
    // Try fast allocation from current page
    // Fall back to page allocation if needed
    // Maintain free lists for efficiency
}
```

### 2.2 Page Allocation Strategy

**File: `allocmds.c` - Memory Page Management**

**Confidence Level: HIGH**

Manages virtual memory pages for different object types:

```c
void allocate_page(int type) {
    // Allocate page from managed data space
    // Update page tables and tracking
    // Handle page overflow conditions
}
```

## 3. Virtual Memory Integration

### 3.1 Address Translation

**File: `adr68k.h` - Address Translation Functions**

**Confidence Level: VERY HIGH**

Provides unified address translation between Lisp and native formats:

```c
LispPTR NativeAligned2FromLAddr(LispPTR laddr) {
    // Convert Lisp virtual address to native pointer
    // Handle alignment and byte-swapping
    // Return properly aligned pointer
}
```

### 3.2 Storage Management

**File: `storagedefs.h` - Storage Definitions**

**Confidence Level: HIGH**

Defines storage management constants and structures:

```c
#define STK_SAFE 32        // Stack safety margin
#define BYTESPER_PAGE 512   // Page size in bytes
```

## 4. Data Structure Definitions

### 4.1 Core Types

**File: `lsptypes.h` - Lisp Type Definitions**

**Confidence Level: VERY HIGH**

Defines fundamental Lisp types and structures:

```c
typedef unsigned int LispPTR;  // 32-bit virtual address
typedef unsigned short DLword; // 16-bit data word
```

### 4.2 Cell Layouts

**File: `cell.h` - Cons Cell Structure**

**Confidence Level: VERY HIGH**

Defines cons cell structure with CDR coding:

```c
typedef struct conscell {
    DLword car_field;      // CAR value
    DLword cdr_code;       // CDR encoding
} ConsCell;
```

## 5. Integration Points

### 5.1 VM Integration

The memory management system integrates with:
- **Instruction Dispatch**: Automatic GC triggering during allocation
- **Stack Management**: Frame allocation and reclamation
- **Address Translation**: Virtual memory access patterns

### 5.2 Performance Considerations

- **Reference Counting**: Adds overhead but enables immediate reclamation
- **Cons Optimization**: CDR coding reduces memory usage significantly
- **Page Management**: Efficient allocation with spatial locality

## 6. Cross-References

- **Memory Layout**: See `memory-layout.typ` for region specifications
- **Address Translation**: See `address-translation.typ` for detailed algorithms
- **Garbage Collection**: See `garbage-collection.typ` for complete GC analysis
- **Implementation Details**: See `implementations/zig-implementation.typ` for Zig version

---

*This file provides the architectural overview of the memory management system. For detailed implementation analysis, see the companion files in this series.*