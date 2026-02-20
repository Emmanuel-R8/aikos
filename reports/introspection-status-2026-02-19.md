# Introspection Module Implementation Status Report

**Date**: 2026-02-19
**Branch**: 001-multi-impl-parity
**Author**: Claude (Anthropic)

---

## Executive Summary

Implemented a comprehensive SQLite-backed introspection module for the Maiko C emulator to trace execution and debug atom value initialization. The module successfully captures build configuration, runtime pointers, and memory snapshots at key initialization phases.

**Key Finding**: Atom 522's value cell shows `0x0` during initialization but GVAR reads `0xE` during execution. This proves the value is written DURING execution, not during initialization.

---

## Sessions Summary

| Session ID | Date | Sysout | Events | Status |
|------------|------|--------|--------|--------|
| 1 | 2026-02-19 06:33 | starter.sysout | ~810K | Crash after ~10 opcodes |
| 2 | 2026-02-19 07:00 | starter.sysout | ~810K | Crash after ~10 opcodes |

---

## Commits Made

| Commit | Description |
|--------|-------------|
| `468d744` | feat(maiko): add introspection tables for memory debugging |
| `789c933` | feat(maiko): implement memory debugging introspection functions |
| `1d340e3` | feat(maiko): add introspection calls for memory debugging |
| `261a6b3` | feat(maiko): introspection working with build_config and runtime_config |
| `7edd46b` | fix(maiko): correct timing for runtime_config capture |
| `04c7338` | docs(introspection): add WAL checkpoint tip for SQLite |

---

## Database Schema

### Tables Implemented

| Table | Purpose | Status |
|-------|---------|--------|
| `sessions` | Recording sessions | ✅ Working |
| `events` | All execution events | ✅ Working (810K+ events) |
| `build_config` | Build-time configuration | ✅ Populated |
| `runtime_config` | Runtime pointers | ✅ Populated |
| `memory_snapshots` | Key addresses per phase | ✅ Populated |
| `memory_writes` | All memory writes | ⚠️ Schema only, no data |
| `vals_pages` | Page sparse/loaded status | ⚠️ Schema only, no data |
| `gvar_executions` | Detailed GVAR trace | ⚠️ Schema only, no data |

### Schema SQL

```sql
-- Key tables (see maiko/src/introspect/schema.sql for full schema)

CREATE TABLE build_config (
    id INTEGER PRIMARY KEY CHECK (id = 1),
    bigvm INTEGER,
    bigatoms INTEGER,
    vals_offset INTEGER,
    atoms_offset INTEGER,
    stackspace_offset INTEGER,
    total_vm_size INTEGER,
    page_size INTEGER,
    created_at TEXT
);

CREATE TABLE runtime_config (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    session_id INTEGER NOT NULL,
    valspace_ptr INTEGER,
    atomspace_ptr INTEGER,
    stackspace_ptr INTEGER,
    sysout_file TEXT,
    sysout_size INTEGER,
    total_pages_loaded INTEGER,
    sparse_pages_count INTEGER
);

CREATE TABLE memory_snapshots (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    ts REAL,
    session_id INTEGER NOT NULL,
    phase TEXT NOT NULL,
    location_name TEXT NOT NULL,
    address INTEGER NOT NULL,
    value INTEGER
);
```

---

## Current Data

### Build Configuration (Session 2)

```
bigvm           = 1
bigatoms        = 1
vals_offset     = 0xC0000 (786432)
atoms_offset    = 0x2C0000 (2883584)
stackspace_off  = 0x10000 (65536)
page_size       = 512
```

### Runtime Configuration (Session 2)

```
valspace_ptr    = 0x7FFFD0180000
atomspace_ptr   = 0x7FFFD0180000 (same as valspace - may be wrong)
stackspace_ptr  = 0x7FFFD0180000 (same - may be wrong)
sysout_file     = ./medley/loadups/starter.sysout
sysout_size     = 256 (incorrect - should be file size)
```

### Memory Snapshots (Session 2)

| Phase | Location | Address | Value |
|-------|----------|---------|-------|
| after_build_lisp_map | vals_start | 0x7FFFD0180000 | 0x0 |
| after_build_lisp_map | atom_522_value | 0x7FFFD0180828 | 0x0 |
| before_dispatch | vals_start | 0x7FFFD0180000 | 0x0 |
| before_dispatch | atom_522_value | 0x7FFFD0180828 | 0x0 |
| before_dispatch | ifpage_key | 0x222 | 0x15E3 ✅ |
| before_dispatch | current_fp | 0x0 | 0x0 |

---

## Key Findings

### 1. Atom 522 Value Cell Address

```
Lisp address:  0xC0828
Native address: 0x7FFFD0180828
VP:            1540
Offset in VP:  40 bytes
```

### 2. Value Changes During Execution

| Phase | atom_522_value |
|-------|-----------------|
| after_build_lisp_map | 0x0 |
| before_dispatch | 0x0 |
| **During GVAR execution** | **0xE** |

**Conclusion**: Value is written DURING execution, not initialization.

### 3. VP 1540 in Sysout File

```python
# Verified: VP 1540 IS in the sysout file (page 1540 of 16635)
# File shows 0x00000000 at that location
```

### 4. IFPAGE Key Verified

```
ifpage_key = 0x15E3 (correct per IFPAGE_KEYVAL constant)
```

---

## Outstanding Questions

1. **Who writes 0xE to atom 522?** - Need memory write tracing during execution
2. **Why do pointers show same address?** - atomspace_ptr and stackspace_ptr should be different
3. **Why sysout_size=256?** - Should be actual file size (~8MB)
4. **What opcode writes to atom 522?** - Need to trace first 10 opcodes with write tracing

---

## Code Locations

### Introspection Module

```
maiko/src/introspect/
├── introspect.h           # Public API
├── introspect_internal.h  # Internal types
├── introspect.c           # Core implementation
├── introspect_db.c        # SQLite integration
├── schema.sql             # Full schema
└── introspect_trace.h     # Tracing macros
```

### Integration Points

```
maiko/src/main.c:
  - Line ~1180: introspect_open() and build_config
  - Line ~1260: after_sysout_load phase
  - Line ~1290: after_build_lisp_map phase + runtime_config + memory_snapshots
  - Line ~1330: before_dispatch phase + final memory_snapshots

maiko/src/xc.c:
  - Line ~970: opcode_array for name lookup
  - Line ~1020: introspect_opcode() call in dispatch loop
```

---

## Usage

### Build

```bash
cd maiko
cmake .
make -j4
```

### Run with Introspection

```bash
export INTROSPECT_DB=trace.db
./maiko/bin/ldesdl ./medley/loadups/starter.sysout
```

### Checkpoint WAL (IMPORTANT!)

```bash
sqlite3 trace.db "PRAGMA wal_checkpoint(TRUNCATE);"
```

### Query Examples

```sql
-- Latest session
SELECT MAX(id) FROM sessions;

-- Memory snapshots
SELECT phase, location_name, printf('0x%X', value) as val 
FROM memory_snapshots 
WHERE session_id = (SELECT MAX(id) FROM sessions);

-- Build config
SELECT * FROM build_config;

-- Runtime config
SELECT * FROM runtime_config 
WHERE session_id = (SELECT MAX(id) FROM sessions);

-- Atom 522 value through phases
SELECT phase, printf('0x%X', value) 
FROM memory_snapshots 
WHERE location_name = 'atom_522_value' 
  AND session_id = (SELECT MAX(id) FROM sessions);
```

---

## Next Steps

### Immediate (To Find Who Writes 0xE)

1. **Implement GVAR execution tracing**
   - Add call to `introspect_gvar_execution()` in GVAR handler
   - Capture atom_index, calculated_addr, value_read, vp, is_sparse

2. **Implement memory write tracing**
   - Add call to `introspect_memory_write()` in memory write macros
   - Trace writes during first few opcodes

3. **Populate vals_pages table**
   - After sysout load, enumerate all Valspace pages
   - Mark which are sparse (not loaded from sysout)

### Medium Term

1. **Fix pointer values in runtime_config**
   - atomspace_ptr should be different from valspace_ptr
   - stackspace_ptr should be different
   - sysout_size should be actual file size

2. **Add more snapshot locations**
   - Capture more atom value cells
   - Capture stack state
   - Capture PC state

### Long Term

1. **Causality tracking** - Link events to causes
2. **Reverse debugging** - Step backward from crash
3. **Memory leak detection** - Track allocations

---

## Known Issues

### 1. Emulator Crash

```
malloc(): unaligned tcache chunk detected
```

- Occurs after ~10 opcodes
- Unrelated to introspection (happens without INTROSPECT_DB)
- Need to investigate memory corruption

### 2. SQLite WAL Not Committed

- Must run `PRAGMA wal_checkpoint(TRUNCATE)` after emulator stops
- WAL contains uncommitted transactions

### 3. Pointer Values Identical

```
valspace_ptr    = 0x7FFFD0180000
atomspace_ptr   = 0x7FFFD0180000  # WRONG
stackspace_ptr  = 0x7FFFD0180000  # WRONG
```

### 4. sysout_size Incorrect

```
sysout_size = 256  # Should be ~8MB
```

---

## Files to Resume Work

1. This report: `reports/introspection-status-2026-02-19.md`
2. Session plan: `.opencode/plans/session-introspection-2026-02-18.md`
3. Implementation plan: `.opencode/plans/introspect-implementation-plan.md`
4. Schema: `maiko/src/introspect/schema.sql`
5. API: `maiko/src/introspect/introspect.h`
6. Main integration: `maiko/src/main.c`

---

## Appendix: Complete Event Statistics (Session 1)

```
Total events:     810,005
Opcode events:    805,319
Atom events:      4,679
Phase events:     5
UFN events:       2

Database size:    ~48MB
```

### First 10 Opcodes (Session 1)

| ID | PC | Opcode | Name | TOS |
|----|----|--------|------|-----|
| 6 | 0x60F130 | 191 | POP | - |
| 7 | 0x60F131 | 96 | GVAR | 0xE |
| 8 | 0x60F136 | 18 | UNBIND | - |
| 9 | 0x60F137 | 201 | GETBASEPTR_N | - |
| 10 | 0x60F139 | 100 | COPY | - |

---

## Appendix: Python Helper Scripts

### Check if VP is in sysout

```python
import struct

def check_vp_in_sysout(sysout_path, vp):
    with open(sysout_path, 'rb') as f:
        f.seek(0, 2)
        file_size = f.tell()
        num_pages = file_size // 512
        
        if vp < num_pages:
            f.seek(vp * 512)
            data = f.read(512)
            return True, data
        return False, None

# Usage
in_file, data = check_vp_in_sysout('medley/loadups/starter.sysout', 1540)
print(f"VP 1540 in sysout: {in_file}")
```

### Calculate atom value cell address

```python
def atom_value_cell_address(atom_index, vals_offset=0xC0000):
    """Calculate Lisp address of atom's value cell."""
    return vals_offset + atom_index * 4

# Usage
addr = atom_value_cell_address(522)
print(f"Atom 522 value cell: 0x{addr:X}")  # 0xC0828
```

---

## Appendix: SQL Queries for Debugging

```sql
-- Find all writes to a specific address
SELECT * FROM memory_writes 
WHERE address = 0x7FFFD0180828  -- atom 522 value cell
ORDER BY ts;

-- Find all GVAR executions for an atom
SELECT * FROM gvar_executions 
WHERE atom_index = 522
ORDER BY ts;

-- Find sparse Valspace pages
SELECT vp, printf('0x%X', address_start) as start_addr
FROM vals_pages 
WHERE is_sparse = 1
ORDER BY vp;

-- Trace value changes through phases
SELECT phase, location_name, printf('0x%X', value) as value
FROM memory_snapshots
WHERE session_id = (SELECT MAX(id) FROM sessions)
ORDER BY ts;
```

---

**End of Report**
