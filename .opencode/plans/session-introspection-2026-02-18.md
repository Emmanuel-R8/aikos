# Session Save: Introspection Module Implementation

**Date**: 2026-02-18
**Branch**: 001-multi-impl-parity
**Status**: In Progress - Introspection working, investigating atom value initialization

---

## Summary

Implemented a SQLite-backed introspection module for the Maiko C emulator to trace execution and debug atom value initialization.

## Commits Made This Session

1. `9891fe1` - feat(maiko): add SQLite-backed introspection module
2. `2957969` - feat(maiko): integrate introspection into main.c
3. `0414afd` - fix(maiko): flush introspection after each phase
4. `9a802df` - feat(maiko): introspection working with phase tracking
5. `69ae623` - feat(maiko): add opcode introspection to dispatch loop
6. `541dea6` - feat(maiko): add atom read introspection to GVAR
7. `3177bdd` - feat(maiko): add UFN (undefined function) call tracing
8. `7e79dde` - fix(maiko): correct opcode analysis - 0xBF is POP not RECLAIMCELL

## Files Created

| File | Purpose |
|------|---------|
| `maiko/src/introspect/introspect.h` | Public API |
| `maiko/src/introspect/introspect_internal.h` | Internal types |
| `maiko/src/introspect/introspect.c` | Core implementation |
| `maiko/src/introspect/introspect_db.c` | SQLite integration |
| `maiko/src/introspect/schema.sql` | Database schema |
| `maiko/src/introspect/introspect_trace.h` | Tracing macros |
| `maiko/scripts/introspect-query` | Python query tool |
| `.opencode/plans/introspect-implementation-plan.md` | Full plan |

## Current Results

```
Database size: ~100MB
Total events: 90,005
  - Opcode events: 89,378
  - Atom events: 620
  - UFN events: 2
  - Phase events: 5
```

## Key Findings

### 1. VP 3076 is NOT in sysout
- Atom 522 (0x20A) value cell is at address 0x180828
- This is in VP 3076
- VP 3076 is NOT in FPtoVP table - page not loaded from file

### 2. Opcode 0xBF is POP (not RECLAIMCELL)
- opc_POP = 191 (0xBF) 
- opc_RECLAIMCELL = 122 (0x7A)
- The C trace timing: TOS logged BEFORE opcode executes

### 3. Value 0x140000 vs Expected 0
- Introspection shows atom 522 value = 0x140000
- If VP 3076 is zero-initialized (mmap MAP_ANON), should be 0
- Mystery: where does 0x140000 come from?

### 4. No Writes to Atom 522
- 43 atom writes captured
- None to atom 522 (0x20A)
- First read of atom 522 happens BEFORE any writes

## Outstanding Questions

1. **Is Laiko zero-initializing missing pages?** - Need to verify
2. **Is C zero-initializing?** - Uses mmap(MAP_ANON) which should zero
3. **Why does C trace show TOS=0x0E?** - This is BEFORE GVAR, from POP
4. **Where does 0x140000 come from?** - If page is zero-init, should be 0

## Next Steps

### Immediate
1. Verify Laiko zero-initializes missing pages
2. Check C emulator memory at 0x180828
3. Add introspection to initialization phases (before dispatch)

### Short Term
1. Fix any zero-initialization bugs
2. Trace memory writes during initialization
3. Compare C vs Laiko memory state

### Long Term
1. Add more memory write tracing (carefully - caused crashes before)
2. Trace function call stack
3. Implement causality tracking

## Usage

```bash
# Build
cd maiko && cmake --fresh . && make

# Run with introspection
INTROSPECT_DB=trace.db ./maiko/ldesdl sysout

# Query
sqlite3 trace.db "SELECT * FROM events WHERE atom_index=522 LIMIT 10"
sqlite3 trace.db "SELECT category, COUNT(*) FROM events GROUP BY category"
```

## Files Modified in maiko/

- `src/main.c` - Added introspection calls at phases
- `src/xc.c` - Added opcode and UFN introspection
- `src/gvar2.c` - Added atom write introspection
- `inc/inlineC.h` - Added GVAR read introspection
- `CMakeLists.txt` - Added introspect module

## Technical Details

### Database Schema

```sql
events (
  id, ts, session_id, category, action,
  pc, sp, fp, addr, atom_index,
  value, value_old, value_new,
  name, opcode, detail, cause_id
)
```

### Categories
- `phase` - Initialization phases
- `opcode` - Instruction execution
- `atom` - Atom value cell access
- `ufn` - Undefined function calls
- `memory` - Memory read/write (not yet active)
- `stack` - Stack operations (not yet active)

### Phase Timeline

```
startup:         0.000s
before_sysout:   2.95s
after_sysout:    3.11s
after_build_map: 3.11s
before_dispatch: 3.12s
first_opcode:    3.12s
```

## Crash Note

Memory write tracing in inlineC.h macros caused crashes. Reverted those changes. Need safer approach.

---

**To Resume**: See next steps above. Focus on zero-initialization verification first.
