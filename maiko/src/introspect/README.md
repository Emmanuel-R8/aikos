# Maiko Introspection Module

A SQLite-backed introspection system for the Maiko C emulator.

## Purpose

This module captures extensive execution data during emulator runs for later querying. It solves the "granular vs volume" problem by capturing everything and allowing targeted SQL queries.

## Features

- **High-volume ingestion**: 100,000+ events/second via batching
- **Causality tracking**: Track what led to what
- **Time-travel debugging**: Query state at any point
- **Agent-friendly**: Structured JSON output from query tool

## Building

The module uses CMake and requires SQLite3:

```bash
cd maiko/build
cmake ..
make
```

## Usage

### 1. Run Emulator with Introspection

```bash
# Enable introspection
INTROSPECT_DB=trace.db ./lde sysout

# Or use default path (introspect.db)
./lde sysout
```

### 2. Query the Database

Using the query tool:

```bash
# Query atom 0x20A
./scripts/introspect-query trace.db atom 0x20A

# Query address
./scripts/introspect-query trace.db addr 0x180828

# Causality chain
./scripts/introspect-query trace.db causality 42

# ValSpace writes
./scripts/introspect-query trace.db valspace-writes --limit 50

# Opcode trace
./scripts/introspect-query trace.db opcode-trace --limit 100

# Phase timeline
./scripts/introspect-query trace.db phase-timeline

# Summary
./scripts/introspect-query trace.db summary

# Raw SQL
./scripts/introspect-query trace.db sql "SELECT * FROM events WHERE id = 100"
```

Using sqlite3 directly:

```bash
sqlite3 trace.db "SELECT * FROM current_session WHERE addr = 0x180828"
```

### 3. Causality Queries

Find what led to an event:

```sql
WITH RECURSIVE chain AS (
    SELECT * FROM events WHERE id = 42
    UNION ALL
    SELECT e.* FROM events e
    INNER JOIN chain c ON c.cause_id = e.id
)
SELECT * FROM chain ORDER BY ts;
```

Find what an event caused:

```sql
WITH RECURSIVE chain AS (
    SELECT * FROM events WHERE id = 42
    UNION ALL
    SELECT e.* FROM events e
    INNER JOIN chain c ON e.cause_id = c.id
)
SELECT * FROM chain ORDER BY ts;
```

## Integration

To add introspection to Maiko:

1. Include the header:
   ```c
   #include "introspect/introspect.h"
   ```

2. Open at startup:
   ```c
   IntrospectDB *g_introspect = introspect_open(getenv("INTROSPECT_DB"));
   introspect_start_session(g_introspect, sysout_path, "run");
   ```

3. Log events:
   ```c
   introspect_phase(g_introspect, "after_sysout_load");
   introspect_opcode(g_introspect, pc, opcode, name, tos, sp, fp);
   introspect_mem_write(g_introspect, addr, old, new, pc);
   ```

4. Close at shutdown:
   ```c
   introspect_close(g_introspect);
   ```

## Database Schema

See `schema.sql` for full schema. Key tables:

- **sessions**: One row per emulator run
- **events**: All logged events with timestamps, categories, addresses, values, and causality

## Performance

- Batching: 10,000 events per transaction
- WAL mode for concurrent reads during writes
- Indexed for fast queries on address, PC, atom_index, cause_id

## License

Same as Maiko emulator.
