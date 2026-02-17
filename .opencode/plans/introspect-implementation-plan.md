# Introspection Module Implementation Plan

**Date**: 2026-02-16
**Status**: READY FOR IMPLEMENTATION
**Dependencies**: SQLite3

## Overview

Create a SQLite-backed introspection system for the Maiko C emulator that captures extensive execution data for later querying. This addresses the "granular vs volume" problem by capturing everything and allowing targeted SQL queries.

## Goals

1. Understand where Valspace values are initialized
2. Enable full causality tracking (what led to what)
3. Support intensive JOIN queries for time-travel debugging
4. Be fully usable by an LLM agent via SQL queries

---

## Part 1: Database Design

### 1.1 Schema with JOIN-Optimized Indexes

```sql
-- maiko/src/introspect/schema.sql

-- Enable WAL mode for better write performance
PRAGMA journal_mode = WAL;
PRAGMA synchronous = NORMAL;

-- Main events table
CREATE TABLE events (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    ts REAL NOT NULL,               -- Timestamp (seconds since start)
    category TEXT NOT NULL,         -- 'phase', 'opcode', 'memory', 'stack', 'atom'
    action TEXT NOT NULL,           -- 'begin', 'end', 'read', 'write', 'push', 'pop', etc.
    pc INTEGER,                     -- Program counter (for execution events)
    addr INTEGER,                   -- Memory address
    value INTEGER,                  -- Primary value
    value_old INTEGER,              -- Old value (for writes)
    value_new INTEGER,              -- New value (for writes)
    name TEXT,                      -- Symbolic name (opcode, phase, etc.)
    detail TEXT,                    -- JSON for extensible data
    session_id INTEGER NOT NULL,    -- For multiple run sessions
    
    -- Causality: what event caused this one
    cause_id INTEGER,
    
    FOREIGN KEY (cause_id) REFERENCES events(id)
);

-- Session table for multiple runs
CREATE TABLE sessions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    started_at TEXT NOT NULL,
    sysout_path TEXT,
    notes TEXT
);

-- ============================================================================
-- INDEXES FOR INTENSIVE JOIN QUERIES
-- ============================================================================

-- Primary lookup indexes
CREATE INDEX idx_events_session ON events(session_id);
CREATE INDEX idx_events_ts ON events(session_id, ts);
CREATE INDEX idx_events_category ON events(session_id, category);
CREATE INDEX idx_events_action ON events(session_id, action);

-- Address-based queries (memory, atoms)
CREATE INDEX idx_events_addr ON events(session_id, addr);
CREATE INDEX idx_events_addr_ts ON events(session_id, addr, ts);

-- PC-based queries (opcode execution)
CREATE INDEX idx_events_pc ON events(session_id, pc);
CREATE INDEX idx_events_pc_ts ON events(session_id, pc, ts);

-- CAUSALITY INDEXES (critical for intensive joins)
CREATE INDEX idx_events_cause ON events(session_id, cause_id);
CREATE INDEX idx_events_cause_rev ON events(session_id, id, cause_id);

-- Value-based queries
CREATE INDEX idx_events_value ON events(session_id, value);

-- Composite for common queries
CREATE INDEX idx_events_cat_act ON events(session_id, category, action);
CREATE INDEX idx_events_cat_addr ON events(session_id, category, addr);

-- ============================================================================
-- VIEWS FOR COMMON QUERIES
-- ============================================================================

-- Current session view (assumes latest session)
CREATE VIEW current_session AS
SELECT * FROM events WHERE session_id = (SELECT MAX(id) FROM sessions);

-- Atom value cell events
CREATE VIEW atom_events AS
SELECT * FROM current_session
WHERE category IN ('atom', 'memory')
  AND addr >= 0x180000 AND addr < 0x1C0000;

-- Opcode execution trace
CREATE VIEW opcode_trace AS
SELECT id, ts, pc, name, value as tos, value_old as sp, value_new as fp
FROM current_session
WHERE category = 'opcode'
ORDER BY ts;

-- Memory writes to Valspace
CREATE VIEW valspace_writes AS
SELECT * FROM current_session
WHERE category = 'memory' AND action = 'write'
  AND addr >= 0x180000 AND addr < 0x1C0000
ORDER BY ts;

-- ============================================================================
-- CAUSALITY VIEWS
-- ============================================================================

-- Direct causes (what caused this event)
CREATE VIEW event_causes AS
SELECT 
    e.id, e.ts, e.category, e.action, e.name,
    c.id as cause_id, c.ts as cause_ts, c.category as cause_cat, c.name as cause_name
FROM current_session e
LEFT JOIN current_session c ON e.cause_id = c.id;
```

### 1.2 Causality Tracking Strategy

For reverse debugging and "how did we get here" queries:

```sql
-- Recursive CTE for full causality chain
-- Find all events that led to event with id = :target_id
WITH RECURSIVE causality_chain AS (
    -- Base case: the target event
    SELECT * FROM events WHERE id = :target_id
    
    UNION ALL
    
    -- Recursive: find what caused each event
    SELECT e.* FROM events e
    INNER JOIN causality_chain c ON e.id = c.cause_id
)
SELECT * FROM causality_chain ORDER BY ts;

-- Forward causality: what did this event cause?
WITH RECURSIVE effects_chain AS (
    SELECT * FROM events WHERE id = :source_id
    UNION ALL
    SELECT e.* FROM events e
    INNER JOIN effects_chain ec ON e.cause_id = ec.id
)
SELECT * FROM effects_chain ORDER BY ts;
```

---

## Part 2: File Structure

```
maiko/src/introspect/
├── introspect.h              # Public API (~80 lines)
├── introspect_db.h           # Database types (~50 lines)
├── introspect.c              # Core implementation (~200 lines)
├── introspect_db.c           # SQLite integration (~250 lines)
├── introspect_batch.c        # Batching logic (~150 lines)
├── schema.sql                # Database schema (~80 lines)
├── CMakeLists.txt            # Build configuration (~40 lines)
└── README.md                 # Usage documentation (~120 lines)

maiko/scripts/
└── introspect-query          # Query helper tool (~200 lines Python)
```

**Total: ~1,170 lines**

---

## Part 3: Public API

### 3.1 introspect.h

```c
#ifndef MAIKO_INTROSPECT_H
#define MAIKO_INTROSPECT_H

#include <stdint.h>
#include "lispemul.h"

/* Opaque database handle */
typedef struct IntrospectDB IntrospectDB;

/* ============================================================================
 * LIFECYCLE
 * ============================================================================ */

/**
 * Open introspection database.
 * Creates if not exists, opens existing if present.
 * 
 * @param path Path to SQLite database file (NULL for default: introspect.db)
 * @return Handle or NULL on error
 */
IntrospectDB* introspect_open(const char *path);

/**
 * Close database and flush any pending events.
 */
void introspect_close(IntrospectDB *db);

/**
 * Start a new session. Call after open.
 * 
 * @param sysout_path Path to sysout file being loaded
 * @param notes Optional notes for this session
 */
void introspect_start_session(IntrospectDB *db, const char *sysout_path, const char *notes);

/* ============================================================================
 * EVENT LOGGING
 * ============================================================================ */

/**
 * Log a phase marker (initialization phases).
 */
void introspect_phase(IntrospectDB *db, const char *phase_name);

/**
 * Log an opcode execution.
 * 
 * @param db Database handle
 * @param pc Program counter
 * @param opcode Bytecode opcode
 * @param name Opcode name (can be NULL)
 * @param tos Top-of-stack value
 * @param sp Stack pointer
 * @param fp Frame pointer
 */
void introspect_opcode(IntrospectDB *db, LispPTR pc, uint8_t opcode, 
                       const char *name, LispPTR tos, LispPTR sp, LispPTR fp);

/**
 * Log a memory read.
 */
void introspect_mem_read(IntrospectDB *db, LispPTR addr, LispPTR value, LispPTR pc);

/**
 * Log a memory write.
 */
void introspect_mem_write(IntrospectDB *db, LispPTR addr, LispPTR old_value, 
                          LispPTR new_value, LispPTR pc);

/**
 * Log an atom operation.
 * 
 * @param action "read", "write", "cell_init", etc.
 */
void introspect_atom(IntrospectDB *db, uint32_t atom_index, const char *action,
                     LispPTR value, LispPTR pc);

/**
 * Log a stack operation.
 * 
 * @param action "push", "pop", "tos_update"
 */
void introspect_stack(IntrospectDB *db, const char *action, 
                      LispPTR value, LispPTR sp);

/**
 * Log a generic event with custom detail.
 */
void introspect_event(IntrospectDB *db, const char *category, const char *action,
                      LispPTR pc, LispPTR addr, LispPTR value,
                      const char *name, const char *detail);

/* ============================================================================
 * CAUSALITY
 * ============================================================================ */

/**
 * Get the ID of the last logged event.
 * Use this to set cause_id for subsequent events.
 */
uint64_t introspect_last_event_id(IntrospectDB *db);

/**
 * Set the cause for the next event.
 * The next logged event will have cause_id = cause_event_id.
 */
void introspect_set_cause(IntrospectDB *db, uint64_t cause_event_id);

/**
 * Clear the cause for subsequent events.
 */
void introspect_clear_cause(IntrospectDB *db);

/* ============================================================================
 * UTILITY
 * ============================================================================ */

/**
 * Flush pending events to database.
 * Normally called automatically when buffer is full.
 */
void introspect_flush(IntrospectDB *db);

/**
 * Get statistics.
 */
uint64_t introspect_event_count(IntrospectDB *db);
double introspect_elapsed_time(IntrospectDB *db);

#endif /* MAIKO_INTROSPECT_H */
```

---

## Part 4: Implementation Details

### 4.1 Batching for Performance

```c
// introspect_batch.c

#define INTROSPECT_BATCH_SIZE 10000

typedef struct {
    double ts;
    char category[32];
    char action[32];
    uint64_t pc;
    uint64_t addr;
    uint64_t value;
    uint64_t value_old;
    uint64_t value_new;
    char name[64];
    char detail[256];
    uint64_t cause_id;
    uint32_t session_id;
} EventRecord;

typedef struct {
    EventRecord records[INTROSPECT_BATCH_SIZE];
    int count;
    uint64_t next_cause_id;  // For causality tracking
} EventBuffer;

// Flush buffer to database in single transaction
int introspect_flush_buffer(IntrospectDB *db);
```

### 4.2 Causality Tracking

Automatic causality tracking strategy:

```c
// In introspect_batch.c

// Global cause stack (for nested operations)
#define CAUSE_STACK_DEPTH 16
static uint64_t g_cause_stack[CAUSE_STACK_DEPTH];
static int g_cause_stack_top = 0;

void introspect_push_cause(IntrospectDB *db, uint64_t event_id) {
    if (g_cause_stack_top < CAUSE_STACK_DEPTH) {
        g_cause_stack[g_cause_stack_top++] = event_id;
    }
}

void introspect_pop_cause(void) {
    if (g_cause_stack_top > 0) {
        g_cause_stack_top--;
    }
}

uint64_t introspect_current_cause(void) {
    if (g_cause_stack_top > 0) {
        return g_cause_stack[g_cause_stack_top - 1];
    }
    return 0;  // No cause
}

// When logging an event:
void introspect_log_event(IntrospectDB *db, EventRecord *rec) {
    rec->cause_id = introspect_current_cause();
    // Add to buffer...
}
```

### 4.3 Integration Points

**Changes to main.c:**

```c
#include "introspect/introspect.h"

// Global introspection handle
IntrospectDB *g_introspect = NULL;

int main(int argc, char *argv[]) {
    // Early init
    g_introspect = introspect_open(getenv("INTROSPECT_DB"));
    if (g_introspect) {
        introspect_start_session(g_introspect, argv[argc-1], "introspection run");
        introspect_phase(g_introspect, "startup");
    }
    
    // ... existing code ...
    
    // Before sysout load
    if (g_introspect) introspect_phase(g_introspect, "before_sysout_load");
    
    sysout_loader(...);
    
    // After sysout load
    if (g_introspect) introspect_phase(g_introspect, "after_sysout_load");
    
    // ... more phases ...
    
    // Before dispatch
    if (g_introspect) introspect_phase(g_introspect, "before_dispatch");
    
    // Dispatch loop
    while (running) {
        // ... existing dispatch ...
    }
    
    // Cleanup
    if (g_introspect) {
        introspect_flush(g_introspect);
        introspect_close(g_introspect);
    }
}
```

**Changes to xc.c (opcode dispatch):**

```c
#include "introspect/introspect.h"
extern IntrospectDB *g_introspect;

// In dispatch loop, after each opcode:
if (g_introspect) {
    introspect_opcode(g_introspect, PC, opcode, opcode_name, TOPOFSTACK, 
                      (LispPTR)CurrentStackPTR, (LispPTR)CurrentFramePTR);
}
```

**Changes to memory access (inlines/macros):**

```c
// In memory.h or similar:

// Wrap write operations
#define STORE_LISPPTR(addr, val) \
    do { \
        LispPTR _old = *((LispPTR*)(addr)); \
        *((LispPTR*)(addr)) = (val); \
        if (g_introspect) \
            introspect_mem_write(g_introspect, (LispPTR)(addr), _old, (val), PC); \
    } while(0)
```

---

## Part 5: Query Helper Tool

### 5.1 introspect-query

```python
#!/usr/bin/env python3
"""
Introspection Query Helper Tool

Usage:
    introspect-query <db> atom <atom_index>
    introspect-query <db> causality <event_id>
    introspect-query <db> valspace-writes [--limit N]
    introspect-query <db> opcode-trace [--start-ts T] [--end-ts T]
    introspect-query <db> phase-timeline
    introspect-query <db> summary
"""

import sqlite3
import sys
import json
from typing import Optional, List, Dict, Any

def query_atom(conn: sqlite3.Connection, atom_index: int) -> List[Dict]:
    """Query all events for a specific atom."""
    # Calculate value cell address
    valspace_base = 0x180000
    addr = valspace_base + (atom_index * 4)
    
    cursor = conn.execute("""
        SELECT * FROM current_session 
        WHERE addr = ? OR (category = 'atom' AND value = ?)
        ORDER BY ts
    """, (addr, atom_index))
    
    return [dict(row) for row in cursor.fetchall()]

def query_causality(conn: sqlite3.Connection, event_id: int, direction: str = 'backward') -> List[Dict]:
    """Query causality chain for an event."""
    if direction == 'backward':
        # What led to this event?
        cursor = conn.execute("""
            WITH RECURSIVE chain AS (
                SELECT * FROM events WHERE id = ?
                UNION ALL
                SELECT e.* FROM events e
                INNER JOIN chain c ON c.cause_id = e.id
            )
            SELECT * FROM chain ORDER BY ts
        """, (event_id,))
    else:
        # What did this event cause?
        cursor = conn.execute("""
            WITH RECURSIVE chain AS (
                SELECT * FROM events WHERE id = ?
                UNION ALL
                SELECT e.* FROM events e
                INNER JOIN chain c ON e.cause_id = c.id
            )
            SELECT * FROM chain ORDER BY ts
        """, (event_id,))
    
    return [dict(row) for row in cursor.fetchall()]

def query_valspace_writes(conn: sqlite3.Connection, limit: int = 100) -> List[Dict]:
    """Query all writes to Valspace."""
    cursor = conn.execute("""
        SELECT * FROM valspace_writes
        LIMIT ?
    """, (limit,))
    return [dict(row) for row in cursor.fetchall()]

def query_opcode_trace(conn: sqlite3.Connection, start_ts: float = 0, end_ts: float = 1e10) -> List[Dict]:
    """Query opcode execution trace."""
    cursor = conn.execute("""
        SELECT * FROM opcode_trace
        WHERE ts BETWEEN ? AND ?
        ORDER BY ts
    """, (start_ts, end_ts))
    return [dict(row) for row in cursor.fetchall()]

def query_phase_timeline(conn: sqlite3.Connection) -> List[Dict]:
    """Query initialization phase timeline."""
    cursor = conn.execute("""
        SELECT ts, name, action
        FROM current_session
        WHERE category = 'phase'
        ORDER BY ts
    """)
    return [dict(row) for row in cursor.fetchall()]

def query_summary(conn: sqlite3.Connection) -> Dict:
    """Get summary statistics."""
    cursor = conn.execute("SELECT COUNT(*) FROM current_session")
    total_events = cursor.fetchone()[0]
    
    cursor = execute("SELECT MIN(ts), MAX(ts) FROM current_session")
    min_ts, max_ts = cursor.fetchone()
    
    cursor = conn.execute("""
        SELECT category, COUNT(*) 
        FROM current_session 
        GROUP BY category
    """)
    by_category = {row[0]: row[1] for row in cursor.fetchall()}
    
    return {
        'total_events': total_events,
        'duration_seconds': max_ts - min_ts if max_ts and min_ts else 0,
        'by_category': by_category
    }

def main():
    if len(sys.argv) < 3:
        print(__doc__)
        sys.exit(1)
    
    db_path = sys.argv[1]
    command = sys.argv[2]
    
    conn = sqlite3.connect(db_path)
    conn.row_factory = sqlite3.Row
    
    if command == 'atom':
        atom_index = int(sys.argv[3], 0)  # Support hex
        result = query_atom(conn, atom_index)
    
    elif command == 'causality':
        event_id = int(sys.argv[3], 0)
        direction = sys.argv[4] if len(sys.argv) > 4 else 'backward'
        result = query_causality(conn, event_id, direction)
    
    elif command == 'valspace-writes':
        limit = int(sys.argv[3]) if len(sys.argv) > 3 else 100
        result = query_valspace_writes(conn, limit)
    
    elif command == 'opcode-trace':
        result = query_opcode_trace(conn)
    
    elif command == 'phase-timeline':
        result = query_phase_timeline(conn)
    
    elif command == 'summary':
        result = query_summary(conn)
    
    else:
        print(f"Unknown command: {command}")
        sys.exit(1)
    
    print(json.dumps(result, indent=2, default=str))

if __name__ == '__main__':
    main()
```

---

## Part 6: CMakeLists.txt

```cmake
# maiko/src/introspect/CMakeLists.txt

find_package(SQLite3 REQUIRED)

add_library(maiko_introspect STATIC
    introspect.c
    introspect_db.c
    introspect_batch.c
)

target_include_directories(maiko_introspect
    PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
    PUBLIC ${SQLite3_INCLUDE_DIRS}
)

target_link_libraries(maiko_introspect
    ${SQLite3_LIBRARIES}
)

# Install schema.sql alongside
install(FILES schema.sql DESTINATION share/maiko/introspect)
```

---

## Part 7: Changes to flake.nix

Add SQLite development libraries:

```nix
# In buildInputs, add:
sqlite.dev    # SQLite development headers
sqlite.out    # SQLite runtime library
```

---

## Part 8: Implementation Order

1. **Create directory structure**
   ```bash
   mkdir -p maiko/src/introspect
   mkdir -p maiko/scripts
   ```

2. **Create schema.sql** (foundation)

3. **Create introspect.h** (public API)

4. **Create introspect_db.h** (internal types)

5. **Create introspect.c** (core implementation)

6. **Create introspect_db.c** (SQLite integration)

7. **Create introspect_batch.c** (batching)

8. **Create CMakeLists.txt**

9. **Create introspect-query** (Python tool)

10. **Create README.md**

11. **Update flake.nix**

12. **Modify main.c** (add integration)

13. **Modify xc.c** (add opcode tracing)

14. **Test and verify**

---

## Part 9: Verification

After implementation, verify with:

```bash
# Build
cd maiko/build
cmake .. && make

# Run with introspection
INTROSPECT_DB=trace.db ./lde sysout

# Query for atom 0x20A
sqlite3 trace.db "SELECT * FROM atom_events WHERE addr = 0x180828"
# OR
./scripts/introspect-query trace.db atom 0x20A

# Query causality
./scripts/introspect-query trace.db causality 100
```

---

## Success Criteria

1. ✅ Database captures all events during init and execution
2. ✅ Can query where atom 0x20A got its value
3. ✅ Can trace causality chains
4. ✅ Batching achieves 100,000+ events/second
5. ✅ Query tool is agent-friendly
6. ✅ No significant slowdown to emulator (batching hides overhead)
