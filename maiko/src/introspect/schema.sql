-- Introspection Database Schema for Maiko Emulator
-- 
-- This database captures extensive execution data for later querying.
-- Designed for:
--   1. High-volume event ingestion (100K+ events/second)
--   2. Intensive JOIN queries for causality tracking
--   3. Agent-friendly querying via SQL
--
-- Location: maiko/src/introspect/schema.sql

-- ============================================================================
-- PERFORMANCE SETTINGS
-- ============================================================================

PRAGMA journal_mode = WAL;      -- Write-Ahead Logging for better concurrency
PRAGMA synchronous = NORMAL;    -- Balanced safety/performance
PRAGMA cache_size = -64000;     -- 64MB cache
PRAGMA temp_store = MEMORY;     -- Temp tables in memory

-- ============================================================================
-- SESSIONS TABLE
-- ============================================================================

CREATE TABLE IF NOT EXISTS sessions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    started_at TEXT NOT NULL,      -- ISO timestamp
    finished_at TEXT,              -- NULL until session ends
    sysout_path TEXT,              -- Path to sysout file
    max_steps INTEGER DEFAULT 0,   -- Max steps configured
    notes TEXT                     -- Free-form notes
);

-- ============================================================================
-- EVENTS TABLE
-- ============================================================================

CREATE TABLE IF NOT EXISTS events (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    
    -- Core fields
    ts REAL NOT NULL,              -- Timestamp (seconds since session start)
    session_id INTEGER NOT NULL,   -- Link to session
    
    -- Categorization
    category TEXT NOT NULL,        -- 'phase', 'opcode', 'memory', 'stack', 'atom', etc.
    action TEXT NOT NULL,          -- 'begin', 'end', 'read', 'write', 'push', 'pop', etc.
    
    -- Execution context
    pc INTEGER,                    -- Program counter
    sp INTEGER,                    -- Stack pointer
    fp INTEGER,                    -- Frame pointer
    
    -- Memory context
    addr INTEGER,                  -- Memory address (for memory events)
    atom_index INTEGER,            -- Atom index (for atom events)
    
    -- Values
    value INTEGER,                 -- Primary value
    value_old INTEGER,             -- Old value (for writes)
    value_new INTEGER,             -- New value (for writes)
    
    -- Naming
    name TEXT,                     -- Symbolic name (opcode, phase, etc.)
    opcode INTEGER,                -- Raw opcode byte
    
    -- Extensibility
    detail TEXT,                   -- JSON for additional data
    
    -- Causality
    cause_id INTEGER,              -- What event caused this one
    
    FOREIGN KEY (session_id) REFERENCES sessions(id),
    FOREIGN KEY (cause_id) REFERENCES events(id)
);

-- ============================================================================
-- INDEXES FOR INTENSIVE JOIN QUERIES
-- ============================================================================

-- Session-scoped indexes (most queries filter by session)
CREATE INDEX IF NOT EXISTS idx_events_session ON events(session_id);
CREATE INDEX IF NOT EXISTS idx_events_session_ts ON events(session_id, ts);
CREATE INDEX IF NOT EXISTS idx_events_session_cat ON events(session_id, category);
CREATE INDEX IF NOT EXISTS idx_events_session_act ON events(session_id, action);

-- Address-based queries (memory operations, atom value cells)
CREATE INDEX IF NOT EXISTS idx_events_addr ON events(session_id, addr);
CREATE INDEX IF NOT EXISTS idx_events_addr_ts ON events(session_id, addr, ts);

-- PC-based queries (opcode execution)
CREATE INDEX IF NOT EXISTS idx_events_pc ON events(session_id, pc);
CREATE INDEX IF NOT EXISTS idx_events_pc_ts ON events(session_id, pc, ts);

-- Atom-based queries
CREATE INDEX IF NOT EXISTS idx_events_atom ON events(session_id, atom_index);
CREATE INDEX IF NOT EXISTS idx_events_atom_ts ON events(session_id, atom_index, ts);

-- CAUSALITY INDEXES (critical for reverse debugging)
-- These enable efficient recursive CTE queries
CREATE INDEX IF NOT EXISTS idx_events_cause ON events(session_id, cause_id);
CREATE INDEX IF NOT EXISTS idx_events_cause_rev ON events(session_id, id, cause_id);

-- Value-based queries (find where specific values appear)
CREATE INDEX IF NOT EXISTS idx_events_value ON events(session_id, value);

-- Composite indexes for common query patterns
CREATE INDEX IF NOT EXISTS idx_events_cat_act ON events(session_id, category, action);
CREATE INDEX IF NOT EXISTS idx_events_cat_addr ON events(session_id, category, addr);
CREATE INDEX IF NOT EXISTS idx_events_cat_pc ON events(session_id, category, pc);

-- ============================================================================
-- CONVENIENCE VIEWS
-- ============================================================================

-- Current session (most recent)
CREATE VIEW IF NOT EXISTS current_session AS
SELECT e.* FROM events e
WHERE e.session_id = (SELECT MAX(id) FROM sessions WHERE finished_at IS NOT NULL OR 1=1);

-- Phase timeline
CREATE VIEW IF NOT EXISTS phase_timeline AS
SELECT id, ts, action as phase_name
FROM current_session
WHERE category = 'phase'
ORDER BY ts;

-- Opcode execution trace
CREATE VIEW IF NOT EXISTS opcode_trace AS
SELECT 
    id, ts, pc, opcode, name, 
    value as tos, sp, fp
FROM current_session
WHERE category = 'opcode'
ORDER BY ts;

-- Memory operations
CREATE VIEW IF NOT EXISTS memory_ops AS
SELECT id, ts, action, addr, value, value_old, value_new, pc
FROM current_session
WHERE category = 'memory'
ORDER BY ts;

-- Valspace operations (value cells)
CREATE VIEW IF NOT EXISTS valspace_ops AS
SELECT * FROM memory_ops
WHERE addr >= 0x180000 AND addr < 0x1C0000;

-- Atom operations
CREATE VIEW IF NOT EXISTS atom_ops AS
SELECT id, ts, action, atom_index, value, pc
FROM current_session
WHERE category = 'atom'
ORDER BY ts;

-- Stack operations
CREATE VIEW IF NOT EXISTS stack_ops AS
SELECT id, ts, action, value, sp
FROM current_session
WHERE category = 'stack'
ORDER BY ts;

-- ============================================================================
-- CAUSALITY HELPER VIEWS
-- ============================================================================

-- Event with its immediate cause
CREATE VIEW IF NOT EXISTS event_with_cause AS
SELECT 
    e.id, e.ts, e.category, e.action, e.name, e.pc, e.addr, e.value,
    c.id as cause_id, c.ts as cause_ts, c.category as cause_category, 
    c.action as cause_action, c.name as cause_name
FROM current_session e
LEFT JOIN current_session c ON e.cause_id = c.id;

-- ============================================================================
-- UTILITY FUNCTIONS (as table-valued functions via recursive CTEs)
-- ============================================================================

-- Note: SQLite doesn't have stored procedures, so these are provided
-- as query templates in the introspect-query script.

-- Template: Causality chain backward (what led to event X)
-- WITH RECURSIVE chain AS (
--     SELECT * FROM events WHERE id = :event_id
--     UNION ALL
--     SELECT e.* FROM events e
--     INNER JOIN chain c ON c.cause_id = e.id
-- ) SELECT * FROM chain ORDER BY ts;

-- Template: Causality chain forward (what did event X cause)
-- WITH RECURSIVE chain AS (
--     SELECT * FROM events WHERE id = :event_id
--     UNION ALL
--     SELECT e.* FROM events e
--     INNER JOIN chain c ON e.cause_id = c.id
-- ) SELECT * FROM chain ORDER BY ts;

-- Template: Events in time range
-- SELECT * FROM events 
-- WHERE session_id = :session_id AND ts BETWEEN :start AND :end
-- ORDER BY ts;

-- Template: All writes to address
-- SELECT * FROM events
-- WHERE session_id = :session_id AND addr = :addr AND action = 'write'
-- ORDER BY ts;

-- Template: Atom value cell history
-- SELECT * FROM events
-- WHERE session_id = :session_id 
--   AND category = 'memory'
--   AND addr = (0x180000 + :atom_index * 4)
-- ORDER BY ts;
