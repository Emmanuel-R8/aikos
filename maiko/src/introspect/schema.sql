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
CREATE INDEX IF NOT EXISTS idx_events_cause ON events(session_id, cause_id);
CREATE INDEX IF NOT EXISTS idx_events_cause_rev ON events(session_id, id, cause_id);

-- Value-based queries (find where specific values appear)
CREATE INDEX IF NOT EXISTS idx_events_value ON events(session_id, value);

-- Composite indexes for common query patterns
CREATE INDEX IF NOT EXISTS idx_events_cat_act ON events(session_id, category, action);
CREATE INDEX IF NOT EXISTS idx_events_cat_addr ON events(session_id, category, addr);
CREATE INDEX IF NOT EXISTS idx_events_cat_pc ON events(session_id, category, pc);

-- ============================================================================
-- BUILD CONFIGURATION TABLE
-- ============================================================================

CREATE TABLE IF NOT EXISTS build_config (
    id INTEGER PRIMARY KEY CHECK (id = 1),  -- Singleton row
    bigvm INTEGER,                          -- 1 if BIGVM defined
    bigatoms INTEGER,                       -- 1 if BIGATOMS defined
    vals_offset INTEGER,                    -- VALS_OFFSET macro value
    atoms_offset INTEGER,                   -- ATOMS_OFFSET macro value
    stackspace_offset INTEGER,              -- STACKS_OFFSET macro value
    total_vm_size INTEGER,                  -- Total virtual memory size (bytes)
    page_size INTEGER,                      -- BYTESPER_PAGE value
    created_at TEXT                         -- When this config was captured
);

-- ============================================================================
-- RUNTIME CONFIGURATION TABLE
-- ============================================================================

CREATE TABLE IF NOT EXISTS runtime_config (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    session_id INTEGER NOT NULL,
    valspace_ptr INTEGER,                   -- Actual Valspace pointer at runtime
    atomspace_ptr INTEGER,                  -- Actual AtomSpace pointer
    stackspace_ptr INTEGER,                 -- Actual Stackspace pointer
    sysout_file TEXT,                       -- Path to sysout file
    sysout_size INTEGER,                    -- Size of sysout file
    total_pages_loaded INTEGER,             -- Number of pages loaded from sysout
    sparse_pages_count INTEGER,             -- Number of sparse (not loaded) pages
    FOREIGN KEY (session_id) REFERENCES sessions(id)
);

-- ============================================================================
-- MEMORY SNAPSHOTS TABLE
-- ============================================================================

CREATE TABLE IF NOT EXISTS memory_snapshots (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    ts REAL,
    session_id INTEGER NOT NULL,
    phase TEXT NOT NULL,                    -- 'after_sysout_load', 'after_build_lisp_map', etc.
    location_name TEXT NOT NULL,            -- 'vals_start', 'atom_522_value', etc.
    address INTEGER NOT NULL,
    value INTEGER,
    FOREIGN KEY (session_id) REFERENCES sessions(id)
);

-- Index for phase-based queries
CREATE INDEX IF NOT EXISTS idx_memory_snapshots_phase ON memory_snapshots(session_id, phase);
CREATE INDEX IF NOT EXISTS idx_memory_snapshots_location ON memory_snapshots(session_id, location_name);

-- ============================================================================
-- MEMORY WRITES TABLE
-- ============================================================================

CREATE TABLE IF NOT EXISTS memory_writes (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    ts REAL,
    session_id INTEGER NOT NULL,
    pc INTEGER,                             -- PC where write occurred
    address INTEGER NOT NULL,
    old_value INTEGER,
    new_value INTEGER NOT NULL,
    vp INTEGER,                             -- Virtual page of address
    size INTEGER DEFAULT 4,                 -- Bytes written (1, 2, or 4)
    opcode INTEGER,                         -- Opcode that caused write
    event_id INTEGER,                       -- Link to events table
    FOREIGN KEY (session_id) REFERENCES sessions(id),
    FOREIGN KEY (event_id) REFERENCES events(id)
);

-- Indexes for write queries
CREATE INDEX IF NOT EXISTS idx_memory_writes_addr ON memory_writes(session_id, address);
CREATE INDEX IF NOT EXISTS idx_memory_writes_ts ON memory_writes(session_id, ts);
CREATE INDEX IF NOT EXISTS idx_memory_writes_vp ON memory_writes(session_id, vp);

-- ============================================================================
-- VALSPACE PAGES TABLE
-- ============================================================================

CREATE TABLE IF NOT EXISTS vals_pages (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    session_id INTEGER NOT NULL,
    vp INTEGER NOT NULL,                    -- Virtual page number
    address_start INTEGER NOT NULL,         -- Start address of this page
    address_end INTEGER NOT NULL,           -- End address of this page
    is_sparse INTEGER NOT NULL,             -- 1 if not in sysout (zero-initialized)
    fp INTEGER,                             -- File page number (NULL if sparse)
    FOREIGN KEY (session_id) REFERENCES sessions(id)
);

-- Index for sparse page lookup
CREATE INDEX IF NOT EXISTS idx_vals_pages_sparse ON vals_pages(session_id, is_sparse);
CREATE INDEX IF NOT EXISTS idx_vals_pages_vp ON vals_pages(session_id, vp);

-- ============================================================================
-- GVAR EXECUTIONS TABLE
-- ============================================================================

CREATE TABLE IF NOT EXISTS gvar_executions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    ts REAL,
    session_id INTEGER NOT NULL,
    pc INTEGER,
    atom_index INTEGER NOT NULL,
    valspace_ptr INTEGER,                   -- Runtime Valspace pointer
    calculated_addr INTEGER,                -- Address calculated from atom_index
    value_read INTEGER,
    vp INTEGER,                             -- Virtual page of calculated_addr
    is_sparse INTEGER,                      -- 1 if page is sparse
    event_id INTEGER,                       -- Link to events table
    FOREIGN KEY (session_id) REFERENCES sessions(id),
    FOREIGN KEY (event_id) REFERENCES events(id)
);

-- Index for GVAR queries
CREATE INDEX IF NOT EXISTS idx_gvar_atom ON gvar_executions(session_id, atom_index);
CREATE INDEX IF NOT EXISTS idx_gvar_sparse ON gvar_executions(session_id, is_sparse);

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
WHERE addr >= 0xC0000 AND addr < 0x100000;  -- Updated for actual VALS_OFFSET

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
-- DEBUGGING VIEWS
-- ============================================================================

-- Atom 522 value through phases
CREATE VIEW IF NOT EXISTS atom_522_history AS
SELECT ts, phase, printf('0x%08X', value) as value_hex
FROM memory_snapshots
WHERE location_name = 'atom_522_value'
ORDER BY ts;

-- Sparse Valspace pages
CREATE VIEW IF NOT EXISTS sparse_vals_pages AS
SELECT vp, printf('0x%X', address_start) as start_addr
FROM vals_pages
WHERE is_sparse = 1
ORDER BY vp;

-- All writes to sparse pages
CREATE VIEW IF NOT EXISTS writes_to_sparse AS
SELECT mw.ts, printf('0x%X', mw.address) as addr, 
       printf('0x%08X', mw.old_value) as old_val,
       printf('0x%08X', mw.new_value) as new_val
FROM memory_writes mw
JOIN vals_pages vp ON mw.vp = vp.vp AND mw.session_id = vp.session_id
WHERE vp.is_sparse = 1
ORDER BY mw.ts;

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
-- SELECT * FROM memory_writes
-- WHERE session_id = :session_id AND address = :addr
-- ORDER BY ts;

-- Template: Atom value cell history
-- SELECT * FROM memory_writes
-- WHERE session_id = :session_id 
--   AND address = (SELECT valspace_ptr FROM runtime_config WHERE session_id = :session_id) + :atom_index * 4
-- ORDER BY ts;
