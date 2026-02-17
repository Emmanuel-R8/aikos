/**
 * @file introspect_internal.h
 * @brief Internal types for introspection module
 */

#ifndef MAIKO_INTROSPECT_INTERNAL_H
#define MAIKO_INTROSPECT_INTERNAL_H

#include <stdint.h>
#include <time.h>

/* ============================================================================
 * CONFIGURATION
 * ============================================================================ */

/** Number of events to buffer before flushing to database */
#define INTROSPECT_BATCH_SIZE 10000

/** Maximum string lengths */
#define INTROSPECT_MAX_NAME 64
#define INTROSPECT_MAX_ACTION 32
#define INTROSPECT_MAX_CATEGORY 32
#define INTROSPECT_MAX_DETAIL 256

/** Causality stack depth */
#define INTROSPECT_CAUSE_STACK_DEPTH 16

/* ============================================================================
 * EVENT RECORD
 * ============================================================================ */

/**
 * Single event record for buffering.
 */
typedef struct {
    /* Timestamp */
    double ts;                              /* Seconds since session start */
    
    /* Session */
    uint32_t session_id;
    
    /* Categorization */
    char category[INTROSPECT_MAX_CATEGORY];
    char action[INTROSPECT_MAX_ACTION];
    
    /* Execution context */
    uint64_t pc;
    uint64_t sp;
    uint64_t fp;
    
    /* Memory context */
    uint64_t addr;
    uint32_t atom_index;
    
    /* Values */
    uint64_t value;
    uint64_t value_old;
    uint64_t value_new;
    
    /* Naming */
    char name[INTROSPECT_MAX_NAME];
    uint8_t opcode;
    
    /* Extensibility */
    char detail[INTROSPECT_MAX_DETAIL];
    
    /* Causality */
    uint64_t cause_id;
    
} EventRecord;

/* ============================================================================
 * EVENT BUFFER
 * ============================================================================ */

/**
 * Batch buffer for efficient writes.
 */
typedef struct {
    EventRecord records[INTROSPECT_BATCH_SIZE];
    int count;
    uint64_t next_event_id;                 /* For causality tracking */
} EventBuffer;

/* ============================================================================
 * CAUSALITY STACK
 * ============================================================================ */

/**
 * Stack for nested causality tracking.
 */
typedef struct {
    uint64_t ids[INTROSPECT_CAUSE_STACK_DEPTH];
    int top;
} CauseStack;

/* ============================================================================
 * DATABASE HANDLE
 * ============================================================================ */

/**
 * Complete introspection database handle.
 */
struct IntrospectDB {
    /* SQLite handle */
    void *db;                               /* sqlite3* */
    void *stmt_insert;                      /* sqlite3_stmt* for insert */
    
    /* Session info */
    uint64_t current_session_id;
    char *db_path;
    
    /* Buffering */
    EventBuffer buffer;
    
    /* Causality */
    CauseStack cause_stack;
    uint64_t pending_cause_id;              /* Set via introspect_set_cause */
    
    /* Timing */
    double start_time;                      /* When session started */
    struct timespec start_ts;               /* For precise timing */
    
    /* Statistics */
    uint64_t total_events;
    uint64_t flush_count;
    
    /* State */
    int enabled;
    int session_active;
    int schema_initialized;
};

/* ============================================================================
 * HELPER MACROS
 * ============================================================================ */

/** Check if db is valid and enabled */
#define INTROSPECT_CHECK(db) \
    do { \
        if ((db) == NULL || !(db)->enabled) return; \
    } while(0)

/** Check if db is valid and enabled, return value */
#define INTROSPECT_CHECK_VAL(db, retval) \
    do { \
        if ((db) == NULL || !(db)->enabled) return (retval); \
    } while(0)

/** Get current timestamp */
#define INTROSPECT_NOW(db) \
    (introspect_get_timestamp(db))

#endif /* MAIKO_INTROSPECT_INTERNAL_H */
