/**
 * @file introspect.c
 * @brief Core implementation of introspection module
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "introspect.h"
#include "introspect_internal.h"

/* ============================================================================
 * TIMING
 * ============================================================================ */

#ifdef _WIN32
#include <windows.h>
#else
#include <time.h>
#include <sys/time.h>
#endif

/**
 * Get current timestamp relative to session start.
 */
static double introspect_get_timestamp(IntrospectDB *db) {
#ifdef _WIN32
    LARGE_INTEGER now, freq;
    QueryPerformanceCounter(&now);
    QueryPerformanceFrequency(&freq);
    return (double)(now.QuadPart - db->start_ts.tv_sec) / (double)freq.QuadPart;
#else
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    return (now.tv_sec - db->start_ts.tv_sec) + 
           (now.tv_nsec - db->start_ts.tv_nsec) / 1e9;
#endif
}

/* ============================================================================
 * CAUSALITY STACK
 * ============================================================================ */

static void cause_stack_init(CauseStack *stack) {
    memset(stack->ids, 0, sizeof(stack->ids));
    stack->top = 0;
}

static void cause_stack_push(CauseStack *stack, uint64_t id) {
    if (stack->top < INTROSPECT_CAUSE_STACK_DEPTH) {
        stack->ids[stack->top++] = id;
    }
}

static void cause_stack_pop(CauseStack *stack) {
    if (stack->top > 0) {
        stack->top--;
    }
}

static uint64_t cause_stack_top(CauseStack *stack) {
    if (stack->top > 0) {
        return stack->ids[stack->top - 1];
    }
    return 0;
}

/* ============================================================================
 * EVENT BUFFER
 * ============================================================================ */

static void buffer_init(EventBuffer *buf) {
    memset(buf, 0, sizeof(*buf));
    buf->count = 0;
    buf->next_event_id = 1;  /* Event IDs start at 1 */
}

static EventRecord* buffer_next(IntrospectDB *db) {
    EventBuffer *buf = &db->buffer;
    
    if (buf->count >= INTROSPECT_BATCH_SIZE) {
        /* Buffer full, need to flush */
        extern int introspect_flush_buffer(IntrospectDB *db);
        introspect_flush_buffer(db);
    }
    
    EventRecord *rec = &buf->records[buf->count++];
    memset(rec, 0, sizeof(*rec));
    rec->cause_id = cause_stack_top(&db->cause_stack);
    if (db->pending_cause_id != 0) {
        rec->cause_id = db->pending_cause_id;
    }
    
    return rec;
}

/* ============================================================================
 * PUBLIC API - LIFECYCLE
 * ============================================================================ */

IntrospectDB* introspect_open(const char *path) {
    IntrospectDB *db = calloc(1, sizeof(IntrospectDB));
    if (!db) {
        return NULL;
    }
    
    /* Get database path */
    if (path) {
        db->db_path = strdup(path);
    } else {
        const char *env_path = getenv("INTROSPECT_DB");
        db->db_path = strdup(env_path ? env_path : "introspect.db");
    }
    
    if (!db->db_path) {
        free(db);
        return NULL;
    }
    
    /* Initialize buffers */
    buffer_init(&db->buffer);
    cause_stack_init(&db->cause_stack);
    
    db->enabled = 1;
    db->session_active = 0;
    db->schema_initialized = 0;
    
    /* Open database (implemented in introspect_db.c) */
    extern int introspect_db_open(IntrospectDB *db);
    if (introspect_db_open(db) != 0) {
        free(db->db_path);
        free(db);
        return NULL;
    }
    
    return db;
}

void introspect_close(IntrospectDB *db) {
    if (!db) return;
    
    /* Flush any pending events */
    introspect_flush(db);
    
    /* End session if active */
    if (db->session_active) {
        introspect_end_session(db);
    }
    
    /* Close database */
    extern void introspect_db_close(IntrospectDB *db);
    introspect_db_close(db);
    
    free(db->db_path);
    free(db);
}

uint64_t introspect_start_session(IntrospectDB *db, const char *sysout_path, 
                                   const char *notes) {
    INTROSPECT_CHECK_VAL(db, 0);
    
    /* Initialize timing */
#ifdef _WIN32
    LARGE_INTEGER start;
    QueryPerformanceCounter(&start);
    db->start_ts.tv_sec = start.QuadPart;
#else
    clock_gettime(CLOCK_MONOTONIC, &db->start_ts);
#endif
    db->start_time = 0.0;
    
    /* Start session in database */
    extern uint64_t introspect_db_start_session(IntrospectDB *db, 
                                                 const char *sysout_path, 
                                                 const char *notes);
    db->current_session_id = introspect_db_start_session(db, sysout_path, notes);
    db->session_active = 1;
    db->total_events = 0;
    
    return db->current_session_id;
}

void introspect_end_session(IntrospectDB *db) {
    INTROSPECT_CHECK(db);
    
    if (!db->session_active) return;
    
    /* Flush remaining events */
    introspect_flush(db);
    
    /* End session in database */
    extern void introspect_db_end_session(IntrospectDB *db);
    introspect_db_end_session(db);
    
    db->session_active = 0;
}

/* ============================================================================
 * PUBLIC API - PHASES
 * ============================================================================ */

void introspect_phase(IntrospectDB *db, const char *phase_name) {
    INTROSPECT_CHECK(db);
    
    EventRecord *rec = buffer_next(db);
    rec->ts = introspect_get_timestamp(db);
    rec->session_id = db->current_session_id;
    strncpy(rec->category, "phase", INTROSPECT_MAX_CATEGORY - 1);
    strncpy(rec->action, "mark", INTROSPECT_MAX_ACTION - 1);
    strncpy(rec->name, phase_name ? phase_name : "unknown", INTROSPECT_MAX_NAME - 1);
}

/* ============================================================================
 * PUBLIC API - OPCODES
 * ============================================================================ */

void introspect_opcode(IntrospectDB *db, uint64_t pc, uint8_t opcode,
                       const char *name, uint64_t tos, uint64_t sp, uint64_t fp) {
    INTROSPECT_CHECK(db);
    
    EventRecord *rec = buffer_next(db);
    rec->ts = introspect_get_timestamp(db);
    rec->session_id = db->current_session_id;
    strncpy(rec->category, "opcode", INTROSPECT_MAX_CATEGORY - 1);
    strncpy(rec->action, "execute", INTROSPECT_MAX_ACTION - 1);
    rec->pc = pc;
    rec->sp = sp;
    rec->fp = fp;
    rec->opcode = opcode;
    rec->value = tos;
    if (name) {
        strncpy(rec->name, name, INTROSPECT_MAX_NAME - 1);
    }
}

/* ============================================================================
 * PUBLIC API - MEMORY
 * ============================================================================ */

void introspect_mem_read(IntrospectDB *db, uint64_t addr, uint64_t value, 
                          uint64_t pc) {
    INTROSPECT_CHECK(db);
    
    EventRecord *rec = buffer_next(db);
    rec->ts = introspect_get_timestamp(db);
    rec->session_id = db->current_session_id;
    strncpy(rec->category, "memory", INTROSPECT_MAX_CATEGORY - 1);
    strncpy(rec->action, "read", INTROSPECT_MAX_ACTION - 1);
    rec->addr = addr;
    rec->value = value;
    rec->pc = pc;
}

void introspect_mem_write(IntrospectDB *db, uint64_t addr, uint64_t old_value,
                           uint64_t new_value, uint64_t pc) {
    INTROSPECT_CHECK(db);
    
    EventRecord *rec = buffer_next(db);
    rec->ts = introspect_get_timestamp(db);
    rec->session_id = db->current_session_id;
    strncpy(rec->category, "memory", INTROSPECT_MAX_CATEGORY - 1);
    strncpy(rec->action, "write", INTROSPECT_MAX_ACTION - 1);
    rec->addr = addr;
    rec->value_old = old_value;
    rec->value_new = new_value;
    rec->value = new_value;
    rec->pc = pc;
}

/* ============================================================================
 * PUBLIC API - ATOMS
 * ============================================================================ */

void introspect_atom(IntrospectDB *db, uint32_t atom_index, const char *action,
                     uint64_t value, uint64_t pc) {
    INTROSPECT_CHECK(db);
    
    EventRecord *rec = buffer_next(db);
    rec->ts = introspect_get_timestamp(db);
    rec->session_id = db->current_session_id;
    strncpy(rec->category, "atom", INTROSPECT_MAX_CATEGORY - 1);
    strncpy(rec->action, action ? action : "unknown", INTROSPECT_MAX_ACTION - 1);
    rec->atom_index = atom_index;
    rec->value = value;
    rec->pc = pc;
}

void introspect_atom_cell(IntrospectDB *db, uint32_t atom_index, 
                          const char *action, uint64_t value, uint64_t pc) {
    INTROSPECT_CHECK(db);
    
    /* Calculate value cell address */
    uint64_t addr = 0x180000 + ((uint64_t)atom_index * 4);
    
    EventRecord *rec = buffer_next(db);
    rec->ts = introspect_get_timestamp(db);
    rec->session_id = db->current_session_id;
    strncpy(rec->category, "atom", INTROSPECT_MAX_CATEGORY - 1);
    strncpy(rec->action, action ? action : "unknown", INTROSPECT_MAX_ACTION - 1);
    rec->atom_index = atom_index;
    rec->addr = addr;
    rec->value = value;
    rec->pc = pc;
}

/* ============================================================================
 * PUBLIC API - STACK
 * ============================================================================ */

void introspect_stack(IntrospectDB *db, const char *action, 
                      uint64_t value, uint64_t sp, uint64_t pc) {
    INTROSPECT_CHECK(db);
    
    EventRecord *rec = buffer_next(db);
    rec->ts = introspect_get_timestamp(db);
    rec->session_id = db->current_session_id;
    strncpy(rec->category, "stack", INTROSPECT_MAX_CATEGORY - 1);
    strncpy(rec->action, action ? action : "unknown", INTROSPECT_MAX_ACTION - 1);
    rec->value = value;
    rec->sp = sp;
    rec->pc = pc;
}

/* ============================================================================
 * PUBLIC API - GENERIC
 * ============================================================================ */

void introspect_event(IntrospectDB *db, const char *category, const char *action,
                      uint64_t pc, uint64_t addr, uint64_t value,
                      const char *name, const char *detail) {
    INTROSPECT_CHECK(db);
    
    EventRecord *rec = buffer_next(db);
    rec->ts = introspect_get_timestamp(db);
    rec->session_id = db->current_session_id;
    if (category) strncpy(rec->category, category, INTROSPECT_MAX_CATEGORY - 1);
    if (action) strncpy(rec->action, action, INTROSPECT_MAX_ACTION - 1);
    rec->pc = pc;
    rec->addr = addr;
    rec->value = value;
    if (name) strncpy(rec->name, name, INTROSPECT_MAX_NAME - 1);
    if (detail) strncpy(rec->detail, detail, INTROSPECT_MAX_DETAIL - 1);
}

/* ============================================================================
 * PUBLIC API - CAUSALITY
 * ============================================================================ */

uint64_t introspect_last_event_id(IntrospectDB *db) {
    INTROSPECT_CHECK_VAL(db, 0);
    return db->buffer.next_event_id - 1;
}

void introspect_set_cause(IntrospectDB *db, uint64_t cause_event_id) {
    INTROSPECT_CHECK(db);
    db->pending_cause_id = cause_event_id;
}

void introspect_clear_cause(IntrospectDB *db) {
    INTROSPECT_CHECK(db);
    db->pending_cause_id = 0;
}

void introspect_push_cause(IntrospectDB *db, uint64_t cause_event_id) {
    INTROSPECT_CHECK(db);
    cause_stack_push(&db->cause_stack, cause_event_id);
}

void introspect_pop_cause(IntrospectDB *db) {
    INTROSPECT_CHECK(db);
    cause_stack_pop(&db->cause_stack);
}

/* ============================================================================
 * PUBLIC API - UTILITY
 * ============================================================================ */

void introspect_flush(IntrospectDB *db) {
    INTROSPECT_CHECK(db);
    
    extern int introspect_flush_buffer(IntrospectDB *db);
    introspect_flush_buffer(db);
}

uint64_t introspect_event_count(IntrospectDB *db) {
    INTROSPECT_CHECK_VAL(db, 0);
    return db->total_events;
}

double introspect_elapsed_time(IntrospectDB *db) {
    INTROSPECT_CHECK_VAL(db, 0.0);
    return introspect_get_timestamp(db);
}

int introspect_is_enabled(IntrospectDB *db) {
    return db && db->enabled;
}

void introspect_set_enabled(IntrospectDB *db, int enabled) {
    if (db) {
        db->enabled = enabled;
    }
}
