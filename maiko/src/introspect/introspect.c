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

#include <time.h>
#include <sys/time.h>

/**
 * Get current timestamp relative to session start.
 */
static double introspect_get_timestamp(IntrospectDB *db)
{
  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);
  return (now.tv_sec - db->start_ts.tv_sec) +
         (now.tv_nsec - db->start_ts.tv_nsec) / 1e9;
}

/* ============================================================================
 * CAUSALITY STACK
 * ============================================================================ */

static void cause_stack_init(CauseStack *stack)
{
  memset(stack->ids, 0, sizeof(stack->ids));
  stack->top = 0;
}

static void cause_stack_push(CauseStack *stack, uint64_t id)
{
  if (stack->top < INTROSPECT_CAUSE_STACK_DEPTH)
  {
    stack->ids[stack->top++] = id;
  }
}

static void cause_stack_pop(CauseStack *stack)
{
  if (stack->top > 0)
  {
    stack->top--;
  }
}

static uint64_t cause_stack_top(CauseStack *stack)
{
  if (stack->top > 0)
  {
    return stack->ids[stack->top - 1];
  }
  return 0;
}

/* ============================================================================
 * EVENT BUFFER
 * ============================================================================ */

static void buffer_init(EventBuffer *buf)
{
  memset(buf, 0, sizeof(*buf));
  buf->count = 0;
  buf->next_event_id = 1; /* Event IDs start at 1 */
}

static EventRecord *buffer_next(IntrospectDB *db)
{
  EventBuffer *buf = &db->buffer;

  if (buf->count >= INTROSPECT_BATCH_SIZE)
  {
    /* Buffer full, need to flush */
    extern int introspect_flush_buffer(IntrospectDB * db);
    introspect_flush_buffer(db);
  }

  EventRecord *rec = &buf->records[buf->count++];
  memset(rec, 0, sizeof(*rec));
  rec->cause_id = cause_stack_top(&db->cause_stack);
  if (db->pending_cause_id != 0)
  {
    rec->cause_id = db->pending_cause_id;
  }

  return rec;
}

/* ============================================================================
 * PUBLIC API - LIFECYCLE
 * ============================================================================ */

IntrospectDB *introspect_open(const char *path)
{
  IntrospectDB *db = calloc(1, sizeof(IntrospectDB));
  if (!db)
  {
    return NULL;
  }

  /* Get database path */
  if (path)
  {
    db->db_path = strdup(path);
  }
  else
  {
    const char *env_path = getenv("INTROSPECT_DB");
    db->db_path = strdup(env_path ? env_path : "introspect.db");
  }

  if (!db->db_path)
  {
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
  extern int introspect_db_open(IntrospectDB * db);
  if (introspect_db_open(db) != 0)
  {
    free(db->db_path);
    free(db);
    return NULL;
  }

  return db;
}

void introspect_close(IntrospectDB *db)
{
  if (!db)
    return;

  /* Flush any pending events */
  introspect_flush(db);

  /* End session if active */
  if (db->session_active)
  {
    introspect_end_session(db);
  }

  /* Close database */
  extern void introspect_db_close(IntrospectDB * db);
  introspect_db_close(db);

  free(db->db_path);
  free(db);
}

uint64_t introspect_start_session(IntrospectDB *db, const char *sysout_path,
                                  const char *notes)
{
  INTROSPECT_CHECK_VAL(db, 0);

  /* Initialize timing */
  clock_gettime(CLOCK_MONOTONIC, &db->start_ts);
  db->start_time = 0.0;

  /* Start session in database */
  extern uint64_t introspect_db_start_session(IntrospectDB * db,
                                              const char *sysout_path,
                                              const char *notes);
  db->current_session_id = introspect_db_start_session(db, sysout_path, notes);
  db->session_active = 1;
  db->total_events = 0;

  return db->current_session_id;
}

void introspect_end_session(IntrospectDB *db)
{
  INTROSPECT_CHECK(db);

  if (!db->session_active)
    return;

  /* Flush remaining events */
  introspect_flush(db);

  /* End session in database */
  extern void introspect_db_end_session(IntrospectDB * db);
  introspect_db_end_session(db);

  db->session_active = 0;
}

/* ============================================================================
 * PUBLIC API - PHASES
 * ============================================================================ */

void introspect_phase(IntrospectDB *db, const char *phase_name)
{
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
                       const char *name, uint64_t tos, uint64_t sp, uint64_t fp)
{
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
  if (name)
  {
    strncpy(rec->name, name, INTROSPECT_MAX_NAME - 1);
  }
}

/* ============================================================================
 * PUBLIC API - MEMORY
 * ============================================================================ */

void introspect_mem_read(IntrospectDB *db, uint64_t addr, uint64_t value,
                         uint64_t pc)
{
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
                          uint64_t new_value, uint64_t pc)
{
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
                     uint64_t value, uint64_t pc)
{
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
                          const char *action, uint64_t value, uint64_t pc)
{
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
                      uint64_t value, uint64_t sp, uint64_t pc)
{
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
                      const char *name, const char *detail)
{
  INTROSPECT_CHECK(db);

  EventRecord *rec = buffer_next(db);
  rec->ts = introspect_get_timestamp(db);
  rec->session_id = db->current_session_id;
  if (category)
    strncpy(rec->category, category, INTROSPECT_MAX_CATEGORY - 1);
  if (action)
    strncpy(rec->action, action, INTROSPECT_MAX_ACTION - 1);
  rec->pc = pc;
  rec->addr = addr;
  rec->value = value;
  if (name)
    strncpy(rec->name, name, INTROSPECT_MAX_NAME - 1);
  if (detail)
    strncpy(rec->detail, detail, INTROSPECT_MAX_DETAIL - 1);
}

/* ============================================================================
 * PUBLIC API - CAUSALITY
 * ============================================================================ */

uint64_t introspect_last_event_id(IntrospectDB *db)
{
  INTROSPECT_CHECK_VAL(db, 0);
  return db->buffer.next_event_id - 1;
}

void introspect_set_cause(IntrospectDB *db, uint64_t cause_event_id)
{
  INTROSPECT_CHECK(db);
  db->pending_cause_id = cause_event_id;
}

void introspect_clear_cause(IntrospectDB *db)
{
  INTROSPECT_CHECK(db);
  db->pending_cause_id = 0;
}

void introspect_push_cause(IntrospectDB *db, uint64_t cause_event_id)
{
  INTROSPECT_CHECK(db);
  cause_stack_push(&db->cause_stack, cause_event_id);
}

void introspect_pop_cause(IntrospectDB *db)
{
  INTROSPECT_CHECK(db);
  cause_stack_pop(&db->cause_stack);
}

/* ============================================================================
 * PUBLIC API - UTILITY
 * ============================================================================ */

void introspect_flush(IntrospectDB *db)
{
  INTROSPECT_CHECK(db);

  extern int introspect_flush_buffer(IntrospectDB * db);
  introspect_flush_buffer(db);
}

uint64_t introspect_event_count(IntrospectDB *db)
{
  INTROSPECT_CHECK_VAL(db, 0);
  return db->total_events;
}

double introspect_elapsed_time(IntrospectDB *db)
{
  INTROSPECT_CHECK_VAL(db, 0.0);
  return introspect_get_timestamp(db);
}

int introspect_is_enabled(IntrospectDB *db)
{
  return db && db->enabled;
}

void introspect_set_enabled(IntrospectDB *db, int enabled)
{
  if (db)
  {
    db->enabled = enabled;
  }
}

/* ============================================================================
 * MEMORY DEBUGGING TABLE IMPLEMENTATIONS
 * ============================================================================ */

void introspect_build_config(IntrospectDB *db, int bigvm, int bigatoms,
                              uint64_t vals_offset, uint64_t atoms_offset,
                              uint64_t stackspace_offset,
                              uint64_t total_vm_size, uint64_t page_size)
{
  INTROSPECT_CHECK(db);
  
  char sql[512];
  snprintf(sql, sizeof(sql),
    "INSERT OR REPLACE INTO build_config "
    "(id, bigvm, bigatoms, vals_offset, atoms_offset, stackspace_offset, "
    "total_vm_size, page_size, created_at) VALUES (1, %d, %d, %lu, %lu, %lu, %lu, %lu, datetime('now'));",
    bigvm, bigatoms, 
    (unsigned long)vals_offset, (unsigned long)atoms_offset,
    (unsigned long)stackspace_offset,
    (unsigned long)total_vm_size, (unsigned long)page_size);
  
  char *err_msg = NULL;
  int rc = sqlite3_exec(db->db, sql, NULL, NULL, &err_msg);
  if (rc != SQLITE_OK)
  {
    fprintf(stderr, "introspect_build_config error: %s\n", err_msg);
    sqlite3_free(err_msg);
  }
}

void introspect_runtime_config(IntrospectDB *db, uint64_t valspace_ptr,
                                uint64_t atomspace_ptr, uint64_t stackspace_ptr,
                                const char *sysout_file, uint64_t sysout_size,
                                uint64_t total_pages_loaded,
                                uint64_t sparse_pages_count)
{
  INTROSPECT_CHECK(db);
  
  char sql[1024];
  snprintf(sql, sizeof(sql),
    "INSERT INTO runtime_config "
    "(session_id, valspace_ptr, atomspace_ptr, stackspace_ptr, sysout_file, "
    "sysout_size, total_pages_loaded, sparse_pages_count) VALUES "
    "(%lu, %lu, %lu, %lu, '%s', %lu, %lu, %lu);",
    (unsigned long)db->current_session_id,
    (unsigned long)valspace_ptr, (unsigned long)atomspace_ptr,
    (unsigned long)stackspace_ptr,
    sysout_file ? sysout_file : "",
    (unsigned long)sysout_size,
    (unsigned long)total_pages_loaded,
    (unsigned long)sparse_pages_count);
  
  char *err_msg = NULL;
  int rc = sqlite3_exec(db->db, sql, NULL, NULL, &err_msg);
  if (rc != SQLITE_OK)
  {
    fprintf(stderr, "introspect_runtime_config error: %s\n", err_msg);
    sqlite3_free(err_msg);
  }
}

void introspect_memory_snapshot(IntrospectDB *db, const char *phase,
                                  const char *location_name, uint64_t address,
                                  uint64_t value)
{
  INTROSPECT_CHECK(db);
  
  double ts = introspect_get_timestamp(db);
  
  char sql[512];
  snprintf(sql, sizeof(sql),
    "INSERT INTO memory_snapshots "
    "(ts, session_id, phase, location_name, address, value) VALUES "
    "(%.9f, %lu, '%s', '%s', %lu, %lu);",
    ts, (unsigned long)db->current_session_id,
    phase, location_name,
    (unsigned long)address, (unsigned long)value);
  
  char *err_msg = NULL;
  int rc = sqlite3_exec(db->db, sql, NULL, NULL, &err_msg);
  if (rc != SQLITE_OK)
  {
    fprintf(stderr, "introspect_memory_snapshot error: %s\n", err_msg);
    sqlite3_free(err_msg);
  }
}

void introspect_memory_write(IntrospectDB *db, uint64_t address,
                              uint64_t old_value, uint64_t new_value,
                              uint64_t pc, uint8_t size, uint8_t opcode)
{
  INTROSPECT_CHECK(db);
  
  double ts = introspect_get_timestamp(db);
  uint32_t vp = address / 4096;  /* Assuming 4KB pages for now */
  
  char sql[512];
  snprintf(sql, sizeof(sql),
    "INSERT INTO memory_writes "
    "(ts, session_id, pc, address, old_value, new_value, vp, size, opcode) VALUES "
    "(%.9f, %lu, %lu, %lu, %lu, %lu, %u, %u, %u);",
    ts, (unsigned long)db->current_session_id,
    (unsigned long)pc, (unsigned long)address,
    (unsigned long)old_value, (unsigned long)new_value,
    vp, size, opcode);
  
  char *err_msg = NULL;
  int rc = sqlite3_exec(db->db, sql, NULL, NULL, &err_msg);
  if (rc != SQLITE_OK)
  {
    fprintf(stderr, "introspect_memory_write error: %s\n", err_msg);
    sqlite3_free(err_msg);
  }
}

void introspect_vals_page(IntrospectDB *db, uint32_t vp, uint64_t address_start,
                           uint64_t address_end, int is_sparse, uint32_t fp)
{
  INTROSPECT_CHECK(db);
  
  char sql[512];
  if (is_sparse)
  {
    snprintf(sql, sizeof(sql),
      "INSERT INTO vals_pages "
      "(session_id, vp, address_start, address_end, is_sparse, fp) VALUES "
      "(%lu, %u, %lu, %lu, 1, NULL);",
      (unsigned long)db->current_session_id, vp,
      (unsigned long)address_start, (unsigned long)address_end);
  }
  else
  {
    snprintf(sql, sizeof(sql),
      "INSERT INTO vals_pages "
      "(session_id, vp, address_start, address_end, is_sparse, fp) VALUES "
      "(%lu, %u, %lu, %lu, 0, %u);",
      (unsigned long)db->current_session_id, vp,
      (unsigned long)address_start, (unsigned long)address_end, fp);
  }
  
  char *err_msg = NULL;
  int rc = sqlite3_exec(db->db, sql, NULL, NULL, &err_msg);
  if (rc != SQLITE_OK)
  {
    fprintf(stderr, "introspect_vals_page error: %s\n", err_msg);
    sqlite3_free(err_msg);
  }
}

void introspect_gvar_execution(IntrospectDB *db, uint64_t pc, uint32_t atom_index,
                                uint64_t valspace_ptr, uint64_t calculated_addr,
                                uint64_t value_read, uint32_t vp, int is_sparse)
{
  INTROSPECT_CHECK(db);
  
  double ts = introspect_get_timestamp(db);
  
  char sql[512];
  snprintf(sql, sizeof(sql),
    "INSERT INTO gvar_executions "
    "(ts, session_id, pc, atom_index, valspace_ptr, calculated_addr, "
    "value_read, vp, is_sparse) VALUES "
    "(%.9f, %lu, %lu, %u, %lu, %lu, %lu, %u, %d);",
    ts, (unsigned long)db->current_session_id,
    (unsigned long)pc, atom_index,
    (unsigned long)valspace_ptr, (unsigned long)calculated_addr,
    (unsigned long)value_read, vp, is_sparse);
  
  char *err_msg = NULL;
  int rc = sqlite3_exec(db->db, sql, NULL, NULL, &err_msg);
  if (rc != SQLITE_OK)
  {
    fprintf(stderr, "introspect_gvar_execution error: %s\n", err_msg);
    sqlite3_free(err_msg);
  }
}
