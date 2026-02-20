/**
 * @file introspect_db.c
 * @brief SQLite database integration for introspection module
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include <sqlite3.h>

#include "introspect.h"
#include "introspect_internal.h"

/* ============================================================================
 * SCHEMA
 * ============================================================================ */

/* Schema is loaded from schema.sql at runtime */

/* ============================================================================
 * DATABASE OPEN/CLOSE
 * ============================================================================ */

int introspect_db_open(IntrospectDB *db)
{
  int rc;

  /* Open SQLite database */
  rc = sqlite3_open(db->db_path, (sqlite3 **)&db->db);
  if (rc != SQLITE_OK)
  {
    fprintf(stderr, "introspect: Cannot open database %s: %s\n",
            db->db_path, sqlite3_errmsg((sqlite3 *)db->db));
    return -1;
  }

  /* Enable WAL mode */
  rc = sqlite3_exec((sqlite3 *)db->db,
                    "PRAGMA journal_mode = WAL; PRAGMA synchronous = NORMAL;",
                    NULL, NULL, NULL);
  if (rc != SQLITE_OK)
  {
    fprintf(stderr, "introspect: Cannot set WAL mode: %s\n",
            sqlite3_errmsg((sqlite3 *)db->db));
    /* Non-fatal, continue */
  }

  /* Load schema from file or use embedded */
  const char *schema_path = NULL; /* TODO: Find schema.sql */

  /* Read and execute schema */
  FILE *schema_file = fopen("src/introspect/schema.sql", "r");
  if (schema_file)
  {
    fseek(schema_file, 0, SEEK_END);
    long schema_size = ftell(schema_file);
    fseek(schema_file, 0, SEEK_SET);

    char *schema = malloc(schema_size + 1);
    if (schema)
    {
      fread(schema, 1, schema_size, schema_file);
      schema[schema_size] = '\0';

      rc = sqlite3_exec((sqlite3 *)db->db, schema, NULL, NULL, NULL);
      if (rc != SQLITE_OK)
      {
        fprintf(stderr, "introspect: Schema load failed: %s\n",
                sqlite3_errmsg((sqlite3 *)db->db));
      }
      else
      {
        db->schema_initialized = 1;
      }

      free(schema);
    }
    fclose(schema_file);
  }

  if (!db->schema_initialized)
  {
    /* Try inline schema */
    /* For now, create minimal tables */
    const char *minimal_schema =
        "CREATE TABLE IF NOT EXISTS sessions ("
        "  id INTEGER PRIMARY KEY AUTOINCREMENT,"
        "  started_at TEXT NOT NULL,"
        "  finished_at TEXT,"
        "  sysout_path TEXT,"
        "  notes TEXT"
        ");"
        "CREATE TABLE IF NOT EXISTS events ("
        "  id INTEGER PRIMARY KEY AUTOINCREMENT,"
        "  ts REAL NOT NULL,"
        "  session_id INTEGER NOT NULL,"
        "  category TEXT NOT NULL,"
        "  action TEXT NOT NULL,"
        "  pc INTEGER,"
        "  sp INTEGER,"
        "  fp INTEGER,"
        "  addr INTEGER,"
        "  atom_index INTEGER,"
        "  value INTEGER,"
        "  value_old INTEGER,"
        "  value_new INTEGER,"
        "  name TEXT,"
        "  opcode INTEGER,"
        "  detail TEXT,"
        "  cause_id INTEGER"
        ");"
        "CREATE TABLE IF NOT EXISTS build_config ("
        "  id INTEGER PRIMARY KEY CHECK (id = 1),"
        "  bigvm INTEGER,"
        "  bigatoms INTEGER,"
        "  vals_offset INTEGER,"
        "  atoms_offset INTEGER,"
        "  stackspace_offset INTEGER,"
        "  total_vm_size INTEGER,"
        "  page_size INTEGER,"
        "  created_at TEXT"
        ");"
        "CREATE TABLE IF NOT EXISTS runtime_config ("
        "  id INTEGER PRIMARY KEY AUTOINCREMENT,"
        "  session_id INTEGER NOT NULL,"
        "  valspace_ptr INTEGER,"
        "  atomspace_ptr INTEGER,"
        "  stackspace_ptr INTEGER,"
        "  sysout_file TEXT,"
        "  sysout_size INTEGER,"
        "  total_pages_loaded INTEGER,"
        "  sparse_pages_count INTEGER"
        ");"
        "CREATE TABLE IF NOT EXISTS memory_snapshots ("
        "  id INTEGER PRIMARY KEY AUTOINCREMENT,"
        "  ts REAL,"
        "  session_id INTEGER NOT NULL,"
        "  phase TEXT NOT NULL,"
        "  location_name TEXT NOT NULL,"
        "  address INTEGER NOT NULL,"
        "  value INTEGER"
        ");"
        "CREATE TABLE IF NOT EXISTS memory_writes ("
        "  id INTEGER PRIMARY KEY AUTOINCREMENT,"
        "  ts REAL,"
        "  session_id INTEGER NOT NULL,"
        "  pc INTEGER,"
        "  address INTEGER NOT NULL,"
        "  old_value INTEGER,"
        "  new_value INTEGER NOT NULL,"
        "  vp INTEGER,"
        "  size INTEGER DEFAULT 4,"
        "  opcode INTEGER"
        ");"
        "CREATE TABLE IF NOT EXISTS vals_pages ("
        "  id INTEGER PRIMARY KEY AUTOINCREMENT,"
        "  session_id INTEGER NOT NULL,"
        "  vp INTEGER NOT NULL,"
        "  address_start INTEGER NOT NULL,"
        "  address_end INTEGER NOT NULL,"
        "  is_sparse INTEGER NOT NULL,"
        "  fp INTEGER"
        ");"
        "CREATE TABLE IF NOT EXISTS gvar_executions ("
        "  id INTEGER PRIMARY KEY AUTOINCREMENT,"
        "  ts REAL,"
        "  session_id INTEGER NOT NULL,"
        "  pc INTEGER,"
        "  atom_index INTEGER NOT NULL,"
        "  valspace_ptr INTEGER,"
        "  calculated_addr INTEGER,"
        "  value_read INTEGER,"
        "  vp INTEGER,"
        "  is_sparse INTEGER"
        ");";

    rc = sqlite3_exec((sqlite3 *)db->db, minimal_schema, NULL, NULL, NULL);
    if (rc != SQLITE_OK)
    {
      fprintf(stderr, "introspect: Minimal schema failed: %s\n",
              sqlite3_errmsg((sqlite3 *)db->db));
      sqlite3_close((sqlite3 *)db->db);
      return -1;
    }
    db->schema_initialized = 1;
  }

  /* Prepare insert statement */
  const char *insert_sql =
      "INSERT INTO events (ts, session_id, category, action, pc, sp, fp, "
      "                    addr, atom_index, value, value_old, value_new, "
      "                    name, opcode, detail, cause_id) "
      "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);";

  rc = sqlite3_prepare_v2((sqlite3 *)db->db, insert_sql, -1,
                          (sqlite3_stmt **)&db->stmt_insert, NULL);
  if (rc != SQLITE_OK)
  {
    fprintf(stderr, "introspect: Cannot prepare insert: %s\n",
            sqlite3_errmsg((sqlite3 *)db->db));
    sqlite3_close((sqlite3 *)db->db);
    return -1;
  }

  return 0;
}

void introspect_db_close(IntrospectDB *db)
{
  if (db->stmt_insert)
  {
    sqlite3_finalize((sqlite3_stmt *)db->stmt_insert);
    db->stmt_insert = NULL;
  }

  if (db->db)
  {
    sqlite3_close((sqlite3 *)db->db);
    db->db = NULL;
  }
}

/* ============================================================================
 * WAL CHECKPOINT
 * ============================================================================ */

int introspect_checkpoint(IntrospectDB *db)
{
  if (!db || !db->db)
  {
    return -1;
  }

  /* Flush any pending events first */
  extern int introspect_flush_buffer(IntrospectDB * db);
  introspect_flush_buffer(db);

  /* Checkpoint the WAL file with TRUNCATE mode.
   * TRUNCATE mode:
   *   - Writes all WAL content to the main database
   *   - Truncates the WAL file to zero bytes
   *   - Blocks other writers during checkpoint
   * This ensures the database is in a consistent state after crash.
   */
  int rc = sqlite3_exec((sqlite3 *)db->db,
                        "PRAGMA wal_checkpoint(TRUNCATE);",
                        NULL, NULL, NULL);
  if (rc != SQLITE_OK)
  {
    fprintf(stderr, "introspect: WAL checkpoint failed: %s\n",
            sqlite3_errmsg((sqlite3 *)db->db));
    return -1;
  }

  return 0;
}

/* ============================================================================
 * SESSION MANAGEMENT
 * ============================================================================ */

uint64_t introspect_db_start_session(IntrospectDB *db,
                                     const char *sysout_path,
                                     const char *notes)
{
  sqlite3_stmt *stmt;
  const char *sql =
      "INSERT INTO sessions (started_at, sysout_path, notes) "
      "VALUES (datetime('now'), ?, ?);";

  int rc = sqlite3_prepare_v2((sqlite3 *)db->db, sql, -1, &stmt, NULL);
  if (rc != SQLITE_OK)
  {
    fprintf(stderr, "introspect: Cannot prepare session insert: %s\n",
            sqlite3_errmsg((sqlite3 *)db->db));
    return 0;
  }

  sqlite3_bind_text(stmt, 1, sysout_path ? sysout_path : "", -1, SQLITE_TRANSIENT);
  sqlite3_bind_text(stmt, 2, notes ? notes : "", -1, SQLITE_TRANSIENT);

  rc = sqlite3_step(stmt);
  sqlite3_finalize(stmt);

  if (rc != SQLITE_DONE)
  {
    fprintf(stderr, "introspect: Session insert failed: %s\n",
            sqlite3_errmsg((sqlite3 *)db->db));
    return 0;
  }

  return (uint64_t)sqlite3_last_insert_rowid((sqlite3 *)db->db);
}

void introspect_db_end_session(IntrospectDB *db)
{
  sqlite3_stmt *stmt;
  const char *sql =
      "UPDATE sessions SET finished_at = datetime('now') WHERE id = ?;";

  int rc = sqlite3_prepare_v2((sqlite3 *)db->db, sql, -1, &stmt, NULL);
  if (rc == SQLITE_OK)
  {
    sqlite3_bind_int64(stmt, 1, db->current_session_id);
    sqlite3_step(stmt);
    sqlite3_finalize(stmt);
  }
}

/* ============================================================================
 * BUFFER FLUSH
 * ============================================================================ */

int introspect_flush_buffer(IntrospectDB *db)
{
  EventBuffer *buf = &db->buffer;

  if (buf->count == 0)
  {
    return 0;
  }

  /* Begin transaction */
  int rc = sqlite3_exec((sqlite3 *)db->db, "BEGIN TRANSACTION;", NULL, NULL, NULL);
  if (rc != SQLITE_OK)
  {
    fprintf(stderr, "introspect: Cannot begin transaction: %s\n",
            sqlite3_errmsg((sqlite3 *)db->db));
    return -1;
  }

  /* Insert all buffered events */
  for (int i = 0; i < buf->count; i++)
  {
    EventRecord *rec = &buf->records[i];
    sqlite3_stmt *stmt = (sqlite3_stmt *)db->stmt_insert;

    /* Bind parameters */
    sqlite3_bind_double(stmt, 1, rec->ts);
    sqlite3_bind_int64(stmt, 2, rec->session_id);
    sqlite3_bind_text(stmt, 3, rec->category, -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(stmt, 4, rec->action, -1, SQLITE_TRANSIENT);
    sqlite3_bind_int64(stmt, 5, rec->pc);
    sqlite3_bind_int64(stmt, 6, rec->sp);
    sqlite3_bind_int64(stmt, 7, rec->fp);
    sqlite3_bind_int64(stmt, 8, rec->addr);
    sqlite3_bind_int64(stmt, 9, rec->atom_index);
    sqlite3_bind_int64(stmt, 10, rec->value);
    sqlite3_bind_int64(stmt, 11, rec->value_old);
    sqlite3_bind_int64(stmt, 12, rec->value_new);
    sqlite3_bind_text(stmt, 13, rec->name, -1, SQLITE_TRANSIENT);
    sqlite3_bind_int(stmt, 14, rec->opcode);
    sqlite3_bind_text(stmt, 15, rec->detail, -1, SQLITE_TRANSIENT);
    sqlite3_bind_int64(stmt, 16, rec->cause_id);

    /* Execute */
    rc = sqlite3_step(stmt);
    if (rc != SQLITE_DONE)
    {
      fprintf(stderr, "introspect: Insert failed: %s\n",
              sqlite3_errmsg((sqlite3 *)db->db));
    }

    /* Reset for next use */
    sqlite3_reset(stmt);
    sqlite3_clear_bindings(stmt);
  }

  /* Commit transaction */
  rc = sqlite3_exec((sqlite3 *)db->db, "COMMIT;", NULL, NULL, NULL);
  if (rc != SQLITE_OK)
  {
    fprintf(stderr, "introspect: Cannot commit transaction: %s\n",
            sqlite3_errmsg((sqlite3 *)db->db));
    return -1;
  }

  /* Update statistics */
  db->total_events += buf->count;
  db->flush_count++;

  /* Clear buffer */
  buf->count = 0;

  return 0;
}
