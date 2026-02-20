/**
 * @file introspect.h
 * @brief Introspection Module for Maiko Emulator
 *
 * This module provides extensive tracing and logging capabilities for
 * debugging the Maiko emulator. All events are stored in a SQLite database
 * for later querying.
 *
 * USAGE:
 *   1. Set INTROSPECT_DB environment variable to database path
 *   2. Call introspect_open() at emulator startup
 *   3. Call introspect_* functions throughout execution
 *   4. Call introspect_close() at shutdown
 *   5. Query the database with introspect-query tool or sqlite3
 *
 * PERFORMANCE:
 *   - Events are batched (10,000 at a time) for high throughput
 *   - Minimal runtime overhead (~1-2% slowdown)
 *   - Database writes happen in background
 *
 * @see introspect-query(1) for querying the database
 */

#ifndef MAIKO_INTROSPECT_H
#define MAIKO_INTROSPECT_H

#include <stdint.h>
#include <stddef.h>

/* Opaque database handle */
typedef struct IntrospectDB IntrospectDB;

/* ============================================================================
 * LIFECYCLE
 * ============================================================================ */

/**
 * Open introspection database.
 *
 * Creates database if not exists, opens existing if present.
 * Initializes schema automatically.
 *
 * @param path Path to SQLite database file.
 *             If NULL, uses INTROSPECT_DB env var or "introspect.db"
 * @return Handle or NULL on error (check errno)
 *
 * Example:
 *   IntrospectDB *db = introspect_open("trace.db");
 */
IntrospectDB *introspect_open(const char *path);

/**
 * Close database and flush any pending events.
 *
 * @param db Database handle (may be NULL)
 */
void introspect_close(IntrospectDB *db);

/**
 * Start a new recording session.
 *
 * Call after introspect_open(). Creates a new session record.
 *
 * @param db Database handle
 * @param sysout_path Path to sysout file (may be NULL)
 * @param notes Optional notes for this session (may be NULL)
 * @return Session ID, or 0 on error
 */
uint64_t introspect_start_session(IntrospectDB *db, const char *sysout_path,
                                  const char *notes);

/**
 * End current session.
 *
 * @param db Database handle
 */
void introspect_end_session(IntrospectDB *db);

/* ============================================================================
 * EVENT LOGGING - PHASES
 * ============================================================================ */

/**
 * Log a phase marker.
 *
 * Phases mark major initialization stages:
 *   - "startup"
 *   - "before_sysout_load"
 *   - "after_sysout_load"
 *   - "build_lisp_map"
 *   - "before_dispatch"
 *   - "shutdown"
 *
 * @param db Database handle
 * @param phase_name Name of the phase
 */
void introspect_phase(IntrospectDB *db, const char *phase_name);

/* ============================================================================
 * EVENT LOGGING - OPCODES
 * ============================================================================ */

/**
 * Log an opcode execution.
 *
 * Call in the dispatch loop after each instruction.
 *
 * @param db Database handle
 * @param pc Program counter (byte offset)
 * @param opcode Raw opcode byte
 * @param name Opcode name (may be NULL for unknown opcodes)
 * @param tos Top-of-stack value
 * @param sp Stack pointer
 * @param fp Frame pointer
 */
void introspect_opcode(IntrospectDB *db, uint64_t pc, uint8_t opcode,
                       const char *name, uint64_t tos, uint64_t sp, uint64_t fp);

/* ============================================================================
 * EVENT LOGGING - MEMORY
 * ============================================================================ */

/**
 * Log a memory read operation.
 *
 * @param db Database handle
 * @param addr Memory address
 * @param value Value read
 * @param pc Program counter (for causality)
 */
void introspect_mem_read(IntrospectDB *db, uint64_t addr, uint64_t value,
                         uint64_t pc);

/**
 * Log a memory write operation.
 *
 * @param db Database handle
 * @param addr Memory address
 * @param old_value Value before write
 * @param new_value Value after write
 * @param pc Program counter (for causality)
 */
void introspect_mem_write(IntrospectDB *db, uint64_t addr, uint64_t old_value,
                          uint64_t new_value, uint64_t pc);

/* ============================================================================
 * EVENT LOGGING - ATOMS
 * ============================================================================ */

/**
 * Log an atom operation.
 *
 * @param db Database handle
 * @param atom_index Atom index
 * @param action "read", "write", "cell_lookup", etc.
 * @param value Value involved in the operation
 * @param pc Program counter (for causality)
 */
void introspect_atom(IntrospectDB *db, uint32_t atom_index, const char *action,
                     uint64_t value, uint64_t pc);

/**
 * Log an atom value cell access (convenience function).
 *
 * Automatically calculates address from atom_index.
 *
 * @param db Database handle
 * @param atom_index Atom index
 * @param action "read" or "write"
 * @param value Value read or written
 * @param pc Program counter
 */
void introspect_atom_cell(IntrospectDB *db, uint32_t atom_index,
                          const char *action, uint64_t value, uint64_t pc);

/* ============================================================================
 * EVENT LOGGING - STACK
 * ============================================================================ */

/**
 * Log a stack operation.
 *
 * @param db Database handle
 * @param action "push", "pop", "tos_update"
 * @param value Value pushed/popped
 * @param sp Stack pointer after operation
 * @param pc Program counter
 */
void introspect_stack(IntrospectDB *db, const char *action,
                      uint64_t value, uint64_t sp, uint64_t pc);

/* ============================================================================
 * EVENT LOGGING - GENERIC
 * ============================================================================ */

/**
 * Log a generic event with custom detail.
 *
 * @param db Database handle
 * @param category Event category
 * @param action Event action
 * @param pc Program counter (may be 0)
 * @param addr Memory address (may be 0)
 * @param value Primary value (may be 0)
 * @param name Symbolic name (may be NULL)
 * @param detail JSON detail string (may be NULL)
 */
void introspect_event(IntrospectDB *db, const char *category, const char *action,
                      uint64_t pc, uint64_t addr, uint64_t value,
                      const char *name, const char *detail);

/* ============================================================================
 * CAUSALITY TRACKING
 * ============================================================================ */

/**
 * Get the ID of the last logged event.
 *
 * Use to set up causality chains.
 *
 * @param db Database handle
 * @return Event ID of last event, or 0 if none
 */
uint64_t introspect_last_event_id(IntrospectDB *db);

/**
 * Set the cause for subsequent events.
 *
 * All events logged after this call will have cause_id = cause_event_id.
 * Call introspect_clear_cause() to stop.
 *
 * @param db Database handle
 * @param cause_event_id Event ID to use as cause
 */
void introspect_set_cause(IntrospectDB *db, uint64_t cause_event_id);

/**
 * Clear the cause for subsequent events.
 *
 * @param db Database handle
 */
void introspect_clear_cause(IntrospectDB *db);

/**
 * Push a cause onto the causality stack.
 *
 * For nested operations (e.g., function calls).
 * The top of the stack is used as cause_id for new events.
 *
 * @param db Database handle
 * @param cause_event_id Event ID to push
 */
void introspect_push_cause(IntrospectDB *db, uint64_t cause_event_id);

/**
 * Pop a cause from the causality stack.
 *
 * @param db Database handle
 */
void introspect_pop_cause(IntrospectDB *db);

/* ============================================================================
 * UTILITY
 * ============================================================================ */

/**
 * Flush pending events to database.
 *
 * Normally called automatically when buffer is full.
 * Call explicitly to ensure events are written before crash/checkpoint.
 *
 * @param db Database handle
 */
void introspect_flush(IntrospectDB *db);

/**
 * Get total event count.
 *
 * @param db Database handle
 * @return Number of events logged this session
 */
uint64_t introspect_event_count(IntrospectDB *db);

/**
 * Get elapsed time since session start.
 *
 * @param db Database handle
 * @return Seconds since session start
 */
double introspect_elapsed_time(IntrospectDB *db);

/**
 * Checkpoint the WAL file to the main database.
 *
 * Forces all WAL content to be written to the main database file.
 * Call before process exit to ensure data durability.
 *
 * @param db Database handle (may be NULL)
 * @return 0 on success, -1 on error
 */
int introspect_checkpoint(IntrospectDB *db);

/**
 * Check if introspection is enabled.
 *
 * @param db Database handle
 * @return 1 if enabled, 0 if disabled
 */
int introspect_is_enabled(IntrospectDB *db);

/**
 * Enable/disable introspection at runtime.
 *
 * When disabled, all introspect_* functions become no-ops.
 *
 * @param db Database handle
 * @param enabled 1 to enable, 0 to disable
 */
void introspect_set_enabled(IntrospectDB *db, int enabled);

/* ============================================================================
 * MEMORY DEBUGGING TABLES
 * ============================================================================ */

/**
 * Record build configuration.
 *
 * Call once at startup after build config is known.
 *
 * @param db Database handle
 * @param bigvm 1 if BIGVM defined, 0 otherwise
 * @param bigatoms 1 if BIGATOMS defined, 0 otherwise
 * @param vals_offset VALS_OFFSET macro value
 * @param atoms_offset ATOMS_OFFSET macro value
 * @param stackspace_offset STACKS_OFFSET macro value
 * @param total_vm_size Total virtual memory size in bytes
 * @param page_size BYTESPER_PAGE value
 */
void introspect_build_config(IntrospectDB *db, int bigvm, int bigatoms,
                             uint64_t vals_offset, uint64_t atoms_offset,
                             uint64_t stackspace_offset,
                             uint64_t total_vm_size, uint64_t page_size);

/**
 * Record runtime configuration.
 *
 * Call once after sysout is loaded.
 *
 * @param db Database handle
 * @param valspace_ptr Actual Valspace pointer
 * @param atomspace_ptr Actual AtomSpace pointer
 * @param stackspace_ptr Actual Stackspace pointer
 * @param sysout_file Path to sysout file
 * @param sysout_size Size of sysout file in bytes
 * @param total_pages_loaded Number of pages loaded from sysout
 * @param sparse_pages_count Number of sparse (not loaded) pages
 */
void introspect_runtime_config(IntrospectDB *db, uint64_t valspace_ptr,
                               uint64_t atomspace_ptr, uint64_t stackspace_ptr,
                               const char *sysout_file, uint64_t sysout_size,
                               uint64_t total_pages_loaded,
                               uint64_t sparse_pages_count);

/**
 * Record a memory snapshot at a phase.
 *
 * @param db Database handle
 * @param phase Phase name ("after_sysout_load", etc.)
 * @param location_name Location identifier ("vals_start", "atom_522_value", etc.)
 * @param address Memory address
 * @param value Value at address
 */
void introspect_memory_snapshot(IntrospectDB *db, const char *phase,
                                const char *location_name, uint64_t address,
                                uint64_t value);

/**
 * Record a memory write.
 *
 * @param db Database handle
 * @param address Memory address written
 * @param old_value Value before write
 * @param new_value Value after write
 * @param pc Program counter
 * @param size Bytes written (1, 2, or 4)
 * @param opcode Opcode causing write (may be 0)
 */
void introspect_memory_write(IntrospectDB *db, uint64_t address,
                             uint64_t old_value, uint64_t new_value,
                             uint64_t pc, uint8_t size, uint8_t opcode);

/**
 * Record a Valspace page status.
 *
 * @param db Database handle
 * @param vp Virtual page number
 * @param address_start Start address of page
 * @param address_end End address of page
 * @param is_sparse 1 if page not in sysout (zero-initialized), 0 if loaded
 * @param fp File page number (only valid if not sparse)
 */
void introspect_vals_page(IntrospectDB *db, uint32_t vp, uint64_t address_start,
                          uint64_t address_end, int is_sparse, uint32_t fp);

/**
 * Record a GVAR execution with page info.
 *
 * @param db Database handle
 * @param pc Program counter
 * @param atom_index Atom index being accessed
 * @param valspace_ptr Valspace pointer value
 * @param calculated_addr Calculated address
 * @param value_read Value read from address
 * @param vp Virtual page of address
 * @param is_sparse 1 if page is sparse
 */
void introspect_gvar_execution(IntrospectDB *db, uint64_t pc, uint32_t atom_index,
                               uint64_t valspace_ptr, uint64_t calculated_addr,
                               uint64_t value_read, uint32_t vp, int is_sparse);

#endif /* MAIKO_INTROSPECT_H */
