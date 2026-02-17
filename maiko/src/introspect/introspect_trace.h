/**
 * @file introspect_trace.h
 * @brief Introspection tracing wrappers for memory and register operations
 * 
 * This file provides wrapper macros that log memory operations to the
 * introspection database when INTROSPECT_ENABLED is defined.
 * 
 * Usage: Include this after lispemul.h in files that need tracing.
 */

#ifndef INTROSPECT_TRACE_H
#define INTROSPECT_TRACE_H

#include "introspect/introspect.h"

#ifdef INTROSPECT_ENABLED

/* External global introspection handle from main.c */
extern IntrospectDB *g_introspect;

/* ============================================================================
 * MEMORY READ/WRITE TRACING
 * ============================================================================ */

/**
 * Trace a memory read operation.
 * Call this after reading to log what was read.
 */
#define INTROSPECT_MEM_READ(addr, value) do { \
    if (g_introspect) { \
        introspect_mem_read(g_introspect, (uint64_t)(addr), (uint64_t)(value), 0); \
    } \
} while(0)

/**
 * Trace a memory write operation.
 * Call this before writing to log old and new values.
 */
#define INTROSPECT_MEM_WRITE(addr, old_value, new_value) do { \
    if (g_introspect) { \
        introspect_mem_write(g_introspect, (uint64_t)(addr), (uint64_t)(old_value), (uint64_t)(new_value), 0); \
    } \
} while(0)

/* ============================================================================
 * STACK OPERATION TRACING
 * ============================================================================ */

/**
 * Trace a stack push operation.
 */
#define INTROSPECT_STACK_PUSH(value, sp) do { \
    if (g_introspect) { \
        introspect_stack(g_introspect, "push", (uint64_t)(value), (uint64_t)(sp), 0); \
    } \
} while(0)

/**
 * Trace a stack pop operation.
 */
#define INTROSPECT_STACK_POP(value, sp) do { \
    if (g_introspect) { \
        introspect_stack(g_introspect, "pop", (uint64_t)(value), (uint64_t)(sp), 0); \
    } \
} while(0)

/* ============================================================================
 * REGISTER TRACING
 * ============================================================================ */

/**
 * Trace a register change.
 */
#define INTROSPECT_REG_CHANGE(reg_name, old_value, new_value) do { \
    if (g_introspect) { \
        char detail[64]; \
        snprintf(detail, sizeof(detail), "register:%s", reg_name); \
        introspect_event(g_introspect, "register", "change", 0, 0, (uint64_t)(new_value), \
                         reg_name, detail); \
    } \
} while(0)

/* ============================================================================
 * ATOM OPERATION TRACING
 * ============================================================================ */

/**
 * Trace an atom value cell access.
 */
#define INTROSPECT_ATOM_READ(atom_index, value) do { \
    if (g_introspect) { \
        introspect_atom_cell(g_introspect, (uint32_t)(atom_index), "read", (uint64_t)(value), 0); \
    } \
} while(0)

#define INTROSPECT_ATOM_WRITE(atom_index, old_value, new_value) do { \
    if (g_introspect) { \
        char detail[64]; \
        snprintf(detail, sizeof(detail), "old:0x%lX", (unsigned long)(old_value)); \
        introspect_atom_cell(g_introspect, (uint32_t)(atom_index), "write", (uint64_t)(new_value), 0); \
    } \
} while(0)

#else /* !INTROSPECT_ENABLED */

/* No-op macros when introspection is disabled */
#define INTROSPECT_MEM_READ(addr, value)
#define INTROSPECT_MEM_WRITE(addr, old_value, new_value)
#define INTROSPECT_STACK_PUSH(value, sp)
#define INTROSPECT_STACK_POP(value, sp)
#define INTROSPECT_REG_CHANGE(reg_name, old_value, new_value)
#define INTROSPECT_ATOM_READ(atom_index, value)
#define INTROSPECT_ATOM_WRITE(atom_index, old_value, new_value)

#endif /* INTROSPECT_ENABLED */

#endif /* INTROSPECT_TRACE_H */
