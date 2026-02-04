#ifndef EXECUTION_TRACE_H
#define EXECUTION_TRACE_H 1

/* $Id: execution_trace.h,v 1.0 2026-02-02 Exp $ (C) Copyright Venue, All Rights Reserved  */

/************************************************************************/
/*									*/
/*	(C) Copyright 2026 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

#include <stdio.h>
#include "lispemul.h" /* for LispPTR, DLword */

/* ============================================================================
 * FILE: execution_trace.h - Execution Trace Logging for Maiko Emulator
 * ============================================================================
 *
 * PURPOSE:
 *   This header provides the interface for execution trace logging that
 *   matches the Zig emulator's trace format for parity comparison.
 *
 * CONFIDENCE LEVEL: HIGH (95%)
 *   - Matches Zig execution trace format
 *   - Supports EMULATOR_MAX_STEPS environment variable
 *   - Generates unified trace format for rapid comparison
 *
 * USAGE:
 *   - Call init_execution_trace() at emulator startup
 *   - Call log_execution_trace() in dispatch loop for each instruction
 *   - Call cleanup_execution_trace() at emulator shutdown
 *
 * ENVIRONMENT VARIABLES:
 *   - EMULATOR_MAX_STEPS: Maximum number of instructions to log (0 = unlimited)
 *
 * CROSS-REFERENCES:
 *   - Zig implementation: @zaiko/src/vm/execution_trace.zig
 *   - Comparison script: @scripts/compare_emulator_execution.sh
 *   - Debugging techniques: @documentation/core/critical-debugging-technique.typ
 */

/* Execution trace state structure */
typedef struct {
    FILE *log_file_ptr;      /* Log file handle */
    unsigned long instruction_count; /* Number of instructions logged */
    int max_steps;            /* Maximum steps to log (from EMULATOR_MAX_STEPS) */
    int enabled;              /* Whether tracing is enabled */
} ExecutionTrace;

/* Forward declarations */
int init_execution_trace(ExecutionTrace *trace, const char *log_path);
int log_execution_trace(ExecutionTrace *trace,
                        unsigned char opcode,
                        unsigned long pc_byte_offset,
                        LispPTR tos_value,
                        unsigned long sp_offset,
                        unsigned long fp_offset,
                        const char *opcode_name);
void cleanup_execution_trace(ExecutionTrace *trace);
int should_continue_logging(ExecutionTrace *trace);

/* Global trace API wrappers */
int init_global_execution_trace(const char *log_path);
int log_global_execution_trace(unsigned char opcode,
                              unsigned long pc_byte_offset,
                              LispPTR tos_value,
                              unsigned long sp_offset,
                              unsigned long fp_offset,
                              const char *opcode_name);
void cleanup_global_execution_trace(void);
int should_continue_global_logging(void);

/* Initialize execution trace logging */
/* Returns 0 on success, -1 on failure */
int init_execution_trace(ExecutionTrace *trace, const char *log_path);

/* Log a single instruction execution */
/* Returns 0 on success, -1 on failure */
int log_execution_trace(ExecutionTrace *trace,
                        unsigned char opcode,
                        unsigned long pc_byte_offset,
                        LispPTR tos_value,
                        unsigned long sp_offset,
                        unsigned long fp_offset,
                        const char *opcode_name);

/* Cleanup and close execution trace */
void cleanup_execution_trace(ExecutionTrace *trace);

/* Check if we should continue logging */
int should_continue_logging(ExecutionTrace *trace);

#endif /* EXECUTION_TRACE_H */