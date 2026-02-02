/* FILE: arithopsdefs.h - Arithmetic Opcode Definitions
 *
 * This file defines the function prototypes for arithmetic operations
 * supported by the Medley Lisp emulator. These functions implement
 * various arithmetic opcodes, including both signed and unsigned versions.
 *
 * HIGH CONFIDENCE: The function prototypes clearly define the arithmetic
 * operations, and the implementation in arithops.c is well-tested.
 *
 * ARITHMETIC OPERATIONS:
 * - Addition: N_OP_plus2, N_OP_iplus2, N_OP_iplusn, N_OP_boxiplus
 * - Subtraction: N_OP_difference, N_OP_idifference, N_OP_idifferencen, N_OP_boxidiff
 * - Logical operations: N_OP_logxor, N_OP_logand, N_OP_logor
 * - Comparison: N_OP_greaterp, N_OP_igreaterp
 * - Multiplication: N_OP_times2, N_OP_itimes2
 * - Division: N_OP_quot, N_OP_iquot, N_OP_iremainder
 * - Number creation: N_OP_makenumber
 *
 * OPCODE TYPES:
 * - Integer operations: N_OP_i* variants (signed)
 * - Floating-point operations: Regular variants
 * - Boxed operations: N_OP_box* variants
 *
 * CROSS-REFERENCE: Implementation in arithops.c
 * CROSS-REFERENCE: Lisp type definitions in lsptypes.h
 * CROSS-REFERENCE: Opcode dispatch in lpmain.c
 */

#ifndef ARITHOPSDEFS_H
#define ARITHOPSDEFS_H 1
#include "lispemul.h" /* for LispPTR */
LispPTR N_OP_plus2(LispPTR tosm1, LispPTR tos);
LispPTR N_OP_iplus2(LispPTR tosm1, LispPTR tos);
LispPTR N_OP_difference(LispPTR tosm1, LispPTR tos);
LispPTR N_OP_idifference(LispPTR tosm1, LispPTR tos);
LispPTR N_OP_logxor(LispPTR tosm1, LispPTR tos);
LispPTR N_OP_logand(LispPTR tosm1, LispPTR tos);
LispPTR N_OP_logor(LispPTR tosm1, LispPTR tos);
LispPTR N_OP_greaterp(LispPTR tosm1, LispPTR tos);
LispPTR N_OP_igreaterp(LispPTR tosm1, LispPTR tos);
LispPTR N_OP_iplusn(LispPTR tos, int n);
LispPTR N_OP_idifferencen(LispPTR tos, int n);
LispPTR N_OP_makenumber(LispPTR tosm1, LispPTR tos);
LispPTR N_OP_boxiplus(LispPTR a, LispPTR tos);
LispPTR N_OP_boxidiff(LispPTR a, LispPTR tos);
LispPTR N_OP_times2(LispPTR tosm1, LispPTR tos);
LispPTR N_OP_itimes2(LispPTR tosm1, LispPTR tos);
LispPTR N_OP_quot(LispPTR tosm1, LispPTR tos);
LispPTR N_OP_iquot(LispPTR tosm1, LispPTR tos);
LispPTR N_OP_iremainder(LispPTR tosm1, LispPTR tos);
#endif
