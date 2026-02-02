/* FILE: arrayopsdefs.h - Array Operation Definitions
 *
 * This file defines the function prototypes for array operations
 * supported by the Medley Lisp emulator. These functions provide
 * access to array elements, both for reading (aref) and writing (aset).
 *
 * HIGH CONFIDENCE: The function prototypes are straightforward
 * array access operations that are well-tested.
 *
 * ARRAY OPERATIONS:
 * - N_OP_misc3: Miscellaneous array operation 3
 * - N_OP_misc4: Miscellaneous array operation 4
 * - N_OP_aref1: Read element from 1-dimensional array
 * - N_OP_aset1: Write element to 1-dimensional array
 * - N_OP_aref2: Read element from 2-dimensional array
 * - N_OP_aset2: Write element to 2-dimensional array
 *
 * ARRAY TYPES:
 * Functions support various array types with type numbers
 * defined in lsptypes.h
 *
 * CROSS-REFERENCE: Implementation in arrayops.c
 * CROSS-REFERENCE: Array structure definitions in array.h
 * CROSS-REFERENCE: Lisp type definitions in lsptypes.h
 * CROSS-REFERENCE: Opcode dispatch in lpmain.c
 */

#ifndef ARRAYOPSDEFS_H
#define ARRAYOPSDEFS_H 1
#include "lispemul.h" /* for LispPTR */
LispPTR N_OP_misc3(LispPTR baseL, LispPTR typenumber, LispPTR inx, int alpha);
LispPTR N_OP_misc4(LispPTR data, LispPTR base, LispPTR typenumber, LispPTR inx, int alpha);
LispPTR N_OP_aref1(LispPTR arrayarg, LispPTR inx);
LispPTR N_OP_aset1(LispPTR data, LispPTR arrayarg, LispPTR inx);
LispPTR N_OP_aref2(LispPTR arrayarg, LispPTR inx0, LispPTR inx1);
LispPTR N_OP_aset2(LispPTR data, LispPTR arrayarg, LispPTR inx0, LispPTR inx1);
#endif
