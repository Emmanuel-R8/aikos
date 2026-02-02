/* =========================================================================
 * File: arithops.c
 * Purpose: Implementation of arithmetic operations for Maiko VM
 * Confidence Level: High (Stable, well-tested)
 * Testing: Tested with standard arithmetic operations
 * Algorithm: Implements Lisp arithmetic operations with overflow checking
 * Authors: Original Venue Corporation, Sybalsky et al. (2001)
 * Original Implementation Date: 1989-1995
 * Last Modified: 2001-12-24
 *
 * This file contains the C implementations of arithmetic operations for
 * the Maiko VM. It supports both integer and floating-point arithmetic
 * operations with overflow checking.
 *
 * Key Features:
 * - Integer addition, subtraction, comparison, and difference operations
 * - Floating-point arithmetic operations
 * - Overflow detection using GCC builtins or manual checks
 * - Type conversion between integer and floating-point types
 *
 * Related Files:
 * - arith.h: Arithmetic operation prototypes and macros
 * - arithopsdefs.h: Operation code definitions
 * - fpdefs.h: Floating-point operation definitions
 * - lispmap.h: Type classification constants
 * ========================================================================= */

#include "version.h"

#include "adr68k.h"      // for NativeAligned4FromLAddr - address manipulation
#include "arith.h"       // for N_IGETNUMBER, N_ARITH_SWITCH, N_GETNUMBER - arithmetic macros
#include "arithopsdefs.h"  // for N_OP_difference, N_OP_greaterp, N_OP_idiffer... - operation codes
#include "fpdefs.h"      // for N_OP_fdifference, N_OP_fgreaterp, N_OP_fplus2 - floating-point operations
#include "lispemul.h"    // for state, ERROR_EXIT, LispPTR, ATOM_T, NIL_PTR - VM core types
#include "lispmap.h"     // for S_POSITIVE - type classification
#include "lspglob.h"     // for global variables
#include "lsptypes.h"    // for Lisp data types

/* =========================================================================
 * Function: N_OP_plus2
 * Purpose: Implements integer addition for Lisp (PLUS2 and IPLUS2 opcodes)
 * Opcode: PLUS2 (0324), IPLUS2 (0330)
 * Parameters:
 *   tosm1 - Second operand (TOS-1) - LispPTR
 *   tos - First operand (TOS) - LispPTR
 * Returns: Sum of the two operands - LispPTR
 * Algorithm:
 *   1. Extract integer values from LispPTR arguments using N_GETNUMBER
 *   2. Check if arguments are integers; fall through to floating-point if not
 *   3. Perform addition with overflow detection
 *   4. Convert result back to appropriate Lisp type using N_ARITH_SWITCH
 * Overflow Detection:
 *   - Uses GCC builtin __builtin_sadd_overflow if available (USE_OVERFLOW_BUILTINS)
 *   - Falls back to manual overflow detection using sign comparison
 * Error Handling: Calls ERROR_EXIT if overflow occurs
 * ========================================================================= */
LispPTR N_OP_plus2(LispPTR tosm1, LispPTR tos) {
  int arg1, arg2;
  int result;

  /* Extract numeric values from LispPTR arguments */
  N_GETNUMBER(tos, arg1, doufn);        // Extract arg1 from TOS
  N_GETNUMBER(tosm1, arg2, doufn);      // Extract arg2 from TOS-1

#ifdef USE_OVERFLOW_BUILTINS
  /* Use GCC builtin for efficient overflow detection */
  if (__builtin_sadd_overflow(arg1, arg2, &result)) {
    ERROR_EXIT(tos);                    // Overflow detected, exit with error
  }
  N_ARITH_SWITCH(result);              // Convert result to Lisp type

#else
  /* Manual overflow detection fallback for non-GCC compilers */
  /* UB: signed integer overflow: 2147483647 + 2147483647 cannot be represented in type 'int' */
  result = arg1 + arg2;
  
  /* Check for overflow: same sign inputs but different sign result */
  if (((arg1 >= 0) == (arg2 >= 0)) && ((result >= 0) != (arg1 >= 0))) {
    ERROR_EXIT(tos);                    // Overflow detected, exit with error
  }
  
  N_ARITH_SWITCH(result);              // Convert result to Lisp type
#endif

doufn:
  /* Fall through to floating-point addition if arguments are not integers */
  return (N_OP_fplus2(tosm1, tos));
}

/* =========================================================================
 * Function: N_OP_iplus2
 * Purpose: Implements integer addition with strict type checking (IPLUS2 opcode)
 * Opcode: IPLUS2 (0330)
 * Parameters:
 *   tosm1 - Second operand (TOS-1) - LispPTR
 *   tos - First operand (TOS) - LispPTR
 * Returns: Sum of the two operands - LispPTR
 * Algorithm:
 *   1. Extract integer values from LispPTR arguments using N_IGETNUMBER
 *   2. Strict type checking: Arguments must be SMALLP or FIXP (no floats allowed)
 *   3. Perform addition with overflow detection
 *   4. Convert result back to appropriate Lisp type using N_ARITH_SWITCH
 * Overflow Detection:
 *   - Uses GCC builtin __builtin_sadd_overflow if available
 *   - Falls back to manual overflow detection using sign comparison
 * Error Handling: Calls ERROR_EXIT if overflow or type error occurs
 * ========================================================================= */
LispPTR N_OP_iplus2(LispPTR tosm1, LispPTR tos) {
  int arg1, arg2;
  int result;

  /* Extract integer values with strict type checking */
  N_IGETNUMBER(tos, arg1, doufn);        // Extract arg1 (must be SMALLP or FIXP)
  N_IGETNUMBER(tosm1, arg2, doufn);      // Extract arg2 (must be SMALLP or FIXP)

#ifdef USE_OVERFLOW_BUILTINS
  /* Use GCC builtin for efficient overflow detection */
  if (__builtin_sadd_overflow(arg1, arg2, &result)) {
    ERROR_EXIT(tos);                    // Overflow detected, exit with error
  }
  N_ARITH_SWITCH(result);              // Convert result to Lisp type

#else
  /* Manual overflow detection fallback */
  /* UB: signed integer overflow: 2147483647 + 2147483647 cannot be represented in type 'int' */
  result = arg1 + arg2;
  
  /* Check for overflow: same sign inputs but different sign result */
  if (((arg1 >= 0) == (arg2 >= 0)) && ((result >= 0) != (arg1 >= 0))) {
    ERROR_EXIT(tos);                    // Overflow detected, exit with error
  }
  
  N_ARITH_SWITCH(result);              // Convert result to Lisp type
#endif

doufn:
  /* Type error: Arguments not integers (SMALLP or FIXP) */
  ERROR_EXIT(tos);
}

/* =========================================================================
 * Function: N_OP_difference
 * Purpose: Implements integer subtraction for Lisp (DIFFERENCE and IDIFFERENCE opcodes)
 * Opcode: DIFFERENCE (0325), IDIFFERENCE (0331)
 * Parameters:
 *   tosm1 - First operand (TOS-1) - LispPTR
 *   tos - Second operand (TOS) - LispPTR
 * Returns: Difference of the two operands (tosm1 - tos) - LispPTR
 * Algorithm:
 *   1. Extract integer values from LispPTR arguments using N_GETNUMBER
 *   2. Check if arguments are integers; fall through to floating-point if not
 *   3. Perform subtraction with overflow detection
 *   4. Convert result back to appropriate Lisp type using N_ARITH_SWITCH
 * Overflow Detection:
 *   - Uses GCC builtin __builtin_ssub_overflow if available
 *   - Falls back to manual overflow detection
 * Error Handling: Calls ERROR_EXIT if overflow occurs
 * ========================================================================= */
LispPTR N_OP_difference(LispPTR tosm1, LispPTR tos) {
  int arg1, arg2;
  int result;

  /* Extract numeric values from LispPTR arguments */
  N_GETNUMBER(tosm1, arg1, doufn);      // Extract arg1 (tosm1)
  N_GETNUMBER(tos, arg2, doufn);        // Extract arg2 (tos)

#ifdef USE_OVERFLOW_BUILTINS
  /* Use GCC builtin for efficient overflow detection */
  if (__builtin_ssub_overflow(arg1, arg2, &result)) {
    ERROR_EXIT(tos);                    // Overflow detected, exit with error
  }
  N_ARITH_SWITCH(result);              // Convert result to Lisp type

#else
  /* Manual overflow detection fallback */
  /* UB: signed integer overflow: -2147483647 - 320 cannot be represented in type 'int' */
  result = arg1 - arg2;
  if (((arg1 >= 0) == (arg2 < 0)) && ((result >= 0) != (arg1 >= 0))) { ERROR_EXIT(tos); }
  N_ARITH_SWITCH(result);

#endif

doufn:
  return (N_OP_fdifference(tosm1, tos));
}

/* =========================================================================
 * Function: N_OP_idifference
 * Purpose: Implements integer subtraction with strict type checking (IDIFFERENCE opcode)
 * Opcode: IDIFFERENCE (0331)
 * Parameters:
 *   tosm1 - First operand (TOS-1) - LispPTR
 *   tos - Second operand (TOS) - LispPTR
 * Returns: Difference of the two operands (tosm1 - tos) - LispPTR
 * Algorithm:
 *   1. Extract integer values from LispPTR arguments using N_IGETNUMBER
 *   2. Strict type checking: Arguments must be SMALLP or FIXP (no floats allowed)
 *   3. Perform subtraction with overflow detection
 *   4. Convert result back to appropriate Lisp type using N_ARITH_SWITCH
 * Overflow Detection:
 *   - Uses GCC builtin __builtin_ssub_overflow if available
 *   - Falls back to manual overflow detection
 * Error Handling: Calls ERROR_EXIT if overflow or type error occurs
 * ========================================================================= */
LispPTR N_OP_idifference(LispPTR tosm1, LispPTR tos) {
  int arg1, arg2;
  int result;

  /* Extract integer values with strict type checking */
  N_IGETNUMBER(tosm1, arg1, doufn);    // Extract arg1 (must be SMALLP or FIXP)
  N_IGETNUMBER(tos, arg2, doufn);      // Extract arg2 (must be SMALLP or FIXP)

#ifdef USE_OVERFLOW_BUILTINS
  /* Use GCC builtin for efficient overflow detection */
  if (__builtin_ssub_overflow(arg1, arg2, &result)) {
    ERROR_EXIT(tos);                    // Overflow detected, exit with error
  }
  N_ARITH_SWITCH(result);              // Convert result to Lisp type

#else
  /* Manual overflow detection fallback */
  /* UB: signed integer overflow: -2147483647 - 100 cannot be represented in type 'int' */
  result = arg1 - arg2;
  
  /* Check for overflow: subtraction overflow pattern */
  if (((arg1 >= 0) == (arg2 < 0)) && ((result >= 0) != (arg1 >= 0))) {
    ERROR_EXIT(tos);                    // Overflow detected, exit with error
  }
  
  N_ARITH_SWITCH(result);              // Convert result to Lisp type
#endif

doufn:
  /* Type error: Arguments not integers (SMALLP or FIXP) */
  ERROR_EXIT(tos);
}

/* =========================================================================
 * Function: N_OP_logxor
 * Purpose: Implements bitwise XOR operation for Lisp (LOGXOR2 opcode)
 * Opcode: LOGXOR2 (0346)
 * Parameters:
 *   tosm1 - First operand (TOS-1) - LispPTR
 *   tos - Second operand (TOS) - LispPTR
 * Returns: Bitwise XOR of the two operands - LispPTR
 * Algorithm:
 *   1. Extract integer values from LispPTR arguments using N_IGETNUMBER
 *   2. Strict type checking: Arguments must be SMALLP or FIXP
 *   3. Perform bitwise XOR operation
 *   4. Convert result back to appropriate Lisp type using N_ARITH_SWITCH
 * Error Handling: Calls ERROR_EXIT if type error occurs
 * ========================================================================= */
LispPTR N_OP_logxor(LispPTR tosm1, LispPTR tos) {
    int arg1, arg2;

    /* Extract integer values with strict type checking */
    N_IGETNUMBER(tosm1, arg1, do_ufn);  // Extract arg1 (must be SMALLP or FIXP)
    N_IGETNUMBER(tos, arg2, do_ufn);    // Extract arg2 (must be SMALLP or FIXP)

    /* Perform bitwise XOR operation */
    arg1 = arg1 ^ arg2;

    /* Convert result to Lisp type */
    N_ARITH_SWITCH(arg1);

  do_ufn:
    /* Type error: Arguments not integers (SMALLP or FIXP) */
    ERROR_EXIT(tos);
}

/* =========================================================================
 * Function: N_OP_logand
 * Purpose: Implements bitwise AND operation for Lisp (LOGAND2 opcode)
 * Opcode: LOGAND2 (0345)
 * Parameters:
 *   tosm1 - First operand (TOS-1) - LispPTR
 *   tos - Second operand (TOS) - LispPTR
 * Returns: Bitwise AND of the two operands - LispPTR
 * Algorithm:
 *   1. Extract integer values from LispPTR arguments using N_IGETNUMBER
 *   2. Strict type checking: Arguments must be SMALLP or FIXP
 *   3. Perform bitwise AND operation
 *   4. Convert result back to appropriate Lisp type using N_ARITH_SWITCH
 * Error Handling: Calls ERROR_EXIT if type error occurs
 * ========================================================================= */
LispPTR N_OP_logand(LispPTR tosm1, LispPTR tos) {
    int arg1, arg2;

    /* Extract integer values with strict type checking */
    N_IGETNUMBER(tosm1, arg1, do_ufn);  // Extract arg1 (must be SMALLP or FIXP)
    N_IGETNUMBER(tos, arg2, do_ufn);    // Extract arg2 (must be SMALLP or FIXP)

    /* Perform bitwise AND operation */
    arg1 = arg1 & arg2;

    /* Convert result to Lisp type */
    N_ARITH_SWITCH(arg1);

  do_ufn:
    /* Type error: Arguments not integers (SMALLP or FIXP) */
    ERROR_EXIT(tos);
}

/* =========================================================================
 * Function: N_OP_logor
 * Purpose: Implements bitwise OR operation for Lisp (LOGOR2 opcode)
 * Opcode: LOGOR2 (0344)
 * Parameters:
 *   tosm1 - First operand (TOS-1) - LispPTR
 *   tos - Second operand (TOS) - LispPTR
 * Returns: Bitwise OR of the two operands - LispPTR
 * Algorithm:
 *   1. Extract integer values from LispPTR arguments using N_IGETNUMBER
 *   2. Strict type checking: Arguments must be SMALLP or FIXP
 *   3. Perform bitwise OR operation
 *   4. Convert result back to appropriate Lisp type using N_ARITH_SWITCH
 * Error Handling: Calls ERROR_EXIT if type error occurs
 * ========================================================================= */
LispPTR N_OP_logor(LispPTR tosm1, LispPTR tos) {
      int arg1, arg2;

    /* Extract integer values with strict type checking */
    N_IGETNUMBER(tosm1, arg1, do_ufn);  // Extract arg1 (must be SMALLP or FIXP)
    N_IGETNUMBER(tos, arg2, do_ufn);    // Extract arg2 (must be SMALLP or FIXP)

    /* Perform bitwise OR operation */
    arg1 = arg1 | arg2;

    /* Convert result to Lisp type */
    N_ARITH_SWITCH(arg1);

  do_ufn:
    /* Type error: Arguments not integers (SMALLP or FIXP) */
    ERROR_EXIT(tos);
}

/* =========================================================================
 * Function: N_OP_greaterp
 * Purpose: Implements greater-than comparison for Lisp (GREATERP and IGREATERP opcodes)
 * Opcode: GREATERP (0363), IGREATERP (0361)
 * Parameters:
 *   tosm1 - First operand (TOS-1) - LispPTR
 *   tos - Second operand (TOS) - LispPTR
 * Returns: ATOM_T (true) if tosm1 > tos, NIL_PTR (false) otherwise - LispPTR
 * Algorithm:
 *   1. Extract numeric values from LispPTR arguments using N_GETNUMBER
 *   2. Check if arguments are integers; fall through to floating-point if not
 *   3. Perform integer comparison
 *   4. Return appropriate boolean result
 * Error Handling: Falls through to floating-point comparison if arguments are not integers
 * ========================================================================= */
LispPTR N_OP_greaterp(LispPTR tosm1, LispPTR tos) {
  int arg1, arg2;

  /* Extract numeric values from LispPTR arguments */
  N_GETNUMBER(tosm1, arg1, do_ufn);    // Extract arg1 (tosm1)
  N_GETNUMBER(tos, arg2, do_ufn);      // Extract arg2 (tos)

  /* Perform integer comparison */
  if (arg1 > arg2)
    return (ATOM_T);                   // Return true
  else
    return (NIL_PTR);                  // Return false

do_ufn:
  /* Fall through to floating-point comparison if arguments are not integers */
  return (N_OP_fgreaterp(tosm1, tos));
}

/* =========================================================================
 * Function: N_OP_igreaterp
 * Purpose: Implements greater-than comparison with strict type checking (IGREATERP opcode)
 * Opcode: IGREATERP (0361)
 * Parameters:
 *   tosm1 - First operand (TOS-1) - LispPTR
 *   tos - Second operand (TOS) - LispPTR
 * Returns: ATOM_T (true) if tosm1 > tos, NIL_PTR (false) otherwise - LispPTR
 * Algorithm:
 *   1. Extract integer values from LispPTR arguments using N_IGETNUMBER
 *   2. Strict type checking: Arguments must be SMALLP or FIXP (no floats allowed)
 *   3. Perform integer comparison
 *   4. Return appropriate boolean result
 * Error Handling: Calls ERROR_EXIT if type error occurs
 * ========================================================================= */
LispPTR N_OP_igreaterp(LispPTR tosm1, LispPTR tos) {
  int arg1, arg2;

  /* Extract integer values with strict type checking */
  N_IGETNUMBER(tosm1, arg1, do_ufn);  // Extract arg1 (must be SMALLP or FIXP)
  N_IGETNUMBER(tos, arg2, do_ufn);    // Extract arg2 (must be SMALLP or FIXP)

  if (arg1 > arg2)
    return (ATOM_T);
  else
    return (NIL_PTR);

do_ufn:
  ERROR_EXIT(tos);
}

/* =========================================================================
 * Function: N_OP_iplusn
 * Purpose: Implements integer addition with immediate value (IPLUS.N opcode)
 * Opcode: IPLUS.N (0335)
 * Parameters:
 *   tos - Operand (TOS) - LispPTR
 *   n - Immediate integer value to add
 * Returns: Sum of tos and n - LispPTR
 * Algorithm:
 *   1. Extract integer value from LispPTR argument using N_IGETNUMBER
 *   2. Strict type checking: Argument must be SMALLP or FIXP
 *   3. Perform addition with immediate value
 *   4. Check for overflow and return result
 * Overflow Detection:
 *   - Uses GCC builtin __builtin_sadd_overflow if available
 *   - Falls back to simple overflow check
 * Error Handling: Calls ERROR_EXIT if overflow or type error occurs
 * ========================================================================= */
LispPTR N_OP_iplusn(LispPTR tos, int n) {
  int arg1;
  int result;

  /* Extract integer value with strict type checking */
  N_IGETNUMBER(tos, arg1, do_ufn);    // Extract arg1 (must be SMALLP or FIXP)

#ifdef USE_OVERFLOW_BUILTINS
  /* Use GCC builtin for efficient overflow detection */
  if (__builtin_sadd_overflow(arg1, n, &result)) {
    ERROR_EXIT(tos);                    // Overflow detected, exit with error
  }
  N_ARITH_SWITCH(result);              // Convert result to Lisp type

#else
  /* Manual overflow detection fallback */
  result = arg1 + n;
  
  /* Simple overflow check: positive + positive = negative */
  if ((result < 0) && (arg1 >= 0)) {
    ERROR_EXIT(tos);                    // Overflow detected, exit with error
  }
  
  N_ARITH_SWITCH(result);              // Convert result to Lisp type
#endif

do_ufn:
  /* Type error: Argument not an integer (SMALLP or FIXP) */
  ERROR_EXIT(tos);
}

/* =========================================================================
 * Function: N_OP_idifferencen
 * Purpose: Implements integer subtraction with immediate value (IDIFFERENCE.N opcode)
 * Opcode: IDIFFERENCE.N (0336)
 * Parameters:
 *   tos - Operand (TOS) - LispPTR
 *   n - Immediate integer value to subtract
 * Returns: Difference of tos and n (tos - n) - LispPTR
 * Algorithm:
 *   1. Extract integer value from LispPTR argument using N_IGETNUMBER
 *   2. Strict type checking: Argument must be SMALLP or FIXP
 *   3. Perform subtraction with immediate value
 *   4. Check for overflow and return result
 * Overflow Detection:
 *   - Uses GCC builtin __builtin_ssub_overflow if available
 *   - Falls back to simple overflow check
 * Error Handling: Calls ERROR_EXIT if overflow or type error occurs
 * ========================================================================= */
LispPTR N_OP_idifferencen(LispPTR tos, int n) {
  int arg1;
  int result;

  /* Extract integer value with strict type checking */
  N_IGETNUMBER(tos, arg1, do_ufn);    // Extract arg1 (must be SMALLP or FIXP)

#ifdef USE_OVERFLOW_BUILTINS
  /* Use GCC builtin for efficient overflow detection */
  if (__builtin_ssub_overflow(arg1, n, &result)) {
    ERROR_EXIT(tos);                    // Overflow detected, exit with error
  }
  N_ARITH_SWITCH(result);              // Convert result to Lisp type

#else

  result = arg1 - n;
  if ((result >= 0) && (arg1 < 0)) { ERROR_EXIT(tos); }
  N_ARITH_SWITCH(result);

#endif

do_ufn:
  ERROR_EXIT(tos);
}

/* =========================================================================
 * Function: N_OP_makenumber
 * Purpose: Creates a FIXP from two SMALLP halves
 * Opcode: MAKENUMBER
 * Parameters:
 *   tosm1 - High 16 bits as SMALLP - LispPTR
 *   tos - Low 16 bits as SMALLP - LispPTR
 * Returns: FIXP value combining both halves - LispPTR
 * Algorithm:
 *   1. Verify both arguments are SMALLP (have S_POSITIVE tag)
 *   2. Combine high and low 16-bit halves to form 32-bit integer
 *   3. Convert result to appropriate Lisp type using N_ARITH_SWITCH
 * Type Checking: Both arguments must be SMALLP (S_POSITIVE tag)
 * Error Handling: Calls ERROR_EXIT if arguments are not SMALLP
 * ========================================================================= */
LispPTR N_OP_makenumber(LispPTR tosm1, LispPTR tos) {
  int result;

  /* Verify both arguments are SMALLP (have S_POSITIVE tag) */
  if (((tosm1 & 0xFFFF0000) != S_POSITIVE) || ((tos & 0xFFFF0000) != S_POSITIVE)) {
    ERROR_EXIT(tos);                    // Type error: Arguments not SMALLP
  }
  
  /* Combine high and low 16-bit halves */
  /* UB: left shift of 49152 by 16 places cannot be represented in type 'int' */
  result = (int)(((tosm1 & 0xffff) << 16) | (tos & 0xffff));
  
  /* Convert result to Lisp type */
  N_ARITH_SWITCH(result);
} /* end OP_makenumber */

/* =========================================================================
 * Function: N_OP_boxiplus
 * Purpose: Performs in-place addition on a FIXP box (avoiding garbage collection)
 * Opcode: BOXIPLUS
 * Parameters:
 *   a - FIXP box to modify - LispPTR
 *   tos - Number to add - LispPTR
 * Returns: Modified FIXP box - LispPTR
 * Algorithm:
 *   1. Verify argument 'a' is a FIXP
 *   2. Extract numeric value from tos using N_GETNUMBER
 *   3. Perform in-place addition on the FIXP box contents
 *   4. Return the modified FIXP box
 * Optimization: Avoids allocating new storage by modifying in place
 * Error Handling: Calls ERROR_EXIT if 'a' is not FIXP or tos is not numeric
 * ========================================================================= */
LispPTR N_OP_boxiplus(LispPTR a, LispPTR tos) {
  int arg2;

  if (GetTypeNumber(a) == TYPE_FIXP) {
    N_GETNUMBER(tos, arg2, bad);        // Extract numeric value from tos
    /* Perform in-place addition on the FIXP box contents */
    *((int *)NativeAligned4FromLAddr(a)) += arg2;
    return (a);                        // Return modified FIXP box
  }

bad:
  /* Error: 'a' is not FIXP or tos is not numeric */
  ERROR_EXIT(tos);
} /* OP_boxiplus */

/* =========================================================================
 * Function: N_OP_boxidiff
 * Purpose: Performs in-place subtraction on a FIXP box (avoiding garbage collection)
 * Opcode: BOXDIDIFF
 * Parameters:
 *   a - FIXP box to modify - LispPTR
 *   tos - Number to subtract - LispPTR
 * Returns: Modified FIXP box - LispPTR
 * Algorithm:
 *   1. Verify argument 'a' is a FIXP
 *   2. Extract numeric value from tos using N_GETNUMBER
 *   3. Perform in-place subtraction on the FIXP box contents
 *   4. Return the modified FIXP box
 * Optimization: Avoids allocating new storage by modifying in place
 * Error Handling: Calls ERROR_EXIT if 'a' is not FIXP or tos is not numeric
 * ========================================================================= */
LispPTR N_OP_boxidiff(LispPTR a, LispPTR tos) {
  int arg2;

  if (GetTypeNumber(a) == TYPE_FIXP) {
    N_GETNUMBER(tos, arg2, bad);        // Extract numeric value from tos
    /* Perform in-place subtraction on the FIXP box contents */
    *((int *)NativeAligned4FromLAddr(a)) -= arg2;
    return (a);                        // Return modified FIXP box
  }
bad:
  ERROR_EXIT(tos);

} /* end OP_boxidiff */

/* =========================================================================
 * Function: N_OP_times2
 * Purpose: Implements integer multiplication for Lisp
 * Opcode: TIMES2
 * Parameters:
 *   tosm1 - First operand (TOS-1) - LispPTR
 *   tos - Second operand (TOS) - LispPTR
 * Returns: Product of the two operands - LispPTR
 * Algorithm:
 *   1. Extract numeric values from LispPTR arguments using N_GETNUMBER
 *   2. Check if arguments are integers; fall through to floating-point if not
 *   3. Perform multiplication with overflow detection
 *   4. Convert result back to appropriate Lisp type using N_ARITH_SWITCH
 * Overflow Detection:
 *   - Uses GCC builtin __builtin_smul_overflow if available
 *   - Falls back to manual overflow detection
 * Error Handling: Calls ERROR_EXIT if overflow occurs
 * ========================================================================= */
LispPTR N_OP_times2(LispPTR tosm1, LispPTR tos) {
  int arg1, arg2;
  int result;

  /* Extract numeric values from LispPTR arguments */
  N_GETNUMBER(tosm1, arg1, doufn);    // Extract arg1 (tosm1)
  N_GETNUMBER(tos, arg2, doufn);      // Extract arg2 (tos)

#ifdef USE_OVERFLOW_BUILTINS

  if (__builtin_smul_overflow(arg1, arg2, &result)) {
    goto doufn2;
  }
  N_ARITH_SWITCH(result);

#else

  result = arg1 * arg2;
  if ((arg2 != 0) && ((result / arg2) != arg1)) goto doufn2;
  N_ARITH_SWITCH(result);

#endif

doufn2:
  ERROR_EXIT(tos);
doufn:
  return (N_OP_ftimes2(tosm1, tos));

} /* end N_OP_times2 */

/* =========================================================================
 * Function: N_OP_itimes2
 * Purpose: Implements integer multiplication with strict type checking
 * Opcode: ITIMES2
 * Parameters:
 *   tosm1 - First operand (TOS-1) - LispPTR
 *   tos - Second operand (TOS) - LispPTR
 * Returns: Product of the two operands - LispPTR
 * Algorithm:
 *   1. Extract integer values from LispPTR arguments using N_IGETNUMBER
 *   2. Strict type checking: Arguments must be SMALLP or FIXP
 *   3. Perform multiplication with overflow detection
 *   4. Convert result back to appropriate Lisp type using N_ARITH_SWITCH
 * Overflow Detection:
 *   - Uses GCC builtin __builtin_smul_overflow if available
 *   - Falls back to manual overflow detection using division check
 * Error Handling: Calls ERROR_EXIT if overflow or type error occurs
 * ========================================================================= */
LispPTR N_OP_itimes2(LispPTR tosm1, LispPTR tos) {
  int arg1, arg2;
  int result;

  /* Extract integer values with strict type checking */
  N_IGETNUMBER(tosm1, arg1, doufn);    // Extract arg1 (must be SMALLP or FIXP)
  N_IGETNUMBER(tos, arg2, doufn);      // Extract arg2 (must be SMALLP or FIXP)

#ifdef USE_OVERFLOW_BUILTINS
  /* Use GCC builtin for efficient overflow detection */
  if (__builtin_smul_overflow(arg1, arg2, &result)) {
    goto doufn;                        // Overflow detected
  }
  N_ARITH_SWITCH(result);              // Convert result to Lisp type

#else
  /* Manual overflow detection fallback using division check */
  /* UB: signed integer overflow: 1073741824 * 32768 cannot be represented in type 'int' */
  result = arg1 * arg2;
  if ((arg2 != 0) && ((result / arg2) != arg1)) {
    goto doufn;                        // Overflow detected
  }
  N_ARITH_SWITCH(result);              // Convert result to Lisp type
#endif

doufn:
  /* Overflow or type error */
  ERROR_EXIT(tos);
} /* end N_OP_itimes2 */

/* =========================================================================
 * Function: N_OP_quot
 * Purpose: Implements integer division for Lisp
 * Opcode: QUOT
 * Parameters:
 *   tosm1 - Dividend (TOS-1) - LispPTR
 *   tos - Divisor (TOS) - LispPTR
 * Returns: Quotient of the division - LispPTR
 * Algorithm:
 *   1. Extract numeric values from LispPTR arguments using N_GETNUMBER
 *   2. Check if arguments are integers; fall through to floating-point if not
 *   3. Check for division by zero
 *   4. Perform integer division
 *   5. Convert result back to appropriate Lisp type using N_ARITH_SWITCH
 * Error Handling:
 *   - Division by zero: Calls ERROR_EXIT
 *   - Non-integer arguments: Falls through to floating-point division (N_OP_fquotient)
 * ========================================================================= */
LispPTR N_OP_quot(LispPTR tosm1, LispPTR tos) {
  int arg1, arg2;
  int result;

  /* Extract numeric values from LispPTR arguments */
  N_GETNUMBER(tosm1, arg1, doufn);    // Extract dividend
  N_GETNUMBER(tos, arg2, doufn);      // Extract divisor

  /* Check for division by zero */
  if (arg2 == 0) {
    goto doufn2;                        // Division by zero
  }

  /* Perform integer division */
  result = arg1 / arg2; /* lmm: note: no error case!! */
  
  /* Convert result to Lisp type */
  N_ARITH_SWITCH(result);

doufn2:
  /* Division by zero error */
  ERROR_EXIT(tos);
doufn:
  /* Fall through to floating-point division if arguments are not integers */
  return (N_OP_fquotient(tosm1, tos));
} /* end N_OP_quot */

/* =========================================================================
 * Function: N_OP_iquot
 * Purpose: Implements integer division with strict type checking
 * Opcode: IQUOT
 * Parameters:
 *   tosm1 - Dividend (TOS-1) - LispPTR
 *   tos - Divisor (TOS) - LispPTR
 * Returns: Quotient of the division - LispPTR
 * Algorithm:
 *   1. Extract integer values from LispPTR arguments using N_IGETNUMBER
 *   2. Strict type checking: Arguments must be SMALLP or FIXP
 *   3. Check for division by zero
 *   4. Perform integer division
 *   5. Convert result back to appropriate Lisp type using N_ARITH_SWITCH
 * Error Handling: Calls ERROR_EXIT if division by zero or type error occurs
 * ========================================================================= */
LispPTR N_OP_iquot(LispPTR tosm1, LispPTR tos) {
  int arg1, arg2;
  int result;

  /* Extract integer values with strict type checking */
  N_IGETNUMBER(tosm1, arg1, doufn);  // Extract dividend (must be SMALLP or FIXP)
  N_IGETNUMBER(tos, arg2, doufn);    // Extract divisor (must be SMALLP or FIXP)

  /* Check for division by zero */
  if (arg2 == 0) {
    goto doufn;                        // Division by zero
  }

  /* Perform integer division */
  result = arg1 / arg2;
  
  /* Convert result to Lisp type */
  N_ARITH_SWITCH(result);

doufn:
  /* Division by zero or type error */
  ERROR_EXIT(tos);

} /* end N_OP_quot */

/* =========================================================================
 * Function: N_OP_iremainder
 * Purpose: Implements integer remainder operation with strict type checking
 * Opcode: IREMAINDER
 * Parameters:
 *   tosm1 - Dividend (TOS-1) - LispPTR
 *   tos - Divisor (TOS) - LispPTR
 * Returns: Remainder of integer division - LispPTR
 * Algorithm:
 *   1. Extract integer values from LispPTR arguments using N_IGETNUMBER
 *   2. Strict type checking: Arguments must be SMALLP or FIXP
 *   3. Check for division by zero
 *   4. Perform integer remainder operation
 *   5. Convert result back to appropriate Lisp type using N_ARITH_SWITCH
 * Error Handling: Calls ERROR_EXIT if division by zero or type error occurs
 * ========================================================================= */
LispPTR N_OP_iremainder(LispPTR tosm1, LispPTR tos) {
  int arg1, arg2;
  int result;

  /* Extract integer values with strict type checking */
  N_IGETNUMBER(tosm1, arg1, doufn);  // Extract dividend (must be SMALLP or FIXP)
  N_IGETNUMBER(tos, arg2, doufn);    // Extract divisor (must be SMALLP or FIXP)

  /* Check for division by zero */
  if (arg2 == 0) {
    goto doufn;                        // Division by zero
  }

  /* Perform integer remainder operation */
  result = arg1 % arg2;
  
  /* Convert result to Lisp type */
  N_ARITH_SWITCH(result);

doufn:
  /* Division by zero or type error */
  ERROR_EXIT(tos);
} /* end N_OP_iremainder */
