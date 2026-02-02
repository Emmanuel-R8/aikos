/* =========================================================================
 * File: arrayops.c
 * Purpose: Implementation of array operations for Maiko VM
 * Confidence Level: High (Stable, well-tested)
 * Testing: Tested with standard array operations
 * Algorithm: Implements array element access and manipulation using type dispatch
 * Authors: Original Venue Corporation, Sybalsky et al.
 * Original Implementation Date: 1989-1995
 * Last Modified: 2001-12-24
 *
 * This file contains the C implementations of array operations for
 * the Maiko VM. It supports various array types and provides functions
 * for accessing and modifying array elements.
 *
 * Key Features:
 * - Array element access (AREF) with type dispatch
 * - Array element modification (ASET)
 * - Support for multiple array element types:
 *   - 1-bit unsigned (type 0)
 *   - 8-bit unsigned (type 3)
 *   - 16-bit unsigned (type 4)
 *   - 16-bit signed (type 20)
 *   - 32-bit signed (type 22)
 *   - 32-bit pointer (type 38)
 *   - 32-bit float (type 54)
 *   - 8-bit character (type 67)
 *   - 16-bit character (type 68)
 *   - 32-bit Xpointer (type 86)
 *
 * Related Files:
 * - arrayopsdefs.h: Array operation prototypes and definitions
 * - my.h: Array element access functions and N_GetPos macro
 * - adr68k.h: Address manipulation macros
 * - lsptypes.h: Lisp data type definitions
 * ========================================================================= */

#include "version.h"
#include "adr68k.h"       // for NativeAligned4FromLAddr - address manipulation
#include "arrayopsdefs.h" // for N_OP_misc3, N_OP_misc4, N_OP_aref1, N_OP_aset1, N_OP_aref2, N_OP_aset2
#include "emlglob.h"      // for global variables
#include "lispemul.h"     // for state, LispPTR, ERROR_EXIT - VM core types
#include "lspglob.h"      // for global variables
#include "lsptypes.h"     // for Lisp data types
#include "my.h"           // for aref_switch, N_GetPos - array operations


/* =========================================================================
 * Function: N_OP_misc3 (CLAREf - Common Lisp ARray Element)
 * Purpose: Implements array element access with type dispatch
 * Opcode: OP 372/9 (base typenumber index)
 * Parameters:
 *   baseL - Array base address - LispPTR
 *   typenumber - Type indicator for array elements - LispPTR
 *   inx - Index into the array - LispPTR
 *   alpha - Operation modifier (must be 9) - int
 * Returns: Array element value - LispPTR
 * Algorithm:
 *   1. Verify operation modifier is valid (must be 9)
 *   2. Extract numeric index from LispPTR using N_GetPos
 *   3. Extract numeric type number from LispPTR using N_GetPos
 *   4. Dispatch to appropriate type-specific accessor using aref_switch
 *   5. Return the accessed array element value
 * Error Handling: Calls ERROR_EXIT if alpha != 9
 * Array Element Types:
 *   Type  | Size | Typenumber | Description
 *   ----- | ---- | ---------- | -----------
 *    0    | 0    | 0          | Unsigned: 1 bit
 *    0    | 3    | 3          | Unsigned: 8 bits
 *    0    | 4    | 4          | Unsigned: 16 bits
 *    1    | 4    | 20         | Signed: 16 bits
 *    1    | 6    | 22         | Signed: 32 bits
 *    2    | 6    | 38         | Pointer: 32 bits
 *    3    | 6    | 54         | Float: 32 bits
 *    4    | 3    | 67         | Character: 8 bits
 *    4    | 4    | 68         | Character: 16 bits
 *    5    | 6    | 86         | Xpointer: 32 bits
 * ========================================================================= */
LispPTR N_OP_misc3(LispPTR baseL, LispPTR typenumber, LispPTR inx, int alpha) {
  int index, type;

  /* Verify operation modifier */
  if (alpha != 9) {
    ERROR_EXIT(inx);                    // Invalid operation modifier
  }

  /* Extract numeric index */
  N_GetPos(inx, index, inx);            // Validate and extract index

  /* Extract numeric type number */
  N_GetPos(typenumber, type, inx);      // Validate and extract type number

  /* Dispatch to type-specific array element accessor */
  return (aref_switch((unsigned)type, inx, baseL, index));
} /*  end N_OP_misc3()  */

/* =========================================================================
 * Function: N_OP_misc4 (CL:ASET - Common Lisp ARray SET)
 * Purpose: Implements array element modification with type dispatch
 * Opcode: OP 373/7 (data, base typenumber, index)
 * Parameters:
 *   data - New value to store - LispPTR
 *   base - Array base address - LispPTR
 *   typenumber - Type indicator for array elements - LispPTR
 *   inx - Index into the array - LispPTR
 *   alpha - Operation modifier (must be 7) - int
 * Returns: Error indicator - LispPTR
 * Algorithm:
 *   1. Verify operation modifier is valid (must be 7)
 *   2. Extract numeric index from LispPTR using N_GetPos
 *   3. Extract numeric type number from LispPTR using N_GetPos
 *   4. Dispatch to appropriate type-specific setter using aset_switch
 * Error Handling: Calls ERROR_EXIT if alpha != 7 or dispatch fails
 * ========================================================================= */
LispPTR N_OP_misc4(LispPTR data, LispPTR base, LispPTR typenumber,
                   LispPTR inx, int alpha) {
  int index, type;

  /* Verify operation modifier */
  if (alpha != 7) {
    ERROR_EXIT(inx);                    // Invalid operation modifier
  }

  /* Extract numeric index */
  N_GetPos(inx, index, inx);            // Validate and extract index

  /* Extract numeric type number */
  N_GetPos(typenumber, type, inx);      // Validate and extract type number

  /* Dispatch to type-specific array element setter */
  aset_switch(type, inx);

doufn:
  /* Dispatch failed or error occurred */
  ERROR_EXIT(inx);
} /*  end N_OP_misc4()  */

/* =========================================================================
 * Function: N_OP_aref1
 * Purpose: Implements one-dimensional array element access
 * Opcode: OP 266 (array index)
 * Parameters:
 *   arrayarg - One-dimensional array object - LispPTR
 *   inx - Index into the array - LispPTR
 * Returns: Array element value - LispPTR
 * Algorithm:
 *   1. Verify the argument is a one-dimensional array
 *   2. Extract and validate index from LispPTR
 *   3. Calculate actual memory index using array offset
 *   4. Dispatch to type-specific accessor using aref_switch
 * Error Handling: Calls ERROR_EXIT if argument is not an array or index is invalid
 * ========================================================================= */
LispPTR N_OP_aref1(LispPTR arrayarg, LispPTR inx) {
  LispPTR baseL;
  int index;
  OneDArray *arrayblk;

  /* Verify argument is a one-dimensional array */
  if (GetTypeNumber(arrayarg) != TYPE_ONED_ARRAY) {
    ERROR_EXIT(inx);                    // Not a one-dimensional array
  }
  arrayblk = (OneDArray *)NativeAligned4FromLAddr(arrayarg);

  /* Extract and validate index */
  N_GetPos(inx, index, inx);            // Validate and extract index
  if (index >= arrayblk->totalsize) {
    ERROR_EXIT(inx);                    // Index out of bounds
  }
  index += arrayblk->offset;            // Apply array offset

  /* Get array base address */
  baseL = arrayblk->base;

  /* Dispatch to type-specific array element accessor */
  return (aref_switch(arrayblk->typenumber, inx, baseL, index));
} /*  end N_OP_aref1()  */

/* =========================================================================
 * Function: N_OP_aset1
 * Purpose: Implements one-dimensional array element modification
 * Opcode: OP 267 (new-value array index)
 * Parameters:
 *   data - New value to store - LispPTR
 *   arrayarg - One-dimensional array object - LispPTR
 *   inx - Index into the array - LispPTR
 * Returns: Error indicator - LispPTR
 * Algorithm:
 *   1. Verify the argument is a one-dimensional array
 *   2. Extract and validate index from LispPTR
 *   3. Calculate actual memory index using array offset
 *   4. Dispatch to type-specific setter using aset_switch
 * Error Handling: Calls ERROR_EXIT if argument is not an array or index is invalid
 * ========================================================================= */
LispPTR N_OP_aset1(LispPTR data, LispPTR arrayarg, LispPTR inx) {
  OneDArray *arrayblk;
  LispPTR base;
  int index;

  /* Verify argument is a one-dimensional array */
  if (GetTypeNumber(arrayarg) != TYPE_ONED_ARRAY) {
    ERROR_EXIT(inx);                    // Not a one-dimensional array
  }
  arrayblk = (OneDArray *)NativeAligned4FromLAddr(arrayarg);

  /* Extract and validate index */
  N_GetPos(inx, index, inx);            // Validate and extract index
  if (index >= arrayblk->totalsize) {
    ERROR_EXIT(inx);                    // Index out of bounds
  }
  index += arrayblk->offset;            // Apply array offset

  /* Get array base address */
  base = arrayblk->base;

  /* Dispatch to type-specific array element setter */
  aset_switch(arrayblk->typenumber, inx);

doufn:
  /* Dispatch failed or error occurred */
  ERROR_EXIT(inx);
} /*  end N_OP_aset1()  */

/* =========================================================================
 * Function: N_OP_aref2
 * Purpose: Implements two-dimensional array element access
 * Opcode: OP 356 (array index0 index1)
 * Parameters:
 *   arrayarg - Two-dimensional array object - LispPTR
 *   inx0 - First dimension index - LispPTR
 *   inx1 - Second dimension index - LispPTR
 * Returns: Array element value - LispPTR
 * Algorithm:
 *   1. Verify the argument is a two-dimensional array
 *   2. Extract and validate both indices from LispPTRs
 *   3. Calculate linear memory index from 2D indices
 *   4. Dispatch to type-specific accessor using aref_switch
 * Error Handling: Calls ERROR_EXIT if argument is not an array or indices are invalid
 * ========================================================================= */
LispPTR N_OP_aref2(LispPTR arrayarg, LispPTR inx0, LispPTR inx1) {
  LispPTR baseL;
  int arindex, temp;
  LispArray *arrayblk;
  int j;

  /* Verify argument is a two-dimensional array */
  if (GetTypeNumber(arrayarg) != TYPE_TWOD_ARRAY) {
    ERROR_EXIT(inx1);                   // Not a two-dimensional array
  }
  arrayblk = (LispArray *)NativeAligned4FromLAddr(arrayarg);
  baseL = arrayblk->base;

  /* Extract and validate indices */
  N_GetPos(inx1, temp, inx1);           // Validate and extract index1
  if (temp >= (j = arrayblk->Dim1)) {
    ERROR_EXIT(inx1);                   // Index1 out of bounds
  }
  N_GetPos(inx0, arindex, inx1);        // Validate and extract index0
  if (arindex >= arrayblk->Dim0) {
    ERROR_EXIT(inx1);                   // Index0 out of bounds
  }
  
  /* Calculate linear memory index from 2D coordinates */
  arindex *= j;
  arindex += temp;

  /* Dispatch to type-specific array element accessor */
  return (aref_switch(arrayblk->typenumber, inx1, baseL, arindex));
} /*  end N_OP_aref2()  */

/* =========================================================================
 * Function: N_OP_aset2
 * Purpose: Implements two-dimensional array element modification
 * Opcode: OP 357 (new-value array index0 index1)
 * Parameters:
 *   data - New value to store - LispPTR
 *   arrayarg - Two-dimensional array object - LispPTR
 *   inx0 - First dimension index - LispPTR
 *   inx1 - Second dimension index - LispPTR
 * Returns: Error indicator - LispPTR
 * Algorithm:
 *   1. Verify the argument is a two-dimensional array
 *   2. Extract and validate both indices from LispPTRs
 *   3. Calculate linear memory index from 2D indices
 *   4. Dispatch to type-specific setter using aset_switch
 * Error Handling: Calls ERROR_EXIT if argument is not an array or indices are invalid
 * ========================================================================= */
LispPTR N_OP_aset2(LispPTR data, LispPTR arrayarg, LispPTR inx0, LispPTR inx1) {
  LispArray *arrayblk;
  LispPTR base;
  int index, temp;
  int j;

  /* Verify argument is a two-dimensional array */
  if (GetTypeNumber(arrayarg) != TYPE_TWOD_ARRAY) {
    ERROR_EXIT(inx1);                   // Not a two-dimensional array
  }
  arrayblk = (LispArray *)NativeAligned4FromLAddr(arrayarg);
  base = arrayblk->base;

  /* Extract and validate indices */
  N_GetPos(inx1, temp, inx1);           // Validate and extract index1
  if (temp >= (j = arrayblk->Dim1)) {
    ERROR_EXIT(inx1);                   // Index1 out of bounds
  }
  N_GetPos(inx0, index, inx1);          // Validate and extract index0
  if (index >= arrayblk->Dim0) {
    ERROR_EXIT(inx1);                   // Index0 out of bounds
  }
  
  /* Calculate linear memory index from 2D coordinates */
  index *= j;
  index += temp;

  /* Dispatch to type-specific array element setter */
  aset_switch(arrayblk->typenumber, inx1);

doufn:
  /* Dispatch failed or error occurred */
  ERROR_EXIT(inx1);
} /*  end N_OP_aset2()  */
