/* $Id: lisp2c.c,v 1.3 1999/05/31 23:35:37 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved
 */

/************************************************************************/
/*								*/
/*	(C) Copyright 1989-95 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*								*/
/************************************************************************/

/* FILE: lisp2c.c - Lisp to C Data Type Conversion Functions
 *
 * HIGH CONFIDENCE: This file provides critical conversion utilities between
 * Lisp data structures and native C data types. These functions are essential
 * for the interface between the Lisp VM and C implementation of various
 * subsystems (I/O, display, networking, etc.).
 *
 * CONVERSION ARCHITECTURE:
 * - Lisp objects live in virtual memory (Lisp_world)
 * - C functions need native pointers and data types
 * - These functions bridge the gap safely and efficiently
 *
 * ENDIANNESS CRITICAL: All conversions must handle BYTESWAP properly
 * MEMORY MODEL: Uses NativeAligned4FromLAddr() for address translation
 * CROSS-REFERENCE: Address translation in adr68k.h, memory management
 *
 * USAGE PATTERNS:
 * - String conversion for file I/O and display text
 * - Integer conversion for arithmetic operations
 * - Array access for structured data manipulation
 *
 * AUTHOR: -jarl (original implementation)
 */

#include "version.h"

#include <stdio.h>       // for sprintf
#include <stdlib.h>      // for abs
#include "adr68k.h"      // for NativeAligned4FromLAddr, LAddrFromNative
#include "commondefs.h"  // for error
#include "emlglob.h"
#include "lisp2cdefs.h"  // for CIntToLispInt, LispIntToCInt, LispStringSimpleLength
#include "lispemul.h"    // for LispPTR
#include "lispmap.h"     // for S_NEGATIVE, S_POSITIVE
#include "lspglob.h"
#include "lsptypes.h"    // for OneDArray, FAT_CHAR_TYPENUMBER, THIN_CHAR_TY...
#include "mkcelldefs.h"  // for createcell68k

/* HIGH CONFIDENCE: Lisp String Type Predicate
 *
 * Determines if a Lisp object is a string (character array).
 * This is used by C code that needs to handle string objects specially.
 *
 * TYPE CHECKING:
 * - Uses GetTypeEntry() or direct type number access
 * - Checks for both THIN_CHAR and FAT_CHAR string types
 * - THIN_CHAR: 8-bit characters (ASCII)
 * - FAT_CHAR: 16-bit characters (Unicode support)
 *
 * PARAMETERS:
 * - object: LispPTR to object being tested
 *
 * RETURNS: Non-zero if object is a string, 0 otherwise
 *
 * CROSS-REFERENCE: String types defined in lsptypes.h
 * CROSS-REFERENCE: Array structure in lsptypes.h:263-277 (OneDArray)
 */
int LispStringP(LispPTR object) {
  int type;

  type = ((OneDArray *)NativeAligned4FromLAddr(object))->typenumber;
  return ((type == THIN_CHAR_TYPENUMBER) || (type == FAT_CHAR_TYPENUMBER));
}

/* HIGH CONFIDENCE: Get Lisp String Length
 *
 * Returns the current length (fill pointer) of a Lisp string.
 * This is used by C code that needs to allocate buffers or iterate
 * over string characters.
 *
 * STRING MODEL:
 * - Lisp strings are arrays with fill pointers
 * - fillpointer indicates current active length
 * - totalsize indicates allocated capacity
 *
 * PARAMETERS:
 * - lispstring: LispPTR to string object
 *
 * RETURNS: Current string length in characters
 *
 * CROSS-REFERENCE: OneDArray structure in lsptypes.h:263-277
 * CROSS-REFERENCE: Fill pointer operations in array opcodes
 */
int LispStringSimpleLength(LispPTR lispstring) {
  OneDArray *arrayp;

  arrayp = (OneDArray *)(NativeAligned4FromLAddr(lispstring));
  return (arrayp->fillpointer);
}

/* XXX: this string conversion is NOT useable on byte-swapped (little-endian) machines
 */
void LispStringToCStr(LispPTR lispstring, char *cstring) {
  OneDArray *arrayp;
  char *base;
  short *sbase;
  int i, Len;

  arrayp = (OneDArray *)(NativeAligned4FromLAddr(lispstring));
  Len = arrayp->fillpointer;

  switch (arrayp->typenumber) {
    case THIN_CHAR_TYPENUMBER:
      base = ((char *)(NativeAligned2FromLAddr(arrayp->base))) + ((int)(arrayp->offset));
      for (i = 0; i < Len; i++) cstring[i] = base[i];
      cstring[Len] = '\0';
      break;

    case FAT_CHAR_TYPENUMBER:
      sbase = ((short *)(NativeAligned2FromLAddr(arrayp->base))) + ((int)(arrayp->offset));
      base = (char *)sbase;
      for (i = 0; i < Len * 2; i++) cstring[i] = base[i];
      cstring[Len * 2] = '\0';
      break;

    default: error("Arg not Lisp string.\n");
  }
}

int LispIntToCInt(LispPTR lispint) {
  switch ((0xFFFF0000 & lispint)) {
    case S_POSITIVE: return (lispint & 0xFFFF);
    case S_NEGATIVE: return (lispint | 0xFFFF0000);
    default:
      if (GetTypeNumber(lispint) == TYPE_FIXP) {
        return (*((int *)NativeAligned4FromLAddr(lispint)));
      } else {
        char msg[200];
        sprintf(msg, "Arg 0x%x isn't a lisp integer.", lispint);
        error(msg);
        /* NOTREACHED */
        return(0);
      }
  }
}

LispPTR CIntToLispInt(int cint) {
  if (abs(cint) > 0xFFFF) { /* its a fixp! */
    LispPTR *wordp;
    wordp = (LispPTR *)createcell68k(TYPE_FIXP);
    *((int *)wordp) = cint;
    return (LAddrFromNative(wordp));
  } else if (cint >= 0) { /* its a positive smallp! */
    return (S_POSITIVE | cint);
  } else { /* its a negative smallp! */
    return (S_NEGATIVE | (0xFFFF & cint));
  }
}
