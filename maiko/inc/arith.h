/* FILE: arith.h - Arithmetic Constants and Macros
 *
 * This file defines constants and macros for arithmetic operations
 * in the Medley Lisp emulator. It provides support for handling
 * smallp and fixp numbers, as well as conversion between different
 * numeric representations.
 *
 * HIGH CONFIDENCE: The arithmetic constants and macros are fundamental
 * to the Lisp system and are well-tested.
 *
 * NUMERIC CONSTANTS:
 * - MAX_SMALL, MIN_SMALL: Range for smallp numbers (-65536 to 65535)
 * - MAX_FIXP, MIN_FIXP: Range for fixp numbers (-2^31 to 2^31-1)
 * - SMALLP_ZERO, SMALLP_MINUSONE: Special smallp values
 *
 * MACROS AND FUNCTIONS:
 * - GetSmalldata: Extract integer from smallp
 * - GetSmallp: Create smallp from integer
 * - GetPosSmallp: Create positive smallp from unsigned integer
 * - FIXP_VALUE: Access fixp value from address
 * - FLOATP_VALUE: Access float value from address
 * - N_GETNUMBER: Extract numeric value from LispPTR (handles smallp/fixp)
 * - N_IGETNUMBER: Extract numeric value (handles smallp/fixp/float)
 * - ARITH_SWITCH: Convert integer to appropriate Lisp numeric type
 *
 * TYPE CONVERSION:
 * Macros handle conversion between:
 * - Smallp (16-bit signed, immediate)
 * - Fixp (32-bit signed, indirect)
 * - Floatp (32-bit float, indirect)
 *
 * CROSS-REFERENCE: Implementation in arithops.c
 * CROSS-REFERENCE: Type definitions in lsptypes.h
 * CROSS-REFERENCE: Address manipulation in adr68k.h
 */

#ifndef ARITH_H
#define ARITH_H 1
/* $Id: arith.h,v 1.2 1999/01/03 02:05:52 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved */

/************************************************************************/
/*									*/
/*	(C) Copyright 1989-92 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

#include "adr68k.h"    // for NativeAligned4FromLAddr, LAddrFromNative
#include "commondefs.h" // for error
#include "lispemul.h"  // for SEGMASK, ERROR_EXIT
#include "lispmap.h"   // for S_NEGATIVE, S_POSITIVE
#include "lsptypes.h"  // for TYPE_FIXP, GetTypeNumber, TYPE_FLOATP
#include "mkcelldefs.h" // for createcell68k

#define MAX_SMALL 65535  /* == 0x0000FFFF  */
#define MIN_SMALL (-65536) /* == 0xFFFF0000  */

#define MAX_FIXP 2147483647  /* == 0x7FFFFFFF  */
#define MIN_FIXP (-2147483648) /* == 0x80000000  */

#define SMALLP_ZERO ((LispPTR)S_POSITIVE)
#define SMALLP_MINUSONE ((LispPTR)(S_NEGATIVE | 0xFFFF))
/**
 * extract an integer value from a smallp
 */
static inline int GetSmalldata(LispPTR x) {
  if ((SEGMASK & (int)x) == S_POSITIVE) return (int)(x & 0xFFFF);
  if ((SEGMASK & (int)x) == S_NEGATIVE) return (int)(x | 0xFFFF0000);
  error("Not smallp address");
  return (0);
}

/**
 * construct a smallp from an integer value
 */

static inline LispPTR GetSmallp(long x) {
  if (x >= 0) {
    if (x <= MAX_SMALL) return (LispPTR)(S_POSITIVE | x);
  } else {
    if (x >= MIN_SMALL) return (LispPTR)(S_NEGATIVE | (x & 0xFFFF));
  }
  error("Not Smallp data");
  return (S_POSITIVE | 0);
}

/**
 * construct a smallp from an unsigned value
 */
static inline LispPTR GetPosSmallp(unsigned long x) {
  if (x <= MAX_SMALL) return (LispPTR)(S_POSITIVE | x);
  error("Not Smallp data");
  return (S_POSITIVE | 0);
}

#define FIXP_VALUE(dest) *((int *)NativeAligned4FromLAddr(dest))

#define FLOATP_VALUE(dest) *((float *)NativeAligned4FromLAddr(dest))

#define N_GETNUMBER(sour, dest, label)                        \
  do {                                                        \
    switch (SEGMASK & (sour)) {                               \
    case S_POSITIVE: (dest) = (int)((sour) & 0xFFFF); break;  \
    case S_NEGATIVE: (dest) = (int)((sour) | 0xFFFF0000); break; \
      default:                                                \
        /* NOLINTNEXTLINE(bugprone-macro-parentheses) */      \
        if (GetTypeNumber(sour) != TYPE_FIXP) goto label;     \
        (dest) = FIXP_VALUE(sour);                            \
    }                                                         \
  } while (0)

#define N_IGETNUMBER(sour, dest, label)                                                   \
  do {                                                                                    \
    switch (SEGMASK & (sour)) {                                                           \
    case S_POSITIVE: (dest) = (int)((sour) & 0xFFFF); break;            \
    case S_NEGATIVE: (dest) = (int)((sour) | 0xFFFF0000); break;        \
      default:                                                                            \
        switch (GetTypeNumber(sour)) {                                                    \
          case TYPE_FIXP: (dest) = FIXP_VALUE(sour); break;               \
          case TYPE_FLOATP: {                                                             \
            float temp;                                                          \
            temp = FLOATP_VALUE(sour);                                                    \
            /* NOLINTNEXTLINE(bugprone-macro-parentheses) */                              \
            if ((temp > ((float)0x7fffffff)) || (temp < ((float)0x80000000))) goto label; \
            (dest) = (int)temp;                                                           \
          } break;                                                                        \
          default: goto label; /* NOLINT(bugprone-macro-parentheses) */                   \
        }                                                                                 \
        break;                                                                            \
    }                                                                                     \
  } while (0)

#define ARITH_SWITCH(arg, result)                                          \
  do {                                                                     \
    switch ((arg) & (int)0xFFFF0000) {                      \
      case 0: (result) = (S_POSITIVE | (int)(arg)); break;                 \
      case (int)0xFFFF0000: (result) = (S_NEGATIVE | (0xFFFF & (arg))); break; \
      default: {                                                           \
        int *fixpp;                                           \
        /* arg is FIXP, call createcell */                                 \
        fixpp = (int *)createcell68k(TYPE_FIXP);                       \
        *fixpp = (int)(arg);                                      \
        (result) = LAddrFromNative(fixpp);                                \
        break;                                                             \
      }                                                                    \
    }                                                                      \
  } while (0)

#define N_ARITH_SWITCH(arg)                                  \
  do {                                                       \
    switch ((arg) & (int)0xFFFF0000) {             \
    case 0: return (LispPTR) (S_POSITIVE | (arg));                       \
    case (int)0xFFFF0000: return (LispPTR)(S_NEGATIVE | (0xFFFF & (arg))); \
      default: {                                             \
        int *fixpp;                             \
        /* arg is FIXP, call createcell */                   \
        fixpp = (int *)createcell68k(TYPE_FIXP);         \
        *fixpp = (int)(arg);				      \
        return (LAddrFromNative(fixpp));                      \
      }                                                      \
    }                                                        \
  } while (0)

#endif /* ARITH_H */
