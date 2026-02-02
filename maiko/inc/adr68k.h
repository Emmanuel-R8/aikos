/* FILE: adr68k.h - Address Translation Functions
 *
 * This file implements inline functions for translating between Lisp
 * addresses (LispPTR) and native system addresses (void*), with support
 * for different alignment requirements and stack operations. It also
 * provides page-level address conversion for virtual memory management.
 *
 * HIGH CONFIDENCE: These functions are fundamental to the emulator's
 * memory management and are used extensively throughout the codebase.
 *
 * ADDRESS TRANSLATION:
 * - LAddrFromNative: Convert native pointer to Lisp address
 * - NativeAligned2FromLAddr: Convert Lisp address to native 2-byte aligned pointer
 * - NativeAligned4FromLAddr: Convert Lisp address to native 4-byte aligned pointer
 * - NativeAligned4FromLPage: Convert Lisp page address to native pointer
 *
 * STACK OPERATIONS:
 * - StackOffsetFromNative: Calculate stack offset from native pointer
 * - NativeAligned2FromStackOffset: Convert stack offset to native pointer
 * - NativeAligned4FromStackOffset: Convert stack offset to 4-byte aligned pointer
 *
 * PAGE OPERATIONS:
 * - LPageFromNative: Get Lisp page number from native pointer
 *
 * ALIGNMENT CHECKING:
 * - Functions include alignment validation and error reporting
 * - Misaligned pointers trigger runtime warnings
 *
 * CROSS-REFERENCE: Lisp memory layout in storage.c
 * CROSS-REFERENCE: Stack management in stack.h
 * CROSS-REFERENCE: Virtual memory in vmemsave.c
 */

#ifndef ADR68K_H
#define ADR68K_H 1
/* $Id: adr68k.h,v 1.2 1999/01/03 02:05:52 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved  */

/*
 *	Copyright (C) 1987 by Fuji Xerox Co., Ltd. All rights reserved.
 *
 *	Author	:	Takeshi Shimizu
 *			Hiroshi Hayata
 */

/************************************************************************/
/*									*/
/*	Copyright 1989, 1990 Venue, Fuji Xerox Co., Ltd, Xerox Corp.	*/
/*									*/
/*	This file is work-product resulting from the Xerox/Venue	*/
/*	Agreement dated 18-August-1989 for support of Medley.		*/
/*									*/
/************************************************************************/

/**********************************************************************/
/*
		Func name :	adr68k.h
		Translate 68k address to Lisp or Lisp to 68k

		Date :		January 16, 1987
		Create :	Takeshi Shimizu
*/
/**********************************************************************/

#include <stddef.h>
#include <stdio.h>
#include "lispemul.h"
#include "lspglob.h"

static inline LispPTR LAddrFromNative(void *NAddr)
{
  if ((uintptr_t)NAddr & 1) {
    printf("Misaligned pointer in LAddrFromNative %p\n", NAddr);
  }
  return (LispPTR)(((DLword *)NAddr) - Lisp_world);
}

static inline DLword *NativeAligned2FromLAddr(LispPTR LAddr)
{
  return (Lisp_world + LAddr);
}

static inline LispPTR *NativeAligned4FromLAddr(LispPTR LAddr)
{
  if (LAddr & 1) {
    printf("Misaligned pointer in NativeAligned4FromLAddr 0x%x\n", LAddr);
  }
  return (void *)(Lisp_world + LAddr);
}

static inline LispPTR *NativeAligned4FromLPage(LispPTR LPage)
{
  return (void *)(Lisp_world + (LPage << 8));
}

static inline DLword StackOffsetFromNative(void *SAddr)
{
  /* Stack offsets are expressed as an offset in DLwords from the stack base */
  ptrdiff_t hoffset = (DLword *)SAddr - Stackspace;
  if (hoffset > 0xffff || hoffset < 0) {
    printf("Stack offset is out of range: 0x%tx\n", hoffset);
  }
  return (DLword)hoffset;
}

static inline DLword *NativeAligned2FromStackOffset(DLword StackOffset)
{
  return Stackspace + StackOffset;
}

static inline LispPTR *NativeAligned4FromStackOffset(DLword StackOffset)
{
  if (StackOffset & 1) {
    printf("Misaligned StackOffset in NativeAligned4FromStackOffset 0x%hx\n", StackOffset);
  }
  return (void *)(Stackspace + StackOffset);
}

static inline LispPTR LPageFromNative(void *NAddr)
{
  if ((uintptr_t)NAddr & 1) {
    printf("Misaligned pointer in LPageFromNative %p\n", NAddr);
  }
  return (LispPTR)((((DLword *)NAddr) - Lisp_world) >> 8);
}
#endif /* ADR68K_H */
