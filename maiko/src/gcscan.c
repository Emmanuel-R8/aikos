/* $Id: gcscan.c,v 1.3 1999/05/31 23:35:33 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved
 */

/************************************************************************/
/*									*/
/*	(C) Copyright 1989-95 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

#include "version.h"

/*************************************************************************/
/*                                                                       */
/*                         File Name : gcscan.c                         */
/*                                                                       */
/*************************************************************************/
/*                                                                       */
/*                         Creation Date : July-7-1987                   */
/*                         Written by Tomoru Teruuchi                    */
/*                                                                       */
/*************************************************************************/
/*                                                                       */
/*           Functions :                                                 */
/*                        gcscan1(probe)                                 */
/*                        gcscan2(probe)                                 */
/*                                                                       */
/*************************************************************************/
/*           Description :                                               */
/*                                                                       */
/* The functions "gcscan1" and "gcscan2" are the translated functions    */
/*  from the Lisp Functions "\GCSCAN1" & "\GCSCAN2".                     */
/* These functions' role is to scan the HTmain Table and return the      */
/*  existing entry(by "gcscan1") & the entry whose STKBIT field is ON    */
/*  (by "gcscan2").These functions are the UFN functions that are called */
/*  by OPCODES "GCSCAN1" & "GCSCAN2".                                    */
/*                                                                       */
/* gcscan1                                                               */
/*    INPUT : probe (the starting offset in the HTmain table)            */
/*    OUTPUT : the entry's offset or NIL (no more entry existing)        */
/*                                                                       */
/* gcscan2                                                               */
/*    INPUT : probe (the starting offset in the HTmain table)            */
/*    OUTPUT : the entry's offset or NIL (no more entry existing)        */
/*************************************************************************/
/*                                                               \Tomtom */
/*************************************************************************/

/* FILE: gcscan.c - Garbage Collection Hash Table Scanning
 *
 * This file implements the hash table scanning functions for the
 * garbage collector. These functions are called by the GCSCAN1 and
 * GCSCAN2 opcodes to traverse the hash table and find entries
 * that need processing.
 *
 * HIGH CONFIDENCE: The scanning algorithms are simple and well-tested.
 * They provide the foundation for the GC's mark phase.
 *
 * FUNCTIONS:
 * - gcscan1: Scan for existing entries (used in first pass)
 * - gcscan2: Scan for entries with stack references (used in second pass)
 *
 * SCANNING ALGORITHM:
 * Both functions scan backwards through the HTmain table from the
 * given probe position. They return the offset of matching entries
 * or -1 when no more entries exist.
 *
 * gcscan1 returns entries that:
 * - Have collision bit set, OR
 * - Have zero stack count (not on stack)
 *
 * gcscan2 returns entries that:
 * - Have stack reference bit set (HTSTKBIT), OR
 * - Have collision bit set
 *
 * BIGVM SUPPORT:
 * Different bit positions are used for BIGVM:
 * - HTSTKBIT: 0x10000 (BIGVM) vs 0x200 (standard)
 * - GetStkCnt: Right shift 16 (BIGVM) vs 9 (standard)
 *
 * The HTENDS and HTLPTR macros provide overlay access to hash table
 * entries with different interpretations (htlinkptr vs hashentry).
 *
 * CROSS-REFERENCE: See gcdata.h for GCENTRY, HTmain
 * CROSS-REFERENCE: See gc2.c for OP_gcscan1, OP_gcscan2 opcodes
 */

#include "lispemul.h"
#include "lspglob.h"
#include "gcdata.h"
#include "lsptypes.h"

#include "gcscandefs.h"

#ifdef BIGVM
#define HTSTKBIT 0x10000 /* = 512 */
#define HTENDS ((struct hashentry *)htlptr)
#define GetStkCnt(entry1) ((entry1) >> 16)
#else
#define HTSTKBIT 0x200 /* = 512 */
#define HTENDS ((struct hashentry *)htlptr)
#define GetStkCnt(entry1) (entry1 >> 9)
#endif /* BIGVM */

int gcscan1(int probe)
/* probe is offset */
{
  struct htlinkptr *htlptr; /* overlay access method */
  int contents;
  while (--probe >= 0) /* End of HTmain Table ? */
  {
    /* Start addr. of scanning */
    htlptr = (struct htlinkptr *)(HTmain + probe);
    contents = ((struct htlinkptr *)GCPTR(htlptr))->contents;
    if (contents && (((struct hashentry *)GCPTR(HTENDS))->collision || (GetStkCnt(contents) == 0)))
      return (probe);
  }
  return (-1);
}

int gcscan2(int probe)
/* probe is offset */
{
  struct htlinkptr *htlptr; /* overlay access method */
  while (--probe >= 0)               /* End of HTmain Table ? */
  {
    htlptr = (struct htlinkptr *)(HTmain + probe);
    /* Start addr. of scanning */
    if (((HTSTKBIT | 1) & ((struct htlinkptr *)GCPTR(htlptr))->contents) != 0)
      return (probe); /* stackref or collision ON */
  }
  return (-1);
}
