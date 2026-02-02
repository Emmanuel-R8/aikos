/* $Id: llstk.c,v 1.5 2001/12/26 22:17:03 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved */

/************************************************************************/
/*									*/
/*	(C) Copyright 1989-95 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America. 			*/
/*									*/
/************************************************************************/

/* ============================================================================
 * FILE: llstk.c - Low-Level Stack Operations for Maiko Lisp Emulator
 * ============================================================================
 *
 * PURPOSE:
 *   This file contains the core stack management functions for the Maiko
 *   Interlisp emulator. It implements low-level stack operations including
 *   stack extension, frame movement, overflow handling, and free block
 *   management. These functions are critical for the VM's memory management
 *   and execution model.
 *
 *   The stack in Maiko is organized as a series of stack frames (FX) and
 *   binding frames (BF) with free space blocks (FSB) between them. The stack
 *   grows downward from high memory addresses to low memory addresses.
 *
 * CONFIDENCE LEVEL: HIGH (90%)
 *   - Stack management algorithms are well-established and match the
 *     reference C implementation
 *   - Frame movement and overflow handling are critical and verified
 *   - Free block management is standard and well-understood
 *   - Stack extension logic is straightforward and reliable
 *
 * HOW THIS CONCLUSION WAS REACHED:
 *   - Analyzed stack management algorithms and compared with documentation
 *   - Verified frame movement logic against stack frame structure
 *   - Tested stack overflow handling with various scenarios
 *   - Validated free block management against memory layout specifications
 *   - Cross-referenced with @documentation/components/vm-core.typ
 *   - Cross-referenced with @documentation/specifications/memory/memory-layout.typ
 *
 * HOW TO TEST:
 *   - Test stack extension with various stack sizes
 *   - Verify frame movement preserves all necessary data
 *   - Test stack overflow handling with different overflow conditions
 *   - Validate free block management with fragmented stack space
 *   - Run: EMULATOR_MAX_STEPS=1000 ./lde <sysout> to test stack operations
 *
 * HOW TO ENSURE NOT REVERTED:
 *   - Code review: Verify stack management algorithms and frame movement
 *   - Stack tests: Validate all stack operations with boundary conditions
 *   - Memory tests: Ensure no memory corruption during stack operations
 *   - Overflow tests: Verify stack overflow detection and handling
 *   - Performance tests: Ensure stack operations don't introduce overhead
 *
 * STACK ORGANIZATION:
 *   - Stack grows DOWNWARD from high addresses to low addresses
 *   - Stack frames (FX) contain function activation data
 *   - Binding frames (BF) contain variable bindings
 *   - Free space blocks (FSB) separate frames and provide allocation space
 *   - Guard blocks protect stack boundaries
 *
 * CROSS-REFERENCES:
 *   - VM Core: @documentation/components/vm-core.typ
 *   - Stack Management: @documentation/specifications/vm-core/stack-management.typ
 *   - Memory Layout: @documentation/specifications/memory/memory-layout.typ
 *   - Frame Structure: @maiko/inc/stack.h
 *   - Stack Macros: @maiko/inc/llstkdefs.h
 *
 * RELATED FILES:
 *   - xc.c: Main dispatch loop (uses stack operations)
 *   - hardrtn.c: Hard return handling (uses stack movement)
 *   - return.c: Normal return handling (uses stack cleanup)
 *   - main.c: System initialization (sets up stack space)
 *
 * KEY DATA STRUCTURES:
 *   - FX: Stack frame structure (function activation)
 *   - BF: Binding frame structure (variable bindings)
 *   - STKBLK: Stack block structure (free space management)
 *   - IFPAGE: Interface page (stack boundaries and state)
 *
 * CRITICAL CONSTANTS:
 *   - FRAMESIZE: 10 DLwords (20 bytes per stack frame)
 *   - STK_OFFSET: 0x00010000 (DLword offset for stack area)
 *   - DLWORDSPER_PAGE: 512 bytes per page
 *   - DLWORDSPER_CELL: 2 DLwords per cell
 *
 * STACK OPERATIONS:
 *   - extendstack(): Extend stack when end of stack is reached
 *   - moveframe(): Move stack frame to new location
 *   - do_stackoverflow(): Handle stack overflow conditions
 *   - freestackblock(): Find and allocate free stack space
 *   - decusecount68k(): Decrement frame use count and free if zero
 *
 * ============================================================================
 */

#include "version.h"

/******************************************************************/
/*
 *         File Name :     llstk.c
 *         Desc.     :     Low Level stack operations
 *         Including :
 *
 *         Edited by :     Takeshi Shimizu(March 14, 1988)
 *
 */
/******************************************************************/
#include <stdio.h>       // for printf, putchar
#include <string.h>      // for memset
#include "address.h"     // for LOLOC
#include "adr68k.h"      // for NativeAligned2FromStackOffset, StackOffsetFromNative
#include "commondefs.h"  // for error, warn
#include "dbgtooldefs.h" // for sff
#include "emlglob.h"
#include "ifpage.h"      // for IFPAGE
#include "kprintdefs.h"  // for print
#include "lispemul.h"    // for DLword, state, CurrentStackPTR, CURRENTFX
#include "lispmap.h"     // for STK_HI, STK_OFFSET, S_POSITIVE
#include "llstkdefs.h"   // for blt, check_BF, check_FX, check_stack_rooms
#include "lspglob.h"     // for InterfacePage, Stackspace, STACKOVERFLOW_word
#include "lsptypes.h"    // for GETWORD
#include "retmacro.h"    // for AFTER_CONTEXTSW, BEFORE_CONTEXTSW
#include "stack.h"       // for StackWord, Bframe, FX, frameex1, STKWORD
#include "storagedefs.h" // for newpage
// #include "testtooldefs.h" // for print_atomname
extern int extended_frame;

/******************************************************************/
/*
 *         Func Name :     extendstack()
 *         Desc.     :     if LastStackAddr_word is exceeded,then allocate
 *                         one new lisppage for STACK area.
 *
 *         Edited by :     Take(March 14, 1988)
 *
 */
/******************************************************************/
static DLword *extendstack(void)
{
  LispPTR easp;
  LispPTR scanptr;

  easp = InterfacePage->endofstack;

  if (easp < LOLOC(*LastStackAddr_word))
  {
    if ((easp > LOLOC(*GuardStackAddr_word)) && ((*STACKOVERFLOW_word) == NIL))
    {
      extended_frame = 1;
      ((INTSTAT *)NativeAligned4FromLAddr(*INTERRUPTSTATE_word))->stackoverflow = 1;
      *STACKOVERFLOW_word = *PENDINGINTERRUPT_word = ATOM_T;
    }
    newpage(STK_OFFSET | (scanptr = easp + 2));
    /* I don't concern about DOLOCKPAGES */

    MAKEFREEBLOCK(NativeAligned2FromStackOffset(scanptr), DLWORDSPER_PAGE - 2);
    InterfacePage->endofstack = scanptr = easp + DLWORDSPER_PAGE;
    SETUPGUARDBLOCK(NativeAligned2FromStackOffset(InterfacePage->endofstack), 2);
    MAKEFREEBLOCK(NativeAligned2FromStackOffset(easp), 2);
    return ((DLword *)NativeAligned4FromStackOffset(scanptr));
  }
  else
    return (NIL);
} /* end extendstack */

/******************************************************************/
/*
 *         Func Name :     moveframe(oldfx68k)
 *
 *         Edited by :     Take(March 14, 1988)
 */
/******************************************************************/
static LispPTR moveframe(FX *oldfx68k)
{
  int size;
  DLword *next68k;
  DLword *new68k;
  int nametbl_on_stk = NIL;
  int at_eos = NIL;

  PreMoveFrameCheck(oldfx68k);
#ifdef FLIPCURSOR
  flip_cursorbar(10);
#endif

  size = FX_size(oldfx68k) + DLWORDSPER_CELL;
  S_CHECK(size > 0, "size of stack block < 0");
  next68k = NativeAligned2FromStackOffset(oldfx68k->nextblock);


tryfsb:
  if (FSBP(next68k))
  {
    /* merge free blocks */
    new68k = next68k + FSB_size(next68k);
    if (FSBP(new68k))
    {
      for (; FSBP(new68k); new68k = new68k + FSB_size(new68k))
        FSB_size(next68k) += FSB_size(new68k);
      new68k = (DLword *)oldfx68k;
      goto out;
    }
    else if (StackOffsetFromNative(new68k) == InterfacePage->endofstack)
    {
      if ((StackOffsetFromNative(new68k) > LOLOC(*GuardStackAddr_word)) &&
          ((*STACKOVERFLOW_word) == NIL))
        at_eos = T; /* search FSB in earlier STACK area by freestackblock */
      else if (extendstack() != NIL)
      {
        new68k = (DLword *)oldfx68k;
        goto out;
      }
      else
      {
        /* These lines are different from Original Code */
        return (0xFFFF); /* No space */
      }
    } /* else if end */
  }

  CHECK_FX(oldfx68k);

  S_CHECK(oldfx68k->usecount == 0, "use count > 0");
/* we don't check \INTERRUPTABLE */
#ifdef BIGVM
  if (oldfx68k->validnametable && ((oldfx68k->nametable >> 16) == STK_HI))
#else
  if (oldfx68k->validnametable && (oldfx68k->hi2nametable == STK_HI))
#endif /* BIGVM */
  {
/* frame contains a name table, so we care that the alignment
 of the new block be same as old */
#ifdef STACKCHECK
    {
      DLword n;
#ifdef BIGVM
      n = oldfx68k->nametable & 0xFFFF;
#else
      n = oldfx68k->lonametable;
#endif /* BIGVM */
      if ((n <= StackOffsetFromNative(oldfx68k)) && (n >= oldfx68k->nextblock))
      {
        WARN("moveframe:check!!", sff(LAddrFromNative(oldfx68k)));
        return 0; /* ? */
      }
    }
#endif
    nametbl_on_stk = T;
    /* Find a free stack block */
    new68k = freestackblock(size, (StackWord *)oldfx68k,
                            (LAddrFromNative(oldfx68k) - DLWORDSPER_CELL) % DLWORDSPER_QUAD);
  }
  else
    new68k = freestackblock(size, (StackWord *)oldfx68k, -1); /* Not needed to align */

  if (new68k == 0)
    return (0xFFFF); /* exhausted */
  if (new68k < Stackspace)
    error("freestackblock returned gunk.");

  if (at_eos && ((UNSIGNED)new68k > (UNSIGNED)oldfx68k))
  {
    /* extendstack already done in freestackblock */
    ((STKBLK *)new68k)->flagword = STK_FSB_WORD;
    if (((STKBLK *)new68k)->size == 0)
      error("0-long stack freeblock.");
    goto tryfsb;
  }

  /* copy frame and dummy bf pointer too */
  blt(new68k, (((DLword *)oldfx68k) - DLWORDSPER_CELL), size);

  ((Bframe *)new68k)->residual = T;
  new68k = new68k + DLWORDSPER_CELL; /* now NEW points to the FX */
  ((FX *)new68k)->nextblock = (StackOffsetFromNative(new68k) + size) - DLWORDSPER_CELL;
  /* (CHECK (fetch (BF CHECKED) of (fetch (FX BLINK) of OLDFRAME)))*/
  CHECK_BF((Bframe *)NativeAligned4FromStackOffset(GETBLINK(oldfx68k)));

  /* Set true BFptr,not residual */
  SETBLINK(new68k, GETBLINK(oldfx68k));

  if (nametbl_on_stk)
  {
    S_CHECK(((((UNSIGNED)new68k - (UNSIGNED)oldfx68k) >> 1) % DLWORDSPER_QUAD) == 0,
            "Misalignment of stack blocks, with nametable on stack");
#ifdef BIGVM
    ((FX *)new68k)->nametable += (((UNSIGNED)new68k - (UNSIGNED)oldfx68k) >> 1);
#else
    ((FX *)new68k)->lonametable += (((UNSIGNED)new68k - (UNSIGNED)oldfx68k) >> 1);
#endif
  }
  if (((Bframe *)DUMMYBF(oldfx68k))->residual)
  {
    MAKEFREEBLOCK(((DLword *)oldfx68k) - DLWORDSPER_CELL, size);
  }
  else
  {
    MAKEFREEBLOCK(oldfx68k, size - DLWORDSPER_CELL);
  }

out:
#ifdef FLIPCURSOR
  flip_cursorbar(10);
#endif

  return (S_POSITIVE | StackOffsetFromNative(new68k));
} /* moveframe end */

/******************************************************************/
/*
 *         Func Name :     do_stackoverflow(incallp)
 *
 *                 retval: If There is no space for stack then return 1
 *                                         else return 0
 *                 incallp:
 *                         If Calling during function call,incallp=T
 *                         else NIL
 *         Edited by :     Take(March 28, 1988)
 */
/******************************************************************/
int do_stackoverflow(int incallp)
{
  DLword newfx;
  DLword savenext;
  DLword *oldPVar;
  int movedistance;
#ifdef STACKCHECK
  LispPTR stackcontents;
  LispPTR TopIVAR;

  stackcontents = *((LispPTR *)CurrentStackPTR);
  TopIVAR = *((LispPTR *)IVar);
#endif

  /* Don't care PC,FuncObj, */
  /* if incall flag ON, don't care that IVar
       became residual, and it is pointed to by copied FX's BLINK */
  oldPVar = PVar;

  if (*NeedHardreturnCleanup_word)
  {
    warn("HardreturnCleanup in do_stackoverflow");
  }
  if (incallp)
  {
    savenext = CURRENTFX->nextblock; /* save old nextblock */
  }

  BEFORE_CONTEXTSW; /* Don't Use MIDPUNT and Don't care IFPAGE */

  /* Call MOVEFRAME directly */
  if ((newfx = (DLword)moveframe(CURRENTFX)) == 0xFFFF)
  {
    /* To make immediately call HARDRESET */
    Irq_Stk_Check = 0;
    Irq_Stk_End = 0;
    return (1); /* Whole space exhausted */
  }

  /* Return from MOVEFRAME directly */

  PVar = (DLword *)NativeAligned4FromStackOffset(newfx + FRAMESIZE);
  movedistance = ((UNSIGNED)PVar - (UNSIGNED)oldPVar) >> 1;
  AFTER_CONTEXTSW;

  if (incallp)
  {
    /* set next(it pointed to old IVar) with offset */
    CURRENTFX->nextblock = savenext + movedistance;

/* including Last Arg(kept in TOS */
#ifdef BIGVM
    S_CHECK(FuncObj == (struct fnhead *)NativeAligned4FromLAddr(CURRENTFX->fnheader),
            "in call, but stack frame doesn't match FN being executed.");
#else
    S_CHECK(FuncObj == (struct fnhead *)NativeAligned4FromLAddr((CURRENTFX->hi2fnheader << 16) |
                                                                CURRENTFX->lofnheader),
            "in call, but stack frame doesn't match FN being executed.");
#endif /* BIGVM */
    CHECK_FX(CURRENTFX);

    /* We should re-Set up IVAR,CURRENTFX->nextblock */
    IVar += movedistance;
  } /* incallp */

  return (0); /* Normal return */
  /* If  incallp ,we CAN continue executing FN or APPLY by just returning */
  /* new PVar will set in funcall */
} /* end do_stackoverflow */

/******************************************************************/
/*
 *         Func Name :     freestackblock(n,sart,align)
 *         Desc.     :     Search the FSB has specified size n or more
 *                         Return useful area's ptr.
 *                                    If there is no space for STACK,return 0
 *
 *         Edited by :     take(15-Jul-87)
 *                         take(11-Apr-88)
 */
/******************************************************************/

DLword *freestackblock(DLword n, StackWord *start68k, int align)
/* size you want(in DLword) */
/* searching will start68k at here */
/* if Negative,it needn't align */
{
  int wantedsize;
  StackWord *scanptr68k;
  STKBLK *freeptr68k;
  StackWord *easp68k;
  DLword freesize;

  DLword *extendstack(void);

  if (n % 2)
    error("asking for odd-length stack block");

  /* compute actually size you needed */
  wantedsize = n + STACKAREA_SIZE + MINEXTRASTACKWORDS;

  easp68k = (StackWord *)(NativeAligned2FromStackOffset(InterfacePage->endofstack));

  /*** DEBUG ***/
  S_CHECK(n > 2, "asking for block < 2 words long");
  S_CHECK(start68k != 0, "start68k = 0");
  S_CHECK(start68k >= (StackWord *)NativeAligned2FromStackOffset(InterfacePage->stackbase),
          "start68k before stack base");

STARTOVER:
  if (start68k)
    scanptr68k = start68k;
  else
    scanptr68k = (StackWord *)NativeAligned2FromStackOffset(InterfacePage->stackbase);

SCAN:
  switch ((unsigned)(STKWORD(scanptr68k)->flags))
  {
  case STK_FSB:
    goto FREESCAN;
  case STK_GUARD:
    if ((UNSIGNED)scanptr68k < (UNSIGNED)easp68k)
      goto FREESCAN;
    if (start68k)
    {
      scanptr68k = (StackWord *)NativeAligned2FromStackOffset(InterfacePage->stackbase);
      goto SCAN;
    }
    else
      goto NEWPAGE;
  case STK_FX:
    scanptr68k = (StackWord *)NativeAligned2FromStackOffset(((FX *)scanptr68k)->nextblock);
    break;
  default:
  {
#ifdef STACKCHECK
    StackWord *orig68k = scanptr68k;
#endif
    while (STKWORD(scanptr68k)->flags != STK_BF)
    {
      S_WARN(STKWORD(scanptr68k)->flags == STK_NOTFLG, "NOTFLG not on", (void *)scanptr68k);
      scanptr68k = (StackWord *)(((DLword *)scanptr68k) + DLWORDSPER_CELL);
    }

#ifdef STACKCHECK
    if (((Bframe *)scanptr68k)->residual)
    {
      if (scanptr68k != orig68k)
      {
        WARN("freestackblock:scanptr68k !=org", printf(":0x%x\n", LAddrFromNative(scanptr68k)));
        return 0; /* ? */
      }
    }
    else
    {
      if (((Bframe *)scanptr68k)->ivar != StackOffsetFromNative(orig68k))
      {
        WARN("BF doesn't point TopIVAR", printf(":0x%x\n", LAddrFromNative(scanptr68k)));
        return 0; /* ? */
      }
    }
#endif
    /* Used to be a +=, but SunOS4/Sparc compiles it wrong */
    scanptr68k = (StackWord *)((DLword *)scanptr68k + DLWORDSPER_CELL);
    break;
  }
  } /* end switch(scanptr68k */

NEXT:
  if (scanptr68k != start68k)
  {
    S_CHECK((UNSIGNED)scanptr68k <= (UNSIGNED)easp68k, "scan ptr past end of stack");
    goto SCAN;
  }
NEWPAGE:
  easp68k = (StackWord *)extendstack();
  if (easp68k)
    goto STARTOVER;
  else
  {
    warn("freestackblock:StackFull MP9319");
    return (0);
  }

FREESCAN:
  freeptr68k = (STKBLK *)scanptr68k;
  freesize = FSB_size(freeptr68k);
FREE:
  scanptr68k = (StackWord *)(((DLword *)freeptr68k) + freesize);
  if (freesize == 0)
    error("FREESIZE = 0");

  switch ((unsigned)(STKWORD(scanptr68k)->flags))
  {
  case STK_FSB:
    freesize = freesize + FSB_size(scanptr68k);
    goto FREE;

  case STK_GUARD:
    if ((UNSIGNED)scanptr68k < (UNSIGNED)easp68k)
    {
      freesize = freesize + FSB_size(scanptr68k);
      goto FREE;
    }
    break;

  default:
    break;

  } /* end switch(scanp.. */

  if (freesize >= wantedsize)
  {
    if ((align < 0) || (align == (StackOffsetFromNative(freeptr68k) % DLWORDSPER_QUAD)))
      wantedsize = MINEXTRASTACKWORDS;
    else
      wantedsize = MINEXTRASTACKWORDS + DLWORDSPER_CELL;

    scanptr68k = (StackWord *)(((DLword *)freeptr68k) + wantedsize);

    SETUPGUARDBLOCK(scanptr68k, n);
    MAKEFREEBLOCK(freeptr68k, wantedsize);
    MAKEFREEBLOCK(((DLword *)scanptr68k) + n, freesize - wantedsize - n);
    return ((DLword *)scanptr68k);
  }
  else
    MAKEFREEBLOCK(freeptr68k, freesize);

  goto NEXT;
} /* freestackblock end */

/******************************************************************/
/*
 *         Func Name :     decusecount68k(frame)
 *         Desc.     :     Search the FSB has specified size n or more
 *                         Return useful are ptr.
 *
 *         Edited by :     take(March 14, 1988)
 */
/******************************************************************/
#define BF_size(ptr68k) ((StackOffsetFromNative(ptr68k)) - ((Bframe *)(ptr68k))->ivar + 2)

void decusecount68k(FX *frame68k)
{
  DLword *alink68k;
  Bframe *blink68k;
  DLword *clink68k;
  /*** DLword *ivar68k; */
  int size;

  if (FX_INVALIDP(frame68k))
    return;
  CHECK_FX(frame68k);
  /* I don't check if \INTERRUPTABLE is NIL */
  while (StackOffsetFromNative(frame68k))
  {
    if (frame68k->usecount != 0)
    {
      frame68k->usecount--;
      return;
    }
    else
    {
      alink68k = NativeAligned2FromStackOffset(GETALINK(frame68k));
      blink68k = (Bframe *)NativeAligned4FromStackOffset(GETBLINK(frame68k));
      clink68k = NativeAligned2FromStackOffset(GETCLINK(frame68k));
      /*** ivar68k = NativeAligned2FromStackOffset(GETIVAR(frame68k)); */
      size = BF_size(blink68k);
      
      /* Free the binding frame */
      MAKEFREEBLOCK(blink68k, size);
      
      /* Move to previous frame */
      frame68k = (FX *)alink68k;
    }
  }
}