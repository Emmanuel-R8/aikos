/* $Id: unwind.c,v 1.3 1999/05/31 23:35:46 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved
 */

/************************************************************************/
/*									*/
/*	(C) Copyright 1989-95 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

#include "version.h"

/******************************************************************/
/*

                File Name  :	unwind.c

                Created    :	jul 17, 1987 by T.Shimizu
                Changed	   :	Sept 21 1988  BK

*/
/******************************************************************/

#include "emlglob.h"
#include "lispemul.h" // for LispPTR, state, DLword, PVar, CurrentStackPTR
#include "lspglob.h"
#include "unwinddefs.h" // for N_OP_unwind

LispPTR *N_OP_unwind(LispPTR *cstkptr, LispPTR tos, int n, int keep)
{
  int num;           /* number of UNBOUND slot */
  LispPTR *endptr;   /* unwind limit */
  LispPTR *lastpvar; /* points PVar slot that is unbounded. */

  /* Slots:
          -----------------
          |		|	<- PVar
          -----------------
          |	.	|
          |	.	|
          -----------------
          |		|	 ALL OF THE FOLLOWING LOCATIONS SCANNED:
          -----------------------------------------------------------------
          | tos if keep	|	<- endptr (PVar[n]) <- Result (no keep)	|
          -----------------						|
          |		|	<- Result (keep)			|
          -----------------						|
          |		|						|
          -----------------						|
          |	.	|						|
          |	.	|						|
          -----------------						|
          |   tos pushed	|	<- Start CSTKPTR			|
          -----------------------------------------------------------------
          |		|	<- CSTKPTR temporarily bumped pushing tos
          -----------------

          NOTE: upon return the emulator does a POP to get the new tos value

  */

  endptr = (LispPTR *)PVar + n; /* set unwind limit */

  if (endptr > cstkptr)
  {
    CurrentStackPTR = (DLword *)cstkptr;
    /* this would be ERROR_EXIT(tos); but for having to return a pointer */
    TopOfStack = tos;
    Error_Exit = 1;
    return (LispPTR *)(-1);
  }
  *cstkptr++ = tos;

  /* UNBOUND MARK loop  */

  while (cstkptr > endptr)
  {
    /* Look for the Next BIND marker */

    if ((num = (int)*--cstkptr) < 0)
    {
      /* Now UNBIND the PVARS indicated by the BIND marker */

      lastpvar = (LispPTR *)(2 + PVar + (unsigned short)num);
      num = ~(num >> 16) + 1;
      for (; --num > 0;)
      {
        *--lastpvar = 0xffffffff; /* Mark as UNBOUND */
      }
    }
  }

  /* endptr = cstkptr */

  if (keep)
  {
    *(cstkptr++) = tos;
  }
  return (cstkptr);

} /* N_OP_unwind */
