/* FILE: dspif.c - Display Interface Abstraction Layer
 *
 * HIGH CONFIDENCE: This file implements the display interface abstraction
 * layer for Maiko. It provides a generic interface that can be implemented
 * by different display backends (X11, SDL, etc.).
 *
 * DISPLAY INTERFACE ARCHITECTURE:
 * - DspInterface: Abstract interface for display operations
 * - curdsp: Current display interface instance
 * - currentdsp: Global pointer to active display interface
 *
 * GENERIC DISPLAY FUNCTIONS:
 * - make_dsp_instance(): Creates display interface instance (empty implementation)
 * - GenericReturnT(): Utility function that returns T (true)
 * - GenericReturnVoid(): Utility function that returns void
 * - GenericPanic(): Panic function for uninitialized display slots
 * - describedsp(): Prints display interface information for debugging
 *
 * DISPLAY CAPABILITIES:
 * - Display dimensions (width, height)
 * - Bits per pixel (depth)
 * - Color information
 * - Graphics mode
 * - Number of banks
 * - Display operations (bitblt_to_screen, cleardisplay, etc.)
 *
 * PLATFORM-SPECIFIC IMPLEMENTATIONS:
 * - X11 backend: Implemented in xinit.c and xlspwin.c
 * - SDL backend: Implemented in sdl.c
 * - Other backends can be added by implementing DspInterface
 *
 * CROSS-REFERENCE: Display operations in dspsubrs.c and sdl.c
 * CROSS-REFERENCE: Display initialization in initdsp.c
 * CROSS-REFERENCE: Display interface definition in devif.h
 */

/************************************************************************/
/*									*/
/*	(C) Copyright 1989, 1990, 1990, 1991, 1992, 1993, 1994,         */
/*                    1995, 1999 Venue.                                 */
/*	    All Rights Reserved.		                        */
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

#include "version.h"

#include <stdio.h>
#include <stdlib.h>
#include "lispemul.h"
#include "dbprint.h"
#include "devif.h"

#include "dspifdefs.h"
#include "xinitdefs.h"

static DspInterfaceRec curdsp = {0};
extern DspInterface currentdsp;
DspInterface currentdsp = &curdsp;

void make_dsp_instance(DspInterface dsp, char *lispbitmap, int width_hint, int height_hint,
                       int depth_hint)
{
} /* Now we know the maximum capabilities of the hardware. */

/*********************************************************************/
/*                                                                   */
/*		     G e n e r i c R e t u r n T                     */
/*                                                                   */
/* Utility function that just returns T                              */
/*                                                                   */
/*********************************************************************/
unsigned long GenericReturnT(void *d)
{
  (void)d;
  return (T);
}
void GenericReturnVoid(void *d)
{
  (void)d;
  return;
}
void GenericPanic(void *d)
{
  (void)d;
  TPRINT(("Enter GenericPanic\n"));
  (void)fprintf(stderr, "Panic! Call to uninitialized display slot!");
  exit(0);
}

void describedsp(DspInterface dsp)
{
  if (dsp == 0)
  {
    printf("describedsp: Not a dsp!\n");
    return;
  }

  printf("\n");
  printf("width= %d\n", dsp->Display.width);
  printf("height= %d\n", dsp->Display.height);
  printf("bitsperpixel= %d\n", dsp->bitsperpixel);
  printf("colors= %lu\n", dsp->colors);
  printf("graphicsmode= %lu\n", dsp->graphicsmode);
  printf("numberofbanks= %lu\n", dsp->numberofbanks);
  printf("bitblt_to_screen= %p\n", (void *)dsp->bitblt_to_screen);
  printf("cleardisplay= %p\n", (void *)dsp->cleardisplay);
  fflush(stdout);
}
