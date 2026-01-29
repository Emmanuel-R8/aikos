/* $Id: llcolor.c,v 1.2 1999/01/03 02:07:15 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved
 */

/************************************************************************/
/*									*/
/*	(C) Copyright 1989-95 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

#include "version.h"

#include <stdio.h>

#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/file.h>

#include "lispemul.h"
#include "lispmap.h"
#include "lsptypes.h"
#include "address.h"
#include "adr68k.h"
#include "lspglob.h"
#include "emlglob.h"
#include "display.h"
#include "devconf.h"

#include "bb.h"
#include "bbtmacro.h"
#include "pilotbbt.h"
#include "dbprint.h"

#include "llcolordefs.h"

extern int MonoOrColor;
extern DLword *ColorDisplayRegion68k;
extern int Dispcolorsize;
extern int Inited_Color;

int MonoOrColor = MONO_SCREEN;
DLword *ColorDisplayRegion68k = NULL;
int Dispcolorsize = 0;
int Inited_Color = NIL;
extern struct pixrect *ColorDisplayPixrect;
extern int displaywidth, displayheight, FrameBufferFd;

/*******************************************************************/
/*	Func name	: cgfour_init_color_display(args)
        Arg(s)		: COLOR BITMAP ADDRESS(LISPPTR)
        Desc		: Assign to SUBR 0210
                          mmap LispPTR to Color Display FB.
        By  Takeshi
*/
/*******************************************************************/
LispPTR cgfour_init_color_display(LispPTR color_bitmapbase) /* SUBR 0210 */ /* COLOR BITMAP ADDRESS */
{
  printf("Color is not supported.\n");
  return (NIL);
}

/*******************************************************************/
/*	Func name	: cgfour_change_screen_mode(which_screen)
        Arg(s)		: MONO_SCREEN OR COLOR_SCREEN
        Desc		: Assign to SUBR 0211
                          Change screen Mono to Color,vice versa.
        By  Takeshi
*/
/*******************************************************************/
LispPTR cgfour_change_screen_mode(LispPTR which_screen)
{
  printf("Color is not supported.\n");
  return (NIL);
}

/*******************************************************************/
/*	Func name	: cgfour_set_colormap(args)
        Arg(s)		: Passed by args
                          index: colormap index(0~255)
                          red,green,blue:(0~255)
        Desc		: Assign to SUBR 0212
                          Set Colormap entry
        By  Takeshi
*/
/*******************************************************************/
LispPTR cgfour_set_colormap(LispPTR args[])
{
  printf("Color is not supported.\n");
  return (NIL);
}
