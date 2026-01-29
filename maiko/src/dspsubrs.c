/* $Id: dspsubrs.c,v 1.3 2001/12/26 22:17:02 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved
 */
/*** ADOPTED NEW VERSION ***/

/************************************************************************/
/*									*/
/*	(C) Copyright 1989-2000 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

#include "version.h"

#include <stdio.h>        // for putc
#include "arith.h"        // for GetSmalldata
#include "display.h"      // for BCPLDISPLAY, CURSORHEIGHT
#include "dspsubrsdefs.h" // for DSP_Cursor, DSP_ScreenHight, DSP_ScreenWidth
#include "lispemul.h"     // for LispPTR, DLword, ATOM_T, NIL
#include "lispmap.h"      // for S_POSITIVE
#include "lsptypes.h"     // for GETWORD
#include "sdldefs.h"

extern int DebugDSP;
extern int displaywidth, displayheight;

/****************************************************
 *
 *	DSP_dspbout() entry of SUBRCALL 9 1
 *			called from (DSPBOUT X)
 *
 ****************************************************/

void DSP_dspbout(LispPTR *args) /* args[0] :	charcode	*/
{
  int charcode = (args[0] & 0x7F);
  /* Interlisp-D uses CR as EOL which isn't useful here */
  putc((charcode == '\r') ? '\n' : charcode, BCPLDISPLAY);
  fflush(BCPLDISPLAY);
}

/****************************************************
 *
 *	DSP_showdisplay() entry of SUBRCALL 19 2
 *			called from (SHOWDISPLAY BASE RASTERWIDTH)
 *
 ****************************************************/

extern int DisplayInitialized;

void DSP_showdisplay(LispPTR *args)
{
  LispPTR base = args[0];        /* pointer to the display bitmap */
  LispPTR rasterwidth = args[1]; /* should be a smallp */

  if (base == NIL)
  {
    DisplayInitialized = 0;
  }
  else
  {
    DisplayInitialized = 1;
  }
}

/****************************************************
 *
 *	DSP_VideoColor() entry of SUBRCALL 66 1
 *			called from (VIDEOCLOR BLACKFLG)
 *
 ****************************************************/

LispPTR DSP_VideoColor(LispPTR *args) /* args[0] :	black flag	*/
{
  int invert;
  invert = args[0] & 0xFFFF;
  sdl_set_invert(invert);
  if (invert)
    return ATOM_T;
  else
    return NIL;
}

extern struct cursor CurrentCursor;

/****************************************************
 *
 *	DSP_Cursor() entry of SUBRCALL 64 2
 *			called from "\HARDCURSORUP" etc.
 *
 ****************************************************/
void DSP_Cursor(LispPTR *args, int argnum)
/* args[0] :	hot spot X
 * args[1] :	hot spot Y
 */
{
  extern int ScreenLocked;
  extern DLword *EmCursorX68K, *EmCursorY68K;
  extern int LastCursorX, LastCursorY;

  sdl_setCursor((int)(args[0] & 0xFFFF), (int)(args[1] & 0xFFFF));
}

/****************************************************
 *
 *	DSP_SetMousePos() entry of SUBRCALL 65 2
 *			called from macro "\SETMOUSEXY" etc.
 *
 ****************************************************/
/* args[0] :	X pos
 * args[1] :	Y pos
 */
void DSP_SetMousePos(LispPTR *args)
{
  int x = (int)(GetSmalldata(args[0]));
  int y = (int)(GetSmalldata(args[1]));
  sdl_setMousePosition(x, y);
}

/****************************************************
 *
 *	DSP_ScreenWidth() entry of SUBRCALL 67 0
 *		called from  UPDATESCREENDIMENSIONS
 *
 ****************************************************/
LispPTR DSP_ScreenWidth(LispPTR *args)
{
  return (S_POSITIVE | (0xFFFF & displaywidth));
}

/****************************************************
 *
 *	DSP_ScreenHight() entry of SUBRCALL 68 0
 *		called from UPDATESCREENDIMENSIONS
 *
 ****************************************************/
LispPTR DSP_ScreenHight(LispPTR *args)
{
  return (S_POSITIVE | (0xFFFF & displayheight));
}

/****************************************************
 *
 *	flip_cursor()
 *
 ****************************************************/

extern DLword *EmCursorBitMap68K;
extern int for_makeinit;

void flip_cursor(void)
{
  DLword *word;
  int cnt;
  extern int ScreenLocked;
  extern DLword *EmCursorX68K, *EmCursorY68K;

  word = EmCursorBitMap68K;

#ifdef INIT

  /* since this is called frequently, and you don't want to have
     to build a different LDE to run the 2 parts of a Loadup, there is
     an ifdef AND a test.  This way we don't generate
     extra code for anybody else building an LDE
     except those who want to try building loadups.  */

  if (!for_makeinit)
  {
    for (cnt = CURSORHEIGHT; (cnt--);)
    {
      GETWORD(word++) ^= 0xFFFF;
    }
  }

#else

  for (cnt = CURSORHEIGHT; (cnt--);)
  {
    GETWORD(word++) ^= 0xFFFF;
  }

#endif

  sdl_setCursor(0, 0); // TODO: keep track of the current hot_x and hot_y
}
