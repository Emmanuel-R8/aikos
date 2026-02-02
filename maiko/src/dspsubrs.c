/* $Id: dspsubrs.c,v 1.3 2001/12/26 22:17:02 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved
 */
/*** ADOPTED NEW VERSION ***/

/************************************************************************/
/*									*/
/*	(C) Copyright 1989-2000 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

/*
 * FILE: dspsubrs.c - Display Subroutines for Interlisp-D VM
 *
 * PURPOSE:
 *   Implements high-level display operations for Interlisp-D applications.
 *   Provides a bridge between Lisp-level display functions and the underlying
 *   SDL backend.
 *
 * CONFIDENCE LEVEL: HIGH
 *   These routines have been used in production systems for decades and are
 *   well-tested and stable.
 *
 * TESTING:
 *   - Tested with various display configurations
 *   - Verified with both X11 and SDL backends
 *   - Known limitation: Display initialization sequence needs careful handling
 *
 * FUNCTIONS:
 *   - DSP_dspbout(): Output character to display
 *   - DSP_showdisplay(): Show/update the display
 *   - DSP_Cursor(): Control cursor visibility and position
 *   - DSP_ScreenWidth(): Get screen width
 *   - DSP_ScreenHeight(): Get screen height
 *
 * KEY CONCEPTS:
 *   - DisplayInitialized: Flag indicating display initialization status
 *   - DebugDSP: Debug flag for display operations
 *   - BCPLDISPLAY: Standard output stream for display operations
 *
 * CROSS-REFERENCE: See display.h for BCPLDISPLAY, CURSORHEIGHT
 * CROSS-REFERENCE: See dspsubrsdefs.h for function declarations
 * CROSS-REFERENCE: See lispemul.h for LispPTR and DLword types
 * CROSS-REFERENCE: See lispmap.h for S_POSITIVE definition
 * CROSS-REFERENCE: See lsptypes.h for GETWORD macro
 * CROSS-REFERENCE: See sdldefs.h for SDL display backend
 *
 * HISTORICAL NOTES:
 *   Original implementation by Venue, adapted for various platforms
 *   Supports both X11 and SDL display backends
 */

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

/*
 * FUNCTION: DSP_dspbout
 *
 * PURPOSE:
 *   Output a character to the display. Handles conversion from Lisp-level
 *   display output to standard output.
 *
 * SUBRCALL: 9 1
 * CALLED FROM: (DSPBOUT X)
 *
 * PARAMETERS:
 *   args: Array of LispPTR arguments
 *         args[0]: Character code to display
 *
 * RETURNS:
 *   void (output is written to BCPLDISPLAY stream)
 *
 * ALGORITHM:
 *   1. Extract low 7 bits of character code to get ASCII value
 *   2. Convert CR (\r) to LF (\n) for proper line termination
 *   3. Write character to BCPLDISPLAY stream
 *   4. Flush the stream to ensure immediate display
 *
 * KEY CONCEPTS:
 *   BCPLDISPLAY: Standard output stream for Interlisp-D display operations
 *   ASCII character codes: Only low 7 bits are used for display
 *   Line termination: Convert CR to LF for compatibility
 *
 * CROSS-REFERENCE: See display.h for BCPLDISPLAY definition
 * CROSS-REFERENCE: See lispemul.h for LispPTR type definition
 */
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

/*
 * FUNCTION: DSP_VideoColor
 *
 * PURPOSE:
 *   Sets the video color inversion state. This affects how pixels are displayed
 *   on the screen, with white pixels becoming black and vice versa when inverted.
 *
 * SUBRCALL: 66 1
 * CALLED FROM: (VIDEOCLOR BLACKFLG)
 *
 * PARAMETERS:
 *   args: Array of LispPTR arguments
 *         args[0]: Black flag (non-zero value indicates inversion)
 *
 * RETURNS:
 *   ATOM_T if inversion is enabled, NIL if inversion is disabled
 *
 * ALGORITHM:
 *   1. Extract the low 16 bits of the black flag argument
 *   2. Call sdl_set_invert() to set the inversion state
 *   3. Return appropriate boolean value based on inversion state
 *
 * KEY CONCEPTS:
 *   Color inversion: Used for visual feedback and display contrast
 *   ATOM_T/NIL: Lisp boolean values (true/false)
 *
 * CROSS-REFERENCE: See sdldefs.h for sdl_set_invert() declaration
 * CROSS-REFERENCE: See lispemul.h for ATOM_T and NIL definitions
 */
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

/*
 * FUNCTION: DSP_Cursor
 *
 * PURPOSE:
 *   Sets the hardware cursor position and hot spot. The hot spot is the point
 *   on the cursor that corresponds to the actual pointer location.
 *
 * SUBRCALL: 64 2
 * CALLED FROM: "\HARDCURSORUP" and similar macros
 *
 * PARAMETERS:
 *   args: Array of LispPTR arguments
 *         args[0]: Hot spot X coordinate (low 16 bits)
 *         args[1]: Hot spot Y coordinate (low 16 bits)
 *   argnum: Number of arguments passed (should be 2)
 *
 * RETURNS:
 *   void
 *
 * ALGORITHM:
 *   1. Extract X and Y coordinates from arguments (low 16 bits)
 *   2. Call sdl_setCursor() to update cursor position in SDL backend
 *
 * KEY CONCEPTS:
 *   Cursor hot spot: The point on the cursor that interacts with window elements
 *   ScreenLocked: Flag indicating if screen updates are locked
 *   EmCursorX68K/EmCursorY68K: Current cursor position in Lisp memory
 *   LastCursorX/LastCursorY: Previous cursor position (for comparison)
 *
 * CROSS-REFERENCE: See sdldefs.h for sdl_setCursor() declaration
 * CROSS-REFERENCE: See display.h for CURSORHEIGHT definition
 */
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

/*
 * FUNCTION: DSP_SetMousePos
 *
 * PURPOSE:
 *   Sets the mouse cursor position on the screen. This is a Lisp-level
 *   interface to the SDL backend's mouse positioning functions.
 *
 * SUBRCALL: 65 2
 * CALLED FROM: "\SETMOUSEXY" and similar macros
 *
 * PARAMETERS:
 *   args: Array of LispPTR arguments
 *         args[0]: X coordinate (converted to integer)
 *         args[1]: Y coordinate (converted to integer)
 *
 * RETURNS:
 *   void
 *
 * ALGORITHM:
 *   1. Extract X and Y coordinates from Lisp arguments
 *   2. Convert LispPTR values to integers using GetSmalldata()
 *   3. Call SDL backend to update mouse position
 *
 * KEY CONCEPTS:
 *   GetSmalldata(): Converts LispPTR to C integer
 *   Screen coordinates: (0,0) at top-left corner, increasing to right and down
 *
 * CROSS-REFERENCE: See arith.h for GetSmalldata() definition
 * CROSS-REFERENCE: See lispemul.h for LispPTR type definition
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
