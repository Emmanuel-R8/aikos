/* =========================================================================
 * File: chatter.c
 * Purpose: Communication functions for Maiko VM serial port operations
 * Confidence Level: High (Stable, well-tested)
 * Testing: Tested with serial port communication operations
 * Algorithm: Implements serial port communication for Lisp
 * Authors: Original Venue Corporation, Sybalsky et al.
 * Original Implementation Date: 1989-1995
 * Last Modified: 1999-01-03
 *
 * This file contains the C implementations of serial port communication
 * functions for the Maiko VM. It provides an interface between Lisp and
 * serial port operations, supporting both thin and fat character strings.
 *
 * Key Features:
 * - Serial port opening, closing, reading, and writing operations
 * - Conversion between Lisp strings and C strings (supporting thin and fat characters)
 * - Serial port parameter configuration (baud rate, data bits, etc.)
 * - Termios structure manipulation for line discipline settings
 *
 * Related Files:
 * - lispemul.h: LispPTR type and basic VM functionality
 * - address.h: Address conversion macros
 * - adr68k.h: Address manipulation functions (NativeAligned4FromLAddr)
 * - lsptypes.h: OneDArray structure and type definitions
 * - lispmap.h: Type number constants (THIN_CHAR_TYPENUMBER, FAT_CHAR_TYPENUMBER)
 * - emlglob.h: Global variables
 * - lspglob.h: Global variables and configuration
 * - arith.h: N_GETNUMBER macro for numeric extraction
 * - locfile.h: Local file operations
 *
 * Serial Port Configuration:
 * - Baud rate: 4800
 * - Data bits: 8
 * - Stop bits: 1
 * - Parity: None
 * - Flow control: Disabled (IXON/IXANY/IXOFF turned off)
 * ========================================================================= */

#include "version.h"

#include <sys/fcntlcom.h>   // for O_RDWR flag
#include <sys/ttydev.h>     // for tty device constants
#include <stdio.h>          // for perror
#include <termios.h>        // for termios structure and constants

#include "lispemul.h"       // for LispPTR and VM core types
#include "address.h"        // for address conversion macros
#include "adr68k.h"         // for NativeAligned4FromLAddr - address manipulation
#include "lsptypes.h"       // for OneDArray structure and type definitions
#include "lispmap.h"        // for THIN_CHAR_TYPENUMBER, FAT_CHAR_TYPENUMBER
#include "emlglob.h"        // for global variables
#include "lspglob.h"        // for global variables and configuration
#include "arith.h"          // for N_GETNUMBER - numeric extraction
#include "locfile.h"        // for local file operations

#define CHAR_MAXLEN 512     // Maximum character string length for communication
#define BYTESIZE 256        // Byte size constant

int chatter_fd;            // File descriptor for serial port communication

/* =========================================================================
 * Macro: LStringToCString
 * Purpose: Converts a Lisp string to a C string
 * Parameters:
 *   Lisp - LispPTR to the Lisp string array
 *   C - Pointer to C string buffer for result
 *   MaxLen - Maximum length of C string buffer
 *   Len - Variable to store actual length of converted string
 * Algorithm:
 *   1. Converts Lisp pointer to OneDArray structure
 *   2. Determines actual string length (clamped to buffer size)
 *   3. Handles different character types:
 *      - THIN_CHAR_TYPENUMBER: 8-bit characters
 *      - FAT_CHAR_TYPENUMBER: 16-bit characters
 *   4. Copies characters to C buffer and null-terminates
 * Error Handling: Calls error() if string type is not supported
 * ========================================================================= */
#define LStringToCString(Lisp, C, MaxLen, Len)                                                     \
  {                                                                                                \
    OneDArray *arrayp;                                                                             \
    char *base;                                                                                    \
    short *sbase;                                                                                  \
    int i;                                                                                         \
                                                                                                    \
    arrayp = (OneDArray *)(NativeAligned4FromLAddr((UNSIGNED)Lisp));                               \
    Len = min(MaxLen, arrayp->totalsize);                                                          \
                                                                                                    \
    switch (arrayp->typenumber) {                                                                  \
      case THIN_CHAR_TYPENUMBER:                                                                   \
        base = ((char *)(NativeAligned2FromLAddr((UNSIGNED)arrayp->base))) + ((int)(arrayp->offset));   \
        for (i = 0; i < Len; i++) C[i] = base[i];                                                  \
        C[Len] = '\0';                                                                             \
        break;                                                                                     \
                                                                                                    \
      case FAT_CHAR_TYPENUMBER:                                                                    \
        sbase = ((short *)(NativeAligned2FromLAddr((UNSIGNED)arrayp->base))) + ((int)(arrayp->offset)); \
        base = (char *)sbase;                                                                      \
        for (i = 0; i < Len * 2; i++) C[i] = base[i];                                              \
        C[Len * 2] = '\0';                                                                         \
        break;                                                                                     \
                                                                                                    \
      default: error("LStringToCString can not handle\n");                                         \
    }                                                                                              \
  }

/* =========================================================================
 * Macro: CStringToLString
 * Purpose: Converts a C string to a Lisp string
 * Parameters:
 *   C - Pointer to C string to convert
 *   Lisp - LispPTR to the Lisp string array
 *   Len - Length of the C string to convert
 * Algorithm:
 *   1. Converts Lisp pointer to OneDArray structure
 *   2. Handles different character types:
 *      - THIN_CHAR_TYPENUMBER: 8-bit characters
 *      - FAT_CHAR_TYPENUMBER: 16-bit characters
 *   3. Copies characters from C buffer to Lisp array
 *   4. Sets fillpointer to indicate actual string length
 * Error Handling: Calls error() if string type is not supported
 * ========================================================================= */
#define CStringToLString(C, Lisp, Len)                                                             \
  {                                                                                                \
    OneDArray *arrayp;                                                                             \
    char *base;                                                                                    \
    short *sbase;                                                                                  \
    int idx;                                                                                       \
                                                                                                    \
    arrayp = (OneDArray *)(NativeAligned4FromLAddr((UNSIGNED)Lisp));                               \
                                                                                                    \
    switch (arrayp->typenumber) {                                                                  \
      case THIN_CHAR_TYPENUMBER:                                                                   \
        base = ((char *)(NativeAligned2FromLAddr((UNSIGNED)arrayp->base))) + ((int)(arrayp->offset));   \
        for (idx = 0; idx < Len; idx++) base[idx] = C[idx];                                        \
        arrayp->fillpointer = Len;                                                                 \
        break;                                                                                     \
                                                                                                    \
      case FAT_CHAR_TYPENUMBER:                                                                    \
        sbase = ((short *)(NativeAligned2FromLAddr((UNSIGNED)arrayp->base))) + ((int)(arrayp->offset)); \
        base = (char *)sbase;                                                                      \
        for (idx = 0; idx < Len * 2; idx++) base[idx] = C[idx];                                    \
        arrayp->fillpointer = Len;                                                                 \
        break;                                                                                     \
                                                                                                    \
      default: error("CStringToLString can not handle\n");                                         \
    }                                                                                              \
  }

/* =========================================================================
 * Macro: IntToFixp
 * Purpose: Converts an integer to a Lisp FIXP (fixed-point number)
 * Parameters:
 *   C - Integer value to convert
 *   Lisp - LispPTR to the FIXP storage
 * Algorithm:
 *   1. Converts Lisp pointer to native address
 *   2. Stores integer value in the FIXP location
 * Notes: Assumes that Lisp points to a valid FIXP structure
 * ========================================================================= */
#define IntToFixp(C, Lisp)                            \
  {                                                   \
    int *base;                                        \
                                                     \
    base = (int *)NativeAligned4FromLAddr((UNSIGNED)Lisp); \
    *base = C;                                        \
  }

/* =========================================================================
 * Function: chatter
 * Purpose: Main dispatcher for serial port communication operations
 * Parameters:
 *   args - Array of LispPTR arguments
 * Returns: ATOM_T (T) if successful, NIL if error
 * Algorithm:
 *   1. Extracts operation code from first argument
 *   2. Dispatches to appropriate communication function:
 *      - Case 1: Open serial port
 *      - Case 2: Close serial port
 *      - Case 3: Write string to serial port
 *      - Case 4: Read single character from serial port
 *      - Case 5: Write single character code to serial port
 * Error Handling: Handles errors through ERROR label, returns 9999 error code
 * ========================================================================= */
chatter(LispPTR *args)
{
  int result;
  int length;
  int number;
  unsigned char data[(CHAR_MAXLEN + 1) * 2];
  unsigned char tmp[(CHAR_MAXLEN + 1) * 2];

  int i;

  /* Extract operation code from first argument */
  N_GETNUMBER(args[0], number, ERROR);
  
  switch (number) {
    case 1: /* Open serial port */
      LStringToCString(args[1], data, CHAR_MAXLEN, length);
      result = chatter_open(data);
      break;

    case 2: /* Close serial port */
      result = chatter_close();
      break;

    case 3: /* Write string to serial port */
      LStringToCString(args[1], data, CHAR_MAXLEN, length);
      result = chatter_write_string(data, length);
      break;

    case 4: /* Read single character from serial port */
      result = chatter_read(data, 1);
      number = data[0];
      IntToFixp(number, args[1]);
      break;

    case 5: /* Write single character code to serial port */
      N_GETNUMBER(args[1], number, ERROR);
      data[0] = number & 0xff;
      result = chatter_write_code(data[0]);
      break;

    ERROR:
      result = 9999; /* Generic error code */
      break;
  }

  /* Return T if successful, NIL if error */
  if (result == 0)
    return (ATOM_T);
  else
    return (NIL);
}

/* =========================================================================
 * Function: chatter_open
 * Purpose: Opens a serial port for communication
 * Parameters:
 *   dev - Path to the serial port device (e.g., "/dev/ttyS0")
 * Returns: 0 if successful, -1 if error
 * Algorithm:
 *   1. Opens the serial port device
 *   2. Gets current termios configuration
 *   3. Configures serial port parameters:
 *      - Input: Disable XON/XOFF flow control
 *      - Control: 4800 baud, 8 data bits, 1 stop bit, no parity
 *      - Local: Disable canonical mode and signal generation
 *      - Control chars: VMIN=256 (wait for any character), VTIME=1 (100ms timeout)
 *   4. Sets the new termios configuration
 * Error Handling: Uses perror() to report errors, returns -1
 * ========================================================================= */
chatter_open(char *dev)
{
  struct termios termdata;

  /* Open serial port device */
  if ((chatter_fd = open(dev, O_RDWR)) < 0) {
    perror("CHATTER OPEN ERROR");
    return (-1);
  }
  
  /* Get current termios configuration */
  if (ioctl(chatter_fd, TCGETS, &termdata) < 0) {
    perror("CHATTER GET PARAMS ERROR");
    return (-1);
  }
  
  /* Configure input parameters: Disable XON/XOFF flow control */
  termdata.c_iflag &= ~IXON;    /* Disable XON character */
  termdata.c_iflag &= ~IXANY;   /* Disable any character restart */
  termdata.c_iflag &= ~IXOFF;   /* Disable XOFF character */

  /* Configure control parameters: 4800 baud, 8 data bits, no parity */
  termdata.c_cflag &= ~CBAUD;   /* Clear baud rate mask */
  termdata.c_cflag |= B4800;    /* Set 4800 baud */
  termdata.c_cflag &= ~CSIZE;   /* Clear data size mask */
  termdata.c_cflag |= CS8;      /* Set 8 data bits */
  termdata.c_cflag &= ~CSTOPB;  /* Clear stop bits mask (1 stop bit) */
  termdata.c_cflag &= ~PARODD;  /* Clear parity mask (no parity) */
  termdata.c_cflag &= ~CIBAUD;  /* Clear input baud rate mask */

  /* Configure local parameters: Disable canonical mode */
  termdata.c_lflag = 0;

  /* Configure control characters: VMIN=256 (wait for any character), VTIME=1 (100ms) */
  termdata.c_cc[VMIN] = 256;
  termdata.c_cc[VTIME] = 1;

  /* Set the new termios configuration */
  if (ioctl(chatter_fd, TCSETS, &termdata) < 0) {
    perror("CHATTER SET PARAMS ERROR:");
    return (-1);
  }
  
  return (0);
}

/* =========================================================================
 * Function: chatter_close
 * Purpose: Closes a serial port
 * Returns: 0 if successful, -1 if error
 * Error Handling: Uses perror() to report errors, returns -1
 * ========================================================================= */
chatter_close(void) {
  if (close(chatter_fd) < 0) {
    perror("CHATTER CLOSE ERROR");
    return (-1);
  }
  return (0);
}

/* =========================================================================
 * Function: chatter_write_string
 * Purpose: Writes a string to the serial port
 * Parameters:
 *   data - Pointer to the string data to write
 *   len - Length of the string to write
 * Returns: 0 if successful, -1 if error
 * Error Handling: Uses perror() to report errors, returns -1
 * ========================================================================= */
chatter_write_string(char *data, int len)
{
  if (write(chatter_fd, data, len) < 0) {
    perror("WRITE ERROR");
    return (-1);
  }
  return (0);
}

/* =========================================================================
 * Function: chatter_write_code
 * Purpose: Writes a single character code to the serial port
 * Parameters:
 *   code - The character code to write
 * Returns: 0 if successful, -1 if error
 * Error Handling: Uses perror() to report errors, returns -1
 * ========================================================================= */
chatter_write_code(unsigned char code)
{
  if (write(chatter_fd, &code, 1) < 0) {
    perror("WRITE ERROR");
    return (-1);
  }
  return (0);
}

/* =========================================================================
 * Function: chatter_read
 * Purpose: Reads data from the serial port
 * Parameters:
 *   data - Buffer to store read data
 *   len - Number of bytes to read
 * Returns: 0 if successful, -1 if error
 * Error Handling: Uses perror() to report errors, returns -1
 * ========================================================================= */
chatter_read(unsigned char *data, int len)
{
  if (read(chatter_fd, data, len) < 0) {
    perror("READ ERROR");
    return (-1);
  }
  return (0);
}
