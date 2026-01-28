/* $Id: bin.c,v 1.3 1999/05/31 23:35:24 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved  */

/************************************************************************/
/*									*/
/*	(C) Copyright 1989-95 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

/* FILE: bin.c - Binary Input Stream Opcode Implementation
 *
 * HIGH CONFIDENCE: This file implements the BIN opcode (0x21) for reading
 * binary data from input streams. This is a fundamental I/O operation used
 * throughout Interlisp for file operations and network communication.
 *
 * OPCODE SPECIFICATION:
 * - Name: BIN (0x21)
 * - Stack Effect: (stream -- byte)
 * - Description: Read next byte from binary input stream
 * - Returns: Byte as positive fixnum (0-255) or error if stream invalid
 *
 * STREAM ARCHITECTURE:
 * - Streams are Lisp objects with buffer management
 * - COFFSET: Current position within buffer
 * - CBUFPTR: Pointer to buffer data in Lisp memory
 * - CBUFSIZE: Total buffer size
 * - BINABLE: Flag indicating stream supports binary operations
 *
 * ERROR HANDLING:
 * - Type check: Must be TYPE_STREAM
 * - Capability check: Stream must have BINABLE flag
 * - Bounds check: COFFSET must be < CBUFSIZE
 * - On error: Returns original TOS via ERROR_EXIT
 *
 * CROSS-REFERENCE: Stream structure in stream.h
 * CROSS-REFERENCE: Complementary BOUT opcode in bout.c
 * CROSS-REFERENCE: Stream creation in unixcomm.c, ether*.c
 *
 * AUTHOR: Takeshi Shimizu (Jul. 22, 1987)
 */
#include "version.h"
#include "adr68k.h"    // for NativeAligned2FromLAddr, NativeAligned4FromLAddr
#include "bindefs.h"   // for N_OP_bin
#include "emlglob.h"
#include "lispmap.h"   // for S_POSITIVE
#include "lspglob.h"
#include "lsptypes.h"  // for state, ERROR_EXIT, GetTypeNumber, Get_BYTE
#include "stream.h"    // for Stream

/* HIGH CONFIDENCE: BIN Opcode Implementation
 *
 * Stack: (stream -- byte)
 * Reads next byte from binary input stream and returns as fixnum.
 *
 * ALGORITHM:
 * 1. Type check: Verify TOS is a stream object
 * 2. Capability check: Verify stream supports binary input
 * 3. Bounds check: Verify current position within buffer
 * 4. Read byte: Get byte at current offset, increment offset
 * 5. Return: Package as positive fixnum (S_POSITIVE | value)
 *
 * ERROR CONDITIONS:
 * - Non-stream object: Return original TOS unchanged
 * - Non-binary stream: Return original TOS unchanged
 * - Buffer overflow: Return original TOS unchanged
 *
 * SIDE EFFECTS:
 * - Increments stream->COFFSET (advances read position)
 * - Does NOT modify stream buffer contents
 *
 * PERFORMANCE: Critical for file I/O and network operations
 */
LispPTR N_OP_bin(LispPTR tos) {
  Stream *stream68k; /* HIGH CONFIDENCE: Stream instance from TOS */
  char *buff68k;     /* HIGH CONFIDENCE: Pointer to stream buffer data */

  /* TYPE CHECK: Verify object is a stream */
  if (GetTypeNumber(tos) == TYPE_STREAM) {
    stream68k = (Stream *)NativeAligned4FromLAddr(tos);

    /* CAPABILITY CHECK: Verify stream supports binary operations */
    if (!stream68k->BINABLE) ERROR_EXIT(tos);

    /* BOUNDS CHECK: Verify read position within buffer */
    if (stream68k->COFFSET >= stream68k->CBUFSIZE) ERROR_EXIT(tos);

    /* BUFFER ACCESS: Get pointer to actual byte data */
    buff68k = (char *)NativeAligned2FromLAddr(stream68k->CBUFPTR);

    /* BYTE READ: Read byte at current position, advance offset */
    return (S_POSITIVE | (Get_BYTE(buff68k + (stream68k->COFFSET)++)));
  } else
    ERROR_EXIT(tos);
}
