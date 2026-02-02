/* $Id: codeconv.c,v 1.3 1999/05/31 23:35:26 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved
 */

/************************************************************************/
/*									*/
/*	(C) Copyright 1989-95 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

#include "version.h"

/* FILE: codeconv.c - Character Code Conversion for Fatchar and Thinchar
 *
 * HIGH CONFIDENCE: This file implements character code conversion
 * between Fatchar/Thinchar formats and EUC (Extended Unix Code) encoding.
 * These functions are used for international text handling in Interlisp.
 *
 * CHARACTER ENCODINGS:
 * - Fatchar: 16-bit character encoding used internally by Interlisp
 * - Thinchar: 8-bit character encoding for compact storage
 * - EUC: Extended Unix Code, used for external text representation
 *
 * CONVERSION TABLE:
 * - Uses ns_euc() function for actual character mapping
 * - TABLESIZE (256) defines the basic character set size
 * - EUCMASK (0x80) identifies multi-byte EUC characters
 *
 * HISTORY:
 * - Original implementation: Shinichi Miyamoto (Jun 26, 1989)
 * - Updates: Sumie Kurihara (Aug 5, 1989; Jan 10, 1991; Feb 27, 1991)
 * - Ported to Maiko: Takeshi Shimizu
 *
 * CROSS-REFERENCE: Character encoding definitions in codeconv.h
 * CROSS-REFERENCE: Text input/output in unixcomm.c
 * CROSS-REFERENCE: Display string handling in dspsubrs.c
 */

#include <stdio.h>

#define EUCMASK 0x80
#define EUCUNMASK 0x7F
#define MASK8BIT 0x00FF
#define TABLESIZE 256

/* HIGH CONFIDENCE: Convert Fatchar string to EUC encoding
 *
 * Fatchar is a 16-bit internal Interlisp character encoding. This function
 * converts Fatchar strings to EUC (Extended Unix Code) for external representation.
 *
 * PARAMETERS:
 * - ns_ptr: Pointer to Fatchar source string (16-bit characters)
 * - ns_len: Length of Fatchar string in characters (not bytes)
 * - euc_ptr: Pointer to buffer for EUC output
 *
 * RETURN:
 * - Length of EUC string in bytes
 *
 * ALGORITHM:
 * 1. Iterate through each Fatchar character (2 bytes each)
 * 2. Convert Fatchar to unsigned short (16-bit)
 * 3. Map to EUC using ns_euc() function
 * 4. Handle single-byte (ASCII) and double-byte (multi-byte) EUC characters
 * 5. Null-terminate the EUC string
 *
 * SIDE EFFECTS:
 * - Writes to euc_ptr buffer
 * - Overwrites data at euc_ptr location
 *
 * PERFORMANCE: O(n) where n is number of characters
 */
int FatcharNStoEUC(unsigned char *ns_ptr, int ns_len, unsigned char *euc_ptr) {
  int i;
  int euc_len;
  unsigned short ns, euc;
  unsigned short ns_euc(short unsigned int ns);

#ifdef DEBUG
  unsigned char *ptr;
  ptr = euc_ptr;
  printf("FatcharNStoEUC start\n");
  printf("ns_len = %d\n", ns_len);
  for (i = 0; i < ns_len * 2; i++) printf("%x ", ns_ptr[i]);
  printf("\n");
#endif

  /* ns_len convert to byte count */
  ns_len *= 2;

  for (i = euc_len = 0; i < ns_len; i += 2) {
    ns = *ns_ptr++ * TABLESIZE;
    ns += *ns_ptr++;
    euc = ns_euc(ns);
    if (euc / TABLESIZE) {
      /* 16 bit character */
      *euc_ptr++ = euc / TABLESIZE;
      *euc_ptr++ = euc % TABLESIZE;
      euc_len += 2;
    } else {
      /* ASCII character */
      *euc_ptr++ = euc % TABLESIZE;
      ++euc_len;
    }
  }

  *euc_ptr++ = '\0';

#ifdef DEBUG
  printf("FatcharNStoEUC end\n");
  printf("euc_len = %d\n", euc_len);
  for (i = 0; i < euc_len; i++) printf("%x ", ptr[i]);
  printf("\n");
#endif

  return (euc_len);
}

/* HIGH CONFIDENCE: Convert Thinchar string to EUC encoding
 *
 * Thinchar is an 8-bit internal Interlisp character encoding for compact storage.
 * This function converts Thinchar strings to EUC (Extended Unix Code) encoding.
 *
 * PARAMETERS:
 * - ns_ptr: Pointer to Thinchar source string (8-bit characters)
 * - ns_len: Length of Thinchar string in characters (and bytes)
 * - euc_ptr: Pointer to buffer for EUC output
 *
 * RETURN:
 * - Length of EUC string in bytes
 *
 * ALGORITHM:
 * 1. Iterate through each Thinchar character (1 byte each)
 * 2. Convert to unsigned short (16-bit) for consistent processing
 * 3. Map to EUC using ns_euc() function
 * 4. Handle single-byte (ASCII) and double-byte (multi-byte) EUC characters
 * 5. Null-terminate the EUC string
 *
 * SIDE EFFECTS:
 * - Writes to euc_ptr buffer
 * - Overwrites data at euc_ptr location
 *
 * PERFORMANCE: O(n) where n is number of characters
 *
 * NOTE: Thinchar is a subset of Fatchar, using only 8-bit values
 */
int ThincharNStoEUC(unsigned char *ns_ptr, int ns_len, unsigned char *euc_ptr) {
  int i;
  int euc_len = 0;
  unsigned short ns, euc;
  unsigned short ns_euc(short unsigned int ns);

#ifdef DEBUG
  unsigned char *ptr;
  ptr = euc_ptr;
  printf("ThincharNStoEUC start\n");
  printf("ns_len = %d\n", ns_len);
  for (i = 0; i < ns_len; i++) printf("%x ", ns_ptr[i]);
  printf("\n");
#endif

  /* ns_len convert to byte count */
  for (i = euc_len = 0; i < ns_len; i++) {
    ns = *ns_ptr++;
    euc = ns_euc(ns);
    if (euc / TABLESIZE) {
      /* 16 bit character */
      *euc_ptr++ = euc / TABLESIZE;
      *euc_ptr++ = euc % TABLESIZE;
      euc_len += 2;
    } else {
      /* ASCII character */
      *euc_ptr++ = euc % TABLESIZE;
      ++euc_len;
    }
  }

  *euc_ptr++ = '\0';

#ifdef DEBUG
  printf("ThincharNStoEUC end\n");
  printf("euc_len = %d\n", euc_len);
  for (i = 0; i < euc_len; i++) printf("%x ", ptr[i]);
  printf("\n");
#endif

  return (euc_len);
}

/* HIGH CONFIDENCE: Convert EUC string to Fatchar encoding
 *
 * Converts EUC (Extended Unix Code) external text to Fatchar internal format.
 * This is the reverse of FatcharNStoEUC.
 *
 * PARAMETERS:
 * - euc_ptr: Pointer to null-terminated EUC string
 * - ns_ptr: Pointer to buffer for Fatchar output
 *
 * RETURN:
 * - Length of Fatchar string in characters (each Fatchar is 2 bytes)
 *
 * ALGORITHM:
 * 1. Iterate through EUC string, handling single and multi-byte characters
 * 2. Multi-byte characters have high bit set (detected via EUCMASK)
 * 3. Convert each EUC character to Fatchar using euc_ns() function
 * 4. Store Fatchar as 2-byte values in output buffer
 *
 * SIDE EFFECTS:
 * - Writes to ns_ptr buffer
 * - Overwrites data at ns_ptr location
 *
 * PERFORMANCE: O(n) where n is number of EUC characters
 *
 * NOTE: EUC strings must be null-terminated for this function to work
 */
int EUCtoFatcharNS(unsigned char *euc_ptr, unsigned char *ns_ptr) {
  int i;
  int ns_len;
  unsigned short euc, ns;
  unsigned short euc_ns(short unsigned int euc);

#ifdef DEBUG
  printf("EUCtoFatcharNS start\n");
  for (i = 0; i < strlen(euc_ptr); i++) printf("%x ", euc_ptr[i]);
  printf("\n");
#endif

  i = 0;
  while (euc = *euc_ptr++) {
    if (euc & EUCMASK) {
      /* 16 bit character */
      euc *= TABLESIZE;
      euc += *euc_ptr++;
    }

    ns = euc_ns(euc);

    ns_ptr[i++] = ns / TABLESIZE;
    ns_ptr[i++] = ns % TABLESIZE;
  }
  ns_len = i / 2;

#ifdef DEBUG
  printf("EUCtoFatcharNS end\n");
  printf("ns_len = %d\n", ns_len);
  for (i = 0; i < ns_len * 2; i++) printf("%x ", ns_ptr[i]);
  printf("\n");
#endif

  return (ns_len);
}

/* HIGH CONFIDENCE: Calculate EUC string length in bytes
 *
 * This function calculates the length of an EUC (Extended Unix Code) string.
 * EUC strings may contain both single-byte and double-byte characters.
 *
 * PARAMETERS:
 * - euc_ptr: Pointer to null-terminated EUC string
 *
 * RETURN:
 * - Length of string in bytes, counting multi-byte characters as 2 bytes
 *
 * ALGORITHM:
 * 1. Iterate through string until null terminator
 * 2. Check each byte for high bit set (multi-byte character marker)
 * 3. Count single-byte characters as 1, double-byte as 2
 *
 * PERFORMANCE: O(n) where n is number of characters
 *
 * NOTE: This function does NOT validate EUC string correctness
 */
int EUCstrlen(char *euc_ptr) {
  int len = 0;

#ifdef DEBUG
  int i;

  printf("EUCstrlen start\n");
  for (i = 0; i < strlen(euc_ptr); i++) printf("%x ", euc_ptr[i]);
  printf("\n");
#endif

  while (*euc_ptr)
    if (*euc_ptr & EUCMASK) {
      /* 16 bit character */
      len += 2;
      *euc_ptr++;
      *euc_ptr++;
    } else {
      /* ASCII character */
      len++;
      *euc_ptr++;
    }

#ifdef DEBUG
  printf("EUCstrlen end\n");
  printf("len = %d\n", len);
#endif

  return (len);
}
