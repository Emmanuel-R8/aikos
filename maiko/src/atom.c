/* =========================================================================
 * File: atom.c
 * Purpose: Implementation of atom operations for Maiko VM
 * Confidence Level: High (Stable, well-tested)
 * Testing: Tested with standard atom operations
 * Algorithm: Implements ATOMCELL.N opcode for accessing atom components
 * Authors: Original Venue Corporation, Naoyuki Mitani (1987), Sybalsky et al.
 * Original Implementation Date: 1987-04-13
 * Last Modified: 1999-05-31
 *
 * This file contains the C implementations of atom operations for
 * the Maiko VM. It supports atom cell access using the ATOMCELL.N opcode.
 *
 * Key Features:
 * - ATOMCELL.N opcode implementation for accessing atom components
 * - Support for accessing different atom fields: DEFS, VALS, PLIS, PNP
 * - Handles atom field offset calculations
 *
 * Related Files:
 * - lispemul.h: VM core types and ERROR_EXIT
 * - lispmap.h: Type classification constants
 * - emlglob.h: Global variables and offsets
 * ========================================================================= */

#include "version.h"

#include "lispemul.h" // for LispPTR, ERROR_EXIT - VM core types
#include "lispmap.h"  // for type classification constants
#include "emlglob.h"  // for DEFS_OFFSET, VALS_OFFSET, PLIS_OFFSET, PNP_OFFSET

/* =========================================================================
 * Function: N_OP_atomcellN
 * Purpose: Implements ATOMCELL.N opcode for accessing atom components
 * Opcode: ATOMCELL.N
 * Parameters:
 *   tos - Atom index or address - int
 *   n - Atom field identifier - int (D_DEFSHI, D_VALSHI, D_PLISHI, D_PNHI)
 * Returns: Address of the specified atom field - LispPTR
 * Algorithm:
 *   1. Validates atom identifier format
 *   2. Handles both traditional XeroxLisp atoms and new-style atoms (BIGATOMS)
 *   3. Calculates field offset based on atom type and field identifier
 *   4. Returns absolute address of the specified atom field
 * Error Handling: Calls ERROR_EXIT if:
 *   - Atom identifier has invalid format
 *   - Field identifier is unknown
 * Constants:
 *   D_DEFSHI  - Definition field
 *   D_VALSHI  - Value field
 *   D_PLISHI  - Property list field
 *   D_PNHI    - Print name field
 * Field Offsets (Non-BIGATOMS):
 *   DEFS_OFFSET   - Definition table offset
 *   VALS_OFFSET   - Value table offset
 *   PLIS_OFFSET   - Property list table offset
 *   PNP_OFFSET    - Print name table offset
 * Field Offsets (BIGATOMS):
 *   NEWATOM_DEFN_OFFSET    - Definition field offset
 *   NEWATOM_VALUE_OFFSET   - Value field offset
 *   NEWATOM_PLIST_OFFSET   - Property list field offset
 *   NEWATOM_PNAME_OFFSET   - Print name field offset
 * ========================================================================= */

#ifndef BIGATOMS
N_OP_atomcellN(int tos, int n)
{
  /* Validate atom identifier format (must be 16-bit value) */
  if ((tos & 0xffff0000) != 0)
  {
    ERROR_EXIT(tos); // Invalid atom identifier format
  }

  /* Calculate byte offset from word index */
  tos = (tos << 1);

  /* Dispatch based on field identifier */
  switch (n)
  {
  case D_DEFSHI:
    return (DEFS_OFFSET + tos); // Return definition field address
  case D_VALSHI:
    return (VALS_OFFSET + tos); // Return value field address
  case D_PLISHI:
    return (PLIS_OFFSET + tos); // Return property list field address
  case D_PNHI:
    return (PNP_OFFSET + tos); // Return print name field address
  default:
    ERROR_EXIT(tos); // Unknown field identifier
  }
}

#else
N_OP_atomcellN(int tos, int n)
{
  /* Check if it's a traditional XeroxLisp atom */
  if ((tos & 0xffff0000) == 0)
  { /* XeroxLisp traditional symbol */
    /* Calculate byte offset from word index */
    tos = (tos << 1);

    /* Dispatch based on field identifier */
    switch (n)
    {
    case D_DEFSHI:
      return (DEFS_OFFSET + tos); // Return definition field address
    case D_VALSHI:
      return (VALS_OFFSET + tos); // Return value field address
    case D_PLISHI:
      return (PLIS_OFFSET + tos); // Return property list field address
    case D_PNHI:
      return (PNP_OFFSET + tos); // Return print name field address
    default:
      ERROR_EXIT(tos); // Unknown field identifier
    }
  }
  else
  { /* New Symbol (BIGATOMS format) */
    /* Dispatch based on field identifier for new-style atoms */
    switch (n)
    {
    case D_DEFSHI:
      return (NEWATOM_DEFN_OFFSET + tos); // Return definition field address
    case D_VALSHI:
      return (NEWATOM_VALUE_OFFSET + tos); // Return value field address
    case D_PLISHI:
      return (NEWATOM_PLIST_OFFSET + tos); // Return property list field address
    case D_PNHI:
      return (NEWATOM_PNAME_OFFSET + tos); // Return print name field address
    default:
      ERROR_EXIT(tos); // Unknown field identifier
    }
  }
}

#endif
