/* $Id: binds.c,v 1.3 1999/05/31 23:35:24 sybalsky Exp $ (C) Copyright Venue, All Rights Reserved */

/************************************************************************/
/*									*/
/*	(C) Copyright 1989-95 Venue. All Rights Reserved.		*/
/*	Manufactured in the United States of America.			*/
/*									*/
/************************************************************************/

#include "version.h"

/* FILE: binds.c - Variable Binding Operations for Maiko Lisp Emulator
 *
 * This file implements BIND and UNBIND operations that manage Lisp variable
 * bindings in the Parameter Variable (PVAR) area of function frames.
 *
 * HIGH CONFIDENCE: The binding mechanism is well understood from documentation
 * and code analysis. BIND creates bindings, UNBIND restores them. This is
 * fundamental to Lisp's dynamic scoping.
 *
 * OPERATIONS IMPLEMENTED:
 * - N_OP_bind: Create variable bindings with marker encoding
 * - N_OP_unbind: Restore variable bindings by walking stack
 * - N_OP_dunbind: Conditional unbinding operation
 *
 * CROSS-REFERENCE: This implements opcodes.BIND (0x11) and opcodes.UNBIND (0x18).
 * CROSS-REFERENCE: Uses stack frame structures from stack.h for frame management.
 * CROSS-REFERENCE: Marker encoding must match UNBIND expectations.
 *
 * BINDING PROCESS:
 * 1. BIND allocates space in PVAR area and sets variable values
 * 2. Function executes with bound variables in lexical scope
 * 3. UNBIND walks stack to find binding marker and restores values
 * 4. Binding marker contains encoded metadata for restoration process
 *
 * MARKER FORMAT: ((~(total_bindings)) << 16) | (pvar_offset << 1)
 * HIGH CONFIDENCE: Critical encoding that UNBIND depends on for proper restoration
 */

#include <stdio.h>
#include "lispemul.h"
#include "lspglob.h"
#include "emlglob.h"
#include "testtooldefs.h"
#include "bindsdefs.h"

/* FUNCTION: N_OP_bind - Implement BIND opcode
 *
 * Creates variable bindings in the PVAR area and pushes a marker onto the stack.
 * The marker contains encoded information for UNBIND to restore the bindings.
 *
 * Algorithm:
 * 1. Bind n1 variables to NIL in PVAR area
 * 2. Bind n2 variables to values from evaluation stack
 * 3. Push binding marker with encoded metadata
 *
 * The marker format: ((~(total_bindings)) << 16) | (pvar_offset << 1)
 *
 * HIGH CONFIDENCE: This matches the documented BIND behavior and UNBIND expectations.
 * The marker encoding is critical for proper unbinding.
 *
 * MEDIUM CONFIDENCE: The exact semantics of n1 vs n2 bindings could be clarified,
 * but the overall flow is well understood.
 */

/**************************************************
N_OP_bind(stack_pointer, tos, n1, n2)

        Entry:	BIND		opcode[021]

        1. bind PVAR slot to NIL. (n1 times)
        2. bind PVAR slot to value of slot in Evaluation stack. (n2 times)
           or push TopOfStack to Evaluation stack.
        3. Push		[upper word]	1's complement of bind slots
                        [lower word]	2word offset from PVar

***************************************************/

LispPTR *N_OP_bind(LispPTR *stack_pointer, LispPTR tos, unsigned byte1, unsigned byte2) {
  unsigned n1;         /* # slots to bind to NIL (0, 0) */
  unsigned n2;         /* # slots to bind to value in stack */
  LispPTR *ppvar; /* pointer to argued slot in Pvar area */
  unsigned i;          /* temporary for control */

#ifdef TRACE
  printPC();
  printf("TRACE: N_OP_bind()\n");
#endif

  /* DECODE OPERANDS
   * byte1 format: [n1:4][n2:4] where n1 and n2 are 4-bit counts
   * n1: number of variables to bind to NIL
   * n2: number of variables to bind to stack values
   *
   * HIGH CONFIDENCE: This matches documented BIND operand encoding
   */
  n1 = byte1 >> 4;
  n2 = byte1 & 0xf;

  /* SETUP PVAR POINTER
   * PVar points to base of PVAR area
   * +1 for the display/closure slot
   * +byte2 for the specific function's PVAR offset
   */
  ppvar = (LispPTR *)PVar + 1 + byte2;

  /* BIND n1 VARIABLES TO NIL
   * Decrement pointer and set each slot to NIL
   * Creates uninitialized variable bindings
   */
  for (i = 0; i < n1; i++) { *--ppvar = NIL_PTR; }

  /* HANDLE n2 VARIABLE BINDINGS FROM STACK
   * n2 == 0: Just push TOS to evaluation stack
   * n2 > 0: Bind n2 variables from stack values
   */
  if (n2 == 0) {
    *stack_pointer++ = tos; /* push TopOfStack to Evaluation stack */
  } else {
    /* Bind first value to TopOfStack */
    *--ppvar = tos; /* bind to TopOfStack */
    /* Bind remaining values from stack */
    for (i = 1; i < n2; i++) { *--ppvar = *(--stack_pointer); }
  }

  /* PUSH BINDING MARKER
   * Marker format: [ones-complement(count)][pvar_offset<<1]
   * Used by UNBIND to find and restore bindings
   *
   * HIGH CONFIDENCE: Critical encoding that UNBIND depends on
   */
  i = ~(n1 + n2); /* x: 1's complement of number of bind slots */
  *stack_pointer = (i << 16) | (byte2 << 1);
  return (stack_pointer);
}

/* FUNCTION: N_OP_unbind - Implement UNBIND opcode
 *
 * Restores variable bindings by walking the stack to find the binding marker
 * and then clearing the PVAR slots that were bound by the corresponding BIND.
 *
 * Algorithm:
 * 1. Scan stack downward looking for binding marker (MSB set in upper word)
 * 2. Extract marker information: number of bindings and PVAR offset
 * 3. Restore PVAR slots to unbound state (0xFFFFFFFF)
 *
 * MARKER SEARCH:
 * - Binding marker has MSB (0x80000000) set in upper word
 * - This distinguishes it from normal data on the stack
 * - HIGH CONFIDENCE: This is the key mechanism for finding the right binding
 *
 * RESTORATION:
 * - num contains the count of variables to unbind
 * - Each slot is set to 0xFFFFFFFF (unbound marker)
 * - HIGH CONFIDENCE: This matches the expected restoration behavior
 */

/**************************************************
LispPTR N_OP_unbind(stackpointer)

        Entry:	UNBIND		opcode[022]

        1. pop stackpointer until the slot (num, lastpvar) is found
           (Note: TOPOFSTACK is ignored)
        2. unbind lastpvar slot (set to 0xFFFF). (num times)

***************************************************/

LispPTR *N_OP_unbind(LispPTR *stack_pointer) {
  DLword num;     /* number of unbind slots */
  LispPTR *ppvar; /* pointer to last PVAR slot. */
  DLword i;       /* temporary for control */
  LispPTR value;

#ifdef TRACE
  printPC();
  printf("TRACE: N_OP_unbind()\n");
#endif

  /* SEARCH FOR BINDING MARKER
   * Scan stack downward until finding marker (MSB set in upper word)
   * Binding marker format: [0x80000000 | count][offset<<1]
   *
   * HIGH CONFIDENCE: The MSB (0x80000000) marks the binding marker
   * This allows UNBIND to find the correct restoration point
   */
  /* now, stack_pointer points the latter part in slot */
  for (; !(*--stack_pointer & 0x80000000);)
    ; /* scan (until MSB == 1) */

  /* EXTRACT MARKER INFORMATION
   * value now contains the marker
   * Upper word (after removing MSB): ones-complement of binding count
   * Lower word: PVAR offset for restoration
   */
  value = *stack_pointer;
  num = (DLword) ~(value >> 16); /* Extract count from ones-complement */

  /* SETUP PVAR POINTER FOR RESTORATION
   * PVar + 2 accounts for display/closure slot
   * GetLoWord extracts the offset from marker
   */
  ppvar = (LispPTR *)(PVar + 2 + GetLoWord(value));

  /* RESTORE PVAR SLOTS TO UNBOUND STATE
   * Set each slot to 0xFFFFFFFF (unbound marker)
   * Loop count equals number of bindings from BIND
   */
  value = 0xffffffff;
  for (i = 0; i < num; i++) { *--ppvar = value; }
  return (stack_pointer);
}

/* FUNCTION: N_OP_dunbind - Implement DUNBIND opcode
 *
 * Conditional unbinding operation. Similar to UNBIND but checks if TOS is unbound
 * before performing the unbind operation.
 *
 * Algorithm:
 * 1. Check if TOS has MSB set (indicates unbound value)
 * 2. If unbound: directly unbind num slots from PVAR
 * 3. If bound: scan stack for marker, then unbind from that location
 * 4. Pop the result to TopOfStack
 *
 * DIFFERENCE FROM UNBIND:
 * - UNBIND always scans stack for marker
 * - DUNBIND checks TOS first, only scans if TOS is bound
 *
 * HIGH CONFIDENCE: This conditional behavior is documented in opcode specifications
 */

/**************************************************
N_OP_dunbind

        Entry:	DUNBIND		opcode[023]

        1. if TopOfStack is unbound
                unbind num slots from PVar.
           if TopOfStack is bound
                pop CurrentStack until the slot (num, lastpvar) is found.
                unbind num slots from lastpvar.
        2. pop the top of CurrentStackPTR to TopOfStack.

***************************************************/

LispPTR *N_OP_dunbind(LispPTR *stack_pointer, LispPTR tos) {
  DLword num;     /* number of unbind slots */
  LispPTR *ppvar; /* pointer to last PVAR slot. */
  DLword i;       /* temporary for control */
  LispPTR value;

#ifdef TRACE
  printPC();
  printf("TRACE: N_OP_dunbind()\n");
#endif

  /* CHECK IF TOS IS UNBOUND
   * MSB of high word set (0x80000000) indicates unbound value
   *
   * HIGH CONFIDENCE: This bit test matches documented DUNBIND semantics
   */
  if (tos & 0x80000000) {
    /* tos is unbound */
    /* Extract count from ones-complement encoding */
    num = ~(GetHiWord(tos));
    value = 0xffffffff;
    if (num != 0) {
      /* Direct unbind from PVAR at specified offset */
      ppvar = (LispPTR *)(PVar + 2 + GetLoWord(tos));
      for (i = 0; i < num; ++i) { *--ppvar = value; }
    }
  } else {
    /* tos is bound */
    /* now, stack_pointer points the latter part in slot */
    for (; !((*--stack_pointer) & 0x80000000);)
      ;
    /* scan (until MSB == 1) */

    value = *stack_pointer;
    num = ~(GetHiWord(value));
    ppvar = (LispPTR *)(PVar + 2 + GetLoWord(value));
    value = 0xffffffff;
    for (i = 0; i < num; i++) { *--ppvar = value; }
  }

  return (stack_pointer);
}
