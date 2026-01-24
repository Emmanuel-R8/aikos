= C Emulator Tracing Implementation Guide

*Navigation*: Zig Implementation Status | Implementations README | Main README

*Date*: 2025-12-23 16:43
*Status*: Implementation Guide - Concrete code changes for enhanced tracing

== Overview

This document provides concrete code changes to implement enhanced tracing in the C emulator. All changes follow the critical debugging technique and systematic debugging approach.

== Implementation 1: Memory Loading Trace

=== File: `maiko/src/ldsout.c`

=== Change 1: Add Memory Trace Log File Handle

*Location*: After line 61 (in `sysout_loader` function)

*Code*:
```c
/* Memory loading trace log */
static FILE *memory_trace_log = NULL;
if (memory_trace_log == NULL) {
  memory_trace_log = fopen("c_emulator_memory_loading_trace.txt", "w");
  if (memory_trace_log == NULL) {
    fprintf(stderr, "WARNING: Failed to open memory trace log\n");
    memory_trace_log = stderr;
  }
  fprintf(memory_trace_log, "=== Memory Loading Trace ===\n");
  fprintf(memory_trace_log, "File: %s\n", sysout_file_name);
  fprintf(memory_trace_log, "File size: %lu bytes\n", (unsigned long)stat_buf.st_size);
  fprintf(memory_trace_log, "Number of pages: %u\n", sysout_size / 2);
}
```

=== Change 2: Trace FPtoVP Table Loading

*Location*: After line 306 (after FPtoVP table read, before byte-swap)

*Code*:
```c
fprintf(memory_trace_log, "\n=== FPtoVP Table Loading ===\n");
fprintf(memory_trace_log, "FPtoVP offset: 0x%lx (%lu bytes)\n", 
        (unsigned long)fptovp_offset, (unsigned long)fptovp_offset);
fprintf(memory_trace_log, "FPtoVP entries: %u\n", sysout_size / 2);

#ifdef BYTESWAP
  unsigned swap_boundary = (sysout_size / 4) + 1;
  fprintf(memory_trace_log, "Byte-swap boundary: %u (first %u entries swapped)\n", 
          swap_boundary, swap_boundary);
#else
  fprintf(memory_trace_log, "Byte-swap: DISABLED\n");
#endif

/* Log first 10 entries and entry 5178 (known PC page mapping) */
fprintf(memory_trace_log, "\nFirst 10 FPtoVP entries:\n");
for (unsigned i = 0; i < 10 && i < (sysout_size / 2); i++) {
  unsigned short vpage = GETFPTOVP(fptovp, i);
  unsigned short pageok = GETPAGEOK(fptovp, i);
  fprintf(memory_trace_log, "  FPtoVP[%u] = %u (0x%04x), GETPAGEOK = 0x%04x\n", 
          i, vpage, vpage, pageok);
}

/* Log entry 5178 (maps to virtual page 6204, PC 0x307898) */
unsigned target_file_page = 5178;
if (target_file_page < (sysout_size / 2)) {
  unsigned short vpage = GETFPTOVP(fptovp, target_file_page);
  unsigned short pageok = GETPAGEOK(fptovp, target_file_page);
  fprintf(memory_trace_log, "\nTarget file page %u:\n", target_file_page);
  fprintf(memory_trace_log, "  FPtoVP[%u] = %u (virtual page)\n", target_file_page, vpage);
  fprintf(memory_trace_log, "  GETPAGEOK[%u] = 0x%04x\n", target_file_page, pageok);
  fprintf(memory_trace_log, "  File offset: 0x%lx (%lu bytes)\n", 
          (unsigned long)(target_file_page * BYTESPER_PAGE),
          (unsigned long)(target_file_page * BYTESPER_PAGE));
  fprintf(memory_trace_log, "  Virtual address: 0x%lx (%lu bytes)\n",
          (unsigned long)(vpage * BYTESPER_PAGE),
          (unsigned long)(vpage * BYTESPER_PAGE));
}
```

=== Change 3: Trace Page Loading

*Location*: In page loading loop (around line 450-550, where pages are loaded)

*Code*:
```c
/* Add at start of page loading loop */
fprintf(memory_trace_log, "\n=== Page Loading ===\n");

/* Inside loop, for each page: */
unsigned file_page = /* current file page */;
unsigned virtual_page = GETFPTOVP(fptovp, file_page);
unsigned short pageok = GETPAGEOK(fptovp, file_page);

/* Only log first 10 pages and target page (5178) */
if (file_page < 10 || file_page == 5178) {
  fprintf(memory_trace_log, "\nFile page %u -> Virtual page %u:\n", 
          file_page, virtual_page);
  fprintf(memory_trace_log, "  File offset: 0x%lx (%lu bytes)\n",
          (unsigned long)(file_page * BYTESPER_PAGE),
          (unsigned long)(file_page * BYTESPER_PAGE));
  fprintf(memory_trace_log, "  Virtual address: 0x%lx (%lu bytes)\n",
          (unsigned long)(virtual_page * BYTESPER_PAGE),
          (unsigned long)(virtual_page * BYTESPER_PAGE));
  fprintf(memory_trace_log, "  GETPAGEOK: 0x%04x\n", pageok);
  
  /* Log raw bytes from file (before byte-swap) */
  fprintf(memory_trace_log, "  Raw bytes (first 16): ");
  for (int i = 0; i < 16 && i < BYTESPER_PAGE; i++) {
    fprintf(memory_trace_log, "%02x ", page_buffer[i]);
  }
  fprintf(memory_trace_log, "\n");
  
  /* Log bytes after byte-swap (if applicable) */
  /* Note: Byte-swap happens in memory, so log from Lisp_world */
  if (virtual_page * BYTESPER_PAGE < /* virtual memory size */) {
    unsigned char *vmem_ptr = (unsigned char *)Lisp_world + (virtual_page * BYTESPER_PAGE);
    fprintf(memory_trace_log, "  Swapped bytes in memory (first 16): ");
    for (int i = 0; i < 16; i++) {
      fprintf(memory_trace_log, "%02x ", vmem_ptr[i]);
    }
    fprintf(memory_trace_log, "\n");
    
    /* For file page 5178, log bytes at PC offset */
    if (file_page == 5178) {
      unsigned pc_offset = 0x307898 % BYTESPER_PAGE; /* 0x98 = 152 bytes */
      fprintf(memory_trace_log, "  Bytes at PC offset 0x%x (0x%x in page): ", 
              pc_offset, pc_offset);
      for (int i = 0; i < 8; i++) {
        if (pc_offset + i < BYTESPER_PAGE) {
          fprintf(memory_trace_log, "%02x ", vmem_ptr[pc_offset + i]);
        }
      }
      fprintf(memory_trace_log, "\n");
    }
  }
}
```

=== Change 4: Trace Critical Address Verification

*Location*: After all pages loaded (end of `sysout_loader`)

*Code*:
```c
fprintf(memory_trace_log, "\n=== Critical Address Verification ===\n");
unsigned pc_target = 0x307898;
unsigned pc_vpage = pc_target / BYTESPER_PAGE;
unsigned pc_offset = pc_target % BYTESPER_PAGE;

fprintf(memory_trace_log, "PC: 0x%06x\n", pc_target);
fprintf(memory_trace_log, "  PC (dec): %u\n", pc_target);
fprintf(memory_trace_log, "  PC / 2: %u (0x%x)\n", pc_target / 2, pc_target / 2);
fprintf(memory_trace_log, "  PC * 2: %u (0x%x)\n", pc_target * 2, pc_target * 2);
fprintf(memory_trace_log, "  Virtual page: %u (0x%x) = PC / %u\n", 
        pc_vpage, pc_vpage, BYTESPER_PAGE);
fprintf(memory_trace_log, "  Virtual page / 2: %u\n", pc_vpage / 2);
fprintf(memory_trace_log, "  Virtual page * 2: %u\n", pc_vpage * 2);
fprintf(memory_trace_log, "  Offset in page: %u (0x%x) = PC %% %u\n",
        pc_offset, pc_offset, BYTESPER_PAGE);
fprintf(memory_trace_log, "  Offset / 2: %u\n", pc_offset / 2);
fprintf(memory_trace_log, "  Offset * 2: %u\n", pc_offset * 2);

/* Find file page mapping */
unsigned found_file_page = 0xFFFF;
for (unsigned fp = 0; fp < (sysout_size / 2); fp++) {
  if (GETFPTOVP(fptovp, fp) == pc_vpage) {
    found_file_page = fp;
    break;
  }
}

if (found_file_page != 0xFFFF) {
  fprintf(memory_trace_log, "  File page mapping: %u -> %u\n", found_file_page, pc_vpage);
  fprintf(memory_trace_log, "  File offset: 0x%lx (%lu bytes)\n",
          (unsigned long)(found_file_page * BYTESPER_PAGE),
          (unsigned long)(found_file_page * BYTESPER_PAGE));
  fprintf(memory_trace_log, "  File offset / 2: 0x%lx\n",
          (unsigned long)(found_file_page * BYTESPER_PAGE / 2));
  fprintf(memory_trace_log, "  File offset * 2: 0x%lx\n",
          (unsigned long)(found_file_page * BYTESPER_PAGE * 2));
} else {
  fprintf(memory_trace_log, "  WARNING: No file page maps to virtual page %u!\n", pc_vpage);
}

unsigned virtual_address = pc_vpage * BYTESPER_PAGE;
fprintf(memory_trace_log, "  Virtual address: 0x%lx (%lu bytes)\n",
        (unsigned long)virtual_address, (unsigned long)virtual_address);
fprintf(memory_trace_log, "  Virtual address / 2: 0x%lx\n",
        (unsigned long)(virtual_address / 2));
fprintf(memory_trace_log, "  Virtual address * 2: 0x%lx\n",
        (unsigned long)(virtual_address * 2));

/* Log memory content at PC */
if ((unsigned char *)Lisp_world + pc_target < 
    (unsigned char *)Lisp_world + /* virtual memory size */) {
  unsigned char *pc_mem = (unsigned char *)Lisp_world + pc_target;
  fprintf(memory_trace_log, "  Memory content at PC: ");
  for (int i = 0; i < 8; i++) {
    fprintf(memory_trace_log, "%02x ", pc_mem[i]);
  }
  fprintf(memory_trace_log, "\n");
}
```

== Implementation 2: Enhanced Execution Log

=== File: `maiko/src/xc.c`

=== Change 1: Add File Page Lookup Function

*Location*: Before `nextopcode` label (around line 520)

*Code*:
```c
/* Lookup file page for virtual page */
static unsigned getFilePageForVirtualPage(unsigned vpage) {
  extern LispPTR *FPtoVP;
  extern unsigned sysout_size;
  
  if (FPtoVP == NULL) return 0xFFFF;
  
  for (unsigned fp = 0; fp < (sysout_size / 2); fp++) {
    if (GETFPTOVP(FPtoVP, fp) == vpage) {
      return fp;
    }
  }
  return 0xFFFF; /* Not found */
}
```

=== Change 2: Enhance Execution Log Format

*Location*: After line 620 (after current debug additions)

*Code*:
```c
/* Enhanced memory loading diagnostics */
if (debug_instruction_count <= 100 || pc_byte_offset == 0x307898) {
  unsigned pc_vpage = pc_byte_offset / BYTESPER_PAGE;
  unsigned pc_offset_in_page = pc_byte_offset % BYTESPER_PAGE;
  unsigned file_page = getFilePageForVirtualPage(pc_vpage);
  unsigned long file_offset = (file_page != 0xFFFF) ? 
    (unsigned long)(file_page * BYTESPER_PAGE) : 0;
  unsigned long virtual_address = (unsigned long)(pc_vpage * BYTESPER_PAGE);
  
  fprintf(debug_log, " [FP:%u VP:%u FO:0x%lx VA:0x%lx]", 
          file_page, pc_vpage, file_offset, virtual_address);
  
  /* Byte-swap status (if we can determine it) */
  if (file_page != 0xFFFF && file_page < (sysout_size / 4) + 1) {
    fprintf(debug_log, " [BS:SWAPPED]");
  } else if (file_page != 0xFFFF) {
    fprintf(debug_log, " [BS:RAW]");
  }
  
  /* Memory content at PC (for verification) */
  fprintf(debug_log, " [MEM:");
  for (int i = 0; i < 8 && (char *)PCMAC + i < (char *)Lisp_world + (64 * 1024 * 1024); i++) {
    fprintf(debug_log, "%02x", *((unsigned char *)PCMAC + i));
  }
  fprintf(debug_log, "]");
}
```

== Implementation 3: Address Calculation Trace

=== File: `maiko/src/xc.c`

=== Change 1: Add Address Trace Log

*Location*: After line 540 (after debug_log initialization)

*Code*:
```c
/* Address calculation trace log */
static FILE *address_trace_log = NULL;
if (address_trace_log == NULL) {
  address_trace_log = fopen("c_emulator_address_trace.txt", "w");
  if (address_trace_log == NULL) {
    address_trace_log = stderr;
  }
}
```

=== Change 2: Add Address Calculation Trace

*Location*: After line 620 (after memory verification)

*Code*:
```c
/* Address calculation trace (first 10 instructions or PC 0x307898) */
if (debug_instruction_count <= 10 || pc_byte_offset == 0x307898) {
  fprintf(address_trace_log, "\n=== Address Calculation Trace (Instruction %d) ===\n", 
          debug_instruction_count);
  fprintf(address_trace_log, "PC: 0x%06x\n\n", pc_byte_offset);
  
  /* Step 1: PC Analysis */
  fprintf(address_trace_log, "Step 1: PC Analysis\n");
  fprintf(address_trace_log, "  PC (dec): %u\n", pc_byte_offset);
  fprintf(address_trace_log, "  PC / 2: %u (0x%x)\n", pc_byte_offset / 2, pc_byte_offset / 2);
  fprintf(address_trace_log, "  PC * 2: %u (0x%x)\n", pc_byte_offset * 2, pc_byte_offset * 2);
  
  /* Step 2: Virtual Page Calculation */
  unsigned pc_vpage = pc_byte_offset / BYTESPER_PAGE;
  unsigned pc_offset_in_page = pc_byte_offset % BYTESPER_PAGE;
  fprintf(address_trace_log, "\nStep 2: Virtual Page Calculation\n");
  fprintf(address_trace_log, "  Virtual page: %u = PC / %u\n", pc_vpage, BYTESPER_PAGE);
  fprintf(address_trace_log, "  Virtual page / 2: %u\n", pc_vpage / 2);
  fprintf(address_trace_log, "  Virtual page * 2: %u\n", pc_vpage * 2);
  fprintf(address_trace_log, "  Offset in page: %u = PC %% %u\n", 
          pc_offset_in_page, BYTESPER_PAGE);
  fprintf(address_trace_log, "  Offset / 2: %u\n", pc_offset_in_page / 2);
  fprintf(address_trace_log, "  Offset * 2: %u\n", pc_offset_in_page * 2);
  
  /* Step 3: File Page Lookup */
  unsigned file_page = getFilePageForVirtualPage(pc_vpage);
  fprintf(address_trace_log, "\nStep 3: File Page Lookup\n");
  if (file_page != 0xFFFF) {
    unsigned short vpage_from_fp = GETFPTOVP(FPtoVP, file_page);
    unsigned short pageok = GETPAGEOK(FPtoVP, file_page);
    fprintf(address_trace_log, "  FPtoVP[%u] = %u (virtual page)\n", file_page, vpage_from_fp);
    fprintf(address_trace_log, "  GETPAGEOK[%u] = 0x%04x\n", file_page, pageok);
    fprintf(address_trace_log, "  File page: %u\n", file_page);
    fprintf(address_trace_log, "  File page / 2: %u\n", file_page / 2);
    fprintf(address_trace_log, "  File page * 2: %u\n", file_page * 2);
  } else {
    fprintf(address_trace_log, "  WARNING: No file page found for virtual page %u\n", pc_vpage);
  }
  
  /* Step 4: File Offset Calculation */
  fprintf(address_trace_log, "\nStep 4: File Offset Calculation\n");
  if (file_page != 0xFFFF) {
    unsigned long file_offset = (unsigned long)(file_page * BYTESPER_PAGE);
    fprintf(address_trace_log, "  File offset: 0x%lx = file_page * %u\n", 
            file_offset, BYTESPER_PAGE);
    fprintf(address_trace_log, "  File offset / 2: 0x%lx\n", file_offset / 2);
    fprintf(address_trace_log, "  File offset * 2: 0x%lx\n", file_offset * 2);
  }
  
  /* Step 5: Virtual Address Calculation */
  unsigned long virtual_address = (unsigned long)(pc_vpage * BYTESPER_PAGE);
  fprintf(address_trace_log, "\nStep 5: Virtual Address Calculation\n");
  fprintf(address_trace_log, "  Virtual address: 0x%lx = virtual_page * %u\n",
          virtual_address, BYTESPER_PAGE);
  fprintf(address_trace_log, "  Virtual address / 2: 0x%lx\n", virtual_address / 2);
  fprintf(address_trace_log, "  Virtual address * 2: 0x%lx\n", virtual_address * 2);
  
  /* Step 6: Memory Content Verification */
  fprintf(address_trace_log, "\nStep 6: Memory Content Verification\n");
  fprintf(address_trace_log, "  Memory at PC: ");
  for (int i = 0; i < 8 && (char *)PCMAC + i < (char *)Lisp_world + (64 * 1024 * 1024); i++) {
    fprintf(address_trace_log, "%02x ", *((unsigned char *)PCMAC + i));
  }
  fprintf(address_trace_log, "\n");
  
  if (virtual_address < /* virtual memory size */) {
    unsigned char *vmem_ptr = (unsigned char *)Lisp_world + virtual_address;
    fprintf(address_trace_log, "  Memory at virtual address: ");
    for (int i = 0; i < 8; i++) {
      fprintf(address_trace_log, "%02x ", vmem_ptr[i]);
    }
    fprintf(address_trace_log, "\n");
    
    /* Check if PC memory matches virtual address memory */
    int match = 1;
    for (int i = 0; i < 8; i++) {
      if (*((unsigned char *)PCMAC + i) != vmem_ptr[pc_offset_in_page + i]) {
        match = 0;
        break;
      }
    }
    fprintf(address_trace_log, "  Match: %s\n", match ? "YES" : "NO");
  }
  
  fprintf(address_trace_log, "\n");
  fflush(address_trace_log);
}
```

== Summary

These code changes will add comprehensive tracing to the C emulator:

1. *Memory Loading Trace*: Complete file page → virtual page mapping with byte-swapping
2. *Enhanced Execution Log*: File page, virtual address, byte-swap status, memory content
3. *Address Calculation Trace*: Step-by-step address calculations with critical debugging technique

All traces apply the critical debugging technique (value, value/2, value*2) to help identify byte/DLword confusion and endianness issues.

== Next Steps

1. Implement code changes in C emulator
2. Run C emulator with enhanced tracing
3. Compare C traces with Zig emulator
4. Use traces to systematically identify and fix memory loading issues
