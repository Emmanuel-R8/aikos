/* Simple program to analyze FPtoVP table and find file page mapping to virtual page 6204 */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#define BYTESPER_PAGE 512
#define IFPAGE_ADDRESS 512

/* BIGVM macros */
#define GETFPTOVP(b, o) ((b)[o] & 0xFFFF)
#define GETPAGEOK(b, o) (((b)[o] >> 16) & 0xFFFF)

typedef struct {
    uint16_t keyval;
    uint16_t nactivepages;
    uint32_t fptovpstart;
    uint32_t currentfxp;
    /* ... other fields ... */
} IFPAGE;

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <sysout_file>\n", argv[0]);
        return 1;
    }

    const char *sysout_file = argv[1];
    int fd = open(sysout_file, O_RDONLY);
    if (fd < 0) {
        perror("Failed to open sysout file");
        return 1;
    }

    /* Read IFPAGE */
    IFPAGE ifpage;
    if (lseek(fd, IFPAGE_ADDRESS, SEEK_SET) < 0) {
        perror("Failed to seek to IFPAGE");
        close(fd);
        return 1;
    }

    if (read(fd, &ifpage, sizeof(IFPAGE)) != sizeof(IFPAGE)) {
        perror("Failed to read IFPAGE");
        close(fd);
        return 1;
    }

    printf("=== IFPAGE Information ===\n");
    printf("Keyval: 0x%04x\n", ifpage.keyval);
    printf("Nactivepages: %u\n", ifpage.nactivepages);
    printf("FPtoVP start: %u\n", ifpage.fptovpstart);
    printf("Sysout size (halfpages): %u\n", ifpage.nactivepages);
    printf("Sysout size (full pages): %u\n", ifpage.nactivepages / 2);
    printf("\n");

    /* Calculate FPtoVP table offset */
    uint32_t fptovp_offset;
    #ifdef BIGVM
    fptovp_offset = (ifpage.fptovpstart - 1) * BYTESPER_PAGE + 4;
    #else
    fptovp_offset = (ifpage.fptovpstart - 1) * BYTESPER_PAGE + 2;
    #endif

    printf("=== FPtoVP Table Location ===\n");
    printf("FPtoVP offset: %u (0x%x) bytes\n", fptovp_offset, fptovp_offset);
    printf("FPtoVP entries: %u\n", ifpage.nactivepages / 2);
    printf("\n");

    /* Read FPtoVP table */
    size_t fptovp_size = (ifpage.nactivepages / 2) * sizeof(uint32_t);
    uint32_t *fptovp = malloc(fptovp_size);
    if (!fptovp) {
        fprintf(stderr, "Failed to allocate memory for FPtoVP table\n");
        close(fd);
        return 1;
    }

    if (lseek(fd, fptovp_offset, SEEK_SET) < 0) {
        perror("Failed to seek to FPtoVP table");
        free(fptovp);
        close(fd);
        return 1;
    }

    if (read(fd, fptovp, fptovp_size) != (ssize_t)fptovp_size) {
        perror("Failed to read FPtoVP table");
        free(fptovp);
        close(fd);
        return 1;
    }

    printf("=== FPtoVP Table Analysis ===\n");
    printf("Read %zu bytes (%u entries)\n", fptovp_size, ifpage.nactivepages / 2);
    printf("\n");

    /* Check byte-swap boundary */
    unsigned swap_boundary = (ifpage.nactivepages / 4) + 1;
    printf("Byte-swap boundary: %u entries (first %u entries swapped)\n", swap_boundary, swap_boundary);
    printf("\n");

    /* Find file pages mapping to virtual page 6204 */
    unsigned target_vpage = 6204;  // PC 0x307898 / 512
    printf("=== Searching for file pages mapping to virtual page %u (PC 0x307898) ===\n", target_vpage);
    
    unsigned found_count = 0;
    unsigned found_file_pages[10];
    
    for (unsigned fp = 0; fp < (ifpage.nactivepages / 2) && found_count < 10; fp++) {
        uint16_t pageok = GETPAGEOK(fptovp, fp);
        if (pageok != 0xFFFF) {  // Not sparse
            uint16_t vpage = GETFPTOVP(fptovp, fp);
            if (vpage == target_vpage) {
                found_file_pages[found_count] = fp;
                found_count++;
                printf("  File page %u -> Virtual page %u (GETPAGEOK=0x%04x)\n", fp, vpage, pageok);
                printf("    File offset: 0x%lx (%lu bytes)\n", 
                       (unsigned long)(fp * BYTESPER_PAGE), (unsigned long)(fp * BYTESPER_PAGE));
                printf("    Swap status: %s\n", fp < swap_boundary ? "SWAPPED" : "NOT SWAPPED");
            }
        }
    }

    if (found_count > 0) {
        printf("\nFound %u file page(s) mapping to virtual page %u\n", found_count, target_vpage);
        printf("*** FIRST mapping (file page %u) is the one used for loading ***\n", found_file_pages[0]);
    } else {
        printf("\nWARNING: No file page maps to virtual page %u!\n", target_vpage);
    }

    /* Check file page 5178 specifically */
    unsigned test_file_page = 5178;
    if (test_file_page < (ifpage.nactivepages / 2)) {
        uint16_t test_vpage = GETFPTOVP(fptovp, test_file_page);
        uint16_t test_pageok = GETPAGEOK(fptovp, test_file_page);
        printf("\n=== File page %u (Zig's assumption) ===\n", test_file_page);
        printf("  Maps to virtual page: %u\n", test_vpage);
        printf("  GETPAGEOK: 0x%04x\n", test_pageok);
        printf("  Swap status: %s\n", test_file_page < swap_boundary ? "SWAPPED" : "NOT SWAPPED");
        if (test_vpage == target_vpage) {
            printf("  *** MATCHES virtual page %u (PC page) ***\n", target_vpage);
        } else {
            printf("  *** Maps to virtual page %u (NOT PC page %u) ***\n", test_vpage, target_vpage);
        }
    }

    /* Check file page 2937 (has correct bytes) */
    unsigned test_file_page2 = 2937;
    if (test_file_page2 < (ifpage.nactivepages / 2)) {
        uint16_t test_vpage2 = GETFPTOVP(fptovp, test_file_page2);
        uint16_t test_pageok2 = GETPAGEOK(fptovp, test_file_page2);
        printf("\n=== File page %u (has correct bytes) ===\n", test_file_page2);
        printf("  Maps to virtual page: %u\n", test_vpage2);
        printf("  GETPAGEOK: 0x%04x\n", test_pageok2);
        printf("  Swap status: %s\n", test_file_page2 < swap_boundary ? "SWAPPED" : "NOT SWAPPED");
        if (test_vpage2 == target_vpage) {
            printf("  *** MATCHES virtual page %u (PC page) ***\n", target_vpage);
        } else {
            printf("  *** Maps to virtual page %u (NOT PC page %u) ***\n", test_vpage2, target_vpage);
        }
    }

    /* Read and display bytes from the correct file page */
    if (found_count > 0) {
        unsigned correct_file_page = found_file_pages[0];
        printf("\n=== Reading bytes from file page %u (correct mapping) ===\n", correct_file_page);
        
        unsigned file_offset = correct_file_page * BYTESPER_PAGE;
        unsigned pc_offset_in_page = 0x307898 % BYTESPER_PAGE;  // 0x98 = 152 bytes
        
        if (lseek(fd, file_offset + pc_offset_in_page, SEEK_SET) < 0) {
            perror("Failed to seek to PC offset in file page");
        } else {
            unsigned char bytes[8];
            if (read(fd, bytes, 8) == 8) {
                printf("  Bytes at PC offset (0x%x) in file page %u:\n", pc_offset_in_page, correct_file_page);
                printf("    ");
                for (int i = 0; i < 8; i++) {
                    printf("%02x ", bytes[i]);
                }
                printf("\n");
                printf("  Expected by C emulator: 00 00 60 bf c9 12 0a 02\n");
            }
        }
    }

    free(fptovp);
    close(fd);
    return 0;
}
