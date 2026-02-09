// FPtoVP (File Page to Virtual Page) table management
// Maps file pages to virtual memory pages

/**
 * FPtoVP table management
 * Manages mapping between file pages and virtual memory pages
 *
 * Background:
 * - Sysout files are divided into 512-byte pages
 * - These pages are mapped into virtual memory at load time
 * - FPtoVP table tracks which virtual page each file page occupies
 * - Each 32-bit entry contains virtual page number (low 16 bits)
 *   and page status flags (high 16 bits)
 */
export class FPtoVPManager {
    /**
     * Lookup file page number for given virtual page
     *
     * Algorithm:
     * - Scan FPtoVP table for matching virtual page
     * - Return file page index when found
     *
     * @param fptovp_table FPtoVP table array (32-bit entries)
     * @param vpage Virtual page number to find
     * @returns File page number, or null if not found
     */
    static getFilePageForVirtualPage(fptovp_table: Uint32Array, vpage: number): number | null {
        for (let i = 0; i < fptovp_table.length; i++) {
            const entry = fptovp_table[ i ];
            const entry_vpage = entry & 0xFFFF;
            if (entry_vpage === vpage) {
                return i;
            }
        }
        return null;
    }

    /**
     * Get page OK flag for file page
     *
     * Status Flags (high 16 bits):
     * - Various flags indicating page validity, swapping status, etc.
     * - "OK" flag indicates page is properly loaded and accessible
     *
     * @param fptovp_table FPtoVP table array
     * @param file_page File page number to check
     * @returns Page status flags
     */
    static getPageOK(fptovp_table: Uint32Array, file_page: number): number {
        if (file_page < fptovp_table.length) {
            return (fptovp_table[ file_page ] >> 16) & 0xFFFF;
        }
        return 0;
    }

    /**
     * Calculate file offset for file page
     *
     * @param file_page File page number
     * @returns Byte offset in sysout file
     */
    static getFileOffset(file_page: number): number {
        return file_page * 512; // BYTESPER_PAGE
    }

    /**
     * Get virtual page number from FPtoVP entry
     *
     * @param entry FPtoVP table entry (32-bit)
     * @returns Virtual page number (low 16 bits)
     */
    static getVirtualPage(entry: number): number {
        return entry & 0xFFFF;
    }

    /**
     * Get page status flags from FPtoVP entry
     *
     * @param entry FPtoVP table entry (32-bit)
     * @returns Page status flags (high 16 bits)
     */
    static getPageStatus(entry: number): number {
        return (entry >> 16) & 0xFFFF;
    }
}
