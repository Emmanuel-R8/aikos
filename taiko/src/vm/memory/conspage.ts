// Cons page management for CONS opcode
// Per maiko/src/conspage.c and maiko/inc/cell.h

import type { LispPTR } from '../../utils/types';
import { MemoryManager } from './manager';
import { NIL_PTR, BYTESPER_PAGE } from '../../utils/constants';

/**
 * ConsPage structure
 * Per maiko/inc/cell.h:76-80 (non-NEWCDRCODING, non-BYTESWAP)
 */
export interface ConsPage {
    count: number;      // Free cells on this page (8 bits)
    next_cell: number;  // Next free cell in chain (8 bits)
    next_page: number;  // Next cons page (DLword)
}

/**
 * ConsCell structure
 * Per maiko/inc/lispemul.h
 */
export interface ConsCell {
    car_field: LispPTR;  // CAR field (32-bit LispPTR)
    cdr_code: number;    // CDR code (8 bits)
}

/**
 * CDR code constants
 * Per maiko/inc/cell.h:48-51
 */
export const CDR_NIL = 0x80;        // CDR is NIL (0x80 in old coding)
export const CDR_ONPAGE = 0x80;     // CDR is on same page
export const CDR_INDIRECT = 0xFE;   // CDR is indirect (stored in next cell)
export const CONSPAGE_LAST = 0xFFFF; // Last cons page marker

/**
 * Cons page size constants
 */
const DLWORDS_PER_PAGE = 256;  // 512 bytes / 2 bytes per DLword
const CONSPAGE_HEADER_SIZE = 4; // ConsPage header is 4 bytes (count + next_cell + next_page)
const CONSCELL_SIZE = 5;        // ConsCell is 5 bytes (4 bytes car + 1 byte cdr_code)
const CONSCELLS_PER_PAGE = Math.floor((BYTESPER_PAGE - CONSPAGE_HEADER_SIZE) / CONSCELL_SIZE);

/**
 * Cons page manager
 * Handles cons cell allocation from cons pages
 */
export class ConsPageManager {
    private memory: Uint8Array;
    private plistSpaceOffset: number; // Byte offset of PLISTSPACE
    private dtdOffset: number;        // Byte offset of DTD space

    /**
     * Initialize cons page manager
     *
     * @param memory Virtual memory array
     * @param plistSpaceOffset Byte offset of PLISTSPACE
     * @param dtdOffset Byte offset of DTD space
     */
    constructor(memory: Uint8Array, plistSpaceOffset: number, dtdOffset: number) {
        this.memory = memory;
        this.plistSpaceOffset = plistSpaceOffset;
        this.dtdOffset = dtdOffset;
    }

    /**
     * Read ConsPage structure from memory
     *
     * @param pageOffset Byte offset of cons page
     * @returns ConsPage structure or null if invalid
     */
    private readConsPage(pageOffset: number): ConsPage | null {
        if (pageOffset + CONSPAGE_HEADER_SIZE > this.memory.length) {
            return null;
        }

        // Read ConsPage header (non-BYTESWAP, non-NEWCDRCODING)
        // Structure: count (1 byte), next_cell (1 byte), next_page (2 bytes)
        const count = this.memory[ pageOffset ];
        const next_cell = this.memory[ pageOffset + 1 ];
        const next_page = MemoryManager.Access.readDLword(this.memory, pageOffset + 2);

        return { count, next_cell, next_page };
    }

    /**
     * Write ConsPage structure to memory
     *
     * @param pageOffset Byte offset of cons page
     * @param page ConsPage structure
     */
    private writeConsPage(pageOffset: number, page: ConsPage): void {
        if (pageOffset + CONSPAGE_HEADER_SIZE > this.memory.length) {
            return;
        }

        this.memory[ pageOffset ] = page.count;
        this.memory[ pageOffset + 1 ] = page.next_cell;
        MemoryManager.Access.writeDLword(this.memory, pageOffset + 2, page.next_page);
    }

    /**
     * Read ConsCell from memory
     *
     * @param cellOffset Byte offset of cons cell
     * @returns ConsCell structure or null if invalid
     */
    private readConsCell(cellOffset: number): ConsCell | null {
        if (cellOffset + CONSCELL_SIZE > this.memory.length) {
            return null;
        }

        const car_field = MemoryManager.Access.readLispPTR(this.memory, cellOffset);
        const cdr_code = this.memory[ cellOffset + 4 ];

        return { car_field, cdr_code };
    }

    /**
     * Write ConsCell to memory
     *
     * @param cellOffset Byte offset of cons cell
     * @param cell ConsCell structure
     */
    private writeConsCell(cellOffset: number, cell: ConsCell): void {
        if (cellOffset + CONSCELL_SIZE > this.memory.length) {
            return;
        }

        MemoryManager.Access.writeLispPTR(this.memory, cellOffset, cell.car_field);
        this.memory[ cellOffset + 4 ] = cell.cdr_code;
    }

    /**
     * Get DTD for LISTP type
     * Per C: ListpDTD = GetDTD(TYPE_LISTP)
     * Per C: GetDTD(typnum) = DTDspace + ((typnum)<<4)+((typnum)<<1) for BIGVM
     *
     * @returns LispPTR to next cons page or 0 if none
     */
    private getNextConsPage(): number {
        // TYPE_LISTP = 5
        const TYPE_LISTP = 5;

        // Calculate DTD offset: GetDTD(TYPE_LISTP) = DTDspace + (TYPE_LISTP * 18) for BIGVM
        // (typnum)<<4 = typnum * 16, (typnum)<<1 = typnum * 2, total = typnum * 18
        const dtdOffset = this.dtdOffset + (TYPE_LISTP * 18); // 18 bytes per DTD entry for BIGVM

        if (dtdOffset + 32 > this.memory.length) {
            return 0; // Invalid DTD offset
        }

        // Read dtd_nextpage from DTD structure
        // For BIGVM non-BYTESWAP structure:
        // Offset 0-3: dtd_name (32 bits)
        // Offset 4-5: dtd_cnt0 (DLword)
        // Offset 6-7: dtd_size (DLword)
        // Offset 8-11: dtd_free (LispPTR)
        // Offset 12-15: dtd_descrs, etc. (32 bits)
        // Offset 16-19: dtd_typespecs (LispPTR)
        // Offset 20-23: dtd_ptrs (LispPTR)
        // Offset 24-27: dtd_oldcnt (int)
        // Offset 28-31: dtd_nextpage (int for BIGVM)

        // Read dtd_nextpage as int (4 bytes) at offset 28
        const nextPageLow = MemoryManager.Access.readDLword(this.memory, dtdOffset + 28);
        const nextPageHigh = MemoryManager.Access.readDLword(this.memory, dtdOffset + 30);
        const nextPage = (nextPageHigh << 16) | nextPageLow;

        // Convert to LispPTR (page number to byte offset)
        if (nextPage === 0 || nextPage === CONSPAGE_LAST) {
            return 0; // No next page
        }

        // Convert page number to byte offset
        // Page numbers are virtual page numbers, need to convert to byte offset
        return MemoryManager.Address.lispPtrToByte(nextPage);
    }

    /**
     * Find free cons cell from existing cons pages
     * Per C: find_free_cons_cell() in maiko/src/conspage.c
     *
     * @returns Byte offset of free cons cell, or null if none found
     */
    private findFreeConsCell(): number | null {
        let nextPage = this.getNextConsPage();
        if (nextPage === 0) {
            return null; // No cons pages allocated yet
        }

        // Walk through cons pages looking for free cells
        while (nextPage !== 0 && nextPage !== CONSPAGE_LAST) {
            const pageOffset = MemoryManager.Address.lispPtrToByte(nextPage);
            const page = this.readConsPage(pageOffset);
            if (!page) break;

            if (page.count > 0) {
                // Found a page with free cells
                // Get cell at next_cell offset
                const cellOffset = pageOffset + CONSPAGE_HEADER_SIZE + (page.next_cell * CONSCELL_SIZE);

                // Read free cell to get next_free
                const nextFree = this.memory[ cellOffset + 4 ]; // next_free is in high byte

                // Update page
                page.count--;
                page.next_cell = nextFree;
                this.writeConsPage(pageOffset, page);

                return cellOffset;
            } else {
                // Empty page, move to next
                nextPage = page.next_page;
            }
        }

        return null;
    }

    /**
     * Allocate new cons page
     * Per C: next_conspage() in maiko/src/conspage.c
     *
     * @returns Byte offset of new cons page, or null if allocation failed
     */
    private allocateNewConsPage(): number | null {
        // TODO: Allocate from MDS (Memory Data Space)
        // For now, allocate from PLISTSPACE
        // Find free space in PLISTSPACE
        const pageOffset = this.plistSpaceOffset;

        // Initialize cons page
        const page: ConsPage = {
            count: CONSCELLS_PER_PAGE - 1, // Reserve first cell
            next_cell: 1,                  // First free cell is at offset 1
            next_page: 0,                   // No next page yet
        };
        this.writeConsPage(pageOffset, page);

        // Initialize free cell chain
        // Each free cell's cdr_code byte contains next_free
        // Per C: freecons structure - CAR is NIL, cdr_code contains next_free
        for (let i = 1; i < CONSCELLS_PER_PAGE - 1; i++) {
            const cellOffset = pageOffset + CONSPAGE_HEADER_SIZE + (i * CONSCELL_SIZE);
            // Initialize CAR to NIL
            MemoryManager.Access.writeLispPTR(this.memory, cellOffset, NIL_PTR);
            // Store next_free in cdr_code byte
            this.memory[ cellOffset + 4 ] = i + 1; // next_free
        }
        // Last cell points to 0 (end of chain)
        const lastCellOffset = pageOffset + CONSPAGE_HEADER_SIZE + ((CONSCELLS_PER_PAGE - 1) * CONSCELL_SIZE);
        MemoryManager.Access.writeLispPTR(this.memory, lastCellOffset, NIL_PTR);
        this.memory[ lastCellOffset + 4 ] = 0; // End of free chain

        return pageOffset;
    }

    /**
     * Allocate a new cons cell
     * Per C: N_OP_cons() in maiko/src/conspage.c
     *
     * @param car CAR value
     * @param cdr CDR value
     * @returns LispPTR to new cons cell, or NIL_PTR if allocation failed
     */
    allocateConsCell(car: LispPTR, cdr: LispPTR): LispPTR {
        let cellOffset: number | null = null;

        if (cdr === NIL_PTR) {
            // Try to find free cell from existing pages
            cellOffset = this.findFreeConsCell();

            if (!cellOffset) {
                // Allocate new cons page
                const pageOffset = this.allocateNewConsPage();
                if (!pageOffset) {
                    return NIL_PTR; // Allocation failed
                }

                // Get first cell from new page
                const page = this.readConsPage(pageOffset);
                if (!page) {
                    return NIL_PTR;
                }

                cellOffset = pageOffset + CONSPAGE_HEADER_SIZE + (page.next_cell * CONSCELL_SIZE);

                // Update page
                page.count--;
                const nextFree = this.memory[ cellOffset + 4 ];
                page.next_cell = nextFree;
                this.writeConsPage(pageOffset, page);
            }

            // Write cons cell with CDR_NIL
            const cell: ConsCell = {
                car_field: car,
                cdr_code: CDR_NIL,
            };
            this.writeConsCell(cellOffset, cell);
        } else {
            // TODO: Handle non-NIL CDR (CDR_ONPAGE or CDR_INDIRECT)
            // For now, use CDR_INDIRECT (requires 2 cells)
            console.warn('CONS: Non-NIL CDR not yet fully implemented, using CDR_INDIRECT');

            // Allocate 2 cells
            const pageOffset = this.allocateNewConsPage();
            if (!pageOffset) {
                return NIL_PTR;
            }

            const page = this.readConsPage(pageOffset);
            if (!page || page.count < 2) {
                return NIL_PTR;
            }

            // Get first cell for indirect CDR
            const indirectCellOffset = pageOffset + CONSPAGE_HEADER_SIZE + (page.next_cell * CONSCELL_SIZE);
            const nextFree1 = this.memory[ indirectCellOffset + 4 ];
            page.next_cell = nextFree1;

            // Get second cell for actual cons cell
            const consCellOffset = pageOffset + CONSPAGE_HEADER_SIZE + (nextFree1 * CONSCELL_SIZE);
            const nextFree2 = this.memory[ consCellOffset + 4 ];
            page.next_cell = nextFree2;
            page.count -= 2;
            this.writeConsPage(pageOffset, page);

            // Write indirect cell (contains CDR value)
            MemoryManager.Access.writeLispPTR(this.memory, indirectCellOffset, cdr);
            this.memory[ indirectCellOffset + 4 ] = 0; // Not used

            // Write cons cell with CDR_INDIRECT
            const cell: ConsCell = {
                car_field: car,
                cdr_code: CDR_INDIRECT,
            };
            this.writeConsCell(consCellOffset, cell);

            // Calculate CDR code for indirect reference
            // CDR code = (byte offset of indirect cell) >> 1
            const indirectOffset = (indirectCellOffset - pageOffset - CONSPAGE_HEADER_SIZE) / CONSCELL_SIZE;
            this.memory[ consCellOffset + 4 ] = indirectOffset;

            cellOffset = consCellOffset;
        }

        // Convert byte offset to LispPTR
        return MemoryManager.Address.byteToLispPtr(cellOffset);
    }
}
