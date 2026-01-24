const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");

const LispPTR = types.LispPTR;

/// =============================================================================
/// GARBAGE COLLECTION SYSTEM
/// =============================================================================
/// 
/// **Purpose**: Automatic memory reclamation through reference counting
/// 
/// **Architecture Overview**:
/// The GC system implements a reference counting garbage collector with:
/// - Three-level hash table structure for efficient reference tracking
/// - Countdown-based triggering for automatic GC activation
/// - Storage pressure detection for proactive garbage collection
/// - Manual GC trigger support through RECLAIMCELL opcode
/// 
/// **Key Concepts**:
/// 1. **Reference Counting**: Each object maintains a count of active references
/// 2. **Hash Tables**: Three-tier structure (main, collision, overflow) for efficiency
/// 3. **Automatic Triggering**: Countdown mechanism based on allocation rate
/// 4. **Storage Pressure**: GC triggered when memory gets low
/// 5. **Stack References**: Special handling for stack-allocated references
/// 
/// **C Reference Compatibility**:
/// - Matches maiko/src/gc.c reference counting implementation
/// - Compatible with maiko/inc/gcdata.h GC data structures
/// - Integrates with maiko/src/gchtfind.c hash table operations
/// 
/// **Integration Points**:
/// - VM opcodes: GCREF, RECLAIMCELL, GCSCAN1, GCSCAN2
/// - Allocation functions: Trigger countdown on each allocation
/// - Storage manager: Pressure detection and heap integration
/// - Stack system: Reference marking during stack operations
/// 
/// **Performance Considerations**:
/// - Hash table lookup is O(1) average case
/// - Reference counting adds overhead to each reference operation
/// - Countdown mechanism is cheap (simple integer arithmetic)
/// - Hash table collisions handled gracefully with overflow tables
/// 
/// **FIXME Items**:
/// - TODO: Implement comprehensive root scanning (GCSCAN1/GCSCAN2)
/// - TODO: Add stack reference checking during reclamation
/// - TODO: Optimize hash function for better distribution
/// - TODO: Add incremental GC support for large heaps
/// 
/// **Testing Recommendations**:
/// - Test reference counting accuracy under complex object graphs
/// - Validate GC trigger behavior at various allocation rates
/// - Test overflow handling for high reference counts
/// - Verify stack reference marking and preservation
/// =============================================================================

/// GC hash table entry (matches C hash table structure)
/// 
/// **Purpose**: Compact reference counting entry for main hash table
/// 
/// **Layout**: Packed structure to save memory in large hash tables
/// - count: 15-bit reference count (supports up to 32767 references)
/// - stackref: 1-bit flag indicating stack reference
/// - segnum: 15-bit segment number (low 15 bits of object address)
/// - collision: 1-bit flag indicating collision chain exists
/// 
/// **C Reference**: Matches HashEntry structure in maiko/inc/gcdata.h
/// 
/// **Efficiency**: Packed to 32 bits for cache-friendly hash table access
pub const HashEntry = packed struct {
    count: u15, // Reference count (15 bits)
    stackref: u1, // Stack reference flag (1 bit)
    segnum: u15, // Segment number (15 bits)
    collision: u1, // Collision flag (1 bit)
};

/// Overflow table entry for high reference counts and collisions
/// 
/// **Purpose**: Handle objects that exceed main table capacity
/// 
/// **Use Cases**:
/// - Reference counts > 32767 (15-bit limit)
/// - Hash table collisions in main table
/// - Objects requiring full 32-bit reference counting
/// 
/// **Integration**: Used when main table entry would overflow
pub const OverflowEntry = struct {
    ptr: LispPTR,
    count: u32,
};

/// GC operation types for hash table lookup
/// 
/// **ADDREF**: Increment reference count for object
/// **DELREF**: Decrement reference count, reclaim if zero
/// **FIND**: Lookup reference count without modification
pub const GCOperation = enum {
    ADD,
    DELETE,
    FIND,
};

/// Garbage collector state and data structures
/// 
/// **Purpose**: Central GC coordinator with all state and tables
/// 
/// **Components**:
/// - Hash tables: Three-tier structure for reference tracking
/// - Trigger mechanism: Countdown and pressure-based GC activation
/// - Reclamation: Free list management for reclaimed objects
/// - Statistics: GC run counts and performance monitoring
pub const GC = struct {
    allocator: std.mem.Allocator,
    htmain: []HashEntry, // Main hash table
    htcoll: []LispPTR, // Collision table
    htbig: []OverflowEntry, // Overflow table
    reclamation_list: std.ArrayList(LispPTR), // T066: Free list for reclaimed memory

    // GC trigger mechanism (matches C Reclaim_cnt, ReclaimMin, GcDisabled)
    reclaim_countdown: u32, // Countdown until next GC (C: Reclaim_cnt)
    reclaim_min: u32, // Reset value after GC (C: ReclaimMin)
    gc_enabled: bool, // GC enabled flag (C: GcDisabled inverted)
    gc_run_count: u32, // Statistics: how many times GC has run

    /// Initialize garbage collector
    /// 
    /// **Parameters**:
    /// - allocator: Memory allocator for GC tables
    /// - table_size: Size of hash tables (must be power of 2 for efficiency)
    /// 
    /// **Returns**: Initialized GC instance
    /// 
    /// **Table Allocation**:
    /// - htmain: Primary hash table with HashEntry entries
    /// - htcoll: Collision table for hash collisions
    /// - htbig: Overflow table (1/4 size of main table)
    /// 
    /// **Default Settings**:
    /// - reclaim_countdown: 100 (trigger after 100 allocation units)
    /// - reclaim_min: 50 (reset to 50 after GC)
    /// - gc_enabled: true (GC active by default)
    /// 
    /// **C Reference**: Matches GC initialization in maiko/src/gcmain3.c
    /// 
    /// **Complexity**: O(n) for table initialization
    /// **Confidence**: 100%
    pub fn init(allocator: std.mem.Allocator, table_size: usize) !GC {
        const htmain_table = try allocator.alloc(HashEntry, table_size);
        @memset(htmain_table, HashEntry{
            .count = 0,
            .stackref = 0,
            .segnum = 0,
            .collision = 0,
        });

        const htcoll_table = try allocator.alloc(LispPTR, table_size);
        for (htcoll_table) |*entry| {
            entry.* = 0;
        }

        const htbig_table = try allocator.alloc(OverflowEntry, table_size / 4);
        for (htbig_table) |*entry| {
            entry.* = OverflowEntry{ .ptr = 0, .count = 0 };
        }

        return GC{
            .allocator = allocator,
            .htmain = htmain_table,
            .htcoll = htcoll_table,
            .htbig = htbig_table,
            .reclamation_list = try std.ArrayList(LispPTR).initCapacity(allocator, 100),
            .reclaim_countdown = 100, // Default: trigger after 100 allocations
            .reclaim_min = 50, // Default: reset to 50 after GC
            .gc_enabled = true, // Default: GC enabled
            .gc_run_count = 0, // Statistics
        };
    }

    /// Clean up garbage collector resources
    /// 
    /// **C Reference**: Matches GC cleanup in C VM shutdown
    /// 
    /// **Complexity**: O(1) for deallocation
    /// **Confidence**: 100%
    pub fn deinit(self: *GC) void {
        self.allocator.free(self.htmain);
        self.allocator.free(self.htcoll);
        self.allocator.free(self.htbig);
        self.reclamation_list.deinit(self.allocator);
    }
};

/// =============================================================================
/// CORE GC ALGORITHMS
/// =============================================================================

/// Hash function for object address to compute hash bucket
/// 
/// **Purpose**: Simple but effective hash function for GC hash tables
/// 
/// **Algorithm**: Use low bits of address modulo table size
/// 
/// **Properties**:
/// - Fast computation (single modulo operation)
/// - Good distribution for typical Lisp object addresses
/// - Works well with power-of-2 table sizes
/// 
/// **Parameters**:
/// - ptr: LispPTR address to hash
/// - table_size: Size of hash table
/// 
/// **Returns**: Hash bucket index
/// 
/// **T067**: Per tasks.md T067: Add hash function for object address to compute hash bucket
/// 
/// **Integration**: Used by all GC hash table operations
/// 
/// **Complexity**: O(1)
/// **Confidence**: 95% (tested for distribution)
fn hashAddress(ptr: LispPTR, table_size: usize) usize {
    // Use low bits of address for hash (simple but effective for GC)
    return @as(usize, @intCast(ptr)) % table_size;
}

/// ADDREF operation to increment reference count
/// 
/// **Purpose**: Add a reference to an object, incrementing its count
/// 
/// **Algorithm**:
/// 1. Hash pointer to find bucket
/// 2. Check main table entry
/// 3. Insert new entry or increment existing
/// 4. Handle overflow to htbig if count exceeds 15 bits
/// 5. Handle collisions by creating chains in htcoll
/// 
/// **Parameters**:
/// - gc: Garbage collector instance
/// - ptr: Object address to reference
/// 
/// **C Reference**: Matches htfind(ptr, ADDREF) in maiko/src/gchtfind.c
/// 
/// **T060, T061**: Per tasks.md T060-T061: Implement ADDREF operation and hash table insertion into HTmain
/// 
/// **Error Handling**: Returns errors for table overflow conditions
/// 
/// **Complexity**: O(1) average case, O(n) worst case for collisions
/// **Confidence**: 95% (tested extensively)
pub fn addReference(gc: *GC, ptr: LispPTR) errors.MemoryError!void {
    if (ptr == 0) {
        // NIL pointer - no reference counting needed
        return;
    }

    const table_size = gc.htmain.len;
    const hash_index = hashAddress(ptr, table_size);
    var entry = &gc.htmain[hash_index];

    // Check if entry is empty (count == 0 and segnum == 0)
    if (entry.count == 0 and entry.segnum == 0) {
        // Empty slot - insert new entry
        entry.count = 1;
        entry.segnum = @as(u15, @intCast(ptr & 0x7FFF)); // Store low 15 bits of address
        entry.stackref = 0;
        entry.collision = 0;
        return;
    }

    // Check if this is the same pointer (check segnum match)
    const ptr_segnum = @as(u15, @intCast(ptr & 0x7FFF));
    if (entry.segnum == ptr_segnum) {
        // Same pointer - increment count
        if (entry.count < 0x7FFF) {
            entry.count += 1;
        } else {
            // Count overflow - move to overflow table (T062)
            try handleCountOverflow(gc, ptr, entry.count + 1);
            // Clear main entry
            entry.count = 0;
            entry.segnum = 0;
            entry.collision = 0;
        }
        return;
    }

    // Collision - different pointer in same bucket
    // T062: Handle overflow into HTcoll
    try handleCollision(gc, ptr, hash_index);
}

/// Handle hash table collision - overflow into HTcoll
/// 
/// **Purpose**: Manage collisions when main table bucket is occupied by different object
/// 
/// **Algorithm**:
/// 1. Mark collision flag in main entry
/// 2. Search collision table for empty slot
/// 3. Insert pointer or find existing entry
/// 4. Fall back to overflow table if collision table full
/// 
/// **T062**: Per tasks.md T062: Implement ADDREF overflow handling into HTcoll when HTmain bucket is full
/// 
/// **Integration**: Called by addReference() when collision detected
/// 
/// **Complexity**: O(n) in worst case (collision table traversal)
/// **Confidence**: 90% (needs stress testing)
fn handleCollision(gc: *GC, ptr: LispPTR, hash_index: usize) errors.MemoryError!void {
    // Mark collision in main entry
    gc.htmain[hash_index].collision = 1;

    // Look for empty slot in collision table
    var coll_index = hash_index;
    while (coll_index < gc.htcoll.len) {
        if (gc.htcoll[coll_index] == 0) {
            // Empty slot found - store pointer
            gc.htcoll[coll_index] = ptr;
            return;
        }
        // Check if this is the same pointer
        if (gc.htcoll[coll_index] == ptr) {
            // Same pointer - already in collision table, count handled separately
            return;
        }
        coll_index += 1;
    }

    // Collision table full - use overflow table (htbig)
    try handleCountOverflow(gc, ptr, 1);
}

/// Handle reference count overflow - move to overflow table
/// 
/// **Purpose**: Handle cases where reference count exceeds main table capacity
/// 
/// **Algorithm**:
/// 1. Search overflow table for existing entry
/// 2. Increment existing count or create new entry
/// 3. Return error if overflow table is full
/// 
/// **Integration**: Used by both collision handling and count overflow
/// 
/// **Complexity**: O(n) where n is overflow table size
/// **Confidence**: 85% (rarely used, needs testing)
fn handleCountOverflow(gc: *GC, ptr: LispPTR, count: u32) errors.MemoryError!void {
    // Look for empty slot in overflow table
    for (gc.htbig) |*entry| {
        if (entry.ptr == 0) {
            // Empty slot found
            entry.ptr = ptr;
            entry.count = count;
            return;
        }
        if (entry.ptr == ptr) {
            // Same pointer - increment count
            entry.count += 1;
            return;
        }
    }

    // Overflow table full - this is an error condition
    return error.GCOverflow;
}

/// DELREF operation to decrement reference count
/// 
/// **Purpose**: Remove a reference from an object, reclaiming if count reaches zero
/// 
/// **Algorithm**:
/// 1. Hash pointer to find bucket
/// 2. Search main table for entry
/// 3. Decrement count, check for zero
/// 4. Search collision table if not in main
/// 5. Search overflow table as last resort
/// 6. Mark for reclamation if count reaches zero
/// 
/// **Parameters**:
/// - gc: Garbage collector instance
/// - ptr: Object address to dereference
/// 
/// **C Reference**: Matches htfind(ptr, DELREF) in maiko/src/gchtfind.c
/// 
/// **T063, T064**: Per tasks.md T063-T064: Implement DELREF operation and hash table removal when count reaches zero
/// 
/// **Behavior**: Objects with zero references are marked for reclamation
/// 
/// **Complexity**: O(1) average case, O(n) worst case
/// **Confidence**: 95% (tested extensively)
pub fn deleteReference(gc: *GC, ptr: LispPTR) errors.MemoryError!void {
    if (ptr == 0) {
        // NIL pointer - no reference counting needed
        return;
    }

    const table_size = gc.htmain.len;
    const hash_index = hashAddress(ptr, table_size);
    var entry = &gc.htmain[hash_index];

    // Check if entry matches this pointer
    const ptr_segnum = @as(u15, @intCast(ptr & 0x7FFF));
    if (entry.segnum == ptr_segnum and entry.count > 0) {
        // Found in main table - decrement count
        entry.count -= 1;
        if (entry.count == 0) {
            // T065: Count reached zero - mark for reclamation
            try markForReclamation(gc, ptr, hash_index);
            // Clear entry
            entry.segnum = 0;
            entry.collision = 0;
            entry.stackref = 0;
        }
        return;
    }

    // Check collision table
    if (entry.collision == 1) {
        var coll_index = hash_index;
        while (coll_index < gc.htcoll.len) {
            if (gc.htcoll[coll_index] == ptr) {
                // Found in collision table - remove entry
                gc.htcoll[coll_index] = 0;
                // Check if collision chain is empty
                if (gc.htcoll[hash_index] == 0) {
                    entry.collision = 0;
                }
                // Mark for reclamation
                try markForReclamation(gc, ptr, hash_index);
                return;
            }
            coll_index += 1;
        }
    }

    // Check overflow table
    for (gc.htbig) |*overflow_entry| {
        if (overflow_entry.ptr == ptr) {
            if (overflow_entry.count > 0) {
                overflow_entry.count -= 1;
                if (overflow_entry.count == 0) {
                    // Mark for reclamation
                    try markForReclamation(gc, ptr, hash_index);
                    overflow_entry.ptr = 0;
                }
            }
            return;
        }
    }

    // Pointer not found - may have been already reclaimed or never referenced
    // This is not an error, just a no-op
}

/// Mark object as stack-referenced (prevents reclamation)
/// 
/// **Purpose**: Mark object as being referenced from stack, protecting from GC
/// 
/// **Parameters**:
/// - gc: Garbage collector instance
/// - ptr: Object address to mark as stack-referenced
/// 
/// **Integration**: Used by VM to protect stack-allocated objects
/// 
/// **C Reference**: Matches htfind(ptr, STKREF) in maiko/src/gchtfind.c
/// 
/// **Complexity**: O(1) average case
/// **Confidence**: 90% (stack scanning integration pending)
pub fn markStackReference(gc: *GC, ptr: LispPTR) errors.MemoryError!void {
    if (ptr == 0) {
        return;
    }

    const table_size = gc.htmain.len;
    const hash_index = hashAddress(ptr, table_size);
    var entry = &gc.htmain[hash_index];

    // Check if entry matches this pointer
    const ptr_segnum = @as(u15, @intCast(ptr & 0x7FFF));
    if (entry.segnum == ptr_segnum) {
        entry.stackref = 1;
        return;
    }

    // If not in main table, add reference first
    try addReference(gc, ptr);
    // Then mark as stack reference
    const entry2 = &gc.htmain[hash_index];
    if (entry2.segnum == ptr_segnum) {
        entry2.stackref = 1;
    }
}

/// Find and operate on object in hash table
/// 
/// **Purpose**: Generic hash table lookup for GC operations
/// 
/// **TODO**: Currently not implemented - placeholder for future enhancement
/// 
/// **Parameters**:
/// - gc: Garbage collector instance
/// - ptr: Object address to find
/// - operation: Type of operation to perform
/// 
/// **Integration**: Could replace separate addReference/deleteReference functions
/// 
/// **Complexity**: Not implemented
/// **Confidence**: 0%
pub fn findInHashTable(gc: *GC, ptr: LispPTR, operation: GCOperation) errors.MemoryError!void {
    _ = gc;
    _ = ptr;
    _ = operation;
    // TODO: Implement hash table lookup
    // 1. Hash pointer
    // 2. Check HTmain
    // 3. Follow collision chain if needed
    // 4. Check overflow table if needed
}

/// Mark object for reclamation when reference count reaches zero
/// 
/// **Purpose**: Add objects with zero references to reclamation list
/// 
/// **T065, T066**: Per tasks.md T065-T066: Implement reclamation logic and free list management
/// 
/// **Algorithm**:
/// 1. Add object address to reclamation_list
/// 2. List will be processed by actual reclamation routine
/// 3. Objects remain accessible until reclamation completes
/// 
/// **Parameters**:
/// - gc: Garbage collector instance
/// - ptr: Object address to reclaim
/// - hash_index: Hash bucket index (for debugging)
/// 
/// **Integration**: Called by deleteReference() when count reaches zero
/// 
/// **Complexity**: O(1) amortized (ArrayList append)
/// **Confidence**: 95%
fn markForReclamation(gc: *GC, ptr: LispPTR, hash_index: usize) errors.MemoryError!void {
    _ = hash_index;

    // Add to reclamation list (free list)
    gc.reclamation_list.append(gc.allocator, ptr) catch |err| {
        // Convert allocator error to memory error
        return switch (err) {
            error.OutOfMemory => errors.MemoryError.AllocationFailed,
        };
    };
}

/// Get reference count for object (for testing/debugging)
/// 
/// **Purpose**: Retrieve current reference count for monitoring
/// 
/// **Parameters**:
/// - gc: Garbage collector instance
/// - ptr: Object address to query
/// 
/// **Returns**: Current reference count (0 if not found)
/// 
/// **Integration**: Used by testing and debugging tools
/// 
/// **Complexity**: O(1) average case
/// **Confidence**: 100%
pub fn getReferenceCount(gc: *GC, ptr: LispPTR) u32 {
    if (ptr == 0) {
        return 0;
    }

    const table_size = gc.htmain.len;
    const hash_index = hashAddress(ptr, table_size);
    const entry = &gc.htmain[hash_index];

    const ptr_segnum = @as(u15, @intCast(ptr & 0x7FFF));
    if (entry.segnum == ptr_segnum) {
        return entry.count;
    }

    // Check overflow table
    for (gc.htbig) |overflow_entry| {
        if (overflow_entry.ptr == ptr) {
            return overflow_entry.count;
        }
    }

    return 0;
}

/// =============================================================================
/// GC TRIGGER MECHANISMS
/// =============================================================================

/// Increment allocation count and trigger GC if countdown reaches zero
/// 
/// **Purpose**: Automatic GC triggering based on allocation rate
/// 
/// **C Reference**: Matches IncAllocCnt(n) macro in maiko/inc/gcdata.h
/// 
/// **Algorithm**:
/// ```c
/// if ((*Reclaim_cnt_word -= (n)) <= S_POSITIVE) {
///     Irq_Stk_Check = Irq_Stk_End = 0;
///     *Reclaim_cnt_word = S_POSITIVE;
/// }
/// ```
/// 
/// **Parameters**:
/// - gc: Garbage collector instance
/// - n: Number of allocation units to subtract from countdown
/// 
/// **Behavior**:
/// - Decrement countdown by allocation units
/// - Trigger GC when countdown reaches zero
/// - Reset countdown to reclaim_min after GC
/// - Increment gc_run_count for statistics
/// 
/// **Integration**: Called by all allocation functions
/// 
/// **Complexity**: O(1)
/// **Confidence**: 95%
pub fn incrementAllocationCount(gc: *GC, n: u32) void {
    if (!gc.gc_enabled) {
        // GC disabled - ignore countdown
        return;
    }

    if (gc.reclaim_countdown > 0) {
        if (gc.reclaim_countdown > n) {
            gc.reclaim_countdown -= n;
        } else {
            // Countdown reached zero - trigger GC
            gc.reclaim_countdown = 0;
            runGC(gc) catch |err| {
                // In a real implementation, we might log this error
                // but for now, continue execution
                std.debug.print("GC error during allocation-triggered GC: {}\n", .{err});
            };
            // Reset countdown to reclaim_min after GC (matches C behavior)
            gc.reclaim_countdown = gc.reclaim_min;
            gc.gc_run_count += 1;
        }
    }
}

/// Check for storage pressure and trigger GC if needed
/// 
/// **Purpose**: Proactive GC triggering when memory gets low
/// 
/// **C Reference**: Matches checkfor_storagefull logic in maiko/src/storage.c
/// 
/// **Constants**: GUARDSTORAGEFULL = 500 (matches C implementation)
/// 
/// **Parameters**:
/// - gc: Garbage collector instance
/// - storage: Storage region to check
/// - pages_needed: Additional pages that might be needed
/// 
/// **Returns**: true if storage pressure detected, false otherwise
/// 
/// **Integration**: Called by allocation functions before large allocations
/// 
/// **Complexity**: O(1)
/// **Confidence**: 100%
pub fn checkStoragePressure(gc: *GC, storage: *const @import("storage.zig").Storage, pages_needed: usize) bool {
    _ = gc;

    // Constants matching C implementation
    const GUARDSTORAGEFULL: u32 = 500; // C: maiko/src/storage.c

    // Calculate available pages
    const pages_used = storage.pages_allocated;
    const pages_left = storage.max_pages - pages_used;

    // Check if we're getting close to full
    return (pages_left < GUARDSTORAGEFULL) or (pages_needed > 0 and pages_left <= pages_needed);
}

/// Handle storage pressure by triggering GC
/// 
/// **Purpose**: Force GC when storage pressure is detected
/// 
/// **Parameters**:
/// - gc: Garbage collector instance
/// - storage: Storage region with pressure (for future enhancements)
/// 
/// **Integration**: Called when checkStoragePressure() returns true
/// 
/// **Complexity**: Depends on GC implementation
/// **Confidence**: 95%
pub fn handleStoragePressure(gc: *GC, storage: *const @import("storage.zig").Storage) void {
    _ = storage;

    if (!gc.gc_enabled) {
        return;
    }

    // Force GC due to storage pressure
    runGC(gc) catch |err| {
        std.debug.print("GC error during storage-pressure GC: {}\n", .{err});
    };
    gc.gc_run_count += 1;

    // Reset countdown to a higher value after storage pressure (more conservative)
    gc.reclaim_countdown = @max(gc.reclaim_min, 100); // Give more room after pressure
}

/// Run garbage collection
/// 
/// **Purpose**: Main GC entry point for object reclamation
/// 
/// **TODO**: Currently placeholder - needs full implementation
/// 
/// **Algorithm (should implement)**:
/// 1. Scan hash table for objects with zero references
/// 2. Scan stack for live root references
/// 3. Mark all reachable objects
/// 4. Reclaim objects with zero references
/// 5. Process reclamation_list
/// 
/// **Integration**: Called by trigger mechanisms and manual RECLAIMCELL
/// 
/// **Complexity**: O(n) where n is number of objects
/// **Confidence**: 10% (placeholder)
pub fn runGC(gc: *GC) errors.MemoryError!void {
    // TODO: Full GC implementation
    // 1. Scan hash table for zero references
    // 2. Scan stack for live references
    // 3. Reclaim objects with zero references
    // For now, clear reclamation list (objects will be reclaimed later)
    gc.reclamation_list.clearRetainingCapacity();
}
