const std = @import("std");
const types = @import("../utils/types.zig");
const errors = @import("../utils/errors.zig");

const LispPTR = types.LispPTR;

/// GC hash table entry (matches C hash table structure)
/// Per data-model.md
pub const HashEntry = packed struct {
    count: u15, // Reference count (15 bits)
    stackref: u1, // Stack reference flag (1 bit)
    segnum: u15, // Segment number (15 bits)
    collision: u1, // Collision flag (1 bit)
};

/// Overflow table entry
pub const OverflowEntry = struct {
    ptr: LispPTR,
    count: u32,
};

/// GC operation type
pub const GCOperation = enum {
    ADD,
    DELETE,
    FIND,
};

/// Garbage collector state
pub const GC = struct {
    allocator: std.mem.Allocator,
    htmain: []HashEntry, // Main hash table
    htcoll: []LispPTR, // Collision table
    htbig: []OverflowEntry, // Overflow table
    reclamation_list: std.ArrayList(LispPTR), // T066: Free list for reclaimed memory

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
        };
    }

    pub fn deinit(self: *GC) void {
        self.allocator.free(self.htmain);
        self.allocator.free(self.htcoll);
        self.allocator.free(self.htbig);
        self.reclamation_list.deinit(self.allocator);
    }
};

/// T067: Hash function for object address to compute hash bucket
/// Per tasks.md T067: Add hash function for object address to compute hash bucket
/// Simple hash function: use low bits of address modulo table size
fn hashAddress(ptr: LispPTR, table_size: usize) usize {
    // Use low bits of address for hash (simple but effective for GC)
    return @as(usize, @intCast(ptr)) % table_size;
}

/// T060, T061: ADDREF operation to increment reference count
/// Per tasks.md T060-T061: Implement ADDREF operation and hash table insertion into HTmain
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

/// T062: Handle collision - overflow into HTcoll when HTmain bucket is full
/// Per tasks.md T062: Implement ADDREF overflow handling into HTcoll when HTmain bucket is full
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

/// Handle count overflow - move to htbig overflow table
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

/// T063, T064: DELREF operation to decrement reference count
/// Per tasks.md T063-T064: Implement DELREF operation and hash table removal when count reaches zero
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

/// Mark stack reference
/// Per contracts/memory-interface.zig
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

/// Find in hash table
/// Per rewrite documentation memory/garbage-collection.md
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

/// T065, T066: Reclamation logic and free list management
/// Per tasks.md T065-T066: Implement reclamation logic and free list management
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

/// Get reference count for pointer (for testing/debugging)
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

/// Run GC
/// Per contracts/memory-interface.zig
pub fn runGC(gc: *GC) errors.MemoryError!void {
    // TODO: Full GC implementation
    // 1. Scan hash table for zero references
    // 2. Scan stack for live references
    // 3. Reclaim objects with zero references
    // For now, clear reclamation list (objects will be reclaimed later)
    gc.reclamation_list.clearRetainingCapacity();
}
