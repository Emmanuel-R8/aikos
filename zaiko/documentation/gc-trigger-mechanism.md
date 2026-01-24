# GC Trigger Mechanism Implementation

## Overview

This document describes the implementation of the missing GC trigger mechanism in the Zig emulator, which matches the countdown-based garbage collection system found in the C implementation.

## Problem Analysis

The original Zig GC implementation had all the data structures and algorithms for garbage collection but lacked a mechanism to **automatically trigger** garbage collection when memory pressure was reached. This is a critical gap because without automatic triggering, garbage collection would never run in practice.

### C Implementation Analysis

The C implementation uses a countdown-based trigger mechanism:

1. **Reclaim_cnt** (`\\RECLAIM.COUNTDOWN`): Countdown timer that decrements with allocations
2. **ReclaimMin** (`\\RECLAIMMIN`): Reset value after garbage collection runs  
3. **Increment_Allocation_Count** macro: Decrements countdown and triggers GC when it reaches zero
4. **doreclaim()**: Main garbage collection function
5. **checkfor_storagefull()**: Checks storage pressure and can trigger GC

## Implementation Details

### Core Changes

#### 1. Extended GC Structure
```zig
pub const GC = struct {
    // ... existing fields ...
    
    // GC trigger mechanism (matches C Reclaim_cnt, ReclaimMin, GcDisabled)
    reclaim_countdown: u32, // Countdown until next GC (C: Reclaim_cnt)
    reclaim_min: u32, // Reset value after GC (C: ReclaimMin)  
    gc_enabled: bool, // GC enabled flag (C: GcDisabled inverted)
    gc_run_count: u32, // Statistics: how many times GC has run
};
```

#### 2. Increment Allocation Count Function
```zig
pub fn incrementAllocationCount(gc: *GC, n: u32) void {
    if (!gc.gc_enabled) return;
    
    if (gc.reclaim_countdown > 0) {
        if (gc.reclaim_countdown > n) {
            gc.reclaim_countdown -= n;
        } else {
            // Countdown reached zero - trigger GC
            gc.reclaim_countdown = 0;
            runGC(gc) catch |err| {
                std.debug.print("GC error during allocation-triggered GC: {}\n", .{err});
            };
            // Reset countdown to reclaim_min after GC (matches C behavior)
            gc.reclaim_countdown = gc.reclaim_min;
            gc.gc_run_count += 1;
        }
    }
}
```

This matches the C `Increment_Allocation_Count` macro:
```c
#define Increment_Allocation_Count(n) \
  do { \
    if (*Reclaim_cnt_word != NIL) { \
      if (*Reclaim_cnt_word > (n)) \
        (*Reclaim_cnt_word) -= (n); \
      else { \
        *Reclaim_cnt_word = NIL; \
        doreclaim(); \
      } \
    } \
  } while (0)
```

#### 3. Storage Pressure Detection
```zig
pub fn checkStoragePressure(gc: *GC, storage: *const Storage, pages_needed: usize) bool {
    const GUARDSTORAGEFULL: u32 = 500; // C: maiko/src/storage.c
    
    const pages_used = storage.pages_allocated;
    const pages_left = storage.max_pages - pages_used;
    
    return (pages_left < GUARDSTORAGEFULL) or (pages_needed > 0 and pages_left <= pages_needed);
}
```

#### 4. Storage Pressure Handling
```zig
pub fn handleStoragePressure(gc: *GC, storage: *const Storage) void {
    if (!gc.gc_enabled) return;
    
    // Force GC due to storage pressure
    runGC(gc) catch |err| {
        std.debug.print("GC error during storage-pressure GC: {}\n", .{err});
    };
    gc.gc_run_count += 1;
    
    // Reset countdown to allow more allocations
    gc.reclaim_countdown = @max(gc.reclaim_min, 100);
}
```

### Integration with Memory Allocation

#### Cons Cell Allocation
```zig
pub fn allocateConsCell(storage: *Storage, gc: ?*GC) errors.MemoryError!LispPTR {
    // ... allocation logic ...
    
    // Trigger GC countdown if GC is available
    if (gc) |gc_inst| {
        const gc_module = @import("gc.zig");
        gc_module.incrementAllocationCount(gc_inst, 1);
    }
    
    return offsetToLispPTR(storage, offset);
}
```

#### Array Allocation
```zig
pub fn allocateArray(storage: *Storage, size: usize, type_code: u8, gc: ?*GC) errors.MemoryError!LispPTR {
    // ... allocation logic ...
    
    // Trigger GC countdown if GC is available
    if (gc) |gc_inst| {
        const gc_module = @import("gc.zig");
        const allocation_units = @as(u32, @intCast((size + header_size + 7) / 8));
        gc_module.incrementAllocationCount(gc_inst, allocation_units);
    }
    
    return addr;
}
```

## Testing

### Countdown Mechanism Tests
1. **Basic Countdown**: Verifies countdown decrements correctly
2. **GC Trigger**: Verifies GC runs when countdown reaches zero
3. **Reset Behavior**: Verifies countdown resets to `reclaim_min` after GC
4. **GC Disabled**: Verifies no triggering when GC is disabled

### Storage Pressure Tests
1. **Pressure Detection**: Verifies storage pressure is detected correctly
2. **Pressure Handling**: Verifies GC is triggered under pressure
3. **Integration**: Verifies allocation functions trigger countdown

### Integration Tests
1. **Cons Cell Allocation**: Tests GC triggering from cons allocation
2. **Array Allocation**: Tests GC triggering from array allocation
3. **VM Integration**: Tests GC works within VM context

## Constants and Values

### C Constants Matching
- `GUARDSTORAGEFULL = 500` (from maiko/src/storage.c:66)
- Default `reclaim_countdown = 100` (reasonable starting value)
- Default `reclaim_min = 50` (matches typical C values)

### Allocation Units
- Cons cell: 1 unit (8 bytes typical)
- Array: `(size + header_size + 7) / 8` units (round up to 8-byte units)

## Error Handling

1. **Non-fatal GC errors**: GC errors are logged but don't stop execution
2. **Graceful degradation**: System continues even if GC fails
3. **Debug output**: Errors printed to debug stream for troubleshooting

## Usage Example

```zig
// Initialize GC with trigger mechanism
var gc = try GC.init(allocator, 1024);
gc.reclaim_countdown = 100;
gc.reclaim_min = 50;
gc.gc_enabled = true;

// Allocate objects - will automatically trigger GC when needed
const cons1 = try allocateConsCell(&storage, &gc);
const array1 = try allocateArray(&storage, 100, TYPE_POINTER, &gc);

// GC runs automatically when reclaim_countdown reaches 0
```

## Performance Considerations

1. **Minimal Overhead**: IncrementAllocationCount is very fast (simple arithmetic)
2. **Configurable Thresholds**: countdown values can be tuned per application
3. **Early Detection**: Storage pressure detection triggers GC before exhaustion
4. **Integration**: Seamless integration with existing allocation functions

## Compatibility

This implementation maintains full compatibility with:
- Existing GC data structures (HTmain, HTcoll, HTbig)
- Reference counting operations (ADDREF, DELREF, STKREF)
- Stack scanning and marking algorithms
- VM integration patterns

## Future Enhancements

1. **Adaptive Thresholds**: Auto-tune reclaim_min based on GC efficiency
2. **Generational Collection**: Separate trigger thresholds for different generations
3. **Concurrent Collection**: Run GC in separate thread when pressure detected
4. **Memory Pressure Callbacks**: Allow application-specific pressure handling