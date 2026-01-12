# Scripts Directory

**Date**: 2025-12-16 12:41  
**Purpose**: Utility scripts for testing, debugging, and validation

## Scripts

### `generate_debug_logs.sh`

Generate execution trace logs from both C and Zig emulators for comparison.

**Usage**:
```bash
./scripts/generate_debug_logs.sh [sysout_file]
```

**Default**: Uses `medley/internal/loadups/starter.sysout`

**Output**:
- `c_emulator_execution_log.txt` - C emulator execution trace
- `zig_emulator_execution_log.txt` - Zig emulator execution trace

### `compare_debug_logs.sh`

Compare execution trace logs from C and Zig emulators field-by-field.

**Usage**:
```bash
./scripts/compare_debug_logs.sh [c_log] [zig_log]
```

**Default**: Uses `c_emulator_execution_log.txt` and `zig_emulator_execution_log.txt`

**Output**: Field-by-field comparison showing:
- PC addresses
- FuncObj offsets
- Instruction bytes
- Opcode names
- Stack depth
- Frame headers

### `compare_execution_logs.sh`

Compare execution logs (legacy script).

### `verify_endianness.sh`

Verify endianness handling in sysout loading.

**Usage**:
```bash
./scripts/verify_endianness.sh [sysout_file]
```

## Related Documentation

- [Execution Trace Format](../../documentation/rewrite-spec/vm-core/execution-trace.md) - Trace format specification
- [Zig Implementation](../../documentation/implementations/zig-implementation.md) - Zig-specific debugging notes
