# Data Model: Runner Scripts + Execution Trace Parity

**Date**: 2026-01-15
**Feature**: 004-emulator-runner

## Overview

This feature manages two related concerns:

1. Runner-script configuration/validation for selecting and launching emulators
2. Execution-trace parity runs and comparisons (C ground truth; Zig must match)

No persistent storage is required; artifacts are ephemeral (log files, comparison reports, lock file).

## Entities

### EmulatorSelection

Represents the user's choice of emulator implementation.

**Attributes**:

- `emulator_type`: Enum (c, zig, lisp) - The selected emulator
- `source`: Enum (command_line, environment, default) - How the selection was made
- `precedence`: Integer - Priority order (command_line > environment > default)

**Validation Rules**:

- Must be one of: c, zig, lisp
- Case insensitive input, normalized to lowercase
- Invalid values trigger error with user-friendly message

**Relationships**:

- 1:1 with RunConfiguration

### EmulatorExecutable

Represents the actual emulator binary to be executed.

**Attributes**:

- `path`: String - Full path to executable
- `emulator_type`: Enum (c, zig, lisp) - Type of emulator
- `exists`: Boolean - Whether file exists
- `executable`: Boolean - Whether file has execute permission
- `size`: Integer - File size in bytes
- `header_valid`: Boolean - Whether file header matches expected format

**Validation Rules**:

- Path must exist
- File must be executable
- Size > 0
- Header validation for executable format

**Relationships**:

- 1:1 with EmulatorSelection

### RunConfiguration

Represents the complete runtime configuration for starting Interlisp.

**Attributes**:

- `emulator_selection`: EmulatorSelection - Chosen emulator
- `sysout_file`: String - Path to sysout file
- `display_options`: Map<String, String> - Display settings
- `memory_settings`: Map<String, String> - Memory configuration
- `other_args`: List<String> - Additional command-line arguments

**Validation Rules**:

- Sysout file must exist and be readable
- Display options must be valid for selected emulator
- Memory settings within platform limits

**Relationships**:

- 1:1 with EmulatorSelection
- 1:1 with EmulatorExecutable

## Data Flow

1. User provides emulator selection (command-line or environment)
2. System resolves EmulatorSelection with precedence rules
3. System locates EmulatorExecutable using unified build paths
4. System validates EmulatorExecutable properties
5. System constructs RunConfiguration with all settings
6. System executes with locking mechanism

### Execution Trace Parity Entities

#### TraceRun

Represents one trace-generation run for a specific emulator and sysout/configuration.

**Attributes**:
- `emulator_impl`: Enum (`c`, `zig`)
- `sysout_file`: String
- `trace_output_path`: String
- `completed`: Boolean
- `max_steps`: Optional<Integer> - runtime cap for fast iteration (if set)

#### TraceComparison

Represents one comparison between C and Zig traces.

**Attributes**:
- `c_trace_path`: String
- `zig_trace_path`: String
- `lcp_lines`: Integer - longest common prefix length (auto-detected)
- `start_line`: Optional<Integer> - manual override for comparison start
- `first_divergence_line`: Optional<Integer>
- `first_divergence_context`: Map<String, String> (e.g., PC/opcode/bytes/stack/frame as available)
- `matches_to_completion`: Boolean

#### ParityStage

Represents the staged parity gate.

**Attributes**:
- `stage`: Enum (`starter_sysout`, `full_sysout`)
- `required`: Boolean
- `completed`: Boolean

## State Transitions

### Emulator Selection States

- `unselected` → `selected` (via command-line, environment, or default)
- `selected` → `validated` (emulator exists and is executable)
- `validated` → `ready` (all configuration complete)

### Lock States

- `unlocked` → `locked` (when Interlisp starts)
- `locked` → `unlocked` (when Interlisp exits normally)
- `locked` → `stale` (detected via timeout, auto-cleanup)

## Constraints

- Only one emulator selection per run
- Lock files prevent concurrent runs per user
- Backward compatibility with existing run-medley arguments
- Platform-specific emulator availability
- Parity staged: `starter.sysout` completion parity required before `full.sysout`
