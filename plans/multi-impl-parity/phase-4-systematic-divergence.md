## Phase 4: Systematic Divergence Identification (Iterative Approach)

### Task 4.1: Iterative STEP-by-STEP Verification

**Action**:

- Use `iterative_parity_workflow.py` to orchestrate the process
- Start at STEP=0 (or resume from last verified STEP)
- For each 6-step window (STEP to STEP+5): - Run C emulator from 0 to STEP+5, extract steps STEP to STEP+5 - Generate structured analysis using `analyze_execution_window.py` - Run all other emulators for same window - Compare traces field-by-field - Identify which implementations match C reference - When multiple implementations match, use them to inform fixes - Fix divergences in non-matching implementations - Re-verify current window after fixes - Document findings (auto-generate from structured analysis) - Update `MULTI_IMPLEMENTATION_TASKS.md` - Commit state and progress to git - Update dashboard file - Only proceed when all implementations match for current window - Increment STEP by 5
- Continue until target step count reached or all implementations match
- **Partial Completion Handling**: If partial completion detected, break down tasks and resume

### Task 4.2: Execution Window Analysis

**Action**:

- For each 6-step window (STEP to STEP+5), generate deep analysis: - Opcode execution details (instruction, operands, addressing modes) - Memory address calculations (virtual to physical translation) - Stack and frame pointer changes - Register state transitions - Memory content analysis at key addresses - Address translation details - Flag changes and condition evaluations
- Store analysis in structured JSON format
- Compress previous window analysis files
- Use analysis for comparison and documentation generation

### Task 4.3: Multi-Implementation Comparison Per Window

**Action**:

- Compare all implementations within each 6-step window (STEP to STEP+5)
- Identify which implementations match C reference
- When multiple match, use them to inform fixes for non-matching ones
- Document divergence patterns per window
- Track which fields diverge (PC, registers, stack, memory, etc.)
- Identify root causes (opcode implementation, memory management, initialization, etc.)
- Log findings in JSON format with git commit reference

### Task 4.4: Incremental Fix and Verification

**Action**:

- Fix identified divergences before moving to next window
- Re-run verification for current window after fixes
- Only proceed to next window when current window matches across all implementations
- Track fix history per STEP range
- Document fixes in codebase with comments referencing STEP range (e.g., `# Fixed for STEP 0-4 window`)
- Update Typst documentation with fix details
- Commit fixes with descriptive messages

