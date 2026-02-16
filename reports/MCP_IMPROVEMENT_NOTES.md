# MCP Improvement Notes

**Purpose**: Notes for MCP developers on deficiencies encountered during Common Lisp development sessions.

---

## Swank Integration Deficiencies

### Problem
Started Swank server but couldn't connect from MCP. Had to create separate SBCL processes with `--eval` for each test, causing very slow iteration cycles.

### Missing Functions

```lisp
;; Start persistent SBCL session
(swank_start :port 4006)  ; Returns connection handle

;; Evaluate in running session
(swank_eval "(+ 1 2)")   ; Uses existing connection

;; Load file in session  
(swank_load "path/to/file.lisp")

;; Inspect variable
(swank_inspect '*vm*)

;; Get backtrace on error
(swank_backtrace)

;; Ensure server is running (start if not)
(swank_ensure_running :port 4006)
```

### Current Workaround
```bash
sbcl --non-interactive --load file1.lisp --load file2.lisp --eval "(test)"
# Reloads ALL files for EVERY test - very slow
```

---

## Error Reporting Issues

### Problem
SBCL tests failed with stack traces instead of useful error messages. Missing exports caused "unbound variable" errors that were hard to diagnose.

### Root Cause
- MCP's SBCL calls used `--disable-debugger` which hid useful info
- No way to get proper error messages from SBCL

### Recommended Fix
Add option to enable debugger for interactive debugging, or capture and format SBCL errors better.

---

## Session State Management

### Problem
No way to maintain state across multiple SBCL evaluations. Each `--eval` starts fresh.

### Needed
- Persistent SBCL session
- State inspection between calls
- Hot-patching code without restart

---

## Summary of Required MCP Functions

| Function | Purpose |
|----------|---------|
| `swank_start` | Start Swank server |
| `swank_eval` | Evaluate in running session |
| `swank_load` | Load file in session |
| `swank_inspect` | Inspect variable |
| `swank_backtrace` | Get backtrace on error |
| `swank_ensure_running` | Start if not running |
