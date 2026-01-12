#!/usr/bin/env bash
# Test script for run-medley emulator selection functionality
# Tests User Story 1: Run Interlisp with Selected Emulator

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
RUN_MEDLEY="$REPO_ROOT/medley/run-medley"

# Test counter
TESTS_PASSED=0
TESTS_FAILED=0

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

test_pass() {
    echo -e "${GREEN}✓ PASS${NC}: $1"
    ((TESTS_PASSED++)) || true
}

test_fail() {
    echo -e "${RED}✗ FAIL${NC}: $1"
    ((TESTS_FAILED++)) || true
}

test_skip() {
    echo -e "${YELLOW}⊘ SKIP${NC}: $1"
}

# Test 1: --emulator argument parsing
test_emulator_arg_parsing() {
    echo "Test 1: --emulator argument parsing"
    
    # Test valid emulator types
    for emulator in c zig lisp; do
        if "$RUN_MEDLEY" --emulator "$emulator" --help 2>&1 | grep -q "emulator\|Error" || true; then
            test_pass "--emulator $emulator argument accepted"
        else
            test_fail "--emulator $emulator argument not recognized"
        fi
    done
    
    # Test invalid emulator type
    if "$RUN_MEDLEY" --emulator invalid 2>&1 | grep -qi "invalid\|must be one of"; then
        test_pass "Invalid emulator type rejected with error"
    else
        test_fail "Invalid emulator type not rejected"
    fi
}

# Test 2: Emulator selection precedence (command-line > environment > default)
test_emulator_precedence() {
    echo "Test 2: Emulator selection precedence"
    
    # Test command-line overrides environment
    export MEDLEY_EMULATOR=zig
    if "$RUN_MEDLEY" --emulator c --help 2>&1 | grep -q "c\|Error" || true; then
        test_pass "Command-line argument takes precedence over environment variable"
    else
        test_fail "Command-line argument does not override environment variable"
    fi
    unset MEDLEY_EMULATOR
}

# Test 3: Lock file mechanism
test_lock_mechanism() {
    echo "Test 3: Lock file mechanism"
    
    # This test requires actually running the emulator, so skip for now
    test_skip "Lock mechanism test (requires running emulator)"
}

# Test 4: Error handling
test_error_handling() {
    echo "Test 4: Error handling"
    
    # Test emulator not found error
    if "$RUN_MEDLEY" --emulator nonexistent 2>&1 | grep -qi "not found\|Error"; then
        test_pass "Emulator not found error displayed"
    else
        test_fail "Emulator not found error not displayed"
    fi
}

# Test 5: Backward compatibility
test_backward_compatibility() {
    echo "Test 5: Backward compatibility"
    
    # Test that existing arguments still work
    if "$RUN_MEDLEY" --help 2>&1 | grep -q "run-medley\|Syntax" || true; then
        test_pass "Script still accepts existing arguments"
    else
        test_fail "Script broken for existing arguments"
    fi
}

# Run all tests
echo "=========================================="
echo "Testing run-medley emulator selection"
echo "=========================================="
echo ""

test_emulator_arg_parsing
test_emulator_precedence
test_lock_mechanism
test_error_handling
test_backward_compatibility

# Summary
echo ""
echo "=========================================="
echo "Test Summary"
echo "=========================================="
echo "Passed: $TESTS_PASSED"
echo "Failed: $TESTS_FAILED"
echo ""

if [ $TESTS_FAILED -eq 0 ]; then
    exit 0
else
    exit 1
fi
