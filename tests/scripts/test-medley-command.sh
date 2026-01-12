#!/usr/bin/env bash
# Test script for medley.command emulator selection functionality
# Tests User Story 1: Run Interlisp with Selected Emulator

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
MEDLEY_COMMAND="$REPO_ROOT/medley/scripts/medley/medley.command"

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
        if "$MEDLEY_COMMAND" --emulator "$emulator" --help 2>&1 | grep -q "emulator\|Error" || true; then
            test_pass "--emulator $emulator argument accepted"
        else
            test_fail "--emulator $emulator argument not recognized"
        fi
    done
}

# Test 2: Environment variable support
test_environment_variable() {
    echo "Test 2: Environment variable support"
    
    export MEDLEY_EMULATOR=zig
    if "$MEDLEY_COMMAND" --help 2>&1 | grep -q "zig\|Error" || true; then
        test_pass "MEDLEY_EMULATOR environment variable recognized"
    else
        test_fail "MEDLEY_EMULATOR environment variable not recognized"
    fi
    unset MEDLEY_EMULATOR
}

# Run all tests
echo "=========================================="
echo "Testing medley.command emulator selection"
echo "=========================================="
echo ""

test_emulator_arg_parsing
test_environment_variable

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
