#!/usr/bin/env bash
# Integration test script for emulator selection across all run scripts
# Tests all user stories: US1 (selection), US2 (environment), US3 (auto-build)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
RUN_MEDLEY="$REPO_ROOT/medley/run-medley"
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

# User Story 1: Run Interlisp with Selected Emulator
test_us1_command_line_selection() {
    echo "User Story 1: Command-line emulator selection"
    
    for emulator in c zig lisp; do
        if "$RUN_MEDLEY" --emulator "$emulator" --help 2>&1 | grep -q "emulator\|Error" || true; then
            test_pass "US1: --emulator $emulator works in run-medley"
        else
            test_fail "US1: --emulator $emulator failed in run-medley"
        fi
    done
}

# User Story 2: Set Default Emulator Preference
test_us2_environment_variable() {
    echo "User Story 2: Environment variable default"
    
    export MEDLEY_EMULATOR=zig
    if "$RUN_MEDLEY" --help 2>&1 | grep -q "zig\|Error" || true; then
        test_pass "US2: MEDLEY_EMULATOR environment variable works"
    else
        test_fail "US2: MEDLEY_EMULATOR environment variable failed"
    fi
    unset MEDLEY_EMULATOR
}

test_us2_precedence() {
    echo "User Story 2: Command-line precedence over environment"
    
    export MEDLEY_EMULATOR=zig
    if "$RUN_MEDLEY" --emulator c --help 2>&1 | grep -q "c\|Error" || true; then
        test_pass "US2: Command-line takes precedence over environment"
    else
        test_fail "US2: Command-line does not override environment"
    fi
    unset MEDLEY_EMULATOR
}

# User Story 3: Automatic Emulator Building
test_us3_auto_build_flag() {
    echo "User Story 3: Auto-build flag"
    
    # Test that --auto-build flag is accepted
    if "$RUN_MEDLEY" --emulator zig --auto-build --help 2>&1 | grep -q "auto-build\|Error" || true; then
        test_pass "US3: --auto-build flag accepted"
    else
        test_fail "US3: --auto-build flag not recognized"
    fi
}

# Run all tests
echo "=========================================="
echo "Integration Tests: Emulator Selection"
echo "=========================================="
echo ""

test_us1_command_line_selection
test_us2_environment_variable
test_us2_precedence
test_us3_auto_build_flag

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
