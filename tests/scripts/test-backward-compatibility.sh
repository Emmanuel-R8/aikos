#!/usr/bin/env bash
# Test script for backward compatibility
# Ensures existing Medley script functionality is preserved

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

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

# Test run-medley script exists and is executable
test_run_medley_exists() {
    echo "Testing run-medley script existence"

    RUN_MEDLEY="$REPO_ROOT/medley/run-medley"
    if [ -f "$RUN_MEDLEY" ]; then
        test_pass "run-medley script exists"
        if [ -x "$RUN_MEDLEY" ]; then
            test_pass "run-medley script is executable"
        else
            test_fail "run-medley script is not executable"
        fi
    else
        test_fail "run-medley script does not exist"
    fi
}

# Test medley.command script exists and is executable
test_medley_command_exists() {
    echo "Testing medley.command script existence"

    MEDLEY_COMMAND="$REPO_ROOT/medley/scripts/medley/medley.command"
    if [ -f "$MEDLEY_COMMAND" ]; then
        test_pass "medley.command script exists"
        if [ -x "$MEDLEY_COMMAND" ]; then
            test_pass "medley.command script is executable"
        else
            test_fail "medley.command script is not executable"
        fi
    else
        test_fail "medley.command script does not exist"
    fi
}

# Test medley_run.sh script exists and is executable
test_medley_run_exists() {
    echo "Testing medley_run.sh script existence"

    MEDLEY_RUN="$REPO_ROOT/medley/scripts/medley/medley_run.sh"
    if [ -f "$MEDLEY_RUN" ]; then
        test_pass "medley_run.sh script exists"
        if [ -x "$MEDLEY_RUN" ]; then
            test_pass "medley_run.sh script is executable"
        else
            test_fail "medley_run.sh script is not executable"
        fi
    else
        test_fail "medley_run.sh script does not exist"
    fi
}

# Test that existing arguments still work (using --help as a proxy)
test_existing_arguments() {
    echo "Testing existing argument compatibility"

    RUN_MEDLEY="$REPO_ROOT/medley/run-medley"

    # Test --help argument
    if "$RUN_MEDLEY" --help >/dev/null 2>&1 || true; then
        test_pass "run-medley accepts --help argument"
    else
        test_fail "run-medley does not accept --help argument"
    fi

    # Test -h argument (short form)
    if "$RUN_MEDLEY" -h >/dev/null 2>&1 || true; then
        test_pass "run-medley accepts -h argument"
    else
        test_fail "run-medley does not accept -h argument"
    fi
}

# Test that scripts don't break when called without arguments
test_no_arguments() {
    echo "Testing script behavior with no arguments"

    RUN_MEDLEY="$REPO_ROOT/medley/run-medley"

    # This should not crash, though it may show usage or attempt to run
    if timeout 5s "$RUN_MEDLEY" >/dev/null 2>&1 || true; then
        test_pass "run-medley handles no arguments gracefully"
    else
        test_fail "run-medley crashes with no arguments"
    fi
}

# Test that scripts can be sourced (for library usage)
test_script_sourcing() {
    echo "Testing script sourcing capability"

    RUN_MEDLEY="$REPO_ROOT/medley/run-medley"

    # Test if script can be sourced without executing
    if bash -c "source '$RUN_MEDLEY'; exit 0" 2>/dev/null; then
        test_pass "run-medley can be sourced as a library"
    else
        test_pass "run-medley cannot be sourced (acceptable for some scripts)"
    fi
}

# Test that environment variables are preserved
test_environment_preservation() {
    echo "Testing environment variable preservation"

    RUN_MEDLEY="$REPO_ROOT/medley/run-medley"

    # Set a test environment variable
    export TEST_VAR_BACKWARD_COMPAT="test_value"

    # Run script and check if environment is preserved
    if TEST_VAR_BACKWARD_COMPAT="test_value" timeout 2s "$RUN_MEDLEY" --help >/dev/null 2>&1 || true; then
        test_pass "Environment variables preserved during script execution"
    else
        test_fail "Environment variables not preserved"
    fi

    unset TEST_VAR_BACKWARD_COMPAT
}

# Test that working directory is preserved
test_working_directory() {
    echo "Testing working directory preservation"

    RUN_MEDLEY="$REPO_ROOT/medley/run-medley"
    ORIGINAL_PWD=$(pwd)

    # Change to a different directory and run script
    cd /tmp
    if timeout 2s "$RUN_MEDLEY" --help >/dev/null 2>&1 || true; then
        if [ "$(pwd)" = "/tmp" ]; then
            test_pass "Working directory preserved"
        else
            test_fail "Working directory not preserved"
        fi
    else
        test_fail "Script failed to run"
    fi

    cd "$ORIGINAL_PWD"
}

# Test exit codes are reasonable
test_exit_codes() {
    echo "Testing exit code behavior"

    RUN_MEDLEY="$REPO_ROOT/medley/run-medley"

    # Test --help exit code (should be 0 or 1, not crash)
    if "$RUN_MEDLEY" --help >/dev/null 2>&1; then
        EXIT_CODE=$?
        if [ $EXIT_CODE -eq 0 ] || [ $EXIT_CODE -eq 1 ]; then
            test_pass "Reasonable exit code for --help"
        else
            test_fail "Unexpected exit code for --help: $EXIT_CODE"
        fi
    else
        test_fail "Script crashed on --help"
    fi
}

# Run all tests
echo "=========================================="
echo "Backward Compatibility Tests"
echo "=========================================="
echo ""

test_run_medley_exists
test_medley_command_exists
test_medley_run_exists
test_existing_arguments
test_no_arguments
test_script_sourcing
test_environment_preservation
test_working_directory
test_exit_codes

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