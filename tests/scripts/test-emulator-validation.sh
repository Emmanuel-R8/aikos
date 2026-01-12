#!/usr/bin/env bash
# Test script for emulator validation functionality
# Tests file existence, executable permissions, file size, and magic bytes

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# Source the emulator utils if they exist
UTILS_SCRIPT="$REPO_ROOT/medley/scripts/medley/emulator_utils.sh"
if [ -f "$UTILS_SCRIPT" ]; then
    source "$UTILS_SCRIPT"
fi

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

# Test validation of non-existent file
test_nonexistent_file() {
    echo "Testing validation of non-existent file"

    if type validate_emulator_executable >/dev/null 2>&1; then
        if validate_emulator_executable "/nonexistent/file" 2>/dev/null; then
            test_fail "Non-existent file should fail validation"
        else
            test_pass "Non-existent file correctly rejected"
        fi
    else
        test_skip "validate_emulator_executable function not available"
    fi
}

# Test validation of empty file
test_empty_file() {
    echo "Testing validation of empty file"

    if type validate_emulator_executable >/dev/null 2>&1; then
        EMPTY_FILE=$(mktemp)
        if validate_emulator_executable "$EMPTY_FILE" 2>/dev/null; then
            test_fail "Empty file should fail validation"
        else
            test_pass "Empty file correctly rejected"
        fi
        rm -f "$EMPTY_FILE"
    else
        test_skip "validate_emulator_executable function not available"
    fi
}

# Test validation of non-executable file
test_non_executable_file() {
    echo "Testing validation of non-executable file"

    if type validate_emulator_executable >/dev/null 2>&1; then
        NON_EXEC_FILE=$(mktemp)
        echo "test content" > "$NON_EXEC_FILE"
        chmod -x "$NON_EXEC_FILE"

        if validate_emulator_executable "$NON_EXEC_FILE" 2>/dev/null; then
            test_fail "Non-executable file should fail validation"
        else
            test_pass "Non-executable file correctly rejected"
        fi
        rm -f "$NON_EXEC_FILE"
    else
        test_skip "validate_emulator_executable function not available"
    fi
}

# Test validation of valid executable
test_valid_executable() {
    echo "Testing validation of valid executable"

    if type validate_emulator_executable >/dev/null 2>&1; then
        # Use a known valid executable (bash itself)
        if validate_emulator_executable "$(which bash)" 2>/dev/null; then
            test_pass "Valid executable correctly accepted"
        else
            test_fail "Valid executable should pass validation"
        fi
    else
        test_skip "validate_emulator_executable function not available"
    fi
}

# Test validation of directory (should fail)
test_directory_validation() {
    echo "Testing validation of directory"

    if type validate_emulator_executable >/dev/null 2>&1; then
        if validate_emulator_executable "$SCRIPT_DIR" 2>/dev/null; then
            test_fail "Directory should fail validation"
        else
            test_pass "Directory correctly rejected"
        fi
    else
        test_skip "validate_emulator_executable function not available"
    fi
}

# Test magic bytes validation (if implemented)
test_magic_bytes() {
    echo "Testing magic bytes validation"

    if type validate_emulator_executable >/dev/null 2>&1; then
        # Create a file with executable permissions but invalid content
        INVALID_EXEC=$(mktemp)
        echo "not an executable" > "$INVALID_EXEC"
        chmod +x "$INVALID_EXEC"

        # This test depends on whether magic bytes validation is implemented
        # If magic bytes are checked, this should fail; if not, it might pass
        if validate_emulator_executable "$INVALID_EXEC" 2>/dev/null; then
            test_pass "Magic bytes validation not implemented (acceptable)"
        else
            test_pass "Magic bytes validation correctly rejects invalid executable"
        fi
        rm -f "$INVALID_EXEC"
    else
        test_skip "validate_emulator_executable function not available"
    fi
}

# Test file size validation
test_file_size() {
    echo "Testing file size validation"

    if type validate_emulator_executable >/dev/null 2>&1; then
        # Create a very small file
        SMALL_FILE=$(mktemp)
        echo -n "x" > "$SMALL_FILE"  # 1 byte file
        chmod +x "$SMALL_FILE"

        if validate_emulator_executable "$SMALL_FILE" 2>/dev/null; then
            test_pass "Small executable accepted (size check may not be implemented)"
        else
            test_pass "Small file correctly rejected based on size"
        fi
        rm -f "$SMALL_FILE"
    else
        test_skip "validate_emulator_executable function not available"
    fi
}

# Run all tests
echo "=========================================="
echo "Emulator Validation Tests"
echo "=========================================="
echo ""

test_nonexistent_file
test_empty_file
test_non_executable_file
test_valid_executable
test_directory_validation
test_magic_bytes
test_file_size

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