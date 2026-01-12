#!/usr/bin/env bash
# Test script for lock mechanism functionality
# Tests lock file creation, stale lock detection, and cleanup

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

# Test lock file creation
test_lock_creation() {
    echo "Testing lock file creation"

    if type acquire_lock >/dev/null 2>&1; then
        # Test successful lock acquisition
        if acquire_lock 2>/dev/null; then
            test_pass "Lock file created successfully"
        else
            test_fail "Failed to create lock file"
        fi

        # Test lock file exists
        LOCK_FILE="${HOME}/.medley/medley.lock"
        if [ -f "$LOCK_FILE" ]; then
            test_pass "Lock file exists at expected location"
        else
            test_fail "Lock file not found at expected location"
        fi

        # Test lock file contains PID
        if grep -q "^[0-9]\+$" "$LOCK_FILE" 2>/dev/null; then
            test_pass "Lock file contains valid PID"
        else
            test_fail "Lock file does not contain valid PID"
        fi

        # Clean up
        release_lock 2>/dev/null || true
    else
        test_skip "acquire_lock function not available"
    fi
}

# Test concurrent lock prevention
test_concurrent_lock_prevention() {
    echo "Testing concurrent lock prevention"

    if type acquire_lock >/dev/null 2>&1; then
        # Acquire first lock
        if acquire_lock 2>/dev/null; then
            test_pass "First lock acquired successfully"

            # Try to acquire second lock (should fail)
            if acquire_lock 2>/dev/null; then
                test_fail "Second lock acquisition should have failed"
                release_lock 2>/dev/null || true
            else
                test_pass "Second lock acquisition correctly prevented"
            fi

            # Clean up
            release_lock 2>/dev/null || true
        else
            test_fail "Could not acquire initial lock for concurrent test"
        fi
    else
        test_skip "acquire_lock function not available"
    fi
}

# Test lock cleanup on exit
test_lock_cleanup() {
    echo "Testing lock cleanup on exit"

    if type acquire_lock >/dev/null 2>&1 && type release_lock >/dev/null 2>&1; then
        # Create a test script that acquires lock and exits
        TEST_SCRIPT=$(mktemp)
        cat > "$TEST_SCRIPT" << 'EOF'
#!/bin/bash
source "'"${UTILS_SCRIPT}"'" 2>/dev/null || true
if command -v acquire_lock >/dev/null 2>&1; then
    acquire_lock
    # Simulate normal exit
    exit 0
else
    exit 1
fi
EOF
        chmod +x "$TEST_SCRIPT"

        # Run the test script
        if "$TEST_SCRIPT"; then
            # Check that lock file was cleaned up
            LOCK_FILE="${HOME}/.medley/medley.lock"
            if [ ! -f "$LOCK_FILE" ]; then
                test_pass "Lock file cleaned up after normal exit"
            else
                test_fail "Lock file not cleaned up after normal exit"
                rm -f "$LOCK_FILE"
            fi
        else
            test_skip "Test script failed to run"
        fi

        rm -f "$TEST_SCRIPT"
    else
        test_skip "Lock functions not available"
    fi
}

# Test stale lock detection
test_stale_lock_detection() {
    echo "Testing stale lock detection"

    if type acquire_lock >/dev/null 2>&1; then
        LOCK_FILE="${HOME}/.medley/medley.lock"

        # Create a fake stale lock file with non-existent PID
        mkdir -p "$(dirname "$LOCK_FILE")"
        echo "999999" > "$LOCK_FILE"  # Very high PID that shouldn't exist
        echo "$(date +%s)" >> "$LOCK_FILE"  # Old timestamp

        # Try to acquire lock (should succeed due to stale lock)
        if acquire_lock 2>/dev/null; then
            test_pass "Stale lock correctly detected and replaced"
            release_lock 2>/dev/null || true
        else
            test_fail "Stale lock not detected"
            rm -f "$LOCK_FILE"
        fi
    else
        test_skip "acquire_lock function not available"
    fi
}

# Run all tests
echo "=========================================="
echo "Lock Mechanism Tests"
echo "=========================================="
echo ""

test_lock_creation
test_concurrent_lock_prevention
test_lock_cleanup
test_stale_lock_detection

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