#!/usr/bin/env bash
# Laiko Parity Test Script
# Compares Laiko Common Lisp implementation with C reference

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SYSOUT="${SCRIPT_DIR}/../medley/internal/loadups/starter.sysout"
LDE="${SCRIPT_DIR}/../maiko/build/c/linux.x86_64/lde"
TRACE_C="c_emulator_unified_trace.txt"
TRACE_LISPK="lai ko_unified_trace.txt"
MAX_STEPS=100

echo "=== Laiko Parity Test ==="
echo "Sysout: ${SYSOUT}"
echo "C Emulator: ${LDE}"

if [ ! -f "${SYSOUT}" ]; then
	echo "ERROR: Sysout file not found: ${SYSOUT}"
	exit 1
fi

if [ ! -x "${LDE}" ]; then
	echo "ERROR: C emulator not found or not executable: ${LDE}"
	exit 1
fi

echo ""
echo "Running C emulator with max ${MAX_STEPS} steps..."
cd "$(dirname "${LDE}")"
timeout 5 ./lde "${SYSOUT}" >/dev/null 2>&1 || true
if [ -f "${TRACE_C}" ]; then
	echo "C trace: $(wc -l <"${TRACE_C}") lines"
else
	echo "ERROR: C trace not generated"
	exit 1
fi

echo ""
echo "Running Laiko Common Lisp implementation..."
cd "${SCRIPT_DIR}"
if [ -f "tests/run-parity.lisp" ]; then
	sbcl --non-interactive \
		--eval "(require :asdf)" \
		--eval "(load \"maiko-lisp.asd\")" \
		--eval "(asdf:load-system :maiko-lisp)" \
		--eval "(setf maiko-lisp.vm:*max-execution-steps* ${MAX_STEPS})" \
		--eval "(setf maiko-lisp.vm:*trace-enabled* t)" \
		--eval "(setf maiko-lisp.vm:*trace-file* \"${TRACE_LAIKO}\")" \
		--eval "(load \"tests/run-parity.lisp\")" \
		--eval "(maiko-lisp-tests::run-parity-test)" \
		--eval "(exit)" 2>&1 || true

	if [ -f "${TRACE_LAIKO}" ]; then
		echo "Laiko trace: $(wc -l <"${TRACE_LAIKO}") lines"
	fi
else
	echo "Parity test file not found: tests/run-parity.lisp"
fi

echo ""
echo "=== Parity Comparison ==="
if [ -f "${TRACE_C}" ] && [ -f "${TRACE_LAIKO}" ]; then
	echo "Comparing traces..."

	C_OPS=$(cut -d'|' -f4 "${TRACE_C}" | sort | uniq -c | sort -rn | head -10)
	echo "Top C operations:"
	echo "${C_OPS}"

	echo ""
	echo "First 10 lines of C trace:"
	head -10 "${TRACE_C}"

	echo ""
	echo "First 10 lines of Laiko trace:"
	head -10 "${TRACE_LAIKO}" 2>/dev/null || echo "(not available)"
fi

echo ""
echo "=== Test Complete ==="
