#!/usr/bin/env bash
# Run script for Laiko Lisp implementation

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Check if SBCL is available
if ! command -v sbcl &>/dev/null; then
	echo "Error: SBCL not found. Please install SBCL."
	exit 1
fi

# Use loader.lisp script to avoid argument parsing issues
sbcl --script loader.lisp "$@"
