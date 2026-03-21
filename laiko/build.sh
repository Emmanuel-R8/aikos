#!/usr/bin/env bash
# Build script for Laiko Lisp implementation

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "Building Laiko Lisp implementation..."

# Check if SBCL is available
if ! command -v sbcl &> /dev/null; then
    echo "Error: SBCL not found. Please install SBCL."
    exit 1
fi

# Load and compile the system
sbcl --noinform --non-interactive \
     --load laiko.asd \
     --eval "(asdf:load-system :laiko)" \
     --eval "(quit)"

echo "Build complete!"