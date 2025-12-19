#!/bin/bash
# Run script for Maiko Lisp implementation

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Check if SBCL is available
if ! command -v sbcl &> /dev/null; then
    echo "Error: SBCL not found. Please install SBCL."
    exit 1
fi

# Load system and run main
sbcl --noinform \
     --load maiko-lisp.asd \
     --eval "(asdf:load-system :maiko-lisp)" \
     --eval "(maiko-lisp:main)" \
     -- "$@"
