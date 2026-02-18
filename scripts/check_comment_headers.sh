#!/bin/bash

SRC_DIR="/home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/maiko/src"
INC_DIR="/home/emmanuel/Sync/Development/Emulation/_gits/Interlisp/maiko/inc"

echo "Checking for files with minimal or missing comprehensive headers..."
echo "--------------------------------------------------------------"

# Check C source files
for file in $SRC_DIR/*.c; do
  # Check if file contains our comprehensive header pattern
  if ! grep -q "Comprehensive C file header" "$file"; then
    echo "File may need header update: $file"
    # Show first 10 lines for quick check
    head -10 "$file"
    echo "---"
  fi
done

echo "--------------------------------------------------------------"
echo "Checking header files..."
echo "--------------------------------------------------------------"

# Check header files
for file in $INC_DIR/*.h; do
  if ! grep -q "Comprehensive C file header" "$file"; then
    echo "File may need header update: $file"
    head -10 "$file"
    echo "---"
  fi
done

echo "--------------------------------------------------------------"
echo "Header check complete!"
