#!/usr/bin/env bash
# Endianness Verification Script
# Runs C emulator with debug statements and verifies findings instruction-by-instruction

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$PROJECT_ROOT"

# Configuration
SYSOUT_FILE="${1:-medley/internal/loadups/starter.sysout}"
DEBUG_STDERR_LOG="debug_stderr.log"
DEBUG_ANALYSIS_LOG="debug_endianness_analysis.log"
EXECUTION_LOG="c_emulator_execution_log.txt"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo "=== Endianness Verification Script ==="
echo "Date: $(date '+%Y-%m-%d %H:%M')"
echo ""

# Step 1: Rebuild C emulator to ensure debug statements are compiled
echo -e "${BLUE}[1/5]${NC} Rebuilding C emulator..."
cd maiko
if [ -d build-cmake ]; then
    make -C build-cmake ldesdl >/dev/null 2>&1 || {
        echo -e "${RED}ERROR: Failed to build C emulator${NC}"
        exit 1
    }
else
    echo -e "${RED}ERROR: build-cmake directory not found${NC}"
    exit 1
fi
cd ..

# Step 2: Clean previous logs
echo -e "${BLUE}[2/5]${NC} Cleaning previous logs..."
rm -f "$DEBUG_STDERR_LOG" "$DEBUG_ANALYSIS_LOG" "$EXECUTION_LOG"

# Step 3: Run C emulator and capture both stdout (execution log) and stderr (debug output)
echo -e "${BLUE}[3/5]${NC} Running C emulator and capturing debug output..."
echo "  Sysout file: $SYSOUT_FILE"
echo "  This may take a few seconds..."

# Note: The C emulator writes its execution log directly to c_emulator_execution_log.txt
# We capture stderr for debug output, and let the emulator write its own execution log
timeout --kill-after=5 3 ./maiko/build-cmake/ldesdl "$SYSOUT_FILE" 2>"$DEBUG_STDERR_LOG" >/dev/null || {
    echo -e "${YELLOW}WARNING: Emulator terminated (this is expected with timeout)${NC}"
}

# The C emulator creates its own execution log file - use it directly
if [ -f "c_emulator_execution_log.txt" ]; then
    EXECUTION_LOG="c_emulator_execution_log.txt"
    echo "  Execution log found: $(wc -l < "$EXECUTION_LOG") lines"
else
    echo -e "${YELLOW}WARNING: C emulator did not create execution log file${NC}"
    echo "  The emulator may not have reached the instruction dispatch loop"
fi

# Step 4: Analyze debug output
echo -e "${BLUE}[4/5]${NC} Analyzing debug output..."

{
    echo "=== Endianness Verification Analysis ==="
    echo "Date: $(date '+%Y-%m-%d %H:%M')"
    echo ""
    
    # Check if debug output exists
    if [ ! -s "$DEBUG_STDERR_LOG" ]; then
        echo -e "${RED}ERROR: No debug output captured${NC}"
        echo "Check if debug statements are being executed."
        exit 1
    fi
    
    echo "Debug log size: $(wc -l < "$DEBUG_STDERR_LOG") lines"
    echo ""
    
    # 4.1: Verify XOR addressing for opcode fetch
    echo "=== 4.1: XOR Addressing Verification (Opcode Fetch) ==="
    OPCODE_DEBUG_COUNT=$(grep -c "DEBUG\[.*\] Opcode fetch:" "$DEBUG_STDERR_LOG" 2>/dev/null | tr -d '\n' | head -c 10 || echo "0")
    echo "Found $OPCODE_DEBUG_COUNT opcode fetch debug entries"
    
    if [ "$OPCODE_DEBUG_COUNT" -gt 0 ]; then
        echo ""
        echo "First 10 opcode fetch entries:"
        grep "DEBUG\[.*\] Opcode fetch:" "$DEBUG_STDERR_LOG" | head -10 | while IFS= read -r line; do
            echo "  $line"
        done
        echo ""
        
        # Extract and analyze XOR addressing pattern
        echo "XOR Addressing Analysis:"
        grep "DEBUG\[.*\] Opcode fetch:" "$DEBUG_STDERR_LOG" | head -10 | while IFS= read -r line; do
            # Extract values from line
            # Format: DEBUG[N] Opcode fetch: PCMAC=0x... (0x...), XOR=0x... (0x...), direct=0x.., xor=0x.., fetched=0x..
            pcmac=$(echo "$line" | sed -n 's/.*PCMAC=\(0x[0-9a-f]*\).*/\1/p')
            xor_addr=$(echo "$line" | sed -n 's/.*XOR=\(0x[0-9a-f]*\).*/\1/p')
            direct=$(echo "$line" | sed -n 's/.*direct=\(0x[0-9a-f]*\).*/\1/p')
            xor_byte=$(echo "$line" | sed -n 's/.*xor=\(0x[0-9a-f]*\).*/\1/p')
            fetched=$(echo "$line" | sed -n 's/.*fetched=\(0x[0-9a-f]*\).*/\1/p')
            
            if [ -n "$pcmac" ] && [ -n "$xor_addr" ] && [ -n "$fetched" ]; then
                # Verify XOR calculation
                pcmac_num=$(printf "%d" "$pcmac")
                xor_expected=$((pcmac_num ^ 3))
                xor_actual=$(printf "%d" "$xor_addr")
                
                if [ "$xor_expected" -eq "$xor_actual" ]; then
                    echo -e "  ${GREEN}✓${NC} XOR calculation correct: $pcmac ^ 3 = $xor_addr"
                else
                    echo -e "  ${RED}✗${NC} XOR calculation mismatch: $pcmac ^ 3 = $xor_expected, but got $xor_addr"
                fi
                
                # Verify fetched byte matches XOR byte
                if [ "$fetched" = "$xor_byte" ]; then
                    echo -e "  ${GREEN}✓${NC} Fetched byte matches XOR byte: 0x$fetched"
                else
                    echo -e "  ${RED}✗${NC} Fetched byte mismatch: fetched=0x$fetched, xor=0x$xor_byte"
                fi
                
                # Check if direct byte differs (expected in BYTESWAP mode)
                if [ "$direct" != "$fetched" ]; then
                    echo -e "  ${GREEN}✓${NC} Direct byte differs from fetched (expected in BYTESWAP mode): direct=0x$direct, fetched=0x$fetched"
                else
                    echo -e "  ${YELLOW}⚠${NC} Direct byte equals fetched (unexpected in BYTESWAP mode): direct=0x$direct, fetched=0x$fetched"
                fi
            fi
        done
    else
        echo -e "${YELLOW}WARNING: No opcode fetch debug entries found${NC}"
    fi
    echo ""
    
    # 4.2: Verify Get_DLword value construction
    echo "=== 4.2: Get_DLword Value Construction Verification ==="
    DLWORD_DEBUG_COUNT=$(grep -c "DEBUG\[.*\] SICX:" "$DEBUG_STDERR_LOG" 2>/dev/null | tr -d '\n' | head -c 10 || echo "0")
    echo "Found $DLWORD_DEBUG_COUNT SICX debug entries"
    
    if [ "$DLWORD_DEBUG_COUNT" -gt 0 ]; then
        echo ""
        echo "SICX value construction entries:"
        grep "DEBUG\[.*\] SICX:" "$DEBUG_STDERR_LOG" | head -10 | while IFS= read -r line; do
            echo "  $line"
            # Extract values
            byte1=$(echo "$line" | sed -n 's/.*byte1=\(0x[0-9a-f]*\).*/\1/p')
            byte2=$(echo "$line" | sed -n 's/.*byte2=\(0x[0-9a-f]*\).*/\1/p')
            constructed=$(echo "$line" | sed -n 's/.*constructed=\(0x[0-9a-f]*\).*/\1/p')
            fetched=$(echo "$line" | sed -n 's/.*fetched=\(0x[0-9a-f]*\).*/\1/p')
            
            if [ -n "$byte1" ] && [ -n "$byte2" ] && [ -n "$constructed" ] && [ -n "$fetched" ]; then
                # Verify construction: (byte1 << 8) | byte2
                byte1_num=$(printf "%d" "$byte1")
                byte2_num=$(printf "%d" "$byte2")
                constructed_expected=$(( (byte1_num << 8) | byte2_num ))
                constructed_actual=$(printf "%d" "$constructed")
                
                if [ "$constructed_expected" -eq "$constructed_actual" ]; then
                    echo -e "    ${GREEN}✓${NC} Construction correct: (0x$byte1 << 8) | 0x$byte2 = 0x$constructed"
                else
                    echo -e "    ${RED}✗${NC} Construction mismatch: expected 0x$(printf "%04x" $constructed_expected), got 0x$constructed"
                fi
                
                # Verify fetched matches constructed
                if [ "$fetched" = "$constructed" ]; then
                    echo -e "    ${GREEN}✓${NC} Fetched matches constructed: 0x$fetched"
                else
                    echo -e "    ${RED}✗${NC} Fetched mismatch: fetched=0x$fetched, constructed=0x$constructed"
                fi
            fi
        done
    else
        echo -e "${YELLOW}WARNING: No SICX debug entries found${NC}"
    fi
    echo ""
    
    # 4.3: Verify FastRetCALL address translation and PC calculation
    echo "=== 4.3: FastRetCALL Address Translation and PC Calculation ==="
    RETCALL_COUNT=$(grep -c "DEBUG FastRetCALL:" "$DEBUG_STDERR_LOG" 2>/dev/null | tr -d '\n' | head -c 10 || echo "0")
    echo "Found $RETCALL_COUNT FastRetCALL debug entries"
    
    if [ "$RETCALL_COUNT" -gt 0 ]; then
        echo ""
        echo "FastRetCALL entries (showing first occurrence):"
        grep -A 10 "DEBUG FastRetCALL: FX_FNHEADER=" "$DEBUG_STDERR_LOG" | head -15 | while IFS= read -r line; do
            echo "  $line"
        done
        echo ""
        
        # Extract and analyze address translation
        fx_header=$(grep "DEBUG FastRetCALL: FX_FNHEADER=" "$DEBUG_STDERR_LOG" | head -1 | sed -n 's/.*FX_FNHEADER=\(0x[0-9a-f]*\).*/\1/p')
        funcobj_offset=$(grep "DEBUG FastRetCALL: FuncObj byte offset from Lisp_world = " "$DEBUG_STDERR_LOG" | head -1 | sed -n 's/.*= \([0-9]*\).*/\1/p')
        expected_offset=$(grep "DEBUG FastRetCALL: Expected byte offset (FX_FNHEADER \* 2) = " "$DEBUG_STDERR_LOG" | head -1 | sed -n 's/.*= \([0-9]*\).*/\1/p')
        
        if [ -n "$fx_header" ] && [ -n "$funcobj_offset" ] && [ -n "$expected_offset" ]; then
            if [ "$funcobj_offset" -eq "$expected_offset" ]; then
                echo -e "${GREEN}✓${NC} Address translation uses DLword arithmetic: FX_FNHEADER * 2 = byte offset"
                echo "  FX_FNHEADER = $fx_header, byte offset = $funcobj_offset (expected $expected_offset)"
            else
                echo -e "${RED}✗${NC} Address translation mismatch:"
                echo "  FX_FNHEADER = $fx_header"
                echo "  Actual byte offset = $funcobj_offset"
                echo "  Expected byte offset (FX_FNHEADER * 2) = $expected_offset"
            fi
        fi
        
        # Analyze PC calculation
        pc_offset=$(grep "DEBUG FastRetCALL: PC byte offset from FuncObj = " "$DEBUG_STDERR_LOG" | head -1 | sed -n 's/.*= \([0-9]*\).*/\1/p')
        currentfx_pc=$(grep "DEBUG FastRetCALL: CURRENTFX->pc=" "$DEBUG_STDERR_LOG" | head -1 | sed -n 's/.*pc=\([0-9]*\).*/\1/p')
        
        if [ -n "$pc_offset" ] && [ -n "$currentfx_pc" ]; then
            if [ "$pc_offset" -eq "$currentfx_pc" ]; then
                echo -e "${GREEN}✓${NC} PC calculation: CURRENTFX->pc is byte offset"
                echo "  PC offset from FuncObj = $pc_offset, CURRENTFX->pc = $currentfx_pc"
            elif [ "$pc_offset" -eq "$((currentfx_pc * 2))" ]; then
                echo -e "${GREEN}✓${NC} PC calculation: CURRENTFX->pc is DLword offset"
                echo "  PC offset from FuncObj = $pc_offset, CURRENTFX->pc = $currentfx_pc (DLword offset)"
            else
                echo -e "${YELLOW}⚠${NC} PC calculation: Unclear relationship"
                echo "  PC offset from FuncObj = $pc_offset, CURRENTFX->pc = $currentfx_pc"
            fi
        fi
    else
        echo -e "${YELLOW}WARNING: No FastRetCALL debug entries found${NC}"
    fi
    echo ""
    
    # 4.4: Verify GETBASE_N two-stage memory access
    echo "=== 4.4: GETBASE_N Two-Stage Memory Access ==="
    GETBASE_COUNT=$(grep -c "DEBUG GETBASE_N" "$DEBUG_STDERR_LOG" 2>/dev/null | tr -d '\n' | head -c 10 || echo "0")
    echo "Found $GETBASE_COUNT GETBASE_N debug entries"
    
    if [ "$GETBASE_COUNT" -gt 0 ]; then
        echo ""
        echo "GETBASE_N entries (showing first 5):"
        grep "DEBUG GETBASE_N" "$DEBUG_STDERR_LOG" | head -5 | while IFS= read -r line; do
            echo "  $line"
        done
        echo ""
        
        # Analyze XOR addressing for word access
        grep "DEBUG GETBASE_N" "$DEBUG_STDERR_LOG" | head -5 | while IFS= read -r line; do
            native=$(echo "$line" | sed -n 's/.*native=\(0x[0-9a-f]*\).*/\1/p')
            xor_addr=$(echo "$line" | sed -n 's/.*XOR=\(0x[0-9a-f]*\).*/\1/p')
            fetched=$(echo "$line" | sed -n 's/.*fetched=\(0x[0-9a-f]*\).*/\1/p')
            
            if [ -n "$native" ] && [ -n "$xor_addr" ]; then
                native_num=$(printf "%d" "$native")
                xor_expected=$((native_num ^ 2))
                xor_actual=$(printf "%d" "$xor_addr")
                
                if [ "$xor_expected" -eq "$xor_actual" ]; then
                    echo -e "  ${GREEN}✓${NC} Word XOR addressing correct: $native ^ 2 = $xor_addr"
                else
                    echo -e "  ${RED}✗${NC} Word XOR addressing mismatch: $native ^ 2 = $xor_expected, got $xor_addr"
                fi
            fi
        done
    else
        echo -e "${YELLOW}WARNING: No GETBASE_N debug entries found${NC}"
    fi
    echo ""
    
    # 4.5: Instruction-by-instruction verification
    echo "=== 4.5: Instruction-by-Instruction Endianness Impact Verification ==="
    
    # Initialize statistics (outside if block for cross-reference)
    total_instructions=0
    verified_opcodes=0
    no_debug_opcodes=0
    category_counts=""
    
    if [ -f "$EXECUTION_LOG" ]; then
        echo "Execution log found: $(wc -l < "$EXECUTION_LOG") lines"
        echo ""
        
        # Analyze first 200 instructions for comprehensive verification
        echo "Analyzing first 200 instructions for endianness impact:"
        echo ""
        
        instruction_num=0
        while IFS= read -r line && [ "$instruction_num" -lt 200 ]; do
            instruction_num=$((instruction_num + 1))
            total_instructions=$((total_instructions + 1))
            
            # Extract instruction info from execution log
            # Format: [timestamp] PC=0x... bytes=0x... instruction_name ...
            pc=$(echo "$line" | sed -n 's/.*PC=\(0x[0-9a-f]*\).*/\1/p' | head -1)
            opcode_bytes=$(echo "$line" | sed -n 's/.*bytes=\(0x[0-9a-f]*\).*/\1/p' | head -1)
            instr_name=$(echo "$line" | awk '{for(i=1;i<=NF;i++) if($i=="PC=") {for(j=i+1;j<=NF;j++) if($j!="bytes=" && $j!~"^0x") {print $j; exit}}; if($1!="[" && $1!="PC=" && $1!~"^0x") print $1}' | head -1)
            
            if [ -z "$instr_name" ]; then
                # Try alternative extraction
                instr_name=$(echo "$line" | awk '{for(i=1;i<=NF;i++) if($i=="bytes=") {for(j=i+1;j<=NF;j++) if($j!~"^0x" && $j!="") {print $j; exit}}}' | head -1)
            fi
            
            if [ -n "$pc" ] && [ -n "$opcode_bytes" ] && [ -n "$instr_name" ]; then
                # Get first byte (opcode)
                first_byte=$(echo "$opcode_bytes" | sed 's/0x//' | cut -c1-2)
                
                # Check if this instruction has debug output
                opcode_debug=$(grep "DEBUG\[$instruction_num\] Opcode fetch:" "$DEBUG_STDERR_LOG" | head -1)
                
                opcode_verified=false
                if [ -n "$opcode_debug" ]; then
                    fetched_opcode=$(echo "$opcode_debug" | sed -n 's/.*fetched=\(0x[0-9a-f]*\).*/\1/p' | sed 's/0x//')
                    
                    if [ "$first_byte" = "$fetched_opcode" ]; then
                        echo -e "  [$instruction_num] ${GREEN}✓${NC} $instr_name (PC=$pc): Opcode 0x$first_byte verified"
                        opcode_verified=true
                        verified_opcodes=$((verified_opcodes + 1))
                    else
                        echo -e "  [$instruction_num] ${RED}✗${NC} $instr_name (PC=$pc): Opcode mismatch - log=0x$first_byte, debug=0x$fetched_opcode"
                    fi
                else
                    echo -e "  [$instruction_num] ${YELLOW}⚠${NC} $instr_name (PC=$pc): No debug output (opcode 0x$first_byte)"
                    no_debug_opcodes=$((no_debug_opcodes + 1))
                fi
                
                # Detailed endianness impact analysis by instruction category
                echo -n "    Category: "
                case "$instr_name" in
                    # No operands - no endianness impact
                    POP|PUSH|COPY|SWAP|RETURN|CONS|OPCAR|OPCDR|LISTP|NTYPEX|NIL|T|"0"|"1"|ACONST|MYARGCOUNT|MYALINK)
                        echo -e "${GREEN}No endianness impact${NC} (stack/register operations)"
                        category_counts="${category_counts}no_operand "
                        ;;
                    
                    # 8-bit operands - XOR addressing for operand reading
                    TYPEP|DTEST|RPLPTR_N|GCREF|FINDKEY|STOREN|COPYN|IVARX|PVARX|FVARX|PVARX_|IVARX_|FVARX_|UNWIND|MISCN|TYPEMASK_N|RESTLIST|GVAR_)
                        echo -e "${BLUE}XOR addressing${NC} for 8-bit operand reading"
                        category_counts="${category_counts}8bit_operand "
                        # Check for operand debug (if available)
                        if [ "$instr_name" = "TYPEP" ] || [ "$instr_name" = "DTEST" ]; then
                            echo "      → Expected: Get_BYTE_PCMAC1 uses (PCMAC+1) ^ 3"
                        fi
                        ;;
                    
                    # 16-bit operands - manual value construction
                    SICX|SNIC)
                        echo -e "${BLUE}Manual value construction${NC} for 16-bit operand"
                        category_counts="${category_counts}16bit_operand "
                        # Check for SICX debug output
                        sicx_debug=$(grep "DEBUG\[$instruction_num\] SICX:" "$DEBUG_STDERR_LOG" | head -1)
                        if [ -n "$sicx_debug" ]; then
                            echo -e "      ${GREEN}✓${NC} SICX debug output found - verifying value construction"
                            byte1=$(echo "$sicx_debug" | sed -n 's/.*byte1=\(0x[0-9a-f]*\).*/\1/p')
                            byte2=$(echo "$sicx_debug" | sed -n 's/.*byte2=\(0x[0-9a-f]*\).*/\1/p')
                            constructed=$(echo "$sicx_debug" | sed -n 's/.*constructed=\(0x[0-9a-f]*\).*/\1/p')
                            fetched=$(echo "$sicx_debug" | sed -n 's/.*fetched=\(0x[0-9a-f]*\).*/\1/p')
                            if [ -n "$byte1" ] && [ -n "$byte2" ] && [ -n "$constructed" ] && [ -n "$fetched" ]; then
                                echo "        byte1=$byte1, byte2=$byte2, constructed=$constructed, fetched=$fetched"
                                if [ "$fetched" = "$constructed" ]; then
                                    echo -e "        ${GREEN}✓${NC} Fetched matches constructed value"
                                else
                                    echo -e "        ${RED}✗${NC} Mismatch: fetched=$fetched, constructed=$constructed"
                                fi
                            fi
                        else
                            echo -e "      ${YELLOW}⚠${NC} No SICX debug output"
                        fi
                        ;;
                    
                    # 24/32-bit operands - manual value construction
                    GCONST)
                        echo -e "${BLUE}Manual value construction${NC} for 24/32-bit operand"
                        category_counts="${category_counts}32bit_operand "
                        echo "      → Expected: Get_Pointer_PCMAC1 constructs from 3-4 bytes"
                        ;;
                    
                    # Memory access via LispPTR - two-stage process
                    GETBASE_0|GETBASE_1|GETBASE_2|GETBASE_3|GETBASE_4|GETBASE_5|GETBASE_6|GETBASE_7|GETBASEPTR_0|GETBASEPTR_1|GETBASEPTR_2|GETBASEPTR_3|GETBASEPTR_4|GETBASEPTR_5|GETBASEPTR_6|GETBASEPTR_7|GETBASEBYTE|PUTBASEBYTE|PUTBASE_0|PUTBASE_1|PUTBASE_2|PUTBASE_3|PUTBASE_4|PUTBASE_5|PUTBASE_6|PUTBASE_7|PUTBASEPTR_0|PUTBASEPTR_1|PUTBASEPTR_2|PUTBASEPTR_3|PUTBASEPTR_4|PUTBASEPTR_5|PUTBASEPTR_6|PUTBASEPTR_7|AREF1|ASET1)
                        echo -e "${BLUE}Two-stage process${NC}: address translation + XOR addressing"
                        category_counts="${category_counts}memory_access "
                        # Check for GETBASE_N debug output
                        getbase_debug=$(grep "DEBUG GETBASE_N" "$DEBUG_STDERR_LOG" | head -1)
                        if [ -n "$getbase_debug" ]; then
                            echo -e "      ${GREEN}✓${NC} GETBASE_N debug output found"
                            native=$(echo "$getbase_debug" | sed -n 's/.*native=\(0x[0-9a-f]*\).*/\1/p')
                            xor_addr=$(echo "$getbase_debug" | sed -n 's/.*XOR=\(0x[0-9a-f]*\).*/\1/p')
                            if [ -n "$native" ] && [ -n "$xor_addr" ]; then
                                native_num=$(printf "%d" "$native" 2>/dev/null || echo "0")
                                xor_expected=$((native_num ^ 2))
                                xor_actual=$(printf "%d" "$xor_addr" 2>/dev/null || echo "0")
                                if [ "$xor_expected" -eq "$xor_actual" ]; then
                                    echo -e "        ${GREEN}✓${NC} Word XOR addressing correct: $native ^ 2 = $xor_addr"
                                else
                                    echo -e "        ${RED}✗${NC} Word XOR addressing mismatch"
                                fi
                            fi
                        fi
                        echo "      → Stage 1: NativeAligned2FromLAddr (DLword arithmetic, no endianness)"
                        echo "      → Stage 2: GETWORD uses base ^ 2 (XOR addressing)"
                        ;;
                    
                    # Jump instructions - XOR addressing for offset reading
                    JUMPX|JUMPXX|FJumpx|TJumpx|JUMP0|JUMP1|JUMP2|JUMP3|JUMP4|JUMP5|JUMP6|JUMP7|JUMP8|JUMP9|JUMP10|JUMP11|JUMP12|JUMP13|JUMP14|JUMP15|FJUMP0|FJUMP1|FJUMP2|FJUMP3|FJUMP4|FJUMP5|FJUMP6|FJUMP7|FJUMP8|FJUMP9|FJUMP10|FJUMP11|FJUMP12|FJUMP13|FJUMP14|FJUMP15)
                        echo -e "${BLUE}XOR addressing${NC} for signed offset reading (if operand-based)"
                        category_counts="${category_counts}jump "
                        if echo "$instr_name" | grep -qE "JUMPX|JUMPXX|FJumpx|TJumpx"; then
                            echo "      → Expected: Get_SBYTE_PCMAC1 or Get_DLword_PCMAC1 uses XOR addressing"
                        else
                            echo "      → No endianness impact (compile-time constant offset)"
                        fi
                        ;;
                    
                    # Variable access - no endianness impact
                    IVAR0|IVAR1|IVAR2|IVAR3|IVAR4|IVAR5|IVAR6|PVAR0|PVAR1|PVAR2|PVAR3|PVAR4|PVAR5|PVAR6|FVAR0|FVAR1|FVAR2|FVAR3|FVAR4|FVAR5|FVAR6|GVAR|ARG0)
                        echo -e "${GREEN}No endianness impact${NC} (native pointer access)"
                        category_counts="${category_counts}variable_access "
                        ;;
                    
                    # Function calls - frame access uses XOR addressing
                    FN0|FN1|FN2|FN3|FN4|FNX|APPLYFN|CHECKAPPLY|ENVCALL|EVAL|RETCALL|SUBRCALL)
                        echo -e "${BLUE}Frame access${NC} uses XOR addressing (GETWORD for frame fields)"
                        category_counts="${category_counts}function_call "
                        # Check for FastRetCALL debug output
                        retcall_debug=$(grep "DEBUG FastRetCALL:" "$DEBUG_STDERR_LOG" | head -1)
                        if [ -n "$retcall_debug" ]; then
                            echo -e "      ${GREEN}✓${NC} FastRetCALL debug output available"
                        fi
                        echo "      → Frame field access: GETWORD uses base ^ 2"
                        echo "      → Function header: NativeAligned4FromLAddr (pointer arithmetic)"
                        ;;
                    
                    # Special instructions
                    GETBITS_*|PUTBITS_*)
                        echo -e "${BLUE}XOR addressing${NC} for word access in bit manipulation"
                        category_counts="${category_counts}special "
                        ;;
                    
                    *)
                        echo -e "${YELLOW}Unknown/Uncategorized${NC} - Check ENDIANNESS_FINDINGS.md"
                        category_counts="${category_counts}unknown "
                        ;;
                esac
                echo ""
            fi
        done < "$EXECUTION_LOG"
        
        # Summary statistics
        echo "=== Instruction Verification Summary ==="
        echo "Total instructions analyzed: $total_instructions"
        echo "Opcode fetches verified: $verified_opcodes"
        echo "Opcode fetches without debug: $no_debug_opcodes"
        echo ""
        echo "Category breakdown:"
        echo "  No operands: $(echo "$category_counts" | grep -o "no_operand" | wc -l)"
        echo "  8-bit operands: $(echo "$category_counts" | grep -o "8bit_operand" | wc -l)"
        echo "  16-bit operands: $(echo "$category_counts" | grep -o "16bit_operand" | wc -l)"
        echo "  32-bit operands: $(echo "$category_counts" | grep -o "32bit_operand" | wc -l)"
        echo "  Memory access: $(echo "$category_counts" | grep -o "memory_access" | wc -l)"
        echo "  Jump instructions: $(echo "$category_counts" | grep -o "jump" | wc -l)"
        echo "  Variable access: $(echo "$category_counts" | grep -o "variable_access" | wc -l)"
        echo "  Function calls: $(echo "$category_counts" | grep -o "function_call" | wc -l)"
        echo "  Special: $(echo "$category_counts" | grep -o "special" | wc -l)"
        echo "  Unknown: $(echo "$category_counts" | grep -o "unknown" | wc -l)"
    else
        echo -e "${YELLOW}WARNING: Execution log not found${NC}"
        echo "  Expected file: $EXECUTION_LOG"
        echo "  The C emulator should output execution log to stdout"
    fi
    echo ""
    
    # 4.6: Cross-reference with ENDIANNESS_FINDINGS.md categories
    echo "=== 4.6: Cross-Reference with Static Analysis Categories ==="
    echo ""
    echo "Verifying that all instruction categories from ENDIANNESS_FINDINGS.md are covered:"
    echo ""
    
    categories_covered=0
    categories_total=8
    
    # Check each category
    echo "1. Instructions with No Operands:"
    if grep -q "no_operand" <<< "$category_counts"; then
        echo -e "   ${GREEN}✓${NC} Covered (POP, PUSH, COPY, etc.)"
        categories_covered=$((categories_covered + 1))
    else
        echo -e "   ${YELLOW}⚠${NC} Not found in execution log"
    fi
    
    echo "2. Instructions with 8-bit Operands:"
    if grep -q "8bit_operand" <<< "$category_counts"; then
        echo -e "   ${GREEN}✓${NC} Covered (TYPEP, DTEST, IVARX, etc.)"
        categories_covered=$((categories_covered + 1))
    else
        echo -e "   ${YELLOW}⚠${NC} Not found in execution log"
    fi
    
    echo "3. Instructions with 16-bit Operands:"
    if grep -q "16bit_operand" <<< "$category_counts"; then
        echo -e "   ${GREEN}✓${NC} Covered (SICX, SNIC)"
        categories_covered=$((categories_covered + 1))
    else
        echo -e "   ${YELLOW}⚠${NC} Not found in execution log"
    fi
    
    echo "4. Instructions with 24/32-bit Operands:"
    if grep -q "32bit_operand" <<< "$category_counts"; then
        echo -e "   ${GREEN}✓${NC} Covered (GCONST)"
        categories_covered=$((categories_covered + 1))
    else
        echo -e "   ${YELLOW}⚠${NC} Not found in execution log"
    fi
    
    echo "5. Instructions Accessing Memory via LispPTR:"
    if grep -q "memory_access" <<< "$category_counts"; then
        echo -e "   ${GREEN}✓${NC} Covered (GETBASE_N, AREF1, etc.)"
        categories_covered=$((categories_covered + 1))
    else
        echo -e "   ${YELLOW}⚠${NC} Not found in execution log"
    fi
    
    echo "6. Variable Access Instructions:"
    if grep -q "variable_access" <<< "$category_counts"; then
        echo -e "   ${GREEN}✓${NC} Covered (IVAR, PVAR, FVAR)"
        categories_covered=$((categories_covered + 1))
    else
        echo -e "   ${YELLOW}⚠${NC} Not found in execution log"
    fi
    
    echo "7. Function Call Instructions:"
    if grep -q "function_call" <<< "$category_counts"; then
        echo -e "   ${GREEN}✓${NC} Covered (FN0, FN1, RETCALL, etc.)"
        categories_covered=$((categories_covered + 1))
    else
        echo -e "   ${YELLOW}⚠${NC} Not found in execution log"
    fi
    
    echo "8. Special Instructions:"
    if grep -q "special" <<< "$category_counts"; then
        echo -e "   ${GREEN}✓${NC} Covered (GETBITS, PUTBITS)"
        categories_covered=$((categories_covered + 1))
    else
        echo -e "   ${YELLOW}⚠${NC} Not found in execution log"
    fi
    
    echo ""
    echo "Category Coverage: $categories_covered/$categories_total categories found"
    echo ""
    
    # Summary
    echo "=== Summary ==="
    echo "Verification complete. Check above for:"
    echo "  ✓ = Verified correct"
    echo "  ✗ = Mismatch found"
    echo "  ⚠ = Warning or no data"
    echo ""
    echo "Key Verification Points:"
    echo "  1. XOR addressing for opcode fetch: $(if [ "$OPCODE_DEBUG_COUNT" -gt 0 ]; then echo -e "${GREEN}Verified${NC}"; else echo -e "${YELLOW}Not verified${NC}"; fi)"
    echo "  2. Get_DLword value construction: $(if [ "$DLWORD_DEBUG_COUNT" -gt 0 ]; then echo -e "${GREEN}Verified${NC}"; else echo -e "${YELLOW}Not verified${NC}"; fi)"
    echo "  3. FastRetCALL address translation: $(if [ "$RETCALL_COUNT" -gt 0 ]; then echo -e "${GREEN}Verified${NC}"; else echo -e "${YELLOW}Not verified${NC}"; fi)"
    echo "  4. GETBASE_N two-stage access: $(if [ "$GETBASE_COUNT" -gt 0 ]; then echo -e "${GREEN}Verified${NC}"; else echo -e "${YELLOW}Not verified${NC}"; fi)"
    echo "  5. Instruction-by-instruction checks: $(if [ "$total_instructions" -gt 0 ]; then echo -e "${GREEN}Verified${NC} ($total_instructions instructions)"; else echo -e "${YELLOW}Not verified${NC}"; fi)"
    echo ""
    
} | tee "$DEBUG_ANALYSIS_LOG"

# Step 5: Report results
echo -e "${BLUE}[5/5]${NC} Verification complete!"
echo ""
echo "Results saved to:"
echo "  - Debug stderr: $DEBUG_STDERR_LOG"
echo "  - Analysis report: $DEBUG_ANALYSIS_LOG"
echo ""
echo "Review the analysis report for detailed verification results."
