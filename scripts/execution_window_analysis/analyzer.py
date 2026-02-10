from __future__ import annotations

import json
from pathlib import Path
from typing import Any, Dict, List, Optional

from .parser import parse_trace_line, parse_sub_fields, parse_hex_value


def analyze_opcode_details(parsed_line: Dict[str, str]) -> Dict[str, Any]:
  """Extract opcode execution details from parsed trace line."""
  opcode_hex = parsed_line.get("opcode", "").strip()
  opcode_value = parse_hex_value(opcode_hex) if opcode_hex else None

  return {
    "instruction": parsed_line.get("instruction", "").strip(),
    "opcode_hex": opcode_hex,
    "opcode_value": opcode_value,
    "operands": parsed_line.get("operands", "").strip(),
    "operands_parsed": parse_sub_fields(parsed_line.get("operands", "")),
  }


def analyze_memory_addresses(parsed_line: Dict[str, str]) -> Dict[str, Any]:
  """Extract memory address calculations and accesses."""
  pc_str = parsed_line.get("pc", "").strip()
  pc_value = parse_hex_value(pc_str) if pc_str else None

  memory_str = parsed_line.get("memory", "").strip()
  memory_parsed = parse_sub_fields(memory_str)

  mapping_str = parsed_line.get("mapping", "").strip()
  mapping_parsed = parse_sub_fields(mapping_str)

  return {
    "pc": {
      "hex": pc_str,
      "value": pc_value,
    },
    "memory_context": memory_parsed,
    "address_translation": {
      "file_page": mapping_parsed.get("FP", ""),
      "virtual_page": mapping_parsed.get("VP", ""),
      "file_offset": mapping_parsed.get("FO", ""),
      "virtual_address": mapping_parsed.get("VA", ""),
    },
  }


def analyze_stack_and_frame(parsed_line: Dict[str, str]) -> Dict[str, Any]:
  """Extract stack pointer and frame pointer changes."""
  sp_fp_str = parsed_line.get("sp_fp", "").strip()
  sp_fp_parsed = parse_sub_fields(sp_fp_str)

  stack_str = parsed_line.get("stack", "").strip()
  stack_parsed = parse_sub_fields(stack_str)

  sp_hex = sp_fp_parsed.get("SP", "")
  fp_hex = sp_fp_parsed.get("FP", "")

  return {
    "stack_pointer": {
      "hex": sp_hex,
      "value": parse_hex_value(sp_hex),
    },
    "frame_pointer": {
      "hex": fp_hex,
      "value": parse_hex_value(fp_hex),
    },
    "stack_summary": {
      "tos": {
        "hex": stack_parsed.get("TOS", ""),
        "value": parse_hex_value(stack_parsed.get("TOS", "")),
      },
      "n1": {
        "hex": stack_parsed.get("N1", ""),
        "value": parse_hex_value(stack_parsed.get("N1", "")),
      },
      "n2": {
        "hex": stack_parsed.get("N2", ""),
        "value": parse_hex_value(stack_parsed.get("N2", "")),
      },
    },
  }


def analyze_registers(parsed_line: Dict[str, str]) -> Dict[str, Any]:
  """Extract register state transitions."""
  registers_str = parsed_line.get("registers", "").strip()
  registers_parsed = parse_sub_fields(registers_str)

  return {
    "r1": {
      "hex": registers_parsed.get("r1", ""),
      "value": parse_hex_value(registers_parsed.get("r1", "")),
    },
    "r2": {
      "hex": registers_parsed.get("r2", ""),
      "value": parse_hex_value(registers_parsed.get("r2", "")),
    },
    "r3": {
      "hex": registers_parsed.get("r3", ""),
      "value": parse_hex_value(registers_parsed.get("r3", "")),
    },
  }


def analyze_flags(parsed_line: Dict[str, str]) -> Dict[str, Any]:
  """Extract flag changes and condition evaluations."""
  flags_str = parsed_line.get("flags", "").strip()
  flags_parsed = parse_sub_fields(flags_str)

  return {
    "zero": flags_parsed.get("Z", ""),
    "negative": flags_parsed.get("N", ""),
    "carry": flags_parsed.get("C", ""),
  }


def analyze_step(
  step_number: int,
  parsed_line: Dict[str, str],
  previous_state: Optional[Dict[str, Any]] = None,
) -> Dict[str, Any]:
  """Analyze a single execution step."""
  step_analysis: Dict[str, Any] = {
    "step": step_number,
    "line_number": parsed_line.get("line", "").strip(),
    "opcode": analyze_opcode_details(parsed_line),
    "memory": analyze_memory_addresses(parsed_line),
    "stack": analyze_stack_and_frame(parsed_line),
    "registers": analyze_registers(parsed_line),
    "flags": analyze_flags(parsed_line),
    "byteswap": parsed_line.get("byteswap", "").strip(),
    "notes": parsed_line.get("notes", "").strip(),
  }

  if previous_state:
    transitions: Dict[str, Any] = {
      "pc_change": None,
      "sp_change": None,
      "fp_change": None,
      "tos_change": None,
    }

    prev_pc = previous_state.get("memory", {}).get("pc", {}).get("value")
    curr_pc = step_analysis["memory"]["pc"]["value"]
    if prev_pc is not None and curr_pc is not None:
      transitions["pc_change"] = curr_pc - prev_pc

    prev_sp = previous_state.get("stack", {}).get("stack_pointer", {}).get("value")
    curr_sp = step_analysis["stack"]["stack_pointer"]["value"]
    if prev_sp is not None and curr_sp is not None:
      transitions["sp_change"] = curr_sp - prev_sp

    prev_fp = previous_state.get("stack", {}).get("frame_pointer", {}).get("value")
    curr_fp = step_analysis["stack"]["frame_pointer"]["value"]
    if prev_fp is not None and curr_fp is not None:
      transitions["fp_change"] = curr_fp - prev_fp

    prev_tos = (
      previous_state.get("stack", {})
      .get("stack_summary", {})
      .get("tos", {})
      .get("value")
    )
    curr_tos = step_analysis["stack"]["stack_summary"]["tos"]["value"]
    if prev_tos is not None and curr_tos is not None:
      transitions["tos_change"] = curr_tos - prev_tos

    step_analysis["transitions"] = transitions

  return step_analysis


def analyze_execution_window(trace_file: str) -> Dict[str, Any]:
  """Analyze execution window from trace file."""
  analysis: Dict[str, Any] = {
    "trace_file": trace_file,
    "steps": [],
    "summary": {
      "total_steps": 0,
      "opcodes_executed": [],
    },
  }

  previous_state: Optional[Dict[str, Any]] = None

  with open(trace_file, "r") as f:
    for step_number, line in enumerate(f):
      parsed_line = parse_trace_line(line)
      if not parsed_line:
        continue

      step_analysis = analyze_step(step_number, parsed_line, previous_state)
      analysis["steps"].append(step_analysis)

      opcode_name = step_analysis["opcode"]["instruction"]
      if opcode_name and opcode_name not in analysis["summary"]["opcodes_executed"]:
        analysis["summary"]["opcodes_executed"].append(opcode_name)

      previous_state = step_analysis

  analysis["summary"]["total_steps"] = len(analysis["steps"])
  return analysis


def write_analysis_to_file(analysis: Dict[str, Any], output_path: str) -> None:
  """Write analysis JSON to disk."""
  out = Path(output_path)
  out.parent.mkdir(parents=True, exist_ok=True)
  with open(out, "w") as f:
    json.dump(analysis, f, indent=2)
