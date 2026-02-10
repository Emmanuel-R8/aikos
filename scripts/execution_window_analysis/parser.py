from __future__ import annotations

from typing import Dict, Optional


def parse_trace_line(line: str) -> Optional[Dict[str, str]]:
  """Parse a unified trace line into fields.

  Format:
    LINE#|PC|INSTRUCTION|OPCODE|OPERANDS|REGISTERS|FLAGS|SP_FP|STACK_SUMMARY|
    MEMORY_CONTEXT|FP_VP_FO_VA|BS_MEM|NOTES
  """
  line = line.strip()
  if not line:
    return None

  fields = line.split("|")
  if len(fields) < 13:
    return None

  return {
    "line": fields[0].strip(),
    "pc": fields[1].strip(),
    "instruction": fields[2].strip(),
    "opcode": fields[3].strip(),
    "operands": fields[4].strip(),
    "registers": fields[5].strip(),
    "flags": fields[6].strip(),
    "sp_fp": fields[7].strip(),
    "stack": fields[8].strip(),
    "memory": fields[9].strip(),
    "mapping": fields[10].strip(),
    "byteswap": fields[11].strip(),
    "notes": fields[12].strip() if len(fields) > 12 else "",
  }


def parse_sub_fields(field_str: str) -> Dict[str, str]:
  """Parse comma-separated sub-fields into a dictionary.

  Examples:
      "r1:0x1234,r2:0x5678,r3:0x9a" -> {"r1": "0x1234", "r2": "0x5678", "r3": "0x9a"}
      "SP:0x012e8a,FP:0x012e72" -> {"SP": "0x012e8a", "FP": "0x012e72"}
      "TOS:0x00000000,N1:0x00000000,N2:0x00000000" -> {"TOS": "0x00000000", "N1": "0x00000000", "N2": "0x00000000"}
  """
  if not field_str or field_str.strip() == "":
    return {}

  result: Dict[str, str] = {}
  for item in field_str.split(","):
    item = item.strip()
    if ":" in item:
      key, value = item.split(":", 1)
      result[key.strip()] = value.strip()

  return result


def parse_hex_value(value_str: str) -> Optional[int]:
  """Parse a hexadecimal value string to integer."""
  if not value_str:
    return None

  value_str = value_str.strip()
  if value_str.startswith(("0x", "0X")):
    try:
      return int(value_str, 16)
    except ValueError:
      return None

  return None
