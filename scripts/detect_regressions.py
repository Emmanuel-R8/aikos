#!/usr/bin/env python3
"""
Regression Detection
Compares current results with baseline and flags regressions/improvements.
"""

import sys
import json
import argparse
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Optional


def load_baseline(baseline_file: Path) -> Dict:
    """Load baseline comparison results."""
    try:
        with open(baseline_file) as f:
            return json.load(f)
    except FileNotFoundError:
        return {}
    except json.JSONDecodeError:
        print(f"ERROR: Invalid JSON in baseline file: {baseline_file}", file=sys.stderr)
        return {}


def save_baseline(results: Dict, baseline_file: Path):
    """Save current results as new baseline."""
    baseline_file.parent.mkdir(parents=True, exist_ok=True)
    with open(baseline_file, 'w') as f:
        json.dump(results, f, indent=2)
    print(f"Baseline saved to: {baseline_file}")


def compare_with_baseline(current: Dict, baseline: Dict) -> Dict:
    """Compare current results with baseline."""
    report = {
        "regressions": [],
        "improvements": [],
        "unchanged": []
    }

    # Compare each implementation
    current_comps = current.get("comparisons", {})
    baseline_comps = baseline.get("comparisons", {})

    for impl_name in set(list(current_comps.keys()) + list(baseline_comps.keys())):
        current_comp = current_comps.get(impl_name, {})
        baseline_comp = baseline_comps.get(impl_name, {})

        current_status = current_comp.get("status", "unknown")
        baseline_status = baseline_comp.get("status", "unknown")

        # Status changes
        if baseline_status == "matches" and current_status == "diverges":
            # Regression: was matching, now diverges
            div = current_comp.get("first_divergence", {})
            report["regressions"].append({
                "implementation": impl_name,
                "type": "new_divergence",
                "baseline": "matches",
                "current": "diverges",
                "divergence_step": div.get("step"),
                "divergence_pc": div.get("pc")
            })
        elif baseline_status == "diverges" and current_status == "matches":
            # Improvement: was diverging, now matches
            baseline_div = baseline_comp.get("first_divergence", {})
            report["improvements"].append({
                "implementation": impl_name,
                "type": "divergence_resolved",
                "baseline": "diverges",
                "current": "matches",
                "previous_divergence_step": baseline_div.get("step"),
                "matched_lines": current_comp.get("matched_lines", 0)
            })
        elif baseline_status == "diverges" and current_status == "diverges":
            # Check if divergence point changed
            baseline_div = baseline_comp.get("first_divergence", {})
            current_div = current_comp.get("first_divergence", {})

            baseline_step = baseline_div.get("step", 0)
            current_step = current_div.get("step", 0)

            if current_step < baseline_step:
                # Improvement: divergence happens later (more steps match)
                report["improvements"].append({
                    "implementation": impl_name,
                    "type": "divergence_delayed",
                    "baseline_step": baseline_step,
                    "current_step": current_step,
                    "improvement": baseline_step - current_step
                })
            elif current_step > baseline_step:
                # Regression: divergence happens earlier (fewer steps match)
                report["regressions"].append({
                    "implementation": impl_name,
                    "type": "divergence_earlier",
                    "baseline_step": baseline_step,
                    "current_step": current_step,
                    "regression": current_step - baseline_step
                })
            else:
                # Unchanged
                report["unchanged"].append({
                    "implementation": impl_name,
                    "status": "diverges",
                    "step": current_step
                })
        elif current_status == baseline_status:
            # Status unchanged
            report["unchanged"].append({
                "implementation": impl_name,
                "status": current_status
            })

    return report


def print_regression_report(report: Dict):
    """Print human-readable regression report."""
    print("=== REGRESSION DETECTION REPORT ===\n")

    if report["regressions"]:
        print("❌ REGRESSIONS DETECTED:")
        for reg in report["regressions"]:
            print(f"  {reg['implementation']}: {reg['type']}")
            if "divergence_step" in reg:
                print(f"    Diverges at step {reg['divergence_step']}, PC {reg.get('divergence_pc', 'N/A')}")
            elif "regression" in reg:
                print(f"    Divergence moved earlier by {reg['regression']} steps")
        print()
    else:
        print("✅ No regressions detected\n")

    if report["improvements"]:
        print("✅ IMPROVEMENTS DETECTED:")
        for imp in report["improvements"]:
            print(f"  {imp['implementation']}: {imp['type']}")
            if "improvement" in imp:
                print(f"    Divergence delayed by {imp['improvement']} steps")
            elif "matched_lines" in imp:
                print(f"    Now matches for {imp['matched_lines']} lines")
        print()

    if report["unchanged"]:
        print("➡️ UNCHANGED:")
        for unchanged in report["unchanged"]:
            print(f"  {unchanged['implementation']}: {unchanged['status']}")
        print()


def main():
    parser = argparse.ArgumentParser(description="Detect regressions and improvements")
    parser.add_argument("current_json", type=Path, help="Current comparison JSON")
    parser.add_argument("--baseline", type=Path, help="Baseline comparison JSON")
    parser.add_argument("--update-baseline", action="store_true",
                       help="Update baseline with current results")
    parser.add_argument("--baseline-dir", type=Path,
                       default=Path("baselines"),
                       help="Baseline directory (default: baselines/)")

    args = parser.parse_args()

    # Load current results
    with open(args.current_json) as f:
        current_results = json.load(f)

    # Determine baseline file
    if args.baseline:
        baseline_file = args.baseline
    else:
        baseline_file = args.baseline_dir / "baseline.json"

    # Load baseline
    baseline_results = load_baseline(baseline_file)

    if not baseline_results:
        print("No baseline found. Creating initial baseline...")
        if args.update_baseline:
            save_baseline(current_results, baseline_file)
        else:
            print("Use --update-baseline to create initial baseline")
        return

    # Compare
    report = compare_with_baseline(current_results, baseline_results)

    # Print report
    print_regression_report(report)

    # Update baseline if requested
    if args.update_baseline:
        save_baseline(current_results, baseline_file)

    # Exit with error if regressions found
    if report["regressions"]:
        sys.exit(1)


if __name__ == "__main__":
    main()
