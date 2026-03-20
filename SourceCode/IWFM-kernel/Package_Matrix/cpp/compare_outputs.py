"""
Compare IWFM simulation output files between Fortran baseline and C++ solver.

Reads matching .out files from two directories and reports:
- Identical files (byte-for-byte match)
- Numeric differences (extracts floating-point values and computes max relative error)
- Missing files in either directory

Usage:
    python compare_outputs.py <baseline_dir> <test_dir> [tolerance]
"""

import sys
import os
import re
from pathlib import Path

def extract_numbers(line):
    """Extract all floating-point numbers from a line."""
    return [float(x) for x in re.findall(r'[-+]?\d*\.?\d+(?:[eE][-+]?\d+)?', line)]

def compare_files(baseline_path, test_path, tol=1e-10):
    """Compare two output files. Returns (identical, max_rel_err, max_abs_err, location)."""
    with open(baseline_path, 'r', errors='replace') as f1:
        lines1 = f1.readlines()
    with open(test_path, 'r', errors='replace') as f2:
        lines2 = f2.readlines()

    # Byte-identical check
    if lines1 == lines2:
        return True, 0.0, 0.0, ""

    max_rel_err = 0.0
    max_abs_err = 0.0
    location = ""
    n_diffs = 0
    n_values = 0

    # Compare line by line
    max_lines = max(len(lines1), len(lines2))
    if len(lines1) != len(lines2):
        location = f"line count differs: {len(lines1)} vs {len(lines2)}"

    for i in range(min(len(lines1), len(lines2))):
        if lines1[i] == lines2[i]:
            continue

        nums1 = extract_numbers(lines1[i])
        nums2 = extract_numbers(lines2[i])

        if len(nums1) != len(nums2):
            n_diffs += 1
            if not location:
                location = f"line {i+1}: different number count ({len(nums1)} vs {len(nums2)})"
            continue

        for j, (v1, v2) in enumerate(zip(nums1, nums2)):
            n_values += 1
            abs_err = abs(v1 - v2)
            if abs_err > max_abs_err:
                max_abs_err = abs_err
            denom = max(abs(v1), abs(v2), 1e-30)
            rel_err = abs_err / denom
            if rel_err > max_rel_err:
                max_rel_err = rel_err
                location = f"line {i+1}, value #{j+1}: {v1} vs {v2}"
            if abs_err > tol:
                n_diffs += 1

    return False, max_rel_err, max_abs_err, location

def main():
    if len(sys.argv) < 3:
        print(f"Usage: {sys.argv[0]} <baseline_dir> <test_dir> [tolerance]")
        sys.exit(1)

    baseline_dir = Path(sys.argv[1])
    test_dir = Path(sys.argv[2])
    tol = float(sys.argv[3]) if len(sys.argv) > 3 else 1e-10

    print(f"=== IWFM Output Regression Comparison ===")
    print(f"Baseline: {baseline_dir}")
    print(f"Test:     {test_dir}")
    print(f"Tolerance: {tol:.0e}")
    print()

    baseline_files = sorted(f.name for f in baseline_dir.glob("*.out"))
    test_files = sorted(f.name for f in test_dir.glob("*.out"))

    all_files = sorted(set(baseline_files) | set(test_files))

    identical = 0
    within_tol = 0
    failed = 0
    missing = 0

    for fname in all_files:
        bp = baseline_dir / fname
        tp = test_dir / fname

        if not bp.exists():
            print(f"  {fname:40s}  NEW (not in baseline)")
            missing += 1
            continue
        if not tp.exists():
            print(f"  {fname:40s}  MISSING (not in test)")
            missing += 1
            continue

        is_identical, max_rel, max_abs, loc = compare_files(bp, tp, tol)

        if is_identical:
            print(f"  {fname:40s}  IDENTICAL")
            identical += 1
        elif max_abs <= tol:
            print(f"  {fname:40s}  MATCH (max_abs={max_abs:.2e}, max_rel={max_rel:.2e})")
            within_tol += 1
        else:
            print(f"  {fname:40s}  DIFFERS (max_abs={max_abs:.2e}, max_rel={max_rel:.2e})")
            print(f"    {' ':40s}  at {loc}")
            failed += 1

    print()
    print(f"Summary: {identical} identical, {within_tol} within tolerance, {failed} failed, {missing} missing")
    sys.exit(1 if failed > 0 else 0)

if __name__ == "__main__":
    main()
