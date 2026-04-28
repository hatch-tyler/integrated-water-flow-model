#!/usr/bin/env python3
"""
Drive the IWFM sample model through IWFM_C_x64.dll and verify the
generated outputs match Results_baseline byte-for-byte.

The standalone Simulation_x64.exe links the kernel sources directly
and does NOT exercise the DLL export layer (IWFM_DLL/*.f90). Only
this script actually tests that DLL-side changes produce the same
simulation results.

Usage:
    py -3 verify-dll-regression.py [path\\to\\SampleModel]

Default SampleModel path is resolved relative to this repo:
    ..\\iwfm-2025.0.1747\\SampleModel
"""

import ctypes
import os
import pathlib
import shutil
import sys

REPO = pathlib.Path(__file__).resolve().parent
DLL_PATH = REPO / "Bin" / "IWFM_C_x64.dll"
DEFAULT_SAMPLE = (REPO / ".." / "iwfm-2025.0.1747" / "SampleModel").resolve()


def load_dll():
    if not DLL_PATH.exists():
        sys.exit(f"DLL not found: {DLL_PATH}. Run build-iwfm.ps1 first.")
    lib = ctypes.WinDLL(str(DLL_PATH))

    int_p = ctypes.POINTER(ctypes.c_int)
    char_buf = ctypes.c_char_p

    lib.IW_Model_New.argtypes = [
        int_p, char_buf,           # LenPPFileName, cPPFileName
        int_p, char_buf,           # LenSimFileName, cSimFileName
        int_p, int_p,              # IsRoutedStreams, IsForInquiry
        int_p, int_p,              # iModelID (out), iStat (out)
    ]
    lib.IW_Model_New.restype = None

    # Track 7: every IW_Model_* export takes iModelID as first arg.
    for name in ("IW_Model_SimulateAll", "IW_Model_PrintResults",
                 "IW_Model_AdvanceTime", "IW_Model_AdvanceState",
                 "IW_Model_Kill"):
        fn = getattr(lib, name)
        fn.restype = None
        fn.argtypes = [int_p, int_p]   # iModelID, iStat

    lib.IW_Model_GetNTimeSteps.argtypes = [int_p, int_p, int_p]  # iModelID, NTimeSteps, iStat
    lib.IW_Model_GetNTimeSteps.restype = None

    lib.IW_SetLogFile.argtypes = [int_p, char_buf, int_p]
    lib.IW_SetLogFile.restype = None
    return lib


def check(lib, i_stat, where):
    if i_stat.value != 0:
        sys.exit(f"[FAIL] {where} returned iStat={i_stat.value}. "
                 f"See SimulationMessages.out for details.")


def run_via_dll(sample_root: pathlib.Path):
    pp_main = sample_root / "Preprocessor" / "PreProcessor_MAIN.IN"
    sim_main = sample_root / "Simulation" / "Simulation_MAIN.IN"
    sim_dir = sim_main.parent
    results_dir = sample_root / "Results"

    for p in (pp_main, sim_main):
        if not p.exists():
            sys.exit(f"Missing input file: {p}")

    # Clear prior results so any missing output surfaces immediately.
    for pattern in ("*.out", "*.hdf"):
        for f in results_dir.glob(pattern):
            f.unlink()
    for f in sim_dir.glob("SimulationMessages.out"):
        f.unlink()

    # DLL expects working directory to be the Simulation folder (matches
    # the standalone EXE, which cd's via run_sample.bat). Relative paths
    # inside Simulation_MAIN.IN resolve from there.
    original_cwd = os.getcwd()
    os.chdir(sim_dir)
    try:
        lib = load_dll()

        pp_bytes = str(pp_main).encode("ascii")
        sim_bytes = str(sim_main).encode("ascii")
        len_pp = ctypes.c_int(len(pp_bytes))
        len_sim = ctypes.c_int(len(sim_bytes))
        is_routed = ctypes.c_int(1)
        is_inquiry = ctypes.c_int(0)
        model_id = ctypes.c_int(0)
        i_stat = ctypes.c_int(0)

        log_bytes = b"SimulationMessages.out"
        len_log = ctypes.c_int(len(log_bytes))
        print("[DLL] IW_SetLogFile -> SimulationMessages.out ...")
        lib.IW_SetLogFile(ctypes.byref(len_log), log_bytes, ctypes.byref(i_stat))
        check(lib, i_stat, "IW_SetLogFile")

        print("[DLL] IW_Model_New ...")
        lib.IW_Model_New(
            ctypes.byref(len_pp), pp_bytes,
            ctypes.byref(len_sim), sim_bytes,
            ctypes.byref(is_routed), ctypes.byref(is_inquiry),
            ctypes.byref(model_id), ctypes.byref(i_stat))
        check(lib, i_stat, "IW_Model_New")
        print(f"[DLL] Model instance ID: {model_id.value}")

        n_steps = ctypes.c_int(0)
        lib.IW_Model_GetNTimeSteps(ctypes.byref(model_id), ctypes.byref(n_steps), ctypes.byref(i_stat))
        check(lib, i_stat, "IW_Model_GetNTimeSteps")
        print(f"[DLL] Expecting {n_steps.value} time steps")

        print("[DLL] IW_Model_SimulateAll ...")
        lib.IW_Model_SimulateAll(ctypes.byref(model_id), ctypes.byref(i_stat))
        check(lib, i_stat, "IW_Model_SimulateAll")

        print("[DLL] IW_Model_Kill ...")
        lib.IW_Model_Kill(ctypes.byref(model_id), ctypes.byref(i_stat))
        check(lib, i_stat, "IW_Model_Kill")
    finally:
        os.chdir(original_cwd)


def compare_results(sample_root: pathlib.Path) -> bool:
    baseline = sample_root / "Results_baseline"
    results = sample_root / "Results"
    baseline_files = sorted(baseline.glob("*.out"))
    if not baseline_files:
        sys.exit(f"No baseline files in {baseline}")

    ok = 0
    failures = []
    for b in baseline_files:
        r = results / b.name
        if not r.exists():
            failures.append(f"missing:  {b.name}")
            continue
        if r.read_bytes() == b.read_bytes():
            ok += 1
        else:
            failures.append(f"differs:  {b.name}  "
                            f"(baseline {b.stat().st_size} B, "
                            f"result {r.stat().st_size} B)")

    total = len(baseline_files)
    print(f"\nByte-for-byte: {ok} / {total}")
    for f in failures:
        print(f"  {f}")
    return not failures


def main():
    sample_root = pathlib.Path(sys.argv[1]).resolve() if len(sys.argv) > 1 \
        else DEFAULT_SAMPLE
    if not sample_root.exists():
        sys.exit(f"Sample model not found: {sample_root}")

    print(f"Sample model: {sample_root}")
    print(f"DLL:          {DLL_PATH}")

    run_via_dll(sample_root)
    ok = compare_results(sample_root)

    if ok:
        print("\n[PASS] DLL-driven run matches baseline byte-for-byte.")
        sys.exit(0)
    else:
        print("\n[FAIL] DLL-driven run diverges from baseline.")
        sys.exit(1)


if __name__ == "__main__":
    main()
