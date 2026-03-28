# IWFM Build Configuration Benchmarks

Comprehensive runtime benchmarks comparing build configurations for IWFM v2025.0.1747.

## Overview

| Parameter | Value |
|-----------|-------|
| Model | C2VSimFG (California Central Valley, 30,000+ finite element nodes) |
| Time Steps | 576 monthly steps (10/1973 - 09/2021) |
| Platform | Windows 11 Pro, Intel oneAPI 2025.3 (ifx/icx), 64-bit |
| Build Mode | Release, OpenMP parallel (`Simulation_PLL`) |
| Metric | Wall-clock simulation time (lower is better) |

## Results

Ranked by wall-clock time:

| # | Configuration | Generator | Dependencies | Wall Time | vs Baseline | CPU/Wall | CPU User |
|---|---------------|-----------|--------------|-----------|-------------|----------|----------|
| 1 | Repo — VS script | ifx direct | Bundled HDF5 1.14.2 + DSS 7 | 3h 28m | -0.2% | 2.82 | 9h 33m |
| 2 | Official DWR release | VS 2019 | Bundled HDF5 1.14.2, DSS 6 | 3h 29m | baseline | 2.76 | 9h 22m |
| 3 | Repo — Ninja + prebuilt deps | Ninja | Prebuilt HDF5 1.14.3 + DSS 7 | 3h 48m | +8.8% | 3.01 | 11h 11m |
| 4 | Repo — Ninja + bundled HDF5 | Ninja | Bundled HDF5 1.14.2 + prebuilt DSS 7 | 3h 48m | +9.0% | 3.03 | 11h 24m |
| 5 | Repo — VS generator + prebuilt | VS 2022 | Prebuilt HDF5 1.14.3 + DSS 7 | 3h 52m | +10.9% | 3.07 | 11h 40m |
| 6 | Repo — NMake | NMake | FetchContent (from source) | 4h 08m | +18.7% | 2.89 | 11h 41m |
| 7 | Repo — Ninja (fixed flags) | Ninja | FetchContent (from source) | 4h 11m | +19.8% | 2.87 | 11h 45m |
| 8 | Repo — Ninja (original) | Ninja | FetchContent (from source) | 4h 14m | +21.7% | 3.19 | 13h 22m |
| 9 | Repo — subsidence v4.0 | Ninja | FetchContent (from source) | 4h 41m | +34.2% | 2.97 | 13h 41m |
| 10 | Repo — monolithic (early) | Ninja | FetchContent (from source) | 4h 46m | +36.8% | 2.67 | 12h 29m |

All runs completed successfully (exit code 0). Testing period: March 23-28, 2026.

## Key Findings

### CMake generator does not affect runtime performance

The VS 2022 generator and Ninja generator produce identical runtime when using the same dependency configuration:
- Ninja + prebuilt deps: **3h 48m**
- VS + prebuilt deps: **3h 52m**

The ~4 minute difference is within run-to-run variance for a 4-hour benchmark. Both use ifx as the Fortran compiler and linker driver.

### HDF5 library version and compiler have no effect

The bundled HDF5 1.14.2 (compiled with Intel classic ifort and MSVC cl.exe from oneAPI 2021.10) performs identically to HDF5 1.14.3 (compiled with Intel LLVM ifx and icx from oneAPI 2025.3):
- Ninja + prebuilt HDF5 1.14.3: **3h 48m**
- Ninja + bundled HDF5 1.14.2: **3h 48m**

### DSS library version has no runtime impact

The C2VSimFG test model does not read or write HEC-DSS files, so the choice between DSS 6 (Fortran-based heclib) and DSS 7 (C-based heclib) has no effect. The official DWR build (DSS 6) and the repo VS script build (DSS 7) produce identical results.

### OpenMP parallelization confirmed effective

All CPU/Wall ratios exceed 2.5, confirming that OpenMP is effectively parallelizing the simulation across multiple cores. The theoretical maximum depends on the fraction of serial code (Amdahl's law).

### Build configuration evolution

The progression from rows 8-10 to rows 3-5 reflects incremental improvements to the CMake build system:
- Removing unconditional `/Qopenmp` from non-parallel targets eliminated thread management overhead
- Pre-building dependencies avoids recompilation variance across runs
- The monolithic build (row 10) was an early experiment before the OpenMP fix

## Methodology

- **Single-run benchmarks**: Each configuration was run once (4+ hour duration per run)
- **Sequential execution**: Runs were not concurrent to avoid resource contention
- **Timing**: PowerShell `Measure-Command` for wall clock; .NET `Process` object for CPU counters
- **Identical inputs**: Same C2VSimFG model for all runs
- **Resumable**: Benchmark script tracks completed runs in a state file
- **Hardware**: Desktop workstation (details in `benchmark2/` directory)

## Decisions Made

Based on these results, the following build system simplifications were applied:

1. **Removed VS generator option from CMake build** — No runtime benefit over Ninja, adds multi-config complexity
2. **Removed bundled HDF5 libraries** (~110 MB) — No runtime benefit; CMake builds HDF5 from source automatically
3. **Removed bundled DSS 6 heclib** (~10 MB) — Obsolete; the codebase uses HEC-DSS 7 via `heclib_compat.f90`
4. **Retained `Build-IWFM-VS.ps1`** — Direct ifx invocation produced the fastest build (row 1), available as an alternative build path via `build-deps.ps1`

## Raw Data

Full timing data is available in `benchmark2/benchmark2_timings.csv` (not in this repository).

| RunId | Wall (s) | CPU User (s) | CPU Kernel (s) | CPU/Wall | Start | End |
|-------|----------|--------------|----------------|----------|-------|-----|
| official-PLL-run1 | 12,549 | 33,698 | 916 | 2.76 | 2026-03-23 16:42 | 2026-03-23 20:11 |
| repo-vs-PLL-run1 | 12,518 | 34,366 | 909 | 2.82 | 2026-03-24 19:30 | 2026-03-24 22:58 |
| repo-prebuilt-PLL-run1 | 13,654 | 40,234 | 841 | 3.01 | 2026-03-27 08:30 | 2026-03-27 12:17 |
| repo-bundled-hdf5-PLL-run1 | 13,682 | 41,033 | 449 | 3.03 | 2026-03-27 19:11 | 2026-03-27 22:59 |
| repo-vs-prebuilt-PLL-run1 | 13,911 | 41,992 | 782 | 3.07 | 2026-03-27 13:03 | 2026-03-27 16:55 |
| repo-nmake-PLL-run1 | 14,896 | 42,091 | 935 | 2.89 | 2026-03-25 13:34 | 2026-03-25 17:42 |
| repo-ninja-fixed-PLL-run1 | 15,035 | 42,278 | 888 | 2.87 | 2026-03-26 20:52 | 2026-03-27 01:03 |
| repo-PLL-run1 | 15,269 | 48,092 | 567 | 3.19 | 2026-03-23 20:11 | 2026-03-24 00:26 |
| repo-v40sub-PLL-run1 | 16,841 | 49,284 | 766 | 2.97 | 2026-03-24 06:27 | 2026-03-24 11:08 |
| repo-mono-PLL-run1 | 17,163 | 44,952 | 803 | 2.67 | 2026-03-24 14:44 | 2026-03-24 19:30 |
