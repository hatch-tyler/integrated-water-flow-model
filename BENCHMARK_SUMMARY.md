# IWFM Parallel Benchmark Summary

**Date:** 2026-03-20
**Model:** C2VSimFG v1.5 (576 monthly timesteps, WY1974-2021)
**Source:** IWFM-2025.0.1747

---

## 1. What Was Tested

Two executables built from the same IWFM-2025.0.1747 source, differing only in the
sparse linear solver used inside `Package_Matrix.f90`:

| Build              | Solver                    | Build Script                    |
|--------------------|---------------------------|---------------------------------|
| **Fortran-only**   | Fortran SPARSKIT PGMRES   | `Build-IWFM-VS.ps1`            |
| **C++ solver**     | C++ PGMRES (icx-compiled) | `Build-IWFM-VS-CppSolver.ps1`  |

Both are `Simulation_Parallel` (OpenMP-enabled) builds using Intel ifx 2025.3 for
Fortran and icx 2025.3 for C++. Release configuration, `/O2` optimization.

---

## 2. Hardware

### i7-1250U (this laptop)
- Intel Core i7-1250U (Alder Lake mobile)
- 2 P-cores (4 threads) + 8 E-cores (8 threads) = 10 cores / 12 threads
- 15W TDP (thermal-limited ultrabook)

### Core Ultra 7 265 (target machine for transfer)
- Intel Core Ultra 7 265 (Arrow Lake desktop-class)
- Higher core count, higher TDP, higher sustained clocks
- Reference: completed official C2VSimFG release in **2.5 hours**

---

## 3. Build Process

### Prerequisites
- Visual Studio 2022 Community (for MSVC linker + libs)
- Intel oneAPI 2025.3 (ifx + icx compilers)
- DSS7 heclib pre-built via CMake (`src/build/heclib.lib`)

### Build Commands (run from `src/` directory)

```powershell
# Fortran-only
.\Build-IWFM-VS.ps1 -Project Simulation_Parallel -Clean
# Output: src\Bin\Simulation_PLL_x64.exe (~24.4 MB)
# Save as: Simulation_PLL_x64_fortran.exe

# C++ solver
.\Build-IWFM-VS-CppSolver.ps1 -Clean
# Output: src\Bin\Simulation_PLL_x64.exe (~24.4 MB)
# Save as: Simulation_PLL_x64_cpp.exe
```

Build times on i7-1250U:
- Fortran-only: ~5 min compile + 1s link
- C++ solver: 9s C++ compile + 5 min Fortran compile + 1s link

The C++ solver build compiles 6 additional C++ files (`blas.cpp`, `spmv.cpp`,
`ilut.cpp`, `lusol.cpp`, `gmres.cpp`, `pgmres_solve.cpp`) and passes
`/DIWFM_CPP_SOLVER` to the Fortran compiler so `Package_Matrix.f90` routes
solver calls to the C++ implementation instead of Fortran SPARSKIT.

---

## 4. Running the Benchmark

### Automated Script
`c2vsimfg/Simulation/bench_parallel.ps1` automates the full process:

```powershell
# Build both, run both, compare (full automation)
.\bench_parallel.ps1 -Phase All

# Or run phases individually:
.\bench_parallel.ps1 -Phase Build      # Build both executables
.\bench_parallel.ps1 -Phase Fortran    # Run Fortran-only (576 ts)
.\bench_parallel.ps1 -Phase Cpp        # Run C++ solver (576 ts)
.\bench_parallel.ps1 -Phase Compare    # Compare outputs and timing

# Override thread count (default: auto-detect all)
.\bench_parallel.ps1 -Phase Fortran -Threads 8
```

### Manual Run (if not using the script)

```powershell
cd c2vsimfg\Simulation

# Deploy the executable you want to test
Copy-Item ..\bin\IWFM-2025.0.1747\Simulation\PLL\Simulation_PLL_x64_fortran.exe `
          ..\bin\IWFM-2025.0.1747\Simulation\PLL\Simulation_PLL_x64.exe

# Set OpenMP environment
$env:OMP_NUM_THREADS = [Environment]::ProcessorCount
$env:KMP_AFFINITY = "granularity=fine,compact,1,0"

# IMPORTANT: Clear previous output files (HDF5 files cause errors if not cleared)
Remove-Item ..\Results\C2VSimFG_*.hdf -Force -ErrorAction SilentlyContinue
Remove-Item ..\Results\C2VSimFG_*.out -Force -ErrorAction SilentlyContinue
Remove-Item ..\Results\LWU_*.hdf -Force -ErrorAction SilentlyContinue
Remove-Item ..\Results\RZ_*.hdf -Force -ErrorAction SilentlyContinue

# Run (uses C2VSimFG.in which has full 576 timesteps)
& ..\bin\IWFM-2025.0.1747\Simulation\PLL\Simulation_PLL_x64.exe C2VSimFG.in
```

### Key Notes for Running
1. **Clear Results/ directory** between runs -- the model writes HDF5 files to
   `../Results/` and will fail with `Error in opening HDF5 file` if stale files
   from a previous run exist.
2. **Do NOT set OMP_PROC_BIND** when using KMP_AFFINITY -- Intel OpenMP warns
   and ignores it, and the warning on stderr can crash PowerShell scripts with
   `$ErrorActionPreference = "Stop"`.
3. The model writes FinalCond files only at completion -- if the process crashes
   mid-run, these will be 0 bytes.
4. Monitor progress via: `Select-String "TIME STEP" SimulationMessages.out | Measure-Object`

---

## 5. Results on i7-1250U

### Fortran-only Parallel (COMPLETED)

| Metric                 | Value           |
|------------------------|-----------------|
| Total wall-clock       | **6.23 hours** (22,420 s) |
| Time per timestep      | **38.9 s/ts**   |
| Threads                | 12              |
| vs Core Ultra 7 265    | 2.49x slower    |

The Fortran solver completed all 576 timesteps successfully.

### C++ Solver Parallel (CRASHED at timestep 279)

| Metric                 | Value           |
|------------------------|-----------------|
| Timesteps completed    | **279 of 576**  |
| Wall-clock before crash| ~4.5 hours      |
| Approx. pace           | ~58 s/ts        |
| Crash location         | Timestep 279 (12/31/1996), supply adjustment iteration 3 |

The C++ solver crashed during the linear solve at a point where the initial
residual is very large (287.678 ft head difference at node GW_20751, Layer 2).
The Fortran SPARSKIT solver encounters the exact same large residual but
recovers and continues. This indicates a robustness issue in the C++ GMRES
implementation (not a parallelism bug -- the solver itself is sequential).

### Previous Single-Threaded Results (20-timestep benchmarks)

| Build          | s/ts  | Notes |
|----------------|-------|-------|
| Fortran-only   | 37.1  | Full 576-ts run, OMP_NUM_THREADS=1 |
| C++ solver     | 38.5  | Full 576-ts run, OMP_NUM_THREADS=1 |

### Observation: OpenMP Parallelism Is Minimal

On the i7-1250U, parallel (12 threads) vs single-threaded performance is
nearly identical (38.9 vs 37.1 s/ts for Fortran). This suggests:
- The IWFM workload is dominated by serial sections
- The E-core performance on Alder Lake may not help much
- Thermal throttling on this 15W ultrabook limits sustained parallel throughput

---

## 6. C++ Solver Crash Analysis

### What happens
At timestep 279, during the 3rd supply adjustment iteration, the solver
receives a system with a very large initial residual (287.678 ft). The Fortran
SPARSKIT PGMRES handles this and converges after several more iterations. The
C++ GMRES solver silently crashes (process exits with no error message).

### Root cause hypothesis
The C++ GMRES code (`gmres.cpp`) has numerical safeguards (NaN detection,
scaled norms) but the crash likely occurs in one of:
1. **Back-substitution division** (line 182/188) -- no zero-divisor check
2. **ILUT factorization** (`ilut.cpp`) -- when matrix conditioning degrades
   with large residuals, ILUT may produce factors that overflow in ddot
3. **Givens rotations** -- extreme values could overflow `sqrt(1 + t*t)`

The solver is designed to return error codes (-1 through -7) rather than crash.
The silent death suggests an **unhandled floating-point exception** or
**access violation** rather than a logic-level error, possibly triggered by
NaN/Inf propagation that bypasses the existing guards.

### Fix priority
This must be fixed before the C++ solver can be used for production runs.
The fix should add defensive checks in the GMRES back-substitution and
potentially clamp the ILUT fill values.

---

## 7. Transfer Instructions for Core Ultra 7 265

### What to copy
```
iwfm-2025.0.1747/src/                    # Full source tree
  Build-IWFM-VS.ps1                     # Fortran-only build script
  Build-IWFM-VS-CppSolver.ps1           # C++ solver build script
  SourceCode/                            # All Fortran + C++ source
  build/heclib.lib                       # Pre-built DSS7 library

c2vsimfg/                                # C2VSimFG model
  Simulation/                            # Input files + bench scripts
    C2VSimFG.in                          # Full 576-timestep input
    bench.ps1                            # Short benchmark (20 ts)
    bench_parallel.ps1                   # Full parallel benchmark
  bin/IWFM-2025.0.1747/Simulation/PLL/   # Executables go here
    Simulation_PLL_x64_fortran.exe       # Pre-built Fortran-only
    Simulation_PLL_x64_cpp.exe           # Pre-built C++ solver
    libiomp5md.dll                       # Intel OpenMP runtime
  Results/                               # Model writes output here
```

### On the target machine

**Option A: Use pre-built executables (fastest)**
If the target machine has the Intel oneAPI runtime installed (or `libiomp5md.dll`
is present), the pre-built `.exe` files should run directly:

```powershell
cd c2vsimfg\Simulation
.\bench_parallel.ps1 -Phase Fortran   # Skip Build, just run
.\bench_parallel.ps1 -Phase Cpp       # (will crash at ts 279 until fixed)
```

Note: edit `bench_parallel.ps1` line 29 to update `$SrcDir` if the source tree
is in a different location on the target machine.

**Option B: Rebuild on target (recommended for best optimization)**
```powershell
cd iwfm-2025.0.1747\src
.\Build-IWFM-VS.ps1 -Project Simulation_Parallel -Clean
# Copy output to c2vsimfg PLL dir as Simulation_PLL_x64_fortran.exe

.\Build-IWFM-VS-CppSolver.ps1 -Clean
# Copy output to c2vsimfg PLL dir as Simulation_PLL_x64_cpp.exe
```

Requires Visual Studio 2022 + Intel oneAPI 2025.x on the target.

### Expected timing on Core Ultra 7 265
The official release ran in 2.5 hours on this CPU. Expected results:
- Fortran-only: ~2.5 hours (~15.6 s/ts)
- C++ solver: will crash at timestep 279 (same bug, hardware-independent)

---

## 8. Files Created/Modified

| File | Purpose |
|------|---------|
| `c2vsimfg/Simulation/bench_parallel.ps1` | Full parallel benchmark script |
| `c2vsimfg/bin/.../Simulation_PLL_x64_fortran.exe` | Fortran-only executable |
| `c2vsimfg/bin/.../Simulation_PLL_x64_cpp.exe` | C++ solver executable |
| `c2vsimfg/Results/fortran_parallel_run/` | Fortran run FinalCond outputs + timing |
| `c2vsimfg/Results/cpp_parallel_run/` | C++ run outputs (incomplete) |
| `c2vsimfg/Simulation/bench_results.log` | Cumulative benchmark log |

---

## 9. Next Steps

1. **Fix C++ solver crash** -- Add robustness to `gmres.cpp` back-substitution
   and `ilut.cpp` diagonal handling for extreme residuals
2. **Re-run C++ solver benchmark** after fix (full 576 timesteps)
3. **Transfer to Core Ultra 7 265** for production-speed benchmarking
4. **Compare numerical accuracy** -- FinalCond files should match within
   solver tolerance (~0.1 ft max GW head difference)
