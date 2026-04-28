# IWFM -- Integrated Water Flow Model

A finite-element hydrological simulation model for conjunctive management of groundwater and surface water resources.

## Overview

IWFM simulates the flow of water through an integrated hydrologic system -- groundwater, streams, lakes, root zone, and unsaturated zone -- under historical or projected conditions. Developed by the California Department of Water Resources (DWR), it is used for water resources planning, groundwater sustainability analysis, and regulatory compliance across California.

This repository is a fork of the official IWFM release (upstream source: <https://data.cnra.ca.gov/dataset/iwfm>). Active development happens on the `2025.0.1747.rev` branch; the `2025.0.1747` branch tracks the unmodified upstream import. Relative to upstream, this fork adds a cross-platform CMake build, additional post-processors, a multi-instance C-callable DLL, modern Fortran cleanup, and pre-built binary [releases](https://github.com/hatch-tyler/integrated-water-flow-model/releases). See [What's in this fork](#whats-in-this-fork) below.

Version **2025.0.1747** (kernel 2025.0.107), latest binary release [`v2025.0.1747-rev3`](https://github.com/hatch-tyler/integrated-water-flow-model/releases/tag/v2025.0.1747-rev3). Written in modern Fortran (F90/F2003).

## What's in this fork

Changes relative to the upstream IWFM 2025.0.1747 release, in descending user impact:

- **Multi-instance DLL with handle-based API** — up to 64 concurrent model instances per process; every `IW_Model_*` export takes `iModelID` as its first argument. **Breaking change** for callers of the upstream DLL — see [Using IWFM as a library](#using-iwfm-as-a-library).
- **Cross-platform CMake build** — Windows PowerShell driver (`build-iwfm.ps1`), Linux Docker images, and automatic dependency fetch (HDF5, HEC-DSS 7, zlib). See [BUILD.md](BUILD.md).
- **Additional post-processors** in the standard release: `IWFM2OBS` (PEST hydrograph converter), `CalcTypeHyd` (cluster type hydrograph), and `ResultsExtract` (generalized hydrograph extractor with native HDF5 input support).
- **Faster inquiry-mode loads** for pywfm / DLL consumers — C2VSimFG cold load reduced from 30 s to 13 s. See [Fast inquiry-mode loads](#fast-inquiry-mode-loads-pywfm--c-dll-users).
- **Modern Fortran cleanup** — SAVE-global state eliminated from the kernel and DLL layers; `Logger` threaded through every derived type; thread-safe DLL exports under shared CRITICAL / per-thread `THREADPRIVATE` regions; `GOTO` removed from utility programs. See [CLAUDE.md](CLAUDE.md).
- **Pre-built binary releases** for Windows and Linux on [GitHub Releases](https://github.com/hatch-tyler/integrated-water-flow-model/releases).
- **pywfm companion** — Python wrapper with multi-instance support on the [`track7-handle-based-api` branch](https://github.com/SGMOModeling/PyWFM/tree/track7-handle-based-api).
- **Fork-only docs** — `BUILD.md`, `BENCHMARKS.md`, and `CLAUDE.md` (architecture and developer guide).

## Quick Start

### Windows (PowerShell)

```powershell
# Requires Intel oneAPI (ifx) -- the script auto-detects the compiler environment
.\Build-IWFM.ps1
```

### CMake (Cross-Platform)

```bash
mkdir build && cd build
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_Fortran_COMPILER=ifx -DCMAKE_C_COMPILER=icx
cmake --build .
```

Dependencies (HDF5, HEC-DSS 7, zlib) are fetched and built automatically on first configure. Output binaries go to `Bin/`.

Or skip the build entirely: pre-built Windows and Linux binaries for each release are available on [GitHub Releases](https://github.com/hatch-tyler/integrated-water-flow-model/releases). Latest: **v2025.0.1747-rev3**.

## Build Targets

| Target | CMake Option | Description |
|--------|-------------|-------------|
| Simulation | `IWFM_BUILD_SIMULATION` | Standard simulation executable |
| Simulation_Parallel | `IWFM_BUILD_PARALLEL` | OpenMP parallel simulation |
| Simulation_MM | `IWFM_BUILD_COARRAY` | Multi-model via Fortran coarrays (Intel only) |
| PreProcessor | `IWFM_BUILD_PREPROCESSOR` | Model pre-processor |
| Budget | `IWFM_BUILD_BUDGET` | Budget post-processor |
| ZBudget | `IWFM_BUILD_ZBUDGET` | Zone budget post-processor |
| IWFM_C_DLL | `IWFM_BUILD_DLL` | C-callable shared library |
| IWFM2OBS | `IWFM_BUILD_IWFM2OBS` | PEST hydrograph converter |
| CalcTypeHyd | `IWFM_BUILD_CALCTYPHYD` | Cluster type hydrograph tool |
| ResultsExtract | `IWFM_BUILD_RESULTSEXTRACT` | Generalized hydrograph extractor |

## Using IWFM as a library

IWFM ships a C-callable shared library — `IWFM_C_x64.dll` on Windows, `libiwfm_c.so` on Linux — exporting the same `BIND(C)` symbols on both platforms. Any language with a C foreign-function interface can drive a model end-to-end: instantiate inputs, simulate (or load post-run results in inquiry mode), query state, and tear down.

Up to **64 concurrent model instances** coexist in one process. `IW_Model_New` returns an `iModelID`; every other `IW_Model_*` export takes that ID as its first argument. There is no shared "current model" / `IW_Model_Switch` indirection.

| Language | Binding mechanism | Example |
|----------|-------------------|---------|
| Python | `ctypes` (or via [pywfm](https://github.com/SGMOModeling/PyWFM)) | [`verify-dll-regression.py`](verify-dll-regression.py), [pywfm `IWFMModel`](https://github.com/SGMOModeling/PyWFM/blob/track7-handle-based-api/src/pywfm/model.py) |
| C# | P/Invoke (`[DllImport]`, `ref int` / `out int`) | — |
| MATLAB | `loadlibrary` / `calllib` | — |
| C / C++ | direct linkage against the import library | `SourceCode/IWFM_DLL/IWFM_Model_Exports_C.f90` (BIND(C) declarations) |
| R | `.C` / `.Call` via the shared library | — |
| Julia | `ccall` | — |
| Java | JNA or JNI | — |

Two repo-root scripts exercise the DLL ABI from Python and double as templates for downstream consumers: [`verify-dll-regression.py`](verify-dll-regression.py) (DLL-layer byte-identical regression vs. the sample model) and [`time-c2vsim-inquiry.py`](time-c2vsim-inquiry.py) (inquiry-mode cold/warm load timing).

**Breaking change in `v2025.0.1747-rev2`.** Bindings written against earlier IWFM C DLLs that called `IW_Model_Switch` or `IW_Model_GetCurrentModelID` will not link against this release — both exports were removed. Update bindings to (a) capture the `iModelID` returned by `IW_Model_New` and (b) pass it as the first argument to every subsequent `IW_Model_*` call. The pywfm [`track7-handle-based-api` branch](https://github.com/SGMOModeling/PyWFM/tree/track7-handle-based-api) is the reference implementation of the migration.

## Requirements

- **CMake** 3.20+
- **Fortran compiler:** Intel ifx (recommended) or GNU gfortran 9+
- **C compiler:** Intel icx, MSVC, GCC, or Clang
- **Build tool:** Ninja (recommended), Make, or Visual Studio

## Docker

```bash
docker build -t iwfm-runtime:2025.0 -f docker/Dockerfile.runtime .
docker run --rm -v /path/to/model:/data iwfm-runtime:2025.0 iwfm-simulation Simulation_MAIN.IN
```

## Fast inquiry-mode loads (pywfm / C-DLL users)

If you use the IWFM C DLL — directly, or through [pywfm](https://github.com/SGMOModeling/PyWFM) — to read simulation results in inquiry mode (`is_for_inquiry=1`), choose **`.hdf`** instead of `.out` or `.dss` for output filenames in your input files. The simulation writes HDF5 natively when the filename ends in `.hdf`; inquiry mode reads it directly with no conversion step.

When the output is `.out` (text) or `.dss`, the first inquiry-mode load after a fresh simulation has to convert it to a sibling `.hdf`. For C2VSimFG (30K nodes × 47 years) this conversion is roughly 15 seconds. Subsequent loads reuse the converted `.hdf` and are fast either way; only the first load suffers.

Output filenames you most likely want to set to `.hdf`:

| Input file | Entry name | Default | Recommended |
|------------|------------|---------|-------------|
| GW main file | `GWALLOUTFL` (heads at every node) | `*.out` | `*.hdf` |
| GW main file | `GWHYDOUTFL` (GW hydrographs at user-defined locations) | `*.out` | `*.hdf` |
| GW main file (subsidence section) | subsidence-at-every-node output | `*.out` | `*.hdf` |
| GW main file (tile drain section) | tile drain hydrograph output | `*.out` | `*.hdf` |
| Stream main file | stream hydrograph output | `*.out` | `*.hdf` |

The simulation also writes smaller binary HDF outputs by default for `*Bud*` (budget) and `*ZBud*` (zone budget) files — those are already HDF and need no change.

If you have an existing `.out`-based model and want fast cold loads without re-running the simulation, the conversion step is automatically performed once and cached, so the **second** inquiry-mode load against the same outputs is fast. The `IW_Model_DeleteInquiryDataFile` DLL function clears only the small `IW_ModelData_ForInquiry.bin` cache; the converted `.hdf` files remain valid as long as the source `.out` is unchanged.

## Documentation

- [BUILD.md](BUILD.md) -- Full build instructions, dependency management, and platform matrix
- [BENCHMARKS.md](BENCHMARKS.md) -- Performance benchmarks
- [CLAUDE.md](CLAUDE.md) -- Architecture reference and developer guide

## License

GPL-2.0. See [LICENSE](LICENSE).

## Contact

IWFMtechsupport@water.ca.gov
