# IWFM -- Integrated Water Flow Model

A finite-element hydrological simulation model for conjunctive management of groundwater and surface water resources.

## Overview

IWFM simulates the flow of water through an integrated hydrologic system -- groundwater, streams, lakes, root zone, and unsaturated zone -- under historical or projected conditions. Developed by the California Department of Water Resources (DWR), it is used for water resources planning, groundwater sustainability analysis, and regulatory compliance across California.

Version **2025.0.1747** (kernel 2025.0.107). Written in modern Fortran (F90/F2003).

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
