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

## Documentation

- [BUILD.md](BUILD.md) -- Full build instructions, dependency management, and platform matrix
- [BENCHMARKS.md](BENCHMARKS.md) -- Performance benchmarks
- [CLAUDE.md](CLAUDE.md) -- Architecture reference and developer guide

## License

GPL-2.0. See [LICENSE](LICENSE).

## Contact

IWFMtechsupport@water.ca.gov
