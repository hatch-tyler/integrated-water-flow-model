# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

IWFM (Integrated Water Flow Model) is a hydrological simulation software developed by the California Department of Water Resources. Version 2025.0.1747 (kernel 2025.0.107). Written in modern Fortran (F90/F2003), licensed GPL v2.

## Build Commands

### Windows (Recommended: PowerShell script)

```powershell
# Build all targets (auto-configures Intel oneAPI environment)
.\Build-IWFM.ps1

# Build specific target
.\Build-IWFM.ps1 -Target Simulation

# Debug build
.\Build-IWFM.ps1 -BuildType Debug

# With OpenMP parallel
.\Build-IWFM.ps1 -Parallel

# Clean and rebuild
.\Build-IWFM.ps1 -Action Clean
.\Build-IWFM.ps1 -Action Build
```

### Windows (batch file, must use cmd.exe not Git Bash)

```batch
build_windows.bat build
build_windows.bat clean
build_windows.bat rebuild
```

### CMake (Cross-Platform)

```bash
mkdir build && cd build

# Intel ifx (recommended)
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release -DCMAKE_Fortran_COMPILER=ifx -DCMAKE_C_COMPILER=icx

# gfortran
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_C_COMPILER=gcc

# Build all
cmake --build .

# Build single target
cmake --build . --target Simulation
```

### Testing

Integration tests require a sample model directory at `../samplemodel`:

```bash
cd build
cmake .. -DIWFM_SAMPLE_MODEL_DIR=/path/to/samplemodel
cmake --build .
ctest -V
```

Tests run sequentially: PreProcessor → Simulation → Budget/ZBudget.

A standalone DSS diagnostic test exists at `SourceCode/tests/test_dss_read.f90` (built as `test_dss_read` when `IWFM_BUILD_TESTS=ON`).

### CMake Options

| Option | Default | Description |
|--------|---------|-------------|
| `IWFM_BUILD_SIMULATION` | ON | Main simulation executable |
| `IWFM_BUILD_PREPROCESSOR` | ON | PreProcessor executable |
| `IWFM_BUILD_BUDGET` | ON | Budget post-processor |
| `IWFM_BUILD_ZBUDGET` | ON | ZBudget post-processor |
| `IWFM_BUILD_DLL` | ON | C-callable shared library |
| `IWFM_BUILD_PARALLEL` | OFF | OpenMP parallel simulation |
| `IWFM_BUILD_COARRAY` | OFF | Coarray multi-model (Intel only) |
| `IWFM_USE_SYSTEM_HDF5` | OFF | Use system HDF5 instead of building from source |
| `IWFM_USE_SYSTEM_HECLIB` | OFF | Use system heclib |

Output binaries go to `Bin/`. Windows names: `*_x64.exe` (release), `*_x64_D.exe` (debug). Linux: no suffix (release), `*_d` (debug).

## Architecture

### Entry Points

- `SourceCode/Simulation/Iwfm_f2.f90` → `PROGRAM IWFM_F2` (standard simulation)
- `SourceCode/PreProcessor/Iwfm_f1.f90` → `PROGRAM IWFM_F1` (pre-processor)
- `SourceCode/Simulation_MM/IWFM_f2_MM.f90` → Multi-model via Fortran co-arrays
- `SourceCode/Budget/Budget.f90` → Budget post-processor
- `SourceCode/ZBudget/ZBudget_Main.f90` → Zone budget post-processor

### Central Orchestrator: ModelType

`SourceCode/Package_Model/Package_Model.f90` defines `ModelType`, the central class that composes all simulation components:

- **Physical Components:** `AppGWType` (groundwater), `AppStreamType` (streams/rivers), `AppLakeType` (lakes), `RootZoneType` (land use), `AppUnsatZoneType` (unsaturated zone), `AppSmallWatershedType`
- **Grid & Solver:** `AppGridType` (FE discretization), `StratigraphyType`, `MatrixType` (sparse linear solver)
- **Connectors:** `StrmGWConnectorType`, `LakeGWConnectorType`, `StrmLakeConnectorType`, `SupplyDestinationConnectorType` — these mediate data exchange between loosely-coupled physical components
- **Support:** `PrecipitationType`, `ETType`, `ConvergenceType`, `SupplyAdjustmentType`, `WSA_ANN_Type` (artificial neural network for water supply availability)

### Versioning Pattern (Abstract Base + Version Subdirectories)

Many packages use **abstract base classes** with **version-specific implementations** in subdirectories. This enables runtime model version selection while maintaining consistent interfaces.

Example — `Package_AppStream`:
- **Base:** `Class_BaseAppStream.f90` → `TYPE,ABSTRACT :: BaseAppStreamType`
- **Versions:** `VERSION_4.0/`, `VERSION_4.1/`, `VERSION_4.2/`, `VERSION_4.2_WSA/`, `VERSION_4.21/`, `VERSION_5.0/`
  - Each contains e.g. `Class_AppStream_v40.f90` → `TYPE,EXTENDS(BaseAppStreamType) :: AppStream_v40_Type`

Packages with versions:
- `Package_AppStream` — 6 versions (4.0–5.0)
- `Package_RootZone` — 7 versions (4.0–5.0)
- `Package_AppGW/Package_AppSubsidence` — versions 4.0, 5.0

### Core Packages (in `SourceCode/IWFM-kernel/`)

**Physical Model Components:**
- `Package_AppGW` — Groundwater: pumping, boundary conditions, subsidence, tile drains
- `Package_AppStream` — Stream/river network, diversions, bypasses
- `Package_AppLake` — Lake simulation
- `Package_RootZone` — Root zone water balance, land use (agricultural, urban, native/riparian)
- `Package_AppUnsatZone` — Unsaturated zone
- `Package_AppSmallWatershed` — Small watershed modeling

**Infrastructure:**
- `Package_Discretization` — Finite element grid, stratigraphy, node/element management
- `Package_Matrix` — Sparse matrix solver (GMRES via `pgmres.f90`, LU via `Ludcmp.f90`/`Lubksb.f90`). Key arrays: `COEFF()`, `RHS()`, `HDelta()` (solution), `NJD()`/`JND()` (sparse structure)
- `Package_ComponentConnectors` — Stream-GW, Lake-GW, Stream-Lake inter-component connectors
- `Package_Supply` — Water supply management and allocation
- `Package_PrecipitationET` — Climate data handling
- `Package_Budget` / `Package_ZBudget` / `Package_GWZBudget` — Water budget accounting
- `Package_Miscellaneous` — Constants, flags, utility types

**Utilities (`IWFM_Util/`):**
- `IOInterface/` — Polymorphic file I/O: `Class_BaseFileType.f90` (abstract), `Class_AsciiFileType.f90`, `Class_HDF5FileType.f90`, `Class_DSSFileType.f90`, `Class_FortBinaryFileType.f90`
- `Utilities/` — `GeneralUtilities.f90` (string/array/file utils), `MessageLogger.f90` (logging), `TimeSeriesUtilities.f90` (time step management), `Class_Version.f90`, `GenericLinkedList.f90`, `Class_BinaryTree.f90`

### HEC-DSS Interface

`SourceCode/IWFM-kernel/heclib_interface/` bridges Fortran to HEC-DSS 7 C library:
- `heclib_c_binding.f90` — `ISO_C_BINDING` interfaces to C functions (zopen, zclose, ztsRetrieve, ztsStore)
- `heclib_compat.f90` — Backward-compatible wrappers using legacy names (ZOPEN, ZCLOSE, ZSRTS, ZRRTS)

**Critical:** HEC-DSS 7 uses `INTEGER(C_LONG_LONG) :: IFLTAB(250)` instead of legacy `INTEGER :: IFLTAB(600)`.

### DLL Interface (C-callable API)

`SourceCode/IWFM_DLL/` exports functions via `BIND(C,NAME='...')`:
- `IWFM_Model_Exports_C.f90` — Model control (supports up to 10 concurrent instances)
- `IWFM_Budget_Exports_C.f90` — Budget queries
- `IWFM_ZBudget_Exports_C.f90` — Zone budget queries
- `IWFM_Misc_Exports_C.f90` — Utilities

### WSA_ANN Component

`SourceCode/WSA_ANN/WSA_ANN.f90` — Artificial Neural Network module for water supply availability analysis. Integrated into `ModelType` as `WSA_ANN_Type`. Outputs to DSS files.

### Version Management

- `SourceCode/IWFM_Version/IWFM_Version.f90` — IWFM application version (`IWFMVersionType`)
- `SourceCode/IWFM-kernel/IWFM_Kernel_Version/` — Kernel version tracking
- CMake version in `CMakeLists.txt` line 13 — keep in sync with `IWFM_Version.f90`

## Build System Internals

### CMake Module Structure (`cmake/`)

- `IWFMCompilerFlags.cmake` — Compiler detection (Intel ifx/ifort, gfortran) and flags. Creates `iwfm_compiler_flags` interface library
- `IWFMFetchDependencies.cmake` — Dependency orchestration
- `IWFMFetchHDF5.cmake` — HDF5 via ExternalProject (first build ~5-10 min)
- `IWFMFetchHeclib.cmake` — heclib from HEC-DSS GitHub repo
- `IWFMFetchZlib.cmake` — zlib via FetchContent
- `IWFMFortranModules.cmake` — Fortran `.mod` file management, compilation order helpers

### Library Build Graph

```
iwfm_compiler_flags (interface)
    └── iwfm_kernel (static lib: all IWFM-kernel/ sources)
        ├── iwfm_version (static)
        ├── iwfm_wsa_ann (static)
        └── iwfm_model (static: Package_Model/)
            ├── Simulation, PreProcessor, etc. (executables)
            └── IWFM_C_DLL (shared library)
```

All Fortran `.mod` files go to `${CMAKE_BINARY_DIR}/modules`.

### Key Compiler Flags

- `/fp:consistent` / `-fp-model=consistent` — Reproducible floating point
- `/assume:norealloc_lhs` / `-assume norealloc_lhs` — Disable LHS automatic reallocation
- `/heap-arrays:900` / `-heap-arrays=900` — Arrays >900KB on heap (prevents stack overflow for large models like C2VSimFG with 30K+ nodes)
- `-std=legacy` (gfortran only) — Allows obsolete features in solver code (PAUSE, ASSIGN)
- 256MB stack configured via linker flags

### Fortran Source Order Matters

Source files in `SourceCode/IWFM-kernel/CMakeLists.txt` are ordered by module dependency (utilities → base classes → derived classes). Ninja auto-handles this, but the order matters for Make/VS generators. If you get "module not found" errors, check source order.

## Runtime

Executables prompt for input file paths interactively. Use `-about` flag for version info.

Log files created in working directory: `SimulationMessages.out`, `PreprocessorMessages.out`, `BudgetMessages.out`, `ZBudgetMessages.out`.

## Docker

Runtime container available for running simulations without compilers:

```bash
docker build -t iwfm-runtime:2025.0 -f docker/Dockerfile.runtime .
docker run --rm -v /path/to/model/Simulation:/data iwfm-runtime:2025.0 iwfm-simulation Simulation_MAIN.IN
```

See `docker/README.md` and `docker/docker-compose.yml` for full pipeline support.

## Technical Support

IWFMtechsupport@water.ca.gov
