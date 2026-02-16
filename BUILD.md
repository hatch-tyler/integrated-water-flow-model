# IWFM Build Instructions

This document provides detailed instructions for building IWFM (Integrated Water Flow Model) using the CMake build system.

## Table of Contents

- [Requirements](#requirements)
- [Quick Start](#quick-start)
- [Build Options](#build-options)
- [Platform-Specific Instructions](#platform-specific-instructions)
  - [Windows](#windows)
  - [Linux](#linux)
  - [macOS](#macos)
- [Build Generators](#build-generators)
- [Dependency Management](#dependency-management)
- [Advanced Configuration](#advanced-configuration)
- [Troubleshooting](#troubleshooting)

---

## Requirements

### Minimum Requirements

| Component | Requirement |
|-----------|-------------|
| CMake | 3.20 or later |
| Fortran Compiler | Intel ifx (recommended) or GNU gfortran 9+ |
| C Compiler | Intel icx, MSVC, GCC, or Clang |
| Build Tool | Ninja (recommended), Make, or Visual Studio |

### Supported Compilers

| Compiler | Platform | Status | Notes |
|----------|----------|--------|-------|
| Intel ifx | Windows, Linux | **Recommended** | Full feature support including Coarray |
| Intel ifort | Windows, Linux | Supported | Legacy Intel compiler |
| GNU gfortran | Linux, macOS | Supported | Requires v9+ for F2003 features |
| GNU gfortran | Windows (MinGW) | Experimental | May have issues with HDF5 |

### Platform Feature Matrix

| Feature | Windows (Intel) | Windows (MSVC+gfortran) | Linux (Intel) | Linux (gfortran) | macOS |
|---------|-----------------|-------------------------|---------------|------------------|-------|
| Simulation | Yes | Experimental | Yes | Yes | Yes |
| PreProcessor | Yes | Experimental | Yes | Yes | Yes |
| Budget | Yes | Experimental | Yes | Yes | Yes |
| ZBudget | Yes | Experimental | Yes | Yes | Yes |
| IWFM_C DLL | Yes | Experimental | Yes | Yes | Yes |
| OpenMP Parallel | Yes | Yes | Yes | Yes | Yes |
| Coarray (Multi-Model) | Yes | No | Yes | Requires OpenCoarrays | No |
| Static Linking | Yes | Partial | Yes | Yes | Partial |

---

## Quick Start

### Windows with Intel oneAPI (Recommended: Build-IWFM.ps1)

The easiest way to build on Windows is using the provided PowerShell script:

```powershell
# Navigate to IWFM source directory
cd C:\path\to\iwfm-2025.0.1747\src

# Build all targets (automatically sets up Intel oneAPI environment)
.\Build-IWFM.ps1

# Or build specific target
.\Build-IWFM.ps1 -Target Simulation

# Clean and rebuild
.\Build-IWFM.ps1 -Action Clean
.\Build-IWFM.ps1 -Action Build

# Build in Debug mode
.\Build-IWFM.ps1 -BuildType Debug
```

**Build-IWFM.ps1 Options:**

| Parameter | Values | Default | Description |
|-----------|--------|---------|-------------|
| `-Action` | Build, Clean, Configure, Test, Package, All | Build | Action to perform |
| `-Target` | Simulation, Simulation_Parallel, Simulation_MM, PreProcessor, Budget, ZBudget, IWFM_C_DLL, all | all | Specific target |
| `-BuildType` | Release, Debug, RelWithDebInfo | Release | Build configuration |
| `-BuildDir` | any path | build | Build directory name |
| `-Parallel` | switch | off | Enable OpenMP parallel simulation (Simulation_PLL) |
| `-Coarray` | switch | off | Enable Coarray multi-model (Simulation_MM, Intel only) |

**Build with OpenMP Parallel Support:**

```powershell
# Build all targets including parallel simulation
.\Build-IWFM.ps1 -Parallel

# Build only the parallel simulation target
.\Build-IWFM.ps1 -Target Simulation_Parallel -Parallel
```

**Build with Coarray Multi-Model Support (Intel only):**

```powershell
.\Build-IWFM.ps1 -Coarray
```

### Windows Manual Build (Alternative)

```powershell
# 1. Open Intel oneAPI Command Prompt (or run setvars.bat)
# 2. Navigate to IWFM source directory
cd C:\path\to\iwfm-2025.0.1747\src

# 3. Create and enter build directory
mkdir build
cd build

# 4. Configure with CMake (Ninja generator recommended)
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release -DCMAKE_Fortran_COMPILER=ifx -DCMAKE_C_COMPILER=icx

# 5. Build
cmake --build . --config Release

# 6. Executables are in src/Bin/
dir ..\Bin\*.exe
```

### Linux with Intel oneAPI

```bash
# 1. Load Intel oneAPI environment
source /opt/intel/oneapi/setvars.sh

# 2. Navigate to IWFM source directory
cd /path/to/iwfm-2025.0.1747/src

# 3. Create and enter build directory
mkdir build && cd build

# 4. Configure with CMake
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_Fortran_COMPILER=ifx \
    -DCMAKE_C_COMPILER=icx

# 5. Build
cmake --build .

# 6. Executables are in src/Bin/
ls -la ../Bin/
```

### Linux with gfortran

```bash
# 1. Ensure gfortran and gcc are installed
sudo apt install gfortran gcc cmake ninja-build  # Debian/Ubuntu
# or
sudo dnf install gcc-gfortran gcc cmake ninja-build  # Fedora/RHEL

# 2. Navigate to IWFM source directory
cd /path/to/iwfm-2025.0.1747/src

# 3. Create and enter build directory
mkdir build && cd build

# 4. Configure with CMake
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_Fortran_COMPILER=gfortran \
    -DCMAKE_C_COMPILER=gcc

# 5. Build
cmake --build .
```

---

## Build Options

### Target Selection

| Option | Default | Description |
|--------|---------|-------------|
| `IWFM_BUILD_SIMULATION` | ON | Build main Simulation executable |
| `IWFM_BUILD_PREPROCESSOR` | ON | Build PreProcessor executable |
| `IWFM_BUILD_BUDGET` | ON | Build Budget post-processor |
| `IWFM_BUILD_ZBUDGET` | ON | Build ZBudget post-processor |
| `IWFM_BUILD_DLL` | ON | Build IWFM C-interface DLL/shared library |
| `IWFM_BUILD_PARALLEL` | OFF | Build OpenMP parallel simulation |
| `IWFM_BUILD_COARRAY` | OFF | Build Coarray Fortran multi-model (Intel only) |

### Dependency Options

| Option | Default | Description |
|--------|---------|-------------|
| `IWFM_USE_SYSTEM_HDF5` | OFF | Use system-installed HDF5 instead of building from source |
| `IWFM_USE_SYSTEM_HECLIB` | OFF | Use system-installed heclib |
| `IWFM_HDF5_USE_BUNDLED` | OFF | Use bundled pre-built HDF5 libraries (Windows only) |
| `IWFM_HDF5_VERSION` | 1.14.3 | HDF5 version to download and build |

### Testing Options

| Option | Default | Description |
|--------|---------|-------------|
| `IWFM_BUILD_TESTS` | ON | Enable integration testing with sample model |
| `IWFM_SAMPLE_MODEL_DIR` | `../samplemodel` | Path to IWFM sample model for testing |

### Example: Minimal Build

Build only the Simulation executable without the DLL:

```bash
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release \
    -DIWFM_BUILD_PREPROCESSOR=OFF \
    -DIWFM_BUILD_BUDGET=OFF \
    -DIWFM_BUILD_ZBUDGET=OFF \
    -DIWFM_BUILD_DLL=OFF
```

### Example: Full Build with OpenMP

```bash
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release \
    -DIWFM_BUILD_PARALLEL=ON
```

### Example: Multi-Model Build with Coarray (Intel only)

```bash
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release \
    -DIWFM_BUILD_COARRAY=ON
```

---

## Platform-Specific Instructions

### Windows

#### Option 1: Ninja Generator (Recommended)

Ninja provides the fastest build times and handles Fortran module dependencies well.

**Prerequisites:**
- Intel oneAPI Base Toolkit + HPC Toolkit
- Ninja build tool (`winget install Ninja-build.Ninja`)
- CMake 3.20+ (`winget install Kitware.CMake`)

**Build Steps:**

```powershell
# Open "Intel oneAPI Command Prompt for Intel 64 for Visual Studio 2022"
# Or manually set up the environment:
& "C:\Program Files (x86)\Intel\oneAPI\setvars.bat"

cd C:\path\to\iwfm-2025.0.1747\src
mkdir build
cd build

# Configure
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release `
    -DCMAKE_Fortran_COMPILER=ifx `
    -DCMAKE_C_COMPILER=icx

# Build
cmake --build . --config Release
```

**Using a batch file:**

Create `build.bat`:
```batch
@echo off
call "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvars64.bat"
set "INTEL_COMPILER=C:\Program Files (x86)\Intel\oneAPI\compiler\latest"
set "PATH=%INTEL_COMPILER%\bin;%PATH%"
set "LIB=%INTEL_COMPILER%\lib;%LIB%"
set "INCLUDE=%INTEL_COMPILER%\include;%INCLUDE%"

cd /d "%~dp0"
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release -DCMAKE_Fortran_COMPILER=ifx -DCMAKE_C_COMPILER=icx
cmake --build . --config Release
```

#### Option 2: Visual Studio Generator

Visual Studio provides an IDE experience with debugging support.

```powershell
# From Intel oneAPI Command Prompt
cd C:\path\to\iwfm-2025.0.1747\src
mkdir build
cd build

# Configure for Visual Studio 2022
cmake .. -G "Visual Studio 17 2022" -A x64 `
    -DCMAKE_Fortran_COMPILER=ifx `
    -DCMAKE_C_COMPILER=icx

# Build from command line
cmake --build . --config Release

# Or open the solution in Visual Studio
start IWFM.sln
```

**Visual Studio Generator Notes:**
- Multi-configuration generator (Debug/Release selected at build time)
- Slower than Ninja for Fortran projects
- Better IDE integration for debugging
- May require manual Fortran compiler selection in project properties

#### Option 3: Bundled HDF5 Libraries (Fastest Windows Setup)

If pre-built HDF5 libraries are available in `SourceCode/IWFM-kernel/HDF5/`:

```powershell
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release `
    -DCMAKE_Fortran_COMPILER=ifx `
    -DCMAKE_C_COMPILER=icx `
    -DIWFM_HDF5_USE_BUNDLED=ON
```

This skips the HDF5 build step and uses pre-compiled libraries.

### Linux

#### Using Intel oneAPI

```bash
# Load Intel environment
source /opt/intel/oneapi/setvars.sh

cd /path/to/iwfm-2025.0.1747/src
mkdir build && cd build

# Configure with Ninja (recommended)
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_Fortran_COMPILER=ifx \
    -DCMAKE_C_COMPILER=icx

# Build
cmake --build .
```

#### Using gfortran

```bash
cd /path/to/iwfm-2025.0.1747/src
mkdir build && cd build

# Configure
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_Fortran_COMPILER=gfortran \
    -DCMAKE_C_COMPILER=gcc

# Build
cmake --build .
```

#### Using Docker (Intel Compilers)

For reproducible builds with Intel compilers:

```dockerfile
# Dockerfile.intel
FROM intel/oneapi-hpckit:latest

RUN apt-get update && apt-get install -y ninja-build

WORKDIR /src
COPY . .

RUN source /opt/intel/oneapi/setvars.sh && \
    mkdir build && cd build && \
    cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_Fortran_COMPILER=ifx \
        -DCMAKE_C_COMPILER=icx && \
    cmake --build .
```

Build with:
```bash
docker build -f Dockerfile.intel -t iwfm-builder .
docker run --rm -v $(pwd)/Bin:/src/Bin iwfm-builder
```

### macOS

**Note:** macOS does not support fully static executables. Some dynamic linking is unavoidable.

```bash
# Install dependencies
brew install cmake ninja gcc

cd /path/to/iwfm-2025.0.1747/src
mkdir build && cd build

# Configure with gfortran
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_Fortran_COMPILER=gfortran-13 \
    -DCMAKE_C_COMPILER=gcc-13

# Build
cmake --build .
```

---

## Build Generators

### Ninja (Recommended)

Ninja is the recommended generator for IWFM builds:

| Advantage | Description |
|-----------|-------------|
| Speed | Fastest build times, especially for incremental builds |
| Fortran Support | Handles Fortran module dependencies correctly |
| Cross-Platform | Works identically on Windows, Linux, and macOS |
| Parallel Builds | Automatically parallelizes compilation |

**Installation:**
```bash
# Windows
winget install Ninja-build.Ninja

# Linux (Debian/Ubuntu)
sudo apt install ninja-build

# Linux (Fedora/RHEL)
sudo dnf install ninja-build

# macOS
brew install ninja
```

**Usage:**
```bash
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release
cmake --build .
```

### Unix Makefiles

Standard Make generator, available on all Unix-like systems:

```bash
cmake .. -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release
make -j$(nproc)
```

**Parallel builds:**
```bash
make -j8  # Use 8 parallel jobs
```

### Visual Studio

Multi-configuration generator for Windows:

```powershell
# Visual Studio 2022
cmake .. -G "Visual Studio 17 2022" -A x64

# Visual Studio 2019
cmake .. -G "Visual Studio 16 2019" -A x64

# Build specific configuration
cmake --build . --config Release
cmake --build . --config Debug
```

### NMake (Windows)

Single-configuration generator using Microsoft NMake:

```powershell
# From Visual Studio Developer Command Prompt
cmake .. -G "NMake Makefiles" -DCMAKE_BUILD_TYPE=Release
nmake
```

---

## Dependency Management

IWFM depends on:
1. **zlib** - Compression library (for heclib)
2. **HDF5** - Hierarchical Data Format with Fortran bindings
3. **heclib** - HEC-DSS 7 C library for time series I/O

### Default Behavior (ExternalProject)

By default, CMake downloads and builds dependencies automatically:

```
Configure → Download → Build Dependencies → Build IWFM
```

**First build takes longer** due to HDF5 compilation (~5-10 minutes).
Subsequent builds reuse the compiled dependencies.

### HDF5 Options

| Method | Option | When to Use |
|--------|--------|-------------|
| ExternalProject | (default) | Cross-platform, automatic |
| System HDF5 | `IWFM_USE_SYSTEM_HDF5=ON` | HDF5 already installed with Fortran bindings |
| Bundled | `IWFM_HDF5_USE_BUNDLED=ON` | Windows with pre-built libraries |

**Using System HDF5:**
```bash
# Install HDF5 with Fortran bindings first
# Linux: sudo apt install libhdf5-fortran-dev
# or build HDF5 from source with -DHDF5_BUILD_FORTRAN=ON

cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release \
    -DIWFM_USE_SYSTEM_HDF5=ON
```

**Important:** System HDF5 must be built with the same Fortran compiler as IWFM to ensure module compatibility.

### Dependency Build Locations

| Dependency | Source | Build Location |
|------------|--------|----------------|
| zlib | FetchContent | `build/_deps/zlib-*` |
| HDF5 | ExternalProject | `build/hdf5-install/` |
| heclib | FetchContent | `build/_deps/heclib-*` |

---

## Advanced Configuration

### Debug Builds

```bash
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Debug
cmake --build .
```

Debug builds include:
- Bounds checking
- Runtime diagnostics
- Tracebacks on errors
- No optimization
- Debug symbols

Executables have `_D` suffix (e.g., `Simulation_x64_D.exe`).

### Release with Debug Info

```bash
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=RelWithDebInfo
cmake --build .
```

### Custom Installation

```bash
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX=/opt/iwfm

cmake --build .
cmake --install .
```

Installs to:
- `/opt/iwfm/bin/` - Executables
- `/opt/iwfm/lib/` - Libraries
- `/opt/iwfm/include/iwfm/` - Fortran module files

### Running Tests

If a sample model is available:

```bash
# Configure with test path
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release \
    -DIWFM_SAMPLE_MODEL_DIR=/path/to/samplemodel

# Build
cmake --build .

# Run tests
ctest -V
```

### Verbose Build Output

```bash
# Ninja
cmake --build . -- -v

# Make
cmake --build . -- VERBOSE=1
```

---

## Troubleshooting

### Common Issues

#### "ifx not found" or "icx not found"

**Cause:** Intel compiler environment not set up.

**Solution:**
```bash
# Linux
source /opt/intel/oneapi/setvars.sh

# Windows (PowerShell)
& "C:\Program Files (x86)\Intel\oneAPI\setvars.bat"
```

#### HDF5 `__float128` error (Windows + Intel icx)

**Cause:** Intel icx on Windows doesn't support `__float128` type.

**Solution:** The CMake configuration automatically patches HDF5 source files. If you see this error, ensure you're using the latest CMake configuration.

#### Linker errors: `__imp_fdopen`, `__imp_read` (Windows)

**Cause:** C runtime library mismatch between HDF5 and IWFM.

**Solution:** The CMake configuration sets `CMAKE_MSVC_RUNTIME_LIBRARY=MultiThreaded` for HDF5. Ensure both are using static CRT (`/MT`).

#### Undefined HDF5 symbols on Linux

**Cause:** Static library link order issues.

**Solution:** The CMake configuration uses `iwfm_link_hdf5()` function to apply correct linker flags. This is handled automatically for Unix builds.

#### "sortfiles.c conflicting types" (heclib)

**Cause:** K&R style C code incompatible with modern compilers.

**Solution:** The CMake configuration excludes problematic source files from heclib build.

#### Stack overflow with large models (C2VSimFG, etc.)

**Cause:** IWFM uses automatic (stack-allocated) arrays with runtime dimensions. For large models (30,000+ nodes), these arrays can exceed the stack limit.

**Solution:** The CMake configuration includes `/heap-arrays:900` (Windows) or `-heap-arrays=900` (Linux) to allocate arrays larger than 900KB on the heap instead of the stack. This is handled automatically in the current build configuration.

If you experience stack overflow with a custom build, ensure your compiler flags include:
- Intel Windows: `/heap-arrays:900`
- Intel Linux: `-heap-arrays=900`
- gfortran: `-fmax-stack-var-size=921600`

### Clean Rebuild

```bash
# Remove build directory completely
rm -rf build
mkdir build && cd build

# Reconfigure
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release
cmake --build .
```

### CMake Cache Issues

If configuration changes aren't taking effect:

```bash
# Delete CMake cache
rm -f CMakeCache.txt
rm -rf CMakeFiles/

# Reconfigure
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release
```

### Checking Compiler Detection

```bash
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release 2>&1 | grep -E "(Fortran|compiler|Detected)"
```

Expected output:
```
-- The Fortran compiler identification is IntelLLVM 2025.x.x
-- Detected Intel ifx compiler
```

### Verbose Dependency Information

```bash
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release 2>&1 | grep -E "(HDF5|heclib|zlib|IWFM)"
```

---

## Output Files

### Windows

| File | Description |
|------|-------------|
| `Bin/Simulation_x64.exe` | Main simulation |
| `Bin/Simulation_x64_D.exe` | Debug simulation |
| `Bin/Simulation_PLL_x64.exe` | OpenMP parallel simulation (with `-Parallel`) |
| `Bin/Simulation_MM_x64.exe` | Coarray multi-model (with `-Coarray`) |
| `Bin/PreProcessor_x64.exe` | Pre-processor |
| `Bin/Budget_x64.exe` | Budget post-processor |
| `Bin/ZBudget_x64.exe` | Zone budget post-processor |
| `Bin/IWFM_C_x64.dll` | C-callable DLL |

### Linux/macOS

| File | Description |
|------|-------------|
| `Bin/Simulation` | Main simulation |
| `Bin/Simulation_d` | Debug simulation |
| `Bin/Simulation_PLL` | OpenMP parallel simulation (with `-DIWFM_BUILD_PARALLEL=ON`) |
| `Bin/Simulation_MM` | Coarray multi-model (with `-DIWFM_BUILD_COARRAY=ON`) |
| `Bin/PreProcessor` | Pre-processor |
| `Bin/Budget` | Budget post-processor |
| `Bin/ZBudget` | Zone budget post-processor |
| `Bin/libiwfm_c.so` | C-callable shared library |

---

## Quick Reference

### Minimal Commands

```bash
# Configure (choose one)
cmake .. -G Ninja                                    # Auto-detect compilers
cmake .. -G Ninja -DCMAKE_Fortran_COMPILER=ifx      # Intel ifx
cmake .. -G Ninja -DCMAKE_Fortran_COMPILER=gfortran # GNU gfortran

# Build
cmake --build .

# Test (if sample model available)
ctest -V
```

### Common Configurations

```bash
# Release build (default)
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release

# Debug build
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Debug

# With OpenMP parallelization
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release -DIWFM_BUILD_PARALLEL=ON

# With Coarray multi-model (Intel only)
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release -DIWFM_BUILD_COARRAY=ON

# Using system HDF5
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release -DIWFM_USE_SYSTEM_HDF5=ON

# Using bundled HDF5 (Windows)
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release -DIWFM_HDF5_USE_BUNDLED=ON
```

---

## Docker Containers

IWFM provides Docker containers for both building and running simulations.

### Container Types

| Container | Image | Size | Purpose |
|-----------|-------|------|---------|
| Build Environment | `iwfm-build:latest` | ~5 GB | Development with Intel compilers |
| Runtime | `iwfm-runtime:2025.0` | ~100 MB | Running model simulations |

### Building the Runtime Container

```bash
cd src
docker build -t iwfm-runtime:2025.0 -f docker/Dockerfile.runtime .
```

### Running Simulations

```bash
# Show version info
docker run --rm iwfm-runtime:2025.0

# Run PreProcessor
docker run --rm -v /path/to/model/PreProcessor:/data \
    iwfm-runtime:2025.0 iwfm-preprocessor PreProcessor_MAIN.IN

# Run Simulation
docker run --rm -v /path/to/model/Simulation:/data \
    iwfm-runtime:2025.0 iwfm-simulation Simulation_MAIN.IN

# Run Budget
docker run --rm -v /path/to/model/Budget:/data \
    iwfm-runtime:2025.0 iwfm-budget Budget.in

# Run ZBudget
docker run --rm -v /path/to/model/ZBudget:/data \
    iwfm-runtime:2025.0 iwfm-zbudget ZBudget.in
```

### Using Docker Compose

```bash
cd src/docker

# Build runtime container
docker compose build iwfm-runtime

# Run simulation with custom model directory
MODEL_DIR=/path/to/model docker compose run --rm iwfm-simulation

# Run complete pipeline (PreProcessor → Simulation → Budget → ZBudget)
MODEL_DIR=/path/to/model docker compose run --rm iwfm-pipeline

# Interactive development shell
docker compose run --rm iwfm-shell
```

### Available Commands in Runtime Container

| Command | Description |
|---------|-------------|
| `iwfm-preprocessor` | Run PreProcessor |
| `iwfm-simulation` | Run Simulation |
| `iwfm-budget` | Run Budget post-processor |
| `iwfm-zbudget` | Run ZBudget post-processor |

See `docker/README.md` for detailed Docker usage instructions.

---

## Maintenance Guide

This section provides guidance for maintaining the CMake build system and updating dependencies.

### CMake Module Structure

```
src/
├── CMakeLists.txt              # Main CMake configuration
├── cmake/
│   ├── IWFMCompilerFlags.cmake     # Compiler detection and flags
│   ├── IWFMFetchDependencies.cmake # Dependency orchestration
│   ├── IWFMFetchZlib.cmake         # zlib configuration
│   ├── IWFMFetchHDF5.cmake         # HDF5 configuration
│   ├── IWFMFetchHeclib.cmake       # heclib configuration
│   └── IWFMFortranModules.cmake    # Fortran module helpers
└── SourceCode/
    └── CMakeLists.txt          # Target definitions
```

### Updating IWFM Version

Edit `CMakeLists.txt` line 12-15:
```cmake
project(IWFM
    VERSION 2025.0.1747    # <- Update version here
    DESCRIPTION "Integrated Water Flow Model"
    LANGUAGES Fortran C
)
```

The version is also defined in `SourceCode/IWFM_Version/IWFM_Version.f90` - both should be kept in sync.

### Updating HDF5 Version

Edit `cmake/IWFMFetchHDF5.cmake`:
```cmake
set(IWFM_HDF5_VERSION "1.14.3" CACHE STRING "HDF5 version to fetch")
```

**Considerations when updating HDF5:**
1. **API compatibility** - HDF5 maintains backward compatibility, but verify Fortran API hasn't changed
2. **CMake compatibility** - HDF5's CMake files sometimes have bugs; test thoroughly
3. **`__float128` support** - Windows+Intel may need patching (see `HDF5_PATCH_COMMAND` in `IWFMFetchHDF5.cmake`)
4. **Fortran module compatibility** - HDF5 must be built with same compiler as IWFM

**Testing a new HDF5 version:**
```bash
rm -rf build
mkdir build && cd build
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release -DIWFM_HDF5_VERSION=1.14.5
cmake --build .
# Run integration tests
ctest -V
```

### Updating heclib (HEC-DSS)

The heclib library is fetched from: `https://github.com/HydrologicEngineeringCenter/hec-dss.git`

Edit `cmake/IWFMFetchHeclib.cmake` to change the version:
```cmake
FetchContent_Declare(heclib
    GIT_REPOSITORY https://github.com/HydrologicEngineeringCenter/hec-dss.git
    GIT_TAG        main    # <- Change to specific tag/commit for reproducibility
    GIT_SHALLOW    TRUE
)
```

**Considerations when updating heclib:**
1. **Source directory structure** - May change between versions; check `HECLIB_SRC_DIR` paths
2. **K&R style C code** - Some files may need exclusion (see `list(FILTER ...)` in the cmake file)
3. **Header locations** - Verify `HECLIB_HEADERS_DIR` paths are correct

### Updating zlib Version

Edit `cmake/IWFMFetchZlib.cmake`:
```cmake
FetchContent_Declare(zlib
    GIT_REPOSITORY https://github.com/madler/zlib.git
    GIT_TAG        v1.3.1    # <- Update version tag
    GIT_SHALLOW    TRUE
)
```

zlib is stable and rarely needs updates.

### Adding a New Compiler

To add support for a new Fortran compiler, edit `cmake/IWFMCompilerFlags.cmake`:

```cmake
# 1. Add detection
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "NewCompiler")
    set(IWFM_USING_NEWCOMPILER TRUE)
    message(STATUS "Detected NewCompiler")

# 2. Add compiler flags
elseif(IWFM_USING_NEWCOMPILER)
    target_compile_options(iwfm_compiler_flags INTERFACE
        # Add flags here
    )
```

**Required flags for IWFM:**
- Preprocessor (`-cpp`, `-fpp`, or equivalent)
- Free-form line length (`-ffree-line-length-none` or equivalent)
- Disable LHS reallocation (`-fno-realloc-lhs` or equivalent)
- Position-independent code for shared library (`-fPIC`)

### Adding a New Build Target

To add a new executable or library:

1. Edit `SourceCode/CMakeLists.txt`:
```cmake
if(IWFM_BUILD_NEWTARGET)
    add_executable(NewTarget
        NewTarget/main.f90
    )
    target_link_libraries(NewTarget PRIVATE iwfm_model)
    iwfm_set_output_name(NewTarget "NewTarget")
    if(UNIX)
        iwfm_link_hdf5(NewTarget)
    endif()
    install(TARGETS NewTarget RUNTIME DESTINATION bin)
endif()
```

2. Add option to main `CMakeLists.txt`:
```cmake
option(IWFM_BUILD_NEWTARGET "Build new target" OFF)
```

### Adding a New Dependency

1. Create `cmake/IWFMFetchNewDep.cmake`:
```cmake
include_guard(GLOBAL)
include(FetchContent)

FetchContent_Declare(newdep
    GIT_REPOSITORY https://github.com/org/newdep.git
    GIT_TAG        v1.0.0
    GIT_SHALLOW    TRUE
)
FetchContent_MakeAvailable(newdep)

# Create alias target
add_library(NEWDEP::NEWDEP ALIAS newdep)
```

2. Include in `cmake/IWFMFetchDependencies.cmake`:
```cmake
include(IWFMFetchNewDep)

# Link to iwfm_dependencies
target_link_libraries(iwfm_dependencies INTERFACE NEWDEP::NEWDEP)
```

### Platform-Specific Patches

When a dependency needs platform-specific modifications:

```cmake
# Example: Patching source files
set(PATCH_COMMAND "")
if(WIN32 AND CMAKE_C_COMPILER_ID MATCHES "IntelLLVM")
    set(PATCH_SCRIPT "${CMAKE_BINARY_DIR}/patch_dep.cmake")
    file(WRITE ${PATCH_SCRIPT} "
        # CMake script to modify source files
        file(READ \"\${SOURCE_DIR}/file.c\" CONTENT)
        string(REPLACE \"old\" \"new\" CONTENT \"\${CONTENT}\")
        file(WRITE \"\${SOURCE_DIR}/file.c\" \"\${CONTENT}\")
    ")
    set(PATCH_COMMAND ${CMAKE_COMMAND} -DSOURCE_DIR=<SOURCE_DIR> -P ${PATCH_SCRIPT})
endif()

ExternalProject_Add(dep
    ...
    PATCH_COMMAND ${PATCH_COMMAND}
    ...
)
```

### Testing Build Changes

Before committing build system changes:

1. **Clean build test:**
```bash
rm -rf build && mkdir build && cd build
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release
cmake --build .
```

2. **Test all configurations:**
```bash
# Debug
cmake .. -DCMAKE_BUILD_TYPE=Debug && cmake --build .

# All targets
cmake .. -DIWFM_BUILD_PARALLEL=ON -DIWFM_BUILD_COARRAY=ON
cmake --build .
```

3. **Test on multiple platforms** (if possible)

4. **Verify executables run:**
```bash
./Bin/Simulation -about
./Bin/PreProcessor -about
```

### Debugging CMake Issues

**Print variable values:**
```cmake
message(STATUS "Variable: ${VARIABLE_NAME}")
```

**Print all variables:**
```cmake
get_cmake_property(_variableNames VARIABLES)
foreach(_variableName ${_variableNames})
    message(STATUS "${_variableName}=${${_variableName}}")
endforeach()
```

**Verbose configuration:**
```bash
cmake .. --trace-expand 2>&1 | grep -i "hdf5"
```

**Check target properties:**
```cmake
get_target_property(_libs some_target INTERFACE_LINK_LIBRARIES)
message(STATUS "Target libs: ${_libs}")
```

### Bundled Library Updates (Windows)

If updating pre-built Windows libraries in `SourceCode/IWFM-kernel/HDF5/`:

1. Build new HDF5 version with matching compiler/flags
2. Copy libraries to appropriate directories:
   - `HDF5/x64/Release/` - 64-bit release libraries
   - `HDF5/x64/Debug/` - 64-bit debug libraries (with `_D` suffix)
3. Copy `.mod` files to same directories
4. Test with `IWFM_HDF5_USE_BUNDLED=ON`

**Required HDF5 libraries:**
- `libhdf5.lib` / `libhdf5_D.lib`
- `libhdf5_fortran.lib` / `libhdf5_fortran_D.lib`
- `libhdf5_f90cstub.lib` / `libhdf5_f90cstub_D.lib`

**Required HDF5 modules (.mod files):**
- `hdf5.mod`
- `h5fortran_types.mod`
- `h5global.mod`
- `h5a.mod`, `h5d.mod`, `h5e.mod`, `h5f.mod`, `h5g.mod`, `h5i.mod`, `h5l.mod`, `h5lib.mod`, `h5o.mod`, `h5p.mod`, `h5r.mod`, `h5s.mod`, `h5t.mod`, `h5vl.mod`, `h5z.mod`

### CI/CD Integration

Example GitHub Actions workflow:

```yaml
# .github/workflows/build.yml
name: Build IWFM

on: [push, pull_request]

jobs:
  build-linux:
    runs-on: ubuntu-latest
    container: intel/oneapi-hpckit:latest
    steps:
      - uses: actions/checkout@v4

      - name: Install Ninja
        run: apt-get update && apt-get install -y ninja-build

      - name: Configure
        run: |
          source /opt/intel/oneapi/setvars.sh
          mkdir build && cd build
          cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release \
            -DCMAKE_Fortran_COMPILER=ifx -DCMAKE_C_COMPILER=icx

      - name: Build
        run: |
          source /opt/intel/oneapi/setvars.sh
          cd build && cmake --build .

      - name: Test
        run: |
          ./Bin/Simulation -about

  build-windows:
    runs-on: windows-latest
    steps:
      - uses: actions/checkout@v4

      # Install Intel compilers via installer or use MSVC+gfortran
      - name: Configure
        run: |
          mkdir build && cd build
          cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release

      - name: Build
        run: cd build && cmake --build .
```

---

## Support

For technical support, contact: IWFMtechsupport@water.ca.gov

For build system issues, refer to the CMake files in `src/cmake/`:
- `IWFMCompilerFlags.cmake` - Compiler-specific settings
- `IWFMFetchDependencies.cmake` - Dependency management
- `IWFMFetchHDF5.cmake` - HDF5 configuration
- `IWFMFetchHeclib.cmake` - heclib configuration
- `IWFMFetchZlib.cmake` - zlib configuration
