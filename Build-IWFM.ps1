<#
.SYNOPSIS
    Master build script for IWFM on Windows.

.DESCRIPTION
    This script provides a unified interface for building, testing, and packaging IWFM.
    It handles all environment setup automatically.

.PARAMETER Action
    The action to perform:
    - Build: Build IWFM executables (default)
    - Clean: Remove build artifacts
    - Configure: Only configure CMake
    - Test: Run tests
    - Package: Create distribution packages
    - All: Build and package

.PARAMETER Target
    Specific target to build (Simulation, PreProcessor, Budget, ZBudget, IWFM_C_DLL, IWFM_C_DLL_Heap).

.PARAMETER BuildType
    Build configuration (Release, Debug, RelWithDebInfo). Default: Release

.PARAMETER BuildDir
    Build directory name. Default: build

.PARAMETER Parallel
    Enable OpenMP parallel simulation build (Simulation_PLL).

.PARAMETER Coarray
    Enable Coarray Fortran multi-model build (Simulation_MM). Intel compiler only.

.PARAMETER Library
    Use the legacy library-mode build (single shared iwfm_kernel.lib for
    every target). The default is monolithic — each executable compiles
    its own kernel sources. Use library mode only if you need to match
    the historical CMake build layout; it forces /Qopenmp onto the
    kernel whenever the parallel/DLL targets are also built, which
    causes the serial Simulation exe to inherit a live OpenMP runtime.

.PARAMETER FortranCompiler
    Select a specific Intel Fortran compiler (ifx) instead of the newest
    system oneAPI install. Use this to match the compiler DWR used for an
    official release (listed on the CNRA data portal per release, e.g.
    IWFM 2026.0.1848 = ifx 2026.1.1); different ifx versions give results
    that differ at round-off level. Accepts:
      - a system oneAPI version folder name, e.g. "2025.3"
        (resolved under "C:\Program Files (x86)\Intel\oneAPI\compiler")
      - a path to ifx.exe
      - an install directory: a oneAPI compiler version folder, or a
        conda prefix (conda install -p <dir> -c conda-forge
        ifx_impl_win-64=<version>), which keeps ifx in Library\bin
    The C compiler (icx) still comes from the newest system oneAPI; it
    builds only the C dependencies (HDF5, heclib, zlib).
    Unless -BuildDir is also given, the build directory defaults to
    "build-ifx-<version>" so each compiler keeps its own CMake cache.
    Executables still go to Bin\.

.EXAMPLE
    .\Build-IWFM.ps1
    Builds all targets in Release mode.

.EXAMPLE
    .\Build-IWFM.ps1 -FortranCompiler C:\Users\me\ifx-2026.1.1
    Builds with ifx 2026.1.1 from a conda prefix, in build-ifx-2026.1.1\.

.EXAMPLE
    .\Build-IWFM.ps1 -Parallel
    Builds all targets including the OpenMP parallel simulation.

.EXAMPLE
    .\Build-IWFM.ps1 -Action Clean
    Cleans the build directory.

.EXAMPLE
    .\Build-IWFM.ps1 -Action Package
    Creates distribution packages.

.EXAMPLE
    .\Build-IWFM.ps1 -Action All
    Builds and packages everything.

.EXAMPLE
    .\Build-IWFM.ps1 -Target Simulation -BuildType Debug
    Builds only Simulation in Debug mode.

.NOTES
    Author: California Department of Water Resources
    License: GPL-2.0
#>

[CmdletBinding()]
param(
    [Parameter(Position = 0)]
    [ValidateSet("Build", "Clean", "Configure", "Test", "Package", "All")]
    [string]$Action = "Build",

    [ValidateSet("Simulation", "Simulation_Parallel", "Simulation_MM", "PreProcessor", "Budget", "ZBudget", "IWFM_C_DLL", "IWFM_C_DLL_Heap", "IWFM2OBS", "CalcTypeHyd", "ResultsExtract", "test_dss_read", "test_inquiry_load", "all")]
    [string]$Target = "all",

    [ValidateSet("Release", "Debug", "RelWithDebInfo")]
    [string]$BuildType = "Release",

    [string]$BuildDir = "build",

    [switch]$Parallel,

    [switch]$Coarray,

    [switch]$IWFM2OBS,

    [switch]$CalcTypeHyd,

    [switch]$ResultsExtract,

    # Use the library-mode build (single shared iwfm_kernel.lib for every
    # target). The default is the monolithic build (each executable
    # compiles its own kernel sources), which is the only build mode that
    # honestly separates serial from parallel: in library mode, /Qopenmp is
    # forced onto iwfm_kernel.lib whenever any of IWFM_BUILD_PARALLEL/
    # IWFM_BUILD_COARRAY/IWFM_BUILD_DLL is on, and the serial Simulation
    # exe inherits a live OpenMP runtime. See SourceCode/IWFM-kernel/
    # CMakeLists.txt:45-59 and cmake/IWFMMonolithic.cmake for the rationale.
    [switch]$Library,

    [ValidateSet("Ninja", "NMake")]
    [string]$Generator = "Ninja",

    [switch]$UsePrebuiltDeps,

    [string]$DepsDir = "deps",

    # Specific ifx to use (version folder name, ifx.exe path, or install
    # dir incl. conda prefix). Empty = newest system oneAPI. See help.
    [string]$FortranCompiler = ""
)

$ErrorActionPreference = "Stop"
$script:ExitCode = 0

# Paths
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$SourceDir = $ScriptDir
$BuildPath = Join-Path $SourceDir $BuildDir
$BinDir = Join-Path $SourceDir "Bin"

# Banner
Write-Host ""
Write-Host "  _____ _    _ ______ __  __" -ForegroundColor Cyan
Write-Host " |_   _| |  | |  ____|  \/  |" -ForegroundColor Cyan
Write-Host "   | | | |  | | |__  | \  / |" -ForegroundColor Cyan
Write-Host "   | | | |/\| |  __| | |\/| |" -ForegroundColor Cyan
Write-Host "  _| |_\  /\  / |    | |  | |" -ForegroundColor Cyan
Write-Host " |_____/\/  \/|_|    |_|  |_|" -ForegroundColor Cyan
Write-Host ""
Write-Host "  Integrated Water Flow Model" -ForegroundColor White
Write-Host "  Build System v2026.0" -ForegroundColor Gray
Write-Host ""

function Select-FortranCompiler {
    # Resolve -FortranCompiler to an ifx.exe and put its bin/lib/include
    # directories ahead of the system oneAPI ones.
    param([string]$Spec, [string]$OneAPIRoot)

    $ifx = $null
    if ($Spec -match '^\d{4}\.\d+(\.\d+)?$') {
        $cand = Join-Path $OneAPIRoot "compiler\$Spec\bin\ifx.exe"
        if (Test-Path $cand) { $ifx = $cand }
        else {
            $have = (Get-ChildItem -Path "$OneAPIRoot\compiler" -Directory |
                     Where-Object { $_.Name -match '^\d+\.\d+' } | ForEach-Object Name) -join ', '
            throw "ifx $Spec not found under $OneAPIRoot\compiler (installed: $have). Pass a path to another install instead."
        }
    } elseif (Test-Path $Spec -PathType Leaf) {
        $ifx = (Resolve-Path $Spec).Path
    } elseif (Test-Path $Spec -PathType Container) {
        foreach ($rel in @("bin\ifx.exe", "Library\bin\ifx.exe")) {
            $cand = Join-Path $Spec $rel
            if (Test-Path $cand) { $ifx = (Resolve-Path $cand).Path; break }
        }
        if (-not $ifx) { throw "No ifx.exe in $Spec\bin or $Spec\Library\bin" }
    } else {
        throw "-FortranCompiler '$Spec' is not an installed version, an ifx.exe path, or a directory"
    }

    $binDir = Split-Path -Parent $ifx
    $root   = Split-Path -Parent $binDir          # <ver> (system) or <prefix>\Library (conda)
    $libDir = Join-Path $root "lib"

    # Intrinsic modules (iso_c_binding.modintr, ...) live in
    # opt\compiler\include[\intel64]: under <ver>\ for a system install,
    # under the prefix root (one level above Library\) for conda.
    $incDirs = @()
    foreach ($base in @($root, (Split-Path -Parent $root))) {
        $inc = Join-Path $base "opt\compiler\include"
        if (Test-Path (Join-Path $inc "intel64\iso_c_binding.modintr")) {
            $incDirs = @((Join-Path $inc "intel64"), $inc)
            break
        }
    }
    if (-not $incDirs) {
        throw "Intrinsic Fortran modules (opt\compiler\include\intel64\iso_c_binding.modintr) not found for $ifx"
    }

    $script:IFX = $ifx
    $env:PATH    = "$binDir;$env:PATH"
    $env:LIB     = "$libDir;$env:LIB"
    $env:INCLUDE = (($incDirs -join ';') + ";$env:INCLUDE")
}

function Assert-CacheCompiler {
    # Refuse to reuse a build directory configured with a different ifx:
    # Invoke-Build only configures when there is no cache, so the old
    # compiler would otherwise be used silently.
    $cache = Join-Path $BuildPath "CMakeCache.txt"
    if (-not (Test-Path $cache)) { return }
    $line = Select-String -Path $cache -Pattern '^CMAKE_Fortran_COMPILER:[A-Z]+=(.*)$' | Select-Object -First 1
    if (-not $line) { return }
    $cached = [System.IO.Path]::GetFullPath($line.Matches[0].Groups[1].Value.Trim())
    $wanted = [System.IO.Path]::GetFullPath($script:IFX)
    if ($cached -ine $wanted) {
        throw ("Build directory '$BuildPath' was configured with`n    $cached`n" +
               "  but this run selects`n    $wanted`n" +
               "  Use a different -BuildDir, or run -Action Clean on that directory first.")
    }
}

function Initialize-BuildEnvironment {
    Write-Host "Initializing build environment..." -ForegroundColor Yellow

    # Find Visual Studio
    $VSWhere = "${env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"
    if (-not (Test-Path $VSWhere)) {
        throw "Visual Studio not found. Install Visual Studio 2022 with C++ tools."
    }

    $script:VSInstallPath = & $VSWhere -latest -property installationPath
    $VCVarsAll = Join-Path $script:VSInstallPath "VC\Auxiliary\Build\vcvars64.bat"

    # Find Intel oneAPI
    $OneAPIRoot = "${env:ProgramFiles(x86)}\Intel\oneAPI"
    $OneAPISetVars = Join-Path $OneAPIRoot "setvars.bat"

    if (-not (Test-Path $OneAPISetVars)) {
        throw "Intel oneAPI not found. Install Intel oneAPI HPC Toolkit."
    }

    # Find Intel compiler version directory
    $IntelCompilerDir = Get-ChildItem -Path "$OneAPIRoot\compiler" -Directory |
        Where-Object { $_.Name -match '^\d+\.\d+' } |
        Sort-Object Name -Descending |
        Select-Object -First 1

    if (-not $IntelCompilerDir) {
        throw "Intel compiler directory not found."
    }

    $script:IntelLibPath = Join-Path $IntelCompilerDir.FullName "lib"
    $script:IntelBinPath = Join-Path $IntelCompilerDir.FullName "bin"

    # Find compilers (full paths)
    $script:IFX = Join-Path $script:IntelBinPath "ifx.exe"
    $script:ICX = Join-Path $script:IntelBinPath "icx.exe"

    if (-not (Test-Path $script:IFX)) {
        throw "Intel Fortran compiler not found at: $script:IFX"
    }
    if (-not (Test-Path $script:ICX)) {
        throw "Intel C compiler not found at: $script:ICX"
    }

    # Find MSVC tools path
    $MSVCToolsDir = Get-ChildItem -Path "$script:VSInstallPath\VC\Tools\MSVC" -Directory |
        Sort-Object Name -Descending |
        Select-Object -First 1

    $script:MSVCBinPath = Join-Path $MSVCToolsDir.FullName "bin\Hostx64\x64"

    # Capture environment from batch files
    $TempBatch = [System.IO.Path]::GetTempFileName() + ".bat"
    $TempEnv = [System.IO.Path]::GetTempFileName()

    # Write batch file with proper quoting
    $BatchContent = "@echo off`r`n"
    $BatchContent += "call `"$VCVarsAll`" >nul 2>&1`r`n"
    $BatchContent += "call `"$OneAPISetVars`" intel64 vs2022 >nul 2>&1`r`n"
    $BatchContent += "set > `"$TempEnv`"`r`n"
    [System.IO.File]::WriteAllText($TempBatch, $BatchContent)

    # Run batch to capture environment
    $proc = Start-Process -FilePath "cmd.exe" -ArgumentList "/c `"$TempBatch`"" -Wait -NoNewWindow -PassThru

    if (Test-Path $TempEnv) {
        Get-Content $TempEnv | ForEach-Object {
            if ($_ -match '^([^=]+)=(.*)$') {
                [Environment]::SetEnvironmentVariable($matches[1], $matches[2], "Process")
            }
        }
        Remove-Item $TempEnv -Force -ErrorAction SilentlyContinue
    }
    Remove-Item $TempBatch -Force -ErrorAction SilentlyContinue

    # Ensure critical paths are set (prepend to avoid Git's link.exe)
    $env:LIB = "$script:IntelLibPath;$env:LIB"
    $env:PATH = "$script:MSVCBinPath;$script:IntelBinPath;$env:PATH"

    # Optional: a specific Fortran compiler (overrides the system ifx).
    if ($FortranCompiler) {
        Select-FortranCompiler -Spec $FortranCompiler -OneAPIRoot $OneAPIRoot
    }

    # Report the exact ifx version (ifx prints its banner on stderr)
    $script:IFXVersion = ""
    $banner = cmd /c "`"$script:IFX`" /QV 2>&1" | Select-String -Pattern 'Version\s+(\d+\.\d+(\.\d+)?)' | Select-Object -First 1
    if ($banner) { $script:IFXVersion = $banner.Matches[0].Groups[1].Value }

    # A non-default compiler gets its own build directory (own CMake cache)
    # unless the caller chose one explicitly.
    if ($FortranCompiler -and -not $PSBoundParameters.ContainsKey('BuildDir') -and $script:IFXVersion) {
        $script:BuildPath = Join-Path $SourceDir "build-ifx-$script:IFXVersion"
    }

    Write-Host "  Visual Studio: $script:VSInstallPath" -ForegroundColor Gray
    Write-Host "  Intel Compiler: $($IntelCompilerDir.Name) (system oneAPI; icx)" -ForegroundColor Gray
    Write-Host "  MSVC Tools: $($MSVCToolsDir.Name)" -ForegroundColor Gray
    Write-Host "  ifx: $script:IFX (version $script:IFXVersion)" -ForegroundColor Gray
    Write-Host "  icx: $script:ICX" -ForegroundColor Gray
    Write-Host "  Build dir: $script:BuildPath" -ForegroundColor Gray
    Write-Host "Environment ready." -ForegroundColor Green
    Write-Host ""
}

function Invoke-Clean {
    Write-Host "=== Cleaning Build Directory ===" -ForegroundColor Cyan
    Write-Host ""

    if (Test-Path $BuildPath) {
        Write-Host "Removing: $BuildPath" -ForegroundColor Yellow
        Remove-Item -Path $BuildPath -Recurse -Force
        Write-Host "Clean complete." -ForegroundColor Green
    } else {
        Write-Host "Build directory does not exist." -ForegroundColor Gray
    }
    Write-Host ""
}

function Invoke-Configure {
    Write-Host "=== Configuring CMake ===" -ForegroundColor Cyan
    Write-Host ""

    if (-not (Test-Path $BuildPath)) {
        New-Item -ItemType Directory -Path $BuildPath | Out-Null
    }

    Push-Location $BuildPath
    try {
        # Use full paths to compilers to avoid PATH issues
        $genName = if ($Generator -eq "NMake") { "NMake Makefiles" } else { "Ninja" }
        $CMakeArgs = @(
            $SourceDir,
            "-G", $genName,
            "-DCMAKE_BUILD_TYPE=$BuildType",
            "-DCMAKE_Fortran_COMPILER=$script:IFX",
            "-DCMAKE_C_COMPILER=$script:ICX"
        )

        # Add optional build flags
        if ($Parallel) {
            $CMakeArgs += "-DIWFM_BUILD_PARALLEL=ON"
            Write-Host "  OpenMP Parallel: Enabled" -ForegroundColor Yellow
        }
        if ($Coarray) {
            $CMakeArgs += "-DIWFM_BUILD_COARRAY=ON"
            Write-Host "  Coarray Multi-Model: Enabled" -ForegroundColor Yellow
        }
        if ($IWFM2OBS) {
            $CMakeArgs += "-DIWFM_BUILD_IWFM2OBS=ON"
            Write-Host "  IWFM2OBS: Enabled" -ForegroundColor Yellow
        }
        if ($CalcTypeHyd) {
            $CMakeArgs += "-DIWFM_BUILD_CALCTYPHYD=ON"
            Write-Host "  CalcTypeHyd: Enabled" -ForegroundColor Yellow
        }
        if ($ResultsExtract) {
            $CMakeArgs += "-DIWFM_BUILD_RESULTSEXTRACT=ON"
            Write-Host "  ResultsExtract: Enabled" -ForegroundColor Yellow
        }
        # Monolithic build is the default. Pass -Library to opt back into
        # the legacy library-mode build (single shared iwfm_kernel.lib),
        # which leaks /Qopenmp from the parallel target into the serial
        # target — see SourceCode/IWFM-kernel/CMakeLists.txt:45-59.
        if ($Library) {
            Write-Host "  Build Mode:     Library (legacy; serial exe inherits OpenMP)" -ForegroundColor Yellow
        } else {
            $CMakeArgs += "-DIWFM_MONOLITHIC_BUILD=ON"
            Write-Host "  Build Mode:     Monolithic (default; serial exe is truly serial)" -ForegroundColor Yellow
        }
        if ($UsePrebuiltDeps) {
            $AbsDepsDir = if ([System.IO.Path]::IsPathRooted($DepsDir)) { $DepsDir } else { Join-Path $SourceDir $DepsDir }
            $CMakeArgs += "-DIWFM_USE_SYSTEM_HDF5=ON"
            $CMakeArgs += "-DHDF5_ROOT=$AbsDepsDir\hdf5-install"
            $CMakeArgs += "-DIWFM_USE_SYSTEM_HECLIB=ON"
            $CMakeArgs += "-DHECLIB_ROOT=$AbsDepsDir\heclib-install"
            $CMakeArgs += "-DIWFM_USE_SYSTEM_ZLIB=ON"
            $CMakeArgs += "-DZLIB_ROOT=$AbsDepsDir\zlib-install"
            Write-Host "  Pre-built Deps: $AbsDepsDir" -ForegroundColor Yellow
        }

        Write-Host "Running: cmake $($CMakeArgs -join ' ')" -ForegroundColor Gray
        & cmake @CMakeArgs

        if ($LASTEXITCODE -ne 0) {
            throw "CMake configuration failed"
        }

        Write-Host ""
        Write-Host "Configuration complete." -ForegroundColor Green
    }
    finally {
        Pop-Location
    }
    Write-Host ""
}

function Invoke-Build {
    Write-Host "=== Building IWFM ===" -ForegroundColor Cyan
    Write-Host "Build type: $BuildType" -ForegroundColor White
    Write-Host ""

    # Configure if needed
    $needsConfigure = -not (Test-Path (Join-Path $BuildPath "CMakeCache.txt"))

    if ($needsConfigure) {
        Invoke-Configure
    }

    Push-Location $BuildPath
    try {
        $BuildArgs = @("--build", ".")
        if ($Generator -ne "NMake") {
            # NMake doesn't support parallel builds
            $BuildArgs += @("--parallel", $env:NUMBER_OF_PROCESSORS)
        }

        if ($Target -ne "all") {
            $BuildArgs += @("--target", $Target)
            Write-Host "Target: $Target" -ForegroundColor White
        }

        & cmake @BuildArgs

        if ($LASTEXITCODE -ne 0) {
            throw "Build failed"
        }

        Write-Host ""
        Write-Host "Build complete." -ForegroundColor Green
    }
    finally {
        Pop-Location
    }

    # Copy the OpenMP runtime next to the OpenMP consumers (Simulation_PLL,
    # IWFM_C_x64.dll). Use the SELECTED compiler's runtime, not whatever ifx
    # is first on PATH. A system oneAPI ships Intel's libiomp5md.dll; the
    # conda-forge ifx links LLVM's libomp.dll (its libiomp5md.lib is an
    # alias), so copy whichever of the two the compiler provides.
    $wantsOmp = $Parallel -or ($Target -eq "all") -or ($Target -like "IWFM_C_DLL*") -or ($Target -eq "Simulation_Parallel")
    if ($wantsOmp -and (Test-Path $BinDir)) {
        $compilerBinDir = Split-Path $script:IFX -Parent
        $copied = @()
        foreach ($name in @("libiomp5md.dll", "libomp.dll")) {
            foreach ($dir in @($compilerBinDir, (Join-Path (Split-Path $compilerBinDir -Parent) "bin"))) {
                $ompDll = Join-Path $dir $name
                if (Test-Path $ompDll) {
                    Copy-Item $ompDll -Destination $BinDir -Force
                    $copied += $name
                    break
                }
            }
        }
        if ($copied) {
            Write-Host "  Copied OpenMP runtime to Bin/: $($copied -join ', ')" -ForegroundColor Green
        } else {
            Write-Host "  WARNING: no OpenMP runtime DLL found next to $script:IFX" -ForegroundColor Yellow
            Write-Host "  IWFM_C_x64.dll / Simulation_PLL may fail to load without it" -ForegroundColor Yellow
        }
    }

    # Show results
    Write-Host ""
    Write-Host "=== Build Artifacts ===" -ForegroundColor Cyan
    if (Test-Path $BinDir) {
        # Map target names to output file patterns
        $TargetToFile = @{
            "Simulation" = "Simulation_x64*.exe"
            "Simulation_Parallel" = "Simulation_PLL_x64*.exe"
            "Simulation_MM" = "Simulation_MM_x64*.exe"
            "PreProcessor" = "PreProcessor_x64*.exe"
            "Budget" = "Budget_x64*.exe"
            "ZBudget" = "ZBudget_x64*.exe"
            "IWFM_C_DLL" = "IWFM_C_x64*.dll"
            "IWFM_C_DLL_Heap" = "IWFM_C_Heap_x64*.dll"
            "IWFM2OBS" = "IWFM2OBS_x64*.exe"
            "CalcTypeHyd" = "CalcTypeHyd_x64*.exe"
            "ResultsExtract" = "ResultsExtract_x64*.exe"
        }

        if ($Target -eq "all") {
            # Show all executables and DLLs
            Get-ChildItem -Path $BinDir -Include "*.exe", "*.dll" -File |
                Sort-Object LastWriteTime -Descending |
                ForEach-Object {
                    $size = [math]::Round($_.Length / 1MB, 1)
                    Write-Host "  $($_.Name) ($size MB)" -ForegroundColor White
                }
        } else {
            # Show only the built target
            $pattern = $TargetToFile[$Target]
            if ($pattern) {
                Get-ChildItem -Path $BinDir -Filter $pattern -File |
                    Sort-Object LastWriteTime -Descending |
                    ForEach-Object {
                        $size = [math]::Round($_.Length / 1MB, 1)
                        Write-Host "  $($_.Name) ($size MB)" -ForegroundColor White
                    }
            } else {
                Write-Host "  (no matching artifacts found for target: $Target)" -ForegroundColor Yellow
            }
        }
    }
    Write-Host ""
}

function Invoke-Test {
    Write-Host "=== Running Tests ===" -ForegroundColor Cyan
    Write-Host ""

    if (-not (Test-Path (Join-Path $BuildPath "CMakeCache.txt"))) {
        throw "Build not configured. Run with -Action Build first."
    }

    Push-Location $BuildPath
    try {
        & ctest -C $BuildType --output-on-failure

        if ($LASTEXITCODE -ne 0) {
            Write-Warning "Some tests failed"
            $script:ExitCode = $LASTEXITCODE
        } else {
            Write-Host "All tests passed." -ForegroundColor Green
        }
    }
    finally {
        Pop-Location
    }
    Write-Host ""
}

function Invoke-Package {
    Write-Host "=== Creating Packages ===" -ForegroundColor Cyan
    Write-Host ""

    if (-not (Test-Path (Join-Path $BuildPath "CMakeCache.txt"))) {
        throw "Build not configured. Run with -Action Build first."
    }

    Push-Location $BuildPath
    try {
        # Binary package
        Write-Host "Generating binary package..." -ForegroundColor Yellow
        & cpack -G ZIP -C $BuildType

        if ($LASTEXITCODE -ne 0) {
            throw "Binary package generation failed"
        }

        # Source package
        Write-Host "Generating source package..." -ForegroundColor Yellow
        & cpack --config CPackSourceConfig.cmake -G ZIP

        if ($LASTEXITCODE -ne 0) {
            throw "Source package generation failed"
        }

        Write-Host ""
        Write-Host "Packages created:" -ForegroundColor Green
        Get-ChildItem -Path $BuildPath -Filter "IWFM-*.zip" |
            Sort-Object LastWriteTime -Descending |
            ForEach-Object {
                $size = [math]::Round($_.Length / 1MB, 1)
                Write-Host "  $($_.Name) ($size MB)" -ForegroundColor White
            }
    }
    finally {
        Pop-Location
    }
    Write-Host ""
}

# Main execution
try {
    Initialize-BuildEnvironment
    if ($Action -ne "Clean") { Assert-CacheCompiler }

    switch ($Action) {
        "Clean" {
            Invoke-Clean
        }
        "Configure" {
            Invoke-Configure
        }
        "Build" {
            Invoke-Build
        }
        "Test" {
            Invoke-Test
        }
        "Package" {
            Invoke-Package
        }
        "All" {
            Invoke-Build
            Invoke-Package
        }
    }

    Write-Host "Done." -ForegroundColor Green
}
catch {
    Write-Host ""
    Write-Host "ERROR: $_" -ForegroundColor Red
    Write-Host ""
    $script:ExitCode = 1
}

exit $script:ExitCode
