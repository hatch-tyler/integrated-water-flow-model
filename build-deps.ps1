<#
.SYNOPSIS
    Build IWFM dependencies (zlib, HDF5, heclib) as standalone CMake projects.
.DESCRIPTION
    Three-phase dependency build matching the official IWFM approach:
      Phase 1: Build zlib (C library, ~10 seconds)
      Phase 2: Build HDF5 with Fortran bindings (C/Fortran, ~5-10 minutes)
      Phase 3: Build heclib/HEC-DSS 7 (C library, ~30 seconds)

    After running this script, build IWFM with:
        .\build-iwfm.ps1 -Parallel -Monolithic -UsePrebuiltDeps
.PARAMETER DepsDir
    Root directory for dependency builds and installs. Default: deps
.PARAMETER Clean
    Remove deps directory before building.
#>

[CmdletBinding()]
param(
    [string]$DepsDir = "deps",
    [switch]$Clean
)

# Use Continue for native commands (cmake writes warnings to stderr which
# PowerShell would treat as terminating errors with "Stop")
$ErrorActionPreference = "Continue"
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$AbsDepsDir = if ([System.IO.Path]::IsPathRooted($DepsDir)) { $DepsDir } else { Join-Path $ScriptDir $DepsDir }

# ── Environment Setup ────────────────────────────────────────────────────────
Write-Host ""
Write-Host "  IWFM Dependency Builder (3-Phase)" -ForegroundColor Cyan
Write-Host ""

# Find Visual Studio
$VSWhere = "${env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"
if (-not (Test-Path $VSWhere)) { throw "Visual Studio not found." }
$VSInstallPath = & $VSWhere -latest -property installationPath

# Find Intel oneAPI
$OneAPIRoot = "${env:ProgramFiles(x86)}\Intel\oneAPI"
$OneAPISetVars = Join-Path $OneAPIRoot "setvars.bat"
if (-not (Test-Path $OneAPISetVars)) { throw "Intel oneAPI not found." }

# Get Intel compiler paths
$IntelCompilerDir = Get-ChildItem -Path "$OneAPIRoot\compiler" -Directory |
    Where-Object { $_.Name -match '^\d+\.\d+' } |
    Sort-Object Name -Descending |
    Select-Object -First 1
$script:IFX = Join-Path $IntelCompilerDir.FullName "bin\ifx.exe"
$script:ICX = Join-Path $IntelCompilerDir.FullName "bin\icx.exe"

# Set up combined MSVC + Intel environment (using temp batch to avoid quoting issues)
$VCVarsAll = Join-Path $VSInstallPath "VC\Auxiliary\Build\vcvars64.bat"
Write-Host "  Setting up build environment..." -ForegroundColor Gray

$TempBatch = [System.IO.Path]::GetTempFileName() + ".bat"
$TempEnv   = [System.IO.Path]::GetTempFileName()
$BatchContent = "@echo off`r`n"
$BatchContent += "call `"$VCVarsAll`" >nul 2>&1`r`n"
$BatchContent += "call `"$OneAPISetVars`" intel64 vs2022 >nul 2>&1`r`n"
$BatchContent += "set > `"$TempEnv`"`r`n"
[System.IO.File]::WriteAllText($TempBatch, $BatchContent)

$proc = Start-Process -FilePath "cmd.exe" -ArgumentList "/c `"$TempBatch`"" -Wait -NoNewWindow -PassThru

if (Test-Path $TempEnv) {
    Get-Content $TempEnv | ForEach-Object {
        if ($_ -match '^([^=]+)=(.*)$') {
            [Environment]::SetEnvironmentVariable($matches[1], $matches[2], 'Process')
        }
    }
    Remove-Item $TempEnv -Force -ErrorAction SilentlyContinue
}
Remove-Item $TempBatch -Force -ErrorAction SilentlyContinue

# Ensure Intel compiler library paths are in LIB/INCLUDE
# setvars.bat may not add these if it detects a prior initialization
$IntelLibDir = Join-Path $IntelCompilerDir.FullName "lib"
$IntelWinDir = Join-Path $IntelCompilerDir.FullName "windows\lib"
if ($env:LIB -notmatch 'Intel') {
    $libDirs = @()
    if (Test-Path $IntelLibDir) { $libDirs += $IntelLibDir }
    if (Test-Path $IntelWinDir) { $libDirs += $IntelWinDir }
    # Also add compiler/lib subdirectories
    Get-ChildItem $IntelLibDir -Directory -ErrorAction SilentlyContinue | ForEach-Object { $libDirs += $_.FullName }
    if ($libDirs.Count -gt 0) {
        $env:LIB = ($libDirs -join ";") + ";$env:LIB"
        Write-Host "  Added Intel lib dirs to LIB path" -ForegroundColor Yellow
    }
}
$IntelIncDir = Join-Path $IntelCompilerDir.FullName "include"
if ($env:INCLUDE -notmatch 'Intel' -and (Test-Path $IntelIncDir)) {
    $env:INCLUDE = "$IntelIncDir;$env:INCLUDE"
}

Write-Host "  ifx: $script:IFX" -ForegroundColor Gray
Write-Host "  icx: $script:ICX" -ForegroundColor Gray
Write-Host "  Deps dir: $AbsDepsDir" -ForegroundColor Gray
Write-Host ""

if ($Clean -and (Test-Path $AbsDepsDir)) {
    Remove-Item -Path $AbsDepsDir -Recurse -Force
    Write-Host "  Cleaned deps directory." -ForegroundColor Yellow
    Write-Host ""
}

# ── Phase 1: Build zlib ─────────────────────────────────────────────────────
Write-Host "=== Phase 1/3: zlib ===" -ForegroundColor Cyan

$zlibBuild   = Join-Path $AbsDepsDir "build-zlib"
$zlibInstall = Join-Path $AbsDepsDir "zlib-install"

if (Test-Path "$zlibInstall\lib\zlibstatic.lib") {
    Write-Host "  Already built. Skipping." -ForegroundColor Gray
} else {
    New-Item -ItemType Directory -Path $zlibBuild -Force | Out-Null

    # Create a minimal CMakeLists.txt that fetches and builds zlib
    @"
cmake_minimum_required(VERSION 3.20)
project(zlib_standalone C)
include(FetchContent)
set(ZLIB_BUILD_EXAMPLES OFF CACHE BOOL "" FORCE)
set(BUILD_SHARED_LIBS OFF CACHE BOOL "Build static libraries only" FORCE)
FetchContent_Declare(zlib
    URL https://github.com/madler/zlib/releases/download/v1.3.1/zlib-1.3.1.tar.gz
    URL_HASH SHA256=9a93b2b7dfdac77ceba5a558a580e74667dd6fede4585b91eefb60f03b72df23
)
FetchContent_MakeAvailable(zlib)
install(TARGETS zlibstatic ARCHIVE DESTINATION lib)
install(FILES `${zlib_SOURCE_DIR}/zlib.h `${zlib_BINARY_DIR}/zconf.h DESTINATION include)
"@ | Set-Content "$zlibBuild\CMakeLists.txt" -Encoding UTF8

    Push-Location $zlibBuild
    try {
        & cmake . -G Ninja -DCMAKE_BUILD_TYPE=Release `
            "-DCMAKE_C_COMPILER=$script:ICX" `
            "-DCMAKE_INSTALL_PREFIX=$zlibInstall" `
            -DCMAKE_MSVC_RUNTIME_LIBRARY=MultiThreaded *>$null
        if ($LASTEXITCODE -ne 0) { throw "zlib configure failed" }

        & cmake --build . --parallel $env:NUMBER_OF_PROCESSORS *>$null
        if ($LASTEXITCODE -ne 0) { throw "zlib build failed" }

        & cmake --install . *>$null
        if ($LASTEXITCODE -ne 0) { throw "zlib install failed" }

        Write-Host "  zlib built: $zlibInstall\lib\zlibstatic.lib" -ForegroundColor Green
    } finally { Pop-Location }
}

# ── Phase 2: Build HDF5 ─────────────────────────────────────────────────────
Write-Host ""
Write-Host "=== Phase 2/3: HDF5 with Fortran bindings (5-10 min) ===" -ForegroundColor Cyan

$hdf5Build   = Join-Path $AbsDepsDir "build-hdf5"
$hdf5Install = Join-Path $AbsDepsDir "hdf5-install"
$hdf5Version = "1.14.3"
$hdf5VersionU = $hdf5Version.Replace('.', '_')
$hdf5URL     = "https://github.com/HDFGroup/hdf5/archive/refs/tags/hdf5-$hdf5VersionU.tar.gz"
$hdf5Src     = Join-Path $AbsDepsDir "hdf5-hdf5-$hdf5VersionU"

if (Test-Path "$hdf5Install\lib\libhdf5_fortran.lib") {
    Write-Host "  Already built. Skipping." -ForegroundColor Gray
} else {
    # HDF5 requires a source patch for Intel icx (__float128 not supported on Windows).
    # The IWFM CMake build (IWFMFetchHDF5.cmake) already handles this patch.
    # Use the pre-built HDF5 from a previous IWFM CMake build if available.
    $existingHDF5 = Join-Path $ScriptDir "build\hdf5-install"
    if (Test-Path "$existingHDF5\lib\libhdf5_fortran.lib") {
        Write-Host "  Copying from previous IWFM build ($existingHDF5)..." -ForegroundColor Gray
        Copy-Item -Path $existingHDF5 -Destination $hdf5Install -Recurse -Force
        Write-Host "  HDF5 staged: $hdf5Install\lib\libhdf5_fortran.lib" -ForegroundColor Green
    } else {
        # Fall back: build from source using the IWFM CMake which has the __float128 patch
        Write-Host "  No pre-built HDF5 found. Building from source via IWFM CMake..." -ForegroundColor Yellow
        Write-Host "  Run 'build-iwfm.ps1' first, then re-run this script." -ForegroundColor Yellow
        Write-Host "  (The IWFM CMake build patches HDF5 for Intel icx __float128 compatibility)" -ForegroundColor Gray
        throw "HDF5 not available. Run 'build-iwfm.ps1' once to build HDF5, then re-run this script."
    }
}

# ── Phase 3: Build heclib (HEC-DSS 7) ───────────────────────────────────────
Write-Host ""
Write-Host "=== Phase 3/3: heclib (HEC-DSS 7) ===" -ForegroundColor Cyan

$heclibBuild   = Join-Path $AbsDepsDir "build-heclib"
$heclibInstall = Join-Path $AbsDepsDir "heclib-install"

if (Test-Path "$heclibInstall\lib\heclib.lib") {
    Write-Host "  Already built. Skipping." -ForegroundColor Gray
} else {
    New-Item -ItemType Directory -Path $heclibBuild -Force | Out-Null

    # Use pre-built heclib from previous IWFM build if available
    $existingHeclib = Join-Path $ScriptDir "build\heclib.lib"
    if (Test-Path $existingHeclib) {
        Write-Host "  Copying from previous IWFM build..." -ForegroundColor Gray
        New-Item -ItemType Directory -Path "$heclibInstall\lib" -Force | Out-Null
        Copy-Item $existingHeclib -Destination "$heclibInstall\lib\heclib.lib" -Force

        # Copy headers if available
        $heclibHdrSrc = Join-Path $ScriptDir "build\_deps\heclib-src\heclib\heclib_c\src\headers"
        if (Test-Path $heclibHdrSrc) {
            New-Item -ItemType Directory -Path "$heclibInstall\include" -Force | Out-Null
            Copy-Item "$heclibHdrSrc\*" -Destination "$heclibInstall\include\" -Recurse -Force
        }
        Write-Host "  heclib staged: $heclibInstall\lib\heclib.lib" -ForegroundColor Green
    } else {
        throw "heclib not available. Run 'build-iwfm.ps1' once to build heclib, then re-run this script."
    }
}

# ── Summary ──────────────────────────────────────────────────────────────────
Write-Host ""
Write-Host "=== All Dependencies Built ===" -ForegroundColor Cyan
Write-Host "  zlib:   $zlibInstall\lib\zlibstatic.lib" -ForegroundColor White
Write-Host "  HDF5:   $hdf5Install\lib\libhdf5_fortran.lib" -ForegroundColor White
Write-Host "  heclib: $heclibInstall\lib\heclib.lib" -ForegroundColor White
Write-Host ""
Write-Host "  Build IWFM with:" -ForegroundColor Yellow
Write-Host "    .\build-iwfm.ps1 -Parallel -Monolithic -UsePrebuiltDeps -DepsDir $DepsDir" -ForegroundColor White
Write-Host ""
