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
    Specific target to build (Simulation, PreProcessor, Budget, ZBudget, IWFM_C_DLL).

.PARAMETER BuildType
    Build configuration (Release, Debug, RelWithDebInfo). Default: Release

.PARAMETER BuildDir
    Build directory name. Default: build

.PARAMETER Parallel
    Enable OpenMP parallel simulation build (Simulation_PLL).

.PARAMETER Coarray
    Enable Coarray Fortran multi-model build (Simulation_MM). Intel compiler only.

.EXAMPLE
    .\Build-IWFM.ps1
    Builds all targets in Release mode.

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

    [ValidateSet("Simulation", "Simulation_Parallel", "Simulation_MM", "PreProcessor", "Budget", "ZBudget", "IWFM_C_DLL", "all")]
    [string]$Target = "all",

    [ValidateSet("Release", "Debug", "RelWithDebInfo")]
    [string]$BuildType = "Release",

    [string]$BuildDir = "build",

    [switch]$Parallel,

    [switch]$Coarray
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
Write-Host "  Build System v2025.0" -ForegroundColor Gray
Write-Host ""

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

    Write-Host "  Visual Studio: $script:VSInstallPath" -ForegroundColor Gray
    Write-Host "  Intel Compiler: $($IntelCompilerDir.Name)" -ForegroundColor Gray
    Write-Host "  MSVC Tools: $($MSVCToolsDir.Name)" -ForegroundColor Gray
    Write-Host "  ifx: $script:IFX" -ForegroundColor Gray
    Write-Host "  icx: $script:ICX" -ForegroundColor Gray
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
        $CMakeArgs = @(
            $SourceDir,
            "-G", "Ninja",
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
        $BuildArgs = @("--build", ".", "--parallel", $env:NUMBER_OF_PROCESSORS)

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
