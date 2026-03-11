<#
.SYNOPSIS
    Build all IWFM targets in Release and Debug, then package for GitHub release.
    Builds HDF5 and heclib from source via CMake (no pre-bundled libraries).
#>
$ErrorActionPreference = "Stop"

$SrcDir = "C:\Users\hatch\OneDrive\Desktop\iwfm-2025.0.1747\src"
$BinDir = Join-Path $SrcDir "Bin"
$StagingDir = Join-Path $SrcDir "release-staging"

# =============================================
# Find and setup Intel oneAPI + MSVC environment
# =============================================
$VSWhere = "${env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"
$VSInstallPath = & $VSWhere -latest -property installationPath
$VCVarsAll = Join-Path $VSInstallPath "VC\Auxiliary\Build\vcvars64.bat"
$OneAPIRoot = "${env:ProgramFiles(x86)}\Intel\oneAPI"
$OneAPISetVars = Join-Path $OneAPIRoot "setvars.bat"

$IntelCompilerDir = Get-ChildItem -Path "$OneAPIRoot\compiler" -Directory |
    Where-Object { $_.Name -match '^\d+\.\d+' } |
    Sort-Object Name -Descending |
    Select-Object -First 1
$IntelBinPath = Join-Path $IntelCompilerDir.FullName "bin"
$IntelLibPath = Join-Path $IntelCompilerDir.FullName "lib"
$IFX = Join-Path $IntelBinPath "ifx.exe"
$ICX = Join-Path $IntelBinPath "icx.exe"

$MSVCToolsDir = Get-ChildItem -Path "$VSInstallPath\VC\Tools\MSVC" -Directory |
    Sort-Object Name -Descending |
    Select-Object -First 1
$MSVCBinPath = Join-Path $MSVCToolsDir.FullName "bin\Hostx64\x64"

# Capture environment from batch files
$TempBatch = [System.IO.Path]::GetTempFileName() + ".bat"
$TempEnv = [System.IO.Path]::GetTempFileName()
$BatchContent = "@echo off`r`ncall `"$VCVarsAll`" >nul 2>&1`r`ncall `"$OneAPISetVars`" intel64 vs2022 >nul 2>&1`r`nset > `"$TempEnv`"`r`n"
[System.IO.File]::WriteAllText($TempBatch, $BatchContent)
Start-Process -FilePath "cmd.exe" -ArgumentList "/c `"$TempBatch`"" -Wait -NoNewWindow
if (Test-Path $TempEnv) {
    Get-Content $TempEnv | ForEach-Object {
        if ($_ -match '^([^=]+)=(.*)$') {
            [Environment]::SetEnvironmentVariable($matches[1], $matches[2], "Process")
        }
    }
    Remove-Item $TempEnv -Force -ErrorAction SilentlyContinue
}
Remove-Item $TempBatch -Force -ErrorAction SilentlyContinue

# Ensure MSVC linker (link.exe) is found before any coreutils link
$env:LIB = "$IntelLibPath;$env:LIB"
$env:PATH = "$MSVCBinPath;$IntelBinPath;$env:PATH"

Write-Host "Environment ready: ifx=$IFX, icx=$ICX" -ForegroundColor Green

# =============================================
# Common CMake args - build HDF5 from source
# =============================================
$CommonArgs = @(
    $SrcDir,
    "-G", "Ninja",
    "-DCMAKE_Fortran_COMPILER=$IFX",
    "-DCMAKE_C_COMPILER=$ICX",
    "-DIWFM_BUILD_PARALLEL=ON",
    "-DIWFM_BUILD_COARRAY=ON",
    "-DIWFM_BUILD_IWFM2OBS=ON",
    "-DIWFM_BUILD_CALCTYPHYD=ON"
)

function Build-Config($BuildType) {
    Write-Host "`n========================================" -ForegroundColor Cyan
    Write-Host "  Building $BuildType" -ForegroundColor Cyan
    Write-Host "========================================" -ForegroundColor Cyan

    $BuildDir = Join-Path $SrcDir "build-$($BuildType.ToLower())"

    # Clean build dir
    if (Test-Path $BuildDir) {
        Remove-Item $BuildDir -Recurse -Force -ErrorAction SilentlyContinue
        Start-Sleep -Seconds 1
    }
    New-Item -ItemType Directory -Path $BuildDir -Force | Out-Null

    Push-Location $BuildDir
    try {
        $cmakeArgs = $CommonArgs + @("-DCMAKE_BUILD_TYPE=$BuildType")
        Write-Host "Configuring..." -ForegroundColor Yellow
        & cmake @cmakeArgs
        if ($LASTEXITCODE -ne 0) { throw "CMake configure failed for $BuildType" }

        # Step 1: Build HDF5 ExternalProject first with limited parallelism
        # to avoid Intel icx intermittent ICE on parallel compilation
        Write-Host "Building HDF5 from source (limited parallelism)..." -ForegroundColor Yellow
        & cmake --build . --target HDF5_External --parallel 4
        if ($LASTEXITCODE -ne 0) { throw "HDF5 build failed for $BuildType" }

        # Step 2: Build everything else with full parallelism
        Write-Host "Building all IWFM targets..." -ForegroundColor Yellow
        & cmake --build . --parallel $env:NUMBER_OF_PROCESSORS
        if ($LASTEXITCODE -ne 0) { throw "Build failed for $BuildType" }

        Write-Host "$BuildType build complete." -ForegroundColor Green
    }
    finally {
        Pop-Location
    }
}

function Package-Config($fileList, $zipName) {
    $zipPath = Join-Path $StagingDir $zipName
    if (Test-Path $zipPath) { Remove-Item $zipPath -Force }
    $paths = $fileList | ForEach-Object { Join-Path $BinDir $_ } | Where-Object { Test-Path $_ }
    Write-Host "  Packaging $($paths.Count) files -> $zipName" -ForegroundColor Yellow
    Compress-Archive -Path $paths -DestinationPath $zipPath
    $size = [math]::Round((Get-Item $zipPath).Length / 1MB, 1)
    Write-Host "  $zipName ($size MB)" -ForegroundColor Green
}

# =============================================
# Build Release
# =============================================
Build-Config "Release"

New-Item -ItemType Directory -Force -Path $StagingDir | Out-Null
Package-Config @(
    "Simulation_x64.exe", "Simulation_PLL_x64.exe", "Simulation_MM_x64.exe",
    "PreProcessor_x64.exe", "Budget_x64.exe", "ZBudget_x64.exe",
    "IWFM_C_x64.dll", "IWFM2OBS_x64.exe", "CalcTypeHyd_x64.exe"
) "IWFM-2025.0.1747-Windows-x64-Release.zip"

# =============================================
# Build Debug
# =============================================
Build-Config "Debug"

Package-Config @(
    "Simulation_x64_D.exe", "Simulation_PLL_x64_D.exe", "Simulation_MM_x64_D.exe",
    "PreProcessor_x64_D.exe", "Budget_x64_D.exe", "ZBudget_x64_D.exe",
    "IWFM_C_x64_D.dll", "IWFM2OBS_x64_D.exe", "CalcTypeHyd_x64_D.exe"
) "IWFM-2025.0.1747-Windows-x64-Debug.zip"

Write-Host "`n========================================" -ForegroundColor Cyan
Write-Host "  All Windows builds complete!" -ForegroundColor Green
Write-Host "========================================" -ForegroundColor Cyan
Get-ChildItem $StagingDir -Filter "*.zip" | ForEach-Object {
    $size = [math]::Round($_.Length / 1MB, 1)
    Write-Host "  $($_.Name) ($size MB)" -ForegroundColor White
}
