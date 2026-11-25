<#
.SYNOPSIS
    Build all IWFM targets in Release and Debug and package the Windows
    release archives.

.DESCRIPTION
    Drives build-iwfm.ps1 (monolithic build, HDF5 and heclib from source)
    once per configuration, then zips the binaries from Bin\ into
    release-staging\.

    Release binaries must be built with the compiler DWR used for the
    official release they are based on, or results differ from the
    official ones at round-off level (2026.0.1848 = ifx 2026.1.1). Pass
    it with -FortranCompiler; see build-iwfm.ps1 for the accepted forms.

.PARAMETER FortranCompiler
    The ifx to build with, e.g. a conda prefix containing ifx 2026.1.1.

.EXAMPLE
    .\build-all-release.ps1 -FortranCompiler C:\Users\me\ifx-2026.1.1
#>
param(
    [Parameter(Mandatory = $true)]
    [string]$FortranCompiler,
    [string]$Version = "2026.0.1848"
)
$ErrorActionPreference = "Stop"

$SrcDir = $PSScriptRoot
$BinDir = Join-Path $SrcDir "Bin"
$StagingDir = Join-Path $SrcDir "release-staging"
$BuildScript = Join-Path $SrcDir "build-iwfm.ps1"

$Targets = @("Simulation", "Simulation_PLL", "Simulation_MM", "PreProcessor",
             "Budget", "ZBudget", "IWFM2OBS", "CalcTypeHyd", "ResultsExtract")

function Build-Config($BuildType) {
    Write-Host "`n=== Building $BuildType ===" -ForegroundColor Cyan
    & $BuildScript -Action Build -BuildType $BuildType `
        -BuildDir "build-release-$($BuildType.ToLower())" `
        -FortranCompiler $FortranCompiler `
        -Parallel -Coarray -IWFM2OBS -CalcTypeHyd -ResultsExtract
    if ($LASTEXITCODE -ne 0) { throw "$BuildType build failed" }
}

function Get-BundledRuntimes($Binaries) {
    # Runtime DLLs the binaries import that build-iwfm.ps1 copied into Bin\
    # (the OpenMP runtime of Simulation_PLL and the C DLL: libomp.dll with
    # conda's ifx, libiomp5md.dll with oneAPI's). System and UCRT DLLs are
    # not in Bin\ and so are never picked up.
    $vswhere = "${env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"
    $vs = & $vswhere -latest -property installationPath
    $dumpbin = Get-ChildItem "$vs\VC\Tools\MSVC\*\bin\Hostx64\x64\dumpbin.exe" |
        Select-Object -Last 1
    $names = foreach ($b in $Binaries) {
        & $dumpbin.FullName /nologo /dependents $b |
            Select-String '^\s+(\S+\.dll)\s*$' | ForEach-Object { $_.Matches[0].Groups[1].Value }
    }
    $names | Sort-Object -Unique | ForEach-Object { Join-Path $BinDir $_ } |
        Where-Object { Test-Path $_ }
}

function Package-Config($Suffix, $ZipName) {
    $files = @($Targets | ForEach-Object { "${_}_x64$Suffix.exe" }) + "IWFM_C_x64$Suffix.dll"
    $paths = $files | ForEach-Object { Join-Path $BinDir $_ }
    $missing = $paths | Where-Object { -not (Test-Path $_) }
    if ($missing) { throw "Missing build outputs:`n  $($missing -join "`n  ")" }
    $paths = @($paths) + @(Get-BundledRuntimes $paths)
    $zipPath = Join-Path $StagingDir $ZipName
    if (Test-Path $zipPath) { Remove-Item $zipPath -Force }
    Compress-Archive -Path $paths -DestinationPath $zipPath
    $size = [math]::Round((Get-Item $zipPath).Length / 1MB, 1)
    Write-Host "  $ZipName ($size MB, $($paths.Count) files)" -ForegroundColor Green
}

New-Item -ItemType Directory -Force -Path $StagingDir | Out-Null

Build-Config "Release"
Package-Config "" "IWFM-$Version-Windows-x64-Release.zip"

Build-Config "Debug"
Package-Config "_D" "IWFM-$Version-Windows-x64-Debug.zip"

Write-Host "`nArchives in $StagingDir" -ForegroundColor Yellow
