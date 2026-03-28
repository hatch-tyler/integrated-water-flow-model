<#
.SYNOPSIS
    Diagnose runtime performance differences between CMake and .vfproj builds.

.DESCRIPTION
    Dumps exact compiler/linker commands from both CMake (monolithic) and
    Build-IWFM-VS.ps1 builds, then diffs them to identify flag differences
    that may explain the 30-50% runtime performance gap.

.PARAMETER Project
    Which project to diagnose. Default: Simulation.

.PARAMETER Configuration
    Release or Debug. Default: Release.

.PARAMETER SkipBuild
    Skip actual builds, only parse existing logs/configs.

.EXAMPLE
    .\diagnose-build.ps1
    Full diagnosis of Simulation Release build.

.EXAMPLE
    .\diagnose-build.ps1 -Project Simulation_Parallel -SkipBuild
    Parse existing build artifacts without rebuilding.
#>

[CmdletBinding()]
param(
    [ValidateSet("Simulation", "Simulation_Parallel", "Budget", "ZBudget")]
    [string]$Project = "Simulation",

    [ValidateSet("Release", "Debug")]
    [string]$Configuration = "Release",

    [switch]$SkipBuild
)

$ErrorActionPreference = "Stop"
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$DiagDir = Join-Path $ScriptDir "diag-output"

if (-not (Test-Path $DiagDir)) {
    New-Item -ItemType Directory -Path $DiagDir -Force | Out-Null
}

Write-Host ""
Write-Host "================================================================" -ForegroundColor Cyan
Write-Host "  IWFM Build Diagnosis: CMake vs .vfproj" -ForegroundColor Cyan
Write-Host "  Project:       $Project" -ForegroundColor White
Write-Host "  Configuration: $Configuration" -ForegroundColor White
Write-Host "================================================================" -ForegroundColor Cyan
Write-Host ""

# =============================================================================
# Step 1: Dump CMake commands
# =============================================================================
Write-Host "=== Step 1: CMake Monolithic Build Commands ===" -ForegroundColor Yellow

$cmakeBuildDir = Join-Path $ScriptDir "build-diag"
$cmakeLog = Join-Path $DiagDir "cmake-commands.log"

if (-not $SkipBuild) {
    Write-Host "  Configuring CMake monolithic build..." -ForegroundColor Gray

    if (Test-Path $cmakeBuildDir) {
        Remove-Item -Path $cmakeBuildDir -Recurse -Force
    }

    # Configure with monolithic mode
    $cmakeArgs = @(
        "-S", $ScriptDir,
        "-B", $cmakeBuildDir,
        "-G", "Ninja",
        "-DCMAKE_BUILD_TYPE=$Configuration",
        "-DCMAKE_Fortran_COMPILER=ifx",
        "-DCMAKE_C_COMPILER=icx",
        "-DIWFM_MONOLITHIC_BUILD=ON",
        "-DIWFM_BUILD_SIMULATION=ON",
        "-DIWFM_BUILD_PREPROCESSOR=OFF",
        "-DIWFM_BUILD_BUDGET=OFF",
        "-DIWFM_BUILD_ZBUDGET=OFF",
        "-DIWFM_BUILD_DLL=OFF",
        "-DIWFM_BUILD_PARALLEL=OFF",
        "-DIWFM_BUILD_COARRAY=OFF",
        "-DIWFM_BUILD_TESTS=OFF"
    )

    # Enable the requested project
    switch ($Project) {
        "Simulation_Parallel" {
            $cmakeArgs += "-DIWFM_BUILD_PARALLEL=ON"
        }
        "Budget" {
            $cmakeArgs += "-DIWFM_BUILD_SIMULATION=OFF"
            $cmakeArgs += "-DIWFM_BUILD_BUDGET=ON"
        }
        "ZBudget" {
            $cmakeArgs += "-DIWFM_BUILD_SIMULATION=OFF"
            $cmakeArgs += "-DIWFM_BUILD_ZBUDGET=ON"
        }
    }

    & cmake @cmakeArgs 2>&1 | Out-File (Join-Path $DiagDir "cmake-configure.log")

    Write-Host "  Building with verbose output..." -ForegroundColor Gray
    & cmake --build $cmakeBuildDir --verbose 2>&1 | Out-File $cmakeLog
    Write-Host "  CMake commands saved to: $cmakeLog" -ForegroundColor Green
} else {
    Write-Host "  Skipping build (using existing logs)" -ForegroundColor Gray
}

# =============================================================================
# Step 2: Dump .vfproj commands
# =============================================================================
Write-Host ""
Write-Host "=== Step 2: .vfproj Build Commands ===" -ForegroundColor Yellow

$vfprojLog = Join-Path $DiagDir "vfproj-commands.log"

if (-not $SkipBuild) {
    Write-Host "  Running Build-IWFM-VS.ps1 -ShowCommands..." -ForegroundColor Gray
    & (Join-Path $ScriptDir "Build-IWFM-VS.ps1") -Project $Project -Configuration $Configuration -ShowCommands -Clean 2>&1 | Out-File $vfprojLog
    Write-Host "  .vfproj commands saved to: $vfprojLog" -ForegroundColor Green
} else {
    Write-Host "  Skipping build (using existing logs)" -ForegroundColor Gray
}

# =============================================================================
# Step 3: Parse and compare flags
# =============================================================================
Write-Host ""
Write-Host "=== Step 3: Flag Comparison ===" -ForegroundColor Yellow

# --- Parse CMake compile flags ---
function Parse-CMakeCompileFlags {
    param([string]$LogFile)

    $flags = [System.Collections.Generic.HashSet[string]]::new([System.StringComparer]::OrdinalIgnoreCase)
    $linkFlags = [System.Collections.Generic.List[string]]::new()
    $objOrder = [System.Collections.Generic.List[string]]::new()
    $rspFiles = [System.Collections.Generic.List[string]]::new()

    if (-not (Test-Path $LogFile)) {
        Write-Host "  WARNING: $LogFile not found" -ForegroundColor Yellow
        return @{ CompileFlags = $flags; LinkFlags = $linkFlags; ObjOrder = $objOrder; RspFiles = $rspFiles }
    }

    foreach ($line in Get-Content $LogFile) {
        # Compile lines: ifx /c ... file.f90
        if ($line -match 'ifx(?:\.exe)?\s+.*\s+/c\s+') {
            # Extract flags (everything between ifx and the source file)
            $parts = $line -split '\s+'
            foreach ($p in $parts) {
                if ($p -match '^[/-]' -and $p -notmatch '\.f90$' -and $p -notmatch '\.obj$' -and $p -ne '/c') {
                    # Normalize: strip path prefixes from /I, /module:, /object:
                    $normalized = $p
                    if ($p -match '^/I"?(.+)"?$') {
                        $normalized = "/I:<path>"
                    } elseif ($p -match '^/module:"?(.+)"?$') {
                        $normalized = "/module:<path>"
                    } elseif ($p -match '^/object:"?(.+)"?$') {
                        continue  # skip object path
                    }
                    $flags.Add($normalized) | Out-Null
                }
                # Check for response files
                if ($p -match '^@(.+)') {
                    $rspFiles.Add($matches[1])
                }
            }
        }
        # Link lines: ifx ... /link ...
        elseif ($line -match 'ifx(?:\.exe)?\s+.*(/link|/Fe:)') {
            $parts = $line -split '\s+'
            $inLink = $false
            foreach ($p in $parts) {
                if ($p -eq '/link') { $inLink = $true; continue }
                if ($inLink) {
                    if ($p -match '\.obj"?$') {
                        $objName = [System.IO.Path]::GetFileNameWithoutExtension($p.Trim('"'))
                        $objOrder.Add($objName)
                    } else {
                        $linkFlags.Add($p)
                    }
                }
            }
        }
        # link.exe lines
        elseif ($line -match 'link(?:\.exe)?\s+') {
            $parts = $line -split '\s+'
            foreach ($p in $parts) {
                if ($p -match '\.obj"?$') {
                    $objName = [System.IO.Path]::GetFileNameWithoutExtension($p.Trim('"'))
                    $objOrder.Add($objName)
                } elseif ($p -match '^[/-]') {
                    $linkFlags.Add($p)
                }
                if ($p -match '^@(.+)') {
                    $rspFiles.Add($matches[1])
                }
            }
        }
    }

    return @{
        CompileFlags = $flags
        LinkFlags = $linkFlags
        ObjOrder = $objOrder
        RspFiles = $rspFiles
    }
}

# --- Parse .vfproj compile flags ---
function Parse-VfprojCompileFlags {
    param([string]$LogFile)

    $flags = [System.Collections.Generic.HashSet[string]]::new([System.StringComparer]::OrdinalIgnoreCase)
    $linkFlags = [System.Collections.Generic.List[string]]::new()
    $objOrder = [System.Collections.Generic.List[string]]::new()

    if (-not (Test-Path $LogFile)) {
        Write-Host "  WARNING: $LogFile not found" -ForegroundColor Yellow
        return @{ CompileFlags = $flags; LinkFlags = $linkFlags; ObjOrder = $objOrder; RspFiles = @() }
    }

    foreach ($line in Get-Content $LogFile) {
        # Compile lines start with [N/M]
        if ($line -match '^\[\d+/\d+\]\s+ifx\s+(.+)') {
            $argStr = $matches[1]
            $parts = $argStr -split '\s+'
            foreach ($p in $parts) {
                if ($p -match '^[/-]' -and $p -ne '/c') {
                    $normalized = $p.Trim('"')
                    if ($normalized -match '^/I') {
                        $normalized = "/I:<path>"
                    } elseif ($normalized -match '^/module:') {
                        $normalized = "/module:<path>"
                    } elseif ($normalized -match '^/object:') {
                        continue
                    }
                    $flags.Add($normalized) | Out-Null
                }
            }
        }
        # Link line: ifx /nologo /Qopenmp ... /link ...
        elseif ($line -match '^ifx\s+.*(/link|/Fe:)') {
            $parts = $line -split '\s+'
            $inLink = $false
            foreach ($p in $parts) {
                if ($p -eq '/link') { $inLink = $true; continue }
                if ($inLink) {
                    if ($p -match '\.obj"?$') {
                        $objName = [System.IO.Path]::GetFileNameWithoutExtension($p.Trim('"'))
                        $objOrder.Add($objName)
                    } else {
                        $linkFlags.Add($p.Trim('"'))
                    }
                }
            }
        }
    }

    return @{
        CompileFlags = $flags
        LinkFlags = $linkFlags
        ObjOrder = $objOrder
        RspFiles = @()
    }
}

$cmake = Parse-CMakeCompileFlags -LogFile $cmakeLog
$vfproj = Parse-VfprojCompileFlags -LogFile $vfprojLog

# =============================================================================
# Step 4: Report differences
# =============================================================================
Write-Host ""
Write-Host "=== Step 4: Difference Report ===" -ForegroundColor Yellow

$report = Join-Path $DiagDir "flag-diff-report.txt"
$reportLines = [System.Collections.Generic.List[string]]::new()

$reportLines.Add("IWFM Build Flag Comparison: CMake Monolithic vs .vfproj")
$reportLines.Add("Project: $Project  Configuration: $Configuration")
$reportLines.Add("Generated: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')")
$reportLines.Add("=" * 70)

# Compile flags
$reportLines.Add("")
$reportLines.Add("COMPILE FLAGS")
$reportLines.Add("-" * 40)

$cmakeOnly = [System.Collections.Generic.List[string]]::new()
$vfprojOnly = [System.Collections.Generic.List[string]]::new()
$shared = [System.Collections.Generic.List[string]]::new()

foreach ($f in $cmake.CompileFlags) {
    if ($vfproj.CompileFlags.Contains($f)) {
        $shared.Add($f)
    } else {
        $cmakeOnly.Add($f)
    }
}
foreach ($f in $vfproj.CompileFlags) {
    if (-not $cmake.CompileFlags.Contains($f)) {
        $vfprojOnly.Add($f)
    }
}

$reportLines.Add("")
$reportLines.Add("Shared flags ($($shared.Count)):")
foreach ($f in ($shared | Sort-Object)) {
    $reportLines.Add("  $f")
}

$reportLines.Add("")
$reportLines.Add("CMake ONLY ($($cmakeOnly.Count)):")
foreach ($f in ($cmakeOnly | Sort-Object)) {
    $line = "  $f"
    # Annotate known performance impacts
    if ($f -match '/Qopenmp') { $line += "  *** PERF: OpenMP overhead on non-parallel targets" }
    if ($f -match '/fp:fast') { $line += "  *** PERF: Different FP model than vfproj" }
    if ($f -match '/MT') { $line += "  (static CRT)" }
    $reportLines.Add($line)
}

$reportLines.Add("")
$reportLines.Add(".vfproj ONLY ($($vfprojOnly.Count)):")
foreach ($f in ($vfprojOnly | Sort-Object)) {
    $line = "  $f"
    if ($f -match '/fp:consistent') { $line += "  *** vfproj uses consistent FP" }
    $reportLines.Add($line)
}

# Response files
$reportLines.Add("")
$reportLines.Add("RESPONSE FILES (CMake/Ninja)")
$reportLines.Add("-" * 40)
if ($cmake.RspFiles.Count -gt 0) {
    foreach ($rsp in $cmake.RspFiles) {
        $reportLines.Add("  @$rsp")
        if (Test-Path $rsp) {
            $reportLines.Add("    Contents: $(Get-Content $rsp -Raw)")
        }
    }
} else {
    $reportLines.Add("  None detected in log (check build.ninja for rspfile directives)")
}

# Link flags
$reportLines.Add("")
$reportLines.Add("LINK FLAGS")
$reportLines.Add("-" * 40)
$reportLines.Add("CMake link flags:")
foreach ($f in $cmake.LinkFlags) { $reportLines.Add("  $f") }
$reportLines.Add(".vfproj link flags:")
foreach ($f in $vfproj.LinkFlags) { $reportLines.Add("  $f") }

# Object link order
$reportLines.Add("")
$reportLines.Add("OBJECT LINK ORDER")
$reportLines.Add("-" * 40)
$reportLines.Add("CMake .obj count: $($cmake.ObjOrder.Count)")
$reportLines.Add(".vfproj .obj count: $($vfproj.ObjOrder.Count)")

if ($cmake.ObjOrder.Count -gt 0 -and $vfproj.ObjOrder.Count -gt 0) {
    $reportLines.Add("")
    $reportLines.Add("First 10 .obj files in each:")
    $reportLines.Add("  CMake:  $($cmake.ObjOrder[0..([Math]::Min(9, $cmake.ObjOrder.Count-1))] -join ', ')")
    $reportLines.Add("  vfproj: $($vfproj.ObjOrder[0..([Math]::Min(9, $vfproj.ObjOrder.Count-1))] -join ', ')")

    # Check if order differs
    $orderDiffers = $false
    $minCount = [Math]::Min($cmake.ObjOrder.Count, $vfproj.ObjOrder.Count)
    for ($i = 0; $i -lt $minCount; $i++) {
        if ($cmake.ObjOrder[$i] -ne $vfproj.ObjOrder[$i]) {
            $orderDiffers = $true
            $reportLines.Add("  First difference at position $i: cmake='$($cmake.ObjOrder[$i])' vs vfproj='$($vfproj.ObjOrder[$i])'")
            break
        }
    }
    if (-not $orderDiffers) {
        $reportLines.Add("  Link order matches!")
    }
}

# Check CMakeCache.txt for hidden flags
$reportLines.Add("")
$reportLines.Add("CMAKE CACHE - HIDDEN FLAGS")
$reportLines.Add("-" * 40)
$cacheFile = Join-Path $cmakeBuildDir "CMakeCache.txt"
if (Test-Path $cacheFile) {
    $cacheContent = Get-Content $cacheFile
    foreach ($line in $cacheContent) {
        if ($line -match '^CMAKE_Fortran_FLAGS') {
            $reportLines.Add("  $line")
        }
        if ($line -match '^CMAKE_EXE_LINKER_FLAGS') {
            $reportLines.Add("  $line")
        }
        if ($line -match '^CMAKE_SHARED_LINKER_FLAGS') {
            $reportLines.Add("  $line")
        }
    }
} else {
    $reportLines.Add("  CMakeCache.txt not found (run without -SkipBuild)")
}

# Check for Ninja response file directives
$reportLines.Add("")
$reportLines.Add("NINJA RESPONSE FILES")
$reportLines.Add("-" * 40)
$buildNinja = Join-Path $cmakeBuildDir "build.ninja"
if (Test-Path $buildNinja) {
    $rspLines = Select-String -Path $buildNinja -Pattern "rspfile" | Select-Object -First 10
    if ($rspLines) {
        foreach ($rl in $rspLines) {
            $reportLines.Add("  $($rl.Line.Trim())")
        }
    } else {
        $reportLines.Add("  No rspfile directives found")
    }
} else {
    $reportLines.Add("  build.ninja not found (run without -SkipBuild)")
}

# =============================================================================
# Step 5: Known issues summary
# =============================================================================
$reportLines.Add("")
$reportLines.Add("=" * 70)
$reportLines.Add("KNOWN PERFORMANCE ISSUES")
$reportLines.Add("=" * 70)

$issues = @()

# Check for OpenMP on non-parallel
if ($Project -ne "Simulation_Parallel" -and $cmake.CompileFlags.Contains("/Qopenmp")) {
    $issues += @{
        Severity = "HIGH"
        Issue = "OpenMP (/Qopenmp) enabled on non-parallel target '$Project'"
        Impact = "30-50% runtime overhead from thread management, barriers, TLS"
        Fix = "Remove unconditional iwfm_add_openmp_flags() in IWFMMonolithic.cmake:69-73"
    }
}

# Check FP model mismatch
$cmakeFP = $cmake.CompileFlags | Where-Object { $_ -match '/fp:' }
$vfprojFP = $vfproj.CompileFlags | Where-Object { $_ -match '/fp:' }
if ($cmakeFP -ne $vfprojFP) {
    $issues += @{
        Severity = "MEDIUM"
        Issue = "FP model mismatch: CMake='$cmakeFP' vs vfproj='$vfprojFP'"
        Impact = "Different codegen for FP operations; /fp:fast allows reordering"
        Fix = "Change CMake to /fp:consistent to match .vfproj"
    }
}

# Check for /MT difference
if ($cmake.CompileFlags.Contains("/MT") -and -not $vfproj.CompileFlags.Contains("/MT")) {
    $issues += @{
        Severity = "LOW"
        Issue = "CMake adds explicit /MT; .vfproj relies on /libs:static /threads"
        Impact = "Minimal — both use static CRT, but /MT may override compiler default"
        Fix = "Verify /MT is not causing CRT mismatch with Intel runtime"
    }
}

foreach ($issue in $issues) {
    $reportLines.Add("")
    $reportLines.Add("[$($issue.Severity)] $($issue.Issue)")
    $reportLines.Add("  Impact: $($issue.Impact)")
    $reportLines.Add("  Fix:    $($issue.Fix)")
}

if ($issues.Count -eq 0) {
    $reportLines.Add("  No known issues detected.")
}

# Write report
$reportContent = $reportLines -join "`r`n"
$reportContent | Out-File $report -Encoding UTF8
Write-Host ""
Write-Host $reportContent
Write-Host ""
Write-Host "Report saved to: $report" -ForegroundColor Green
Write-Host ""

# =============================================================================
# Step 6: Quick hypothesis tests (if build artifacts exist)
# =============================================================================
Write-Host "=== Step 5: Hypothesis Testing Guide ===" -ForegroundColor Yellow
Write-Host ""
Write-Host "To test each hypothesis, rebuild and benchmark:" -ForegroundColor White
Write-Host ""
Write-Host "  Hypothesis A (OpenMP overhead on non-parallel targets):" -ForegroundColor Cyan
Write-Host "    The fix in IWFMMonolithic.cmake removes unconditional /Qopenmp."
Write-Host "    Rebuild: .\build-iwfm.ps1 -Target Simulation" -ForegroundColor Gray
Write-Host "    Compare: runtime of old vs new CMake Simulation on sample model" -ForegroundColor Gray
Write-Host ""
Write-Host "  Hypothesis B (FP model):" -ForegroundColor Cyan
Write-Host "    CMake now uses /fp:consistent (matching .vfproj)."
Write-Host "    If still slow, try: edit IWFMCompilerFlags.cmake -> /fp:fast=1" -ForegroundColor Gray
Write-Host ""
Write-Host "  Hypothesis C (Pre-built deps):" -ForegroundColor Cyan
Write-Host "    .\build-deps.ps1 && .\build-iwfm.ps1 -UsePrebuiltDeps" -ForegroundColor Gray
Write-Host "    Pre-build dependencies to avoid recompilation variance." -ForegroundColor Gray
Write-Host ""
Write-Host "  Hypothesis D (Link order / code layout):" -ForegroundColor Cyan
Write-Host "    Compare .obj order in the report above." -ForegroundColor Gray
Write-Host "    Different order changes instruction cache behavior." -ForegroundColor Gray
Write-Host ""
