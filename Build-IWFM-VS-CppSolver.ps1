<#
.SYNOPSIS
    Build IWFM with C++ solver from .vfproj files via direct ifx.exe/icx.exe invocation.

.DESCRIPTION
    Extends Build-IWFM-VS.ps1 to compile the C++ sparse solver library and link
    it into the IWFM executable. Passes /DIWFM_CPP_SOLVER to the Fortran compiler
    so that Package_Matrix.f90 uses iwfm_pgmres_solve() instead of the Fortran
    SPARSKIT reverse-communication loop.

.PARAMETER Project
    Which project to build.

.PARAMETER Configuration
    Release or Debug.

.PARAMETER Clean
    Remove intermediate directory before building.

.PARAMETER ShowCommands
    Print each compiler/linker invocation.

.EXAMPLE
    .\Build-IWFM-VS-CppSolver.ps1 -Clean
    Clean build of Simulation_Parallel with C++ solver.
#>

[CmdletBinding()]
param(
    [ValidateSet("Simulation", "Simulation_Parallel", "PreProcessor", "Budget", "ZBudget")]
    [string]$Project = "Simulation_Parallel",

    [ValidateSet("Release", "Debug")]
    [string]$Configuration = "Release",

    [switch]$Clean,

    [switch]$ShowCommands
)

$ErrorActionPreference = "Stop"
$script:ExitCode = 0

# ── Paths ──────────────────────────────────────────────────────────────────────
$ScriptDir   = Split-Path -Parent $MyInvocation.MyCommand.Path
$BinDir      = Join-Path $ScriptDir "Bin"
$SourceDir   = Join-Path $ScriptDir "SourceCode"
$CppSolverDir = Join-Path $SourceDir "IWFM-kernel\Package_Matrix\cpp"

# ── Banner ─────────────────────────────────────────────────────────────────────
Write-Host ""
Write-Host "  Build IWFM + C++ Solver (vfproj-style)" -ForegroundColor Cyan
Write-Host "  Project:       $Project" -ForegroundColor White
Write-Host "  Configuration: $Configuration" -ForegroundColor White
Write-Host "  C++ Solver:    ENABLED" -ForegroundColor Yellow
Write-Host ""

# ── Environment Setup ─────────────────────────────────────────────────────────
function Initialize-BuildEnvironment {
    Write-Host "Initializing build environment..." -ForegroundColor Yellow

    $VSWhere = "${env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"
    if (-not (Test-Path $VSWhere)) {
        throw "Visual Studio not found."
    }

    $script:VSInstallPath = & $VSWhere -latest -property installationPath
    $VCVarsAll = Join-Path $script:VSInstallPath "VC\Auxiliary\Build\vcvars64.bat"

    $OneAPIRoot = "${env:ProgramFiles(x86)}\Intel\oneAPI"
    $OneAPISetVars = Join-Path $OneAPIRoot "setvars.bat"

    if (-not (Test-Path $OneAPISetVars)) {
        throw "Intel oneAPI not found."
    }

    $IntelCompilerDir = Get-ChildItem -Path "$OneAPIRoot\compiler" -Directory |
        Where-Object { $_.Name -match '^\d+\.\d+' } |
        Sort-Object Name -Descending |
        Select-Object -First 1

    $script:IntelLibPath = Join-Path $IntelCompilerDir.FullName "lib"
    $script:IntelBinPath = Join-Path $IntelCompilerDir.FullName "bin"
    $script:IFX = Join-Path $script:IntelBinPath "ifx.exe"
    $script:ICX = Join-Path $script:IntelBinPath "icx.exe"

    $MSVCToolsDir = Get-ChildItem -Path "$script:VSInstallPath\VC\Tools\MSVC" -Directory |
        Sort-Object Name -Descending |
        Select-Object -First 1
    $script:MSVCBinPath = Join-Path $MSVCToolsDir.FullName "bin\Hostx64\x64"

    # Capture environment
    $TempBatch = [System.IO.Path]::GetTempFileName() + ".bat"
    $TempEnv   = [System.IO.Path]::GetTempFileName()
    $BatchContent = "@echo off`r`ncall `"$VCVarsAll`" >nul 2>&1`r`ncall `"$OneAPISetVars`" intel64 vs2022 >nul 2>&1`r`nset > `"$TempEnv`"`r`n"
    [System.IO.File]::WriteAllText($TempBatch, $BatchContent)
    Start-Process -FilePath "cmd.exe" -ArgumentList "/c `"$TempBatch`"" -Wait -NoNewWindow -PassThru | Out-Null
    if (Test-Path $TempEnv) {
        Get-Content $TempEnv | ForEach-Object {
            if ($_ -match '^([^=]+)=(.*)$') {
                [Environment]::SetEnvironmentVariable($matches[1], $matches[2], "Process")
            }
        }
        Remove-Item $TempEnv -Force -ErrorAction SilentlyContinue
    }
    Remove-Item $TempBatch -Force -ErrorAction SilentlyContinue

    $env:LIB  = "$script:IntelLibPath;$env:LIB"
    $env:PATH = "$script:MSVCBinPath;$script:IntelBinPath;$env:PATH"

    Write-Host "  ifx: $script:IFX" -ForegroundColor Gray
    Write-Host "  icx: $script:ICX" -ForegroundColor Gray
    Write-Host "Environment ready." -ForegroundColor Green
    Write-Host ""
}

# ── Run a process and check exit code ─────────────────────────────────────────
function Invoke-Compiler {
    param(
        [string]$Exe,
        [string]$CompilerArgs,
        [string]$Label,
        [switch]$Show
    )
    if ($Show) {
        Write-Host "$Label $CompilerArgs" -ForegroundColor Gray
    }
    $psi = New-Object System.Diagnostics.ProcessStartInfo
    $psi.FileName = $Exe
    $psi.Arguments = $CompilerArgs
    $psi.UseShellExecute = $false
    $psi.RedirectStandardOutput = $true
    $psi.RedirectStandardError = $true
    $psi.CreateNoWindow = $true

    $process = [System.Diagnostics.Process]::Start($psi)
    $stdout = $process.StandardOutput.ReadToEnd()
    $stderr = $process.StandardError.ReadToEnd()
    $process.WaitForExit()

    if ($stdout.Trim()) { Write-Host $stdout }
    if ($stderr.Trim()) { Write-Host $stderr -ForegroundColor Yellow }

    if ($process.ExitCode -ne 0) {
        throw "Compilation failed (exit code $($process.ExitCode)): $Label"
    }
}

# ── Fortran module scanning (same as Build-IWFM-VS.ps1) ──────────────────────
function Get-FortranModuleInfo {
    param([string]$FilePath)
    $provides = [System.Collections.Generic.List[string]]::new()
    $uses     = [System.Collections.Generic.List[string]]::new()
    foreach ($line in [System.IO.File]::ReadLines($FilePath)) {
        $trimmed = $line.Trim()
        if ($trimmed.StartsWith('!')) { continue }
        if ($trimmed -match '(?i)^\s*MODULE\s+(?!PROCEDURE\b|FUNCTION\b|SUBROUTINE\b)(\w+)') {
            $modName = $matches[1].ToLower()
            if (-not $provides.Contains($modName)) { $provides.Add($modName) }
        }
        if ($trimmed -match '(?i)^\s*USE\s+(\w+)') {
            $usedMod = $matches[1].ToLower()
            if (-not $uses.Contains($usedMod)) { $uses.Add($usedMod) }
        }
    }
    return @{ Provides = $provides; Uses = $uses }
}

function Build-SourceTreeIndex {
    param([string]$SourceRoot)
    $index = @{}
    $allF90 = Get-ChildItem -Path $SourceRoot -Filter "*.f90" -Recurse -File
    foreach ($f in $allF90) {
        foreach ($line in [System.IO.File]::ReadLines($f.FullName)) {
            $trimmed = $line.Trim()
            if ($trimmed.StartsWith('!')) { continue }
            if ($trimmed -match '(?i)^\s*MODULE\s+(?!PROCEDURE\b|FUNCTION\b|SUBROUTINE\b)(\w+)') {
                $modName = $matches[1].ToLower()
                if (-not $index.ContainsKey($modName)) {
                    $index[$modName] = $f.FullName
                }
            }
        }
    }
    return $index
}

function Resolve-AndSort {
    param(
        [string[]]$Files,
        [string]$SourceRoot
    )

    $externalModules = [System.Collections.Generic.HashSet[string]]::new(
        [string[]]@("iso_fortran_env", "iso_c_binding", "omp_lib", "omp_lib_kinds",
                     "ifport", "ifcore", "ieee_arithmetic", "ieee_exceptions", "ieee_features"),
        [System.StringComparer]::OrdinalIgnoreCase)

    Write-Host "  Indexing source tree for module discovery..." -ForegroundColor Gray
    $treeIndex = Build-SourceTreeIndex -SourceRoot $SourceRoot

    $fileSet = [System.Collections.Generic.HashSet[string]]::new(
        [string[]]$Files, [System.StringComparer]::OrdinalIgnoreCase)
    $fileList = [System.Collections.Generic.List[string]]::new([string[]]$Files)

    $changed = $true
    while ($changed) {
        $changed = $false
        $moduleProvider = @{}
        $fileUses = @{}
        foreach ($f in @($fileList)) {
            $info = Get-FortranModuleInfo -FilePath $f
            $fileUses[$f] = $info.Uses
            foreach ($mod in $info.Provides) {
                if (-not $moduleProvider.ContainsKey($mod)) {
                    $moduleProvider[$mod] = $f
                }
            }
        }
        foreach ($f in @($fileList)) {
            foreach ($mod in $fileUses[$f]) {
                if ($externalModules.Contains($mod)) { continue }
                if ($moduleProvider.ContainsKey($mod)) { continue }
                if ($treeIndex.ContainsKey($mod)) {
                    $provider = $treeIndex[$mod]
                    if (-not $fileSet.Contains($provider)) {
                        $fileSet.Add($provider) | Out-Null
                        $fileList.Add($provider)
                        Write-Host "  Auto-added: $([System.IO.Path]::GetFileName($provider)) (provides '$mod')" -ForegroundColor Gray
                        $changed = $true
                    }
                }
            }
        }
    }

    # Final scan + topological sort (Kahn's algorithm)
    $moduleProvider = @{}
    $fileUses = @{}
    foreach ($f in @($fileList)) {
        $info = Get-FortranModuleInfo -FilePath $f
        $fileUses[$f] = $info.Uses
        foreach ($mod in $info.Provides) {
            if (-not $moduleProvider.ContainsKey($mod)) { $moduleProvider[$mod] = $f }
        }
    }
    $deps = @{}
    foreach ($f in @($fileList)) {
        $deps[$f] = [System.Collections.Generic.List[string]]::new()
        foreach ($mod in $fileUses[$f]) {
            if ($moduleProvider.ContainsKey($mod)) {
                $provider = $moduleProvider[$mod]
                if ($provider -ne $f -and $fileSet.Contains($provider)) {
                    if (-not $deps[$f].Contains($provider)) { $deps[$f].Add($provider) }
                }
            }
        }
    }
    $allFiles = @($fileList)
    $inDeg = @{}; $dependents = @{}
    foreach ($f in $allFiles) {
        $inDeg[$f] = $deps[$f].Count
        $dependents[$f] = [System.Collections.Generic.List[string]]::new()
    }
    foreach ($f in $allFiles) {
        foreach ($dep in $deps[$f]) { $dependents[$dep].Add($f) }
    }
    $queue = [System.Collections.Generic.Queue[string]]::new()
    foreach ($f in $allFiles) {
        if ($inDeg[$f] -eq 0) { $queue.Enqueue($f) }
    }
    $sorted = [System.Collections.Generic.List[string]]::new()
    while ($queue.Count -gt 0) {
        $f = $queue.Dequeue()
        $sorted.Add($f)
        foreach ($dependent in $dependents[$f]) {
            $inDeg[$dependent]--
            if ($inDeg[$dependent] -eq 0) { $queue.Enqueue($dependent) }
        }
    }
    if ($sorted.Count -lt $allFiles.Count) {
        foreach ($f in $allFiles) {
            if (-not $sorted.Contains($f)) { $sorted.Add($f) }
        }
    }
    return @($sorted)
}

# ── Resolve VS macros ─────────────────────────────────────────────────────────
function Resolve-VSMacros {
    param([string]$Text, [string]$ProjectDir, [string]$ConfigName, [string]$TargetName, [string]$TargetExt)
    $result = $Text
    $result = $result -replace '\$\(ProjectDir\)',       $ProjectDir
    $result = $result -replace '\$\(PlatformName\)',     'x64'
    $result = $result -replace '\$\(ConfigurationName\)',$ConfigName
    $result = $result -replace '\$\(TargetName\)',       $TargetName
    $result = $result -replace '\$\(TargetExt\)',        $TargetExt
    $outDir = "${ProjectDir}x64\${ConfigName}\"
    $result = $result -replace '\$\(OutDir\)', $outDir
    return $result
}

# ── Parse .vfproj (same as Build-IWFM-VS.ps1) ────────────────────────────────
function Parse-VFProj {
    param([string]$VFProjFile, [string]$Config)

    [xml]$xml = Get-Content $VFProjFile -Raw
    $root = $xml.VisualStudioProject
    $isDLL = ($root.GetAttribute("ProjectType") -eq "typeDynamicLibrary")
    $ProjectDir = (Split-Path -Parent (Resolve-Path $VFProjFile)).TrimEnd('\') + '\'
    $configName = "${Config}|x64"
    $configNode = $root.Configurations.Configuration | Where-Object { $_.Name -eq $configName }
    if (-not $configNode) { throw "Configuration '$configName' not found" }
    $configType = $configNode.GetAttribute("ConfigurationType")
    if ($configType -eq "typeDynamicLibrary") { $isDLL = $true }
    $targetName = $configNode.TargetName
    $targetExt  = if ($isDLL) { ".dll" } else { ".exe" }
    $intDir = Resolve-VSMacros -Text $configNode.IntermediateDirectory -ProjectDir $ProjectDir -ConfigName $Config -TargetName $targetName -TargetExt $targetExt
    if (-not $intDir -or $intDir -eq $configNode.IntermediateDirectory) { $intDir = "${ProjectDir}x64\${Config}" }
    if (-not [System.IO.Path]::IsPathRooted($intDir)) { $intDir = Join-Path $ProjectDir $intDir }
    $intDir = [System.IO.Path]::GetFullPath($intDir)

    $compilerTool = $configNode.Tool | Where-Object { $_.Name -eq "VFFortranCompilerTool" }
    $compilerFlags = @()
    if ($compilerTool.SuppressStartupBanner -eq "true") { $compilerFlags += "/nologo" }
    if ($compilerTool.Preprocess -eq "preprocessYes") { $compilerFlags += "/fpp" }
    if ($compilerTool.F2003Semantics -eq "true") { $compilerFlags += "/standard-semantics" }
    if ($compilerTool.OpenMP -eq "OpenMPParallelCode") { $compilerFlags += "/Qopenmp" }
    $optAttr = $compilerTool.Optimization
    switch ($optAttr) {
        "optimizeDisabled" { $compilerFlags += "/Od" }
        "optimizeFull"     { $compilerFlags += "/O2" }
        default { if ($Config -eq "Release") { $compilerFlags += "/O2" } }
    }
    if ($compilerTool.DebugInformationFormat -eq "debugEnabled") { $compilerFlags += "/debug:full" }
    if ($compilerTool.Traceback -eq "true") { $compilerFlags += "/traceback" }
    if ($compilerTool.BoundsCheck -eq "true") { $compilerFlags += "/check:bounds" }
    if ($compilerTool.StackFrameCheck -eq "true") { $compilerFlags += "/Gs0" }
    $rtLib = $compilerTool.RuntimeLibrary
    switch ($rtLib) {
        "rtMultiThreadedDebug" { $compilerFlags += "/libs:static", "/threads", "/dbglibs" }
        default { $compilerFlags += "/libs:static", "/threads" }
    }
    if ($compilerTool.HeapArrays) { $compilerFlags += "/heap-arrays:$($compilerTool.HeapArrays)" }
    if ($compilerTool.WarnInterfaces -eq "true") { $compilerFlags += "/warn:interfaces" }
    if ($compilerTool.WarnUnusedVariables -eq "true") { $compilerFlags += "/warn:unused" }
    if ($compilerTool.WarnUncalled -eq "true") { $compilerFlags += "/warn:uncalled" }
    if ($compilerTool.DisableSpecificDiagnostics) {
        foreach ($diag in ($compilerTool.DisableSpecificDiagnostics -split '[;,]')) {
            $diag = $diag.Trim(); if ($diag) { $compilerFlags += "/Qdiag-disable:$diag" }
        }
    }
    if ($compilerTool.AdditionalOptions) {
        $compilerFlags += ($compilerTool.AdditionalOptions -split '\s+' | Where-Object { $_ })
    }

    $includeDirs = @()
    if ($compilerTool.AdditionalIncludeDirectories) {
        foreach ($inc in ($compilerTool.AdditionalIncludeDirectories -split ';')) {
            $inc = $inc.Trim(); if (-not $inc) { continue }
            $resolved = Resolve-VSMacros -Text $inc -ProjectDir $ProjectDir -ConfigName $Config -TargetName $targetName -TargetExt $targetExt
            if (-not [System.IO.Path]::IsPathRooted($resolved)) { $resolved = Join-Path $ProjectDir $resolved }
            $includeDirs += [System.IO.Path]::GetFullPath($resolved)
        }
    }

    $linkerTool = $configNode.Tool | Where-Object { $_.Name -eq "VFLinkerTool" }
    $linkerFlags = @()
    if ($linkerTool.SuppressStartupBanner -eq "true") { $linkerFlags += "/nologo" }
    switch ($linkerTool.SubSystem) {
        "subSystemConsole" { $linkerFlags += "/SUBSYSTEM:CONSOLE" }
        "subSystemWindows" { $linkerFlags += "/SUBSYSTEM:WINDOWS" }
    }
    if ($linkerTool.StackReserveSize) { $linkerFlags += "/STACK:$($linkerTool.StackReserveSize)" }
    if ($linkerTool.GenerateDebugInformation -eq "true") { $linkerFlags += "/DEBUG" }
    if ($linkerTool.LinkIncremental -eq "linkIncrementalNo") { $linkerFlags += "/INCREMENTAL:NO" }

    $libDirs = @()
    if ($linkerTool.AdditionalLibraryDirectories) {
        foreach ($ld in ($linkerTool.AdditionalLibraryDirectories -split ';')) {
            $ld = $ld.Trim(); if (-not $ld) { continue }
            $resolved = Resolve-VSMacros -Text $ld -ProjectDir $ProjectDir -ConfigName $Config -TargetName $targetName -TargetExt $targetExt
            if (-not [System.IO.Path]::IsPathRooted($resolved)) { $resolved = Join-Path $ProjectDir $resolved }
            $libDirs += [System.IO.Path]::GetFullPath($resolved)
        }
    }

    $libs = @()
    if ($linkerTool.AdditionalDependencies) {
        $libs = ($linkerTool.AdditionalDependencies -split '\s+' | Where-Object { $_ })
    }

    # Collect source files from vfproj
    $sourceFiles = [System.Collections.Generic.List[string]]::new()
    function Collect-Files($node, $fileList, $projDir) {
        foreach ($child in $node.ChildNodes) {
            if ($child.LocalName -eq "File") {
                $relPath = $child.GetAttribute("RelativePath")
                if ($relPath -and $relPath -match '\.f90$') {
                    $absPath = [System.IO.Path]::GetFullPath((Join-Path $projDir $relPath))
                    if (-not $fileList.Contains($absPath)) { $fileList.Add($absPath) }
                }
            } elseif ($child.LocalName -eq "Filter") {
                Collect-Files $child $fileList $projDir
            }
        }
    }
    Collect-Files $root.Files $sourceFiles $ProjectDir

    return @{
        TargetName = $targetName; TargetExt = $targetExt; IsDLL = $isDLL; IntDir = $intDir
        CompilerFlags = $compilerFlags; IncludeDirs = $includeDirs
        LinkerFlags = $linkerFlags; LibDirs = $libDirs; Libs = $libs
        SourceFiles = @($sourceFiles); ProjectDir = $ProjectDir
    }
}

# ── Main Build ─────────────────────────────────────────────────────────────────
try {
    Initialize-BuildEnvironment

    # Map project names to .vfproj paths
    $ProjectMap = @{
        "Simulation"          = "Simulation\Simulation.vfproj"
        "Simulation_Parallel" = "Simulation_Parallel\Simulation_Parallel.vfproj"
        "PreProcessor"        = "PreProcessor\PreProcessor.vfproj"
        "Budget"              = "Budget\Budget.vfproj"
        "ZBudget"             = "ZBudget\Z-Budget.vfproj"
    }
    $VFProjPath = Join-Path $ScriptDir $ProjectMap[$Project]
    if (-not (Test-Path $VFProjPath)) { throw "Project file not found: $VFProjPath" }

    # Parse project
    Write-Host "Parsing vfproj..." -ForegroundColor Yellow
    $proj = Parse-VFProj -VFProjFile $VFProjPath -Config $Configuration
    Write-Host "  Target: $($proj.TargetName)$($proj.TargetExt) ($($proj.SourceFiles.Count) Fortran files)" -ForegroundColor Gray

    # Resolve dependencies and sort
    Write-Host "Resolving module dependencies..." -ForegroundColor Yellow
    $proj.SourceFiles = Resolve-AndSort -Files $proj.SourceFiles -SourceRoot $SourceDir
    Write-Host "  $($proj.SourceFiles.Count) files total after dependency resolution." -ForegroundColor Gray
    Write-Host ""

    # Clean
    if ($Clean -and (Test-Path $proj.IntDir)) {
        Write-Host "Cleaning: $($proj.IntDir)" -ForegroundColor Yellow
        Remove-Item -Path $proj.IntDir -Recurse -Force
    }
    if (-not (Test-Path $proj.IntDir)) { New-Item -ItemType Directory -Path $proj.IntDir -Force | Out-Null }
    if (-not (Test-Path $BinDir)) { New-Item -ItemType Directory -Path $BinDir -Force | Out-Null }

    # ══════════════════════════════════════════════════════════════════════════
    # STEP 1: Compile C++ solver sources with icx.exe
    # ══════════════════════════════════════════════════════════════════════════
    Write-Host "=== Compiling C++ Solver ===" -ForegroundColor Cyan
    $sw = [System.Diagnostics.Stopwatch]::StartNew()

    $cppSources = @("blas.cpp", "spmv.cpp", "ilut.cpp", "lusol.cpp", "gmres.cpp", "pgmres_solve.cpp")
    $cppObjFiles = @()

    # C++ compiler flags matching the CMake configuration
    $cppFlags = @("/nologo", "/c", "/std:c++17", "/EHsc")
    if ($Configuration -eq "Release") {
        $cppFlags += "/O2", "/DNDEBUG", "/MT"
    } else {
        $cppFlags += "/Od", "/Zi", "/MTd"
    }
    # OpenMP for spmv.cpp
    $cppFlags += "/Qopenmp"

    $cppNum = 0
    foreach ($src in $cppSources) {
        $cppNum++
        $srcPath = Join-Path $CppSolverDir $src
        $objFile = Join-Path $proj.IntDir ([System.IO.Path]::ChangeExtension($src, ".obj"))
        $cppObjFiles += $objFile

        Write-Host "  [$cppNum/$($cppSources.Count)] $src" -ForegroundColor White

        $argsStr = ($cppFlags -join " ") + " /I""$CppSolverDir"" /Fo""$objFile"" ""$srcPath"""
        Invoke-Compiler -Exe $script:ICX -CompilerArgs $argsStr -Label "icx" -Show:$ShowCommands
    }

    $cppTime = $sw.Elapsed
    Write-Host "C++ solver compiled: $($cppTime.Seconds)s ($($cppObjFiles.Count) files)" -ForegroundColor Green
    Write-Host ""

    # ══════════════════════════════════════════════════════════════════════════
    # STEP 2: Compile Fortran sources with /DIWFM_CPP_SOLVER
    # ══════════════════════════════════════════════════════════════════════════
    Write-Host "=== Compiling Fortran ($($proj.SourceFiles.Count) files) ===" -ForegroundColor Cyan
    $sw.Restart()
    $objFiles = @()
    $fileNum = 0
    $totalFiles = $proj.SourceFiles.Count

    foreach ($srcFile in $proj.SourceFiles) {
        $fileNum++
        $baseName = [System.IO.Path]::GetFileNameWithoutExtension($srcFile)
        $objFile  = Join-Path $proj.IntDir "$baseName.obj"
        $objFiles += $objFile

        $args = @("/c")
        $args += $proj.CompilerFlags
        # Add IWFM_CPP_SOLVER preprocessor define
        $args += "/DIWFM_CPP_SOLVER"
        foreach ($inc in $proj.IncludeDirs) { $args += "/I""$inc""" }
        $args += "/module:""$($proj.IntDir)"""
        $args += "/I""$($proj.IntDir)"""
        $args += "/object:""$objFile"""
        $args += """$srcFile"""

        $argsStr = $args -join " "

        if ($ShowCommands) {
            Write-Host "[$fileNum/$totalFiles] ifx $argsStr" -ForegroundColor Gray
        } else {
            $pct = [math]::Floor(($fileNum / $totalFiles) * 100)
            Write-Host "  [$fileNum/$totalFiles] ($pct%) $baseName.f90" -ForegroundColor White -NoNewline
            Write-Host ""
        }

        Invoke-Compiler -Exe $script:IFX -CompilerArgs $argsStr -Label "ifx" -Show:$false
    }

    $compileTime = $sw.Elapsed
    Write-Host "Fortran compiled: $($compileTime.Minutes)m $($compileTime.Seconds)s" -ForegroundColor Green
    Write-Host ""

    # ══════════════════════════════════════════════════════════════════════════
    # STEP 3: Link everything (Fortran .obj + C++ .obj + libraries)
    # ══════════════════════════════════════════════════════════════════════════
    Write-Host "=== Linking ===" -ForegroundColor Cyan
    $sw.Restart()

    $outDir = Join-Path $proj.ProjectDir "x64\$Configuration"
    if (-not (Test-Path $outDir)) { New-Item -ItemType Directory -Path $outDir -Force | Out-Null }
    $outputFile = Join-Path $outDir "$($proj.TargetName)$($proj.TargetExt)"

    $linkArgs = @("/nologo")

    # OpenMP
    if ($proj.CompilerFlags -contains "/Qopenmp") { $linkArgs += "/Qopenmp" }

    # All Fortran object files
    foreach ($obj in $objFiles) { $linkArgs += """$obj""" }

    # All C++ solver object files
    foreach ($obj in $cppObjFiles) { $linkArgs += """$obj""" }

    $linkArgs += "/Fe:""$outputFile"""
    $linkArgs += "/link"
    $linkArgs += $proj.LinkerFlags

    foreach ($ld in $proj.LibDirs) { $linkArgs += "/LIBPATH:""$ld""" }

    # DSS7 heclib from CMake build
    $cmakeBuildDir = Join-Path $ScriptDir "build"
    $dss7Lib = Join-Path $cmakeBuildDir "heclib.lib"
    $zlibLib = Join-Path $cmakeBuildDir "_deps\zlib-build\zlibstatic.lib"

    $hasHeclibCompat = $proj.SourceFiles | Where-Object { $_ -match 'heclib_compat\.f90$' }
    if ($hasHeclibCompat -and (Test-Path $dss7Lib)) {
        Write-Host "  Using DSS7 heclib from CMake build" -ForegroundColor Gray
        $filteredLibs = $proj.Libs | Where-Object { $_ -notmatch 'heclib' }
        foreach ($lib in $filteredLibs) { $linkArgs += $lib }
        $linkArgs += """$dss7Lib"""
        if (Test-Path $zlibLib) { $linkArgs += """$zlibLib""" }
        $linkArgs += "ws2_32.lib"
        $linkArgs += "shlwapi.lib"
    } else {
        foreach ($lib in $proj.Libs) { $linkArgs += $lib }
    }

    $linkArgsStr = $linkArgs -join " "
    if ($ShowCommands) { Write-Host "ifx $linkArgsStr" -ForegroundColor Gray }

    Invoke-Compiler -Exe $script:IFX -CompilerArgs $linkArgsStr -Label "ifx link" -Show:$false

    $linkTime = $sw.Elapsed
    Write-Host "Linking complete: $($linkTime.Seconds)s" -ForegroundColor Green
    Write-Host ""

    # ── Post-build: copy to Bin ────────────────────────────────────────────────
    Write-Host "=== Post-build ===" -ForegroundColor Cyan
    if (Test-Path $outputFile) {
        $destFile = Join-Path $BinDir "$($proj.TargetName)$($proj.TargetExt)"
        Copy-Item -Path $outputFile -Destination $destFile -Force
        $size = [math]::Round((Get-Item $destFile).Length / 1MB, 1)
        Write-Host "  Copied to: $destFile ($size MB)" -ForegroundColor White
    }

    Write-Host ""
    Write-Host "Build successful (with C++ solver)." -ForegroundColor Green
    Write-Host "  Output:    $destFile" -ForegroundColor White
    Write-Host "  C++ time:  $($cppTime.Seconds)s" -ForegroundColor Gray
    Write-Host "  F90 time:  $($compileTime.Minutes)m $($compileTime.Seconds)s" -ForegroundColor Gray
    Write-Host "  Link time: $($linkTime.Seconds)s" -ForegroundColor Gray
    Write-Host ""
}
catch {
    Write-Host ""
    Write-Host "ERROR: $_" -ForegroundColor Red
    Write-Host $_.ScriptStackTrace -ForegroundColor DarkGray
    $script:ExitCode = 1
}

exit $script:ExitCode
