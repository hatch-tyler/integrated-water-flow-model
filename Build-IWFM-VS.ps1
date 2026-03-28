<#
.SYNOPSIS
    Build IWFM projects from .vfproj files via direct ifx.exe invocation.

.DESCRIPTION
    Parses .vfproj XML and drives ifx.exe directly, compiling files sequentially
    to respect Fortran module dependencies. This replicates the VS2022 IDE build
    without requiring MSBuild (which rejects .vfproj) or devenv.exe (which fails
    on clean builds due to parallel compilation of dependent modules).

.PARAMETER Project
    Which project to build.

.PARAMETER Configuration
    Release or Debug.

.PARAMETER Clean
    Remove intermediate directory before building.

.PARAMETER Verbose
    Print each compiler/linker invocation.

.EXAMPLE
    .\Build-IWFM-VS.ps1 -Project Simulation_Parallel -Clean
    Clean build of the OpenMP parallel simulation.

.EXAMPLE
    .\Build-IWFM-VS.ps1 -Project Budget -Configuration Debug -Verbose
    Debug build of Budget with full command output.
#>

[CmdletBinding()]
param(
    [ValidateSet("Simulation", "Simulation_Parallel", "Simulation_MM", "PreProcessor", "Budget", "ZBudget", "IWFM_C_DLL")]
    [string]$Project = "Simulation_Parallel",

    [ValidateSet("Release", "Debug")]
    [string]$Configuration = "Release",

    [switch]$Clean,

    [switch]$ShowCommands,

    [string]$DepsDir = "deps",

    [switch]$BuildDeps
)

$ErrorActionPreference = "Stop"
$script:ExitCode = 0

# ── Paths ──────────────────────────────────────────────────────────────────────
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$BinDir    = Join-Path $ScriptDir "Bin"

# Map project names to .vfproj paths
$ProjectMap = @{
    "Simulation"          = "Simulation\Simulation.vfproj"
    "Simulation_Parallel" = "Simulation_Parallel\Simulation_Parallel.vfproj"
    "Simulation_MM"       = "Simulation_MM\Simulation_MM.vfproj"
    "PreProcessor"        = "PreProcessor\PreProcessor.vfproj"
    "Budget"              = "Budget\Budget.vfproj"
    "ZBudget"             = "ZBudget\Z-Budget.vfproj"
    "IWFM_C_DLL"         = "IWFM_DLL\IWFM_C_DLL_Heap.vfproj"
}

$VFProjRelPath = $ProjectMap[$Project]
$VFProjPath    = Join-Path $ScriptDir $VFProjRelPath

if (-not (Test-Path $VFProjPath)) {
    Write-Host "ERROR: Project file not found: $VFProjPath" -ForegroundColor Red
    exit 1
}

# ── Banner ─────────────────────────────────────────────────────────────────────
Write-Host ""
Write-Host "  Build IWFM from .vfproj" -ForegroundColor Cyan
Write-Host "  Project:       $Project" -ForegroundColor White
Write-Host "  Configuration: $Configuration" -ForegroundColor White
Write-Host "  Dependencies:  $DepsDir" -ForegroundColor White
Write-Host "  VFProj:        $VFProjPath" -ForegroundColor Gray
Write-Host ""

# ── Environment Setup (reused from build-iwfm.ps1) ────────────────────────────
function Initialize-BuildEnvironment {
    Write-Host "Initializing build environment..." -ForegroundColor Yellow

    $VSWhere = "${env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"
    if (-not (Test-Path $VSWhere)) {
        throw "Visual Studio not found. Install Visual Studio 2022 with C++ tools."
    }

    $script:VSInstallPath = & $VSWhere -latest -property installationPath
    $VCVarsAll = Join-Path $script:VSInstallPath "VC\Auxiliary\Build\vcvars64.bat"

    $OneAPIRoot = "${env:ProgramFiles(x86)}\Intel\oneAPI"
    $OneAPISetVars = Join-Path $OneAPIRoot "setvars.bat"

    if (-not (Test-Path $OneAPISetVars)) {
        throw "Intel oneAPI not found. Install Intel oneAPI HPC Toolkit."
    }

    $IntelCompilerDir = Get-ChildItem -Path "$OneAPIRoot\compiler" -Directory |
        Where-Object { $_.Name -match '^\d+\.\d+' } |
        Sort-Object Name -Descending |
        Select-Object -First 1

    if (-not $IntelCompilerDir) {
        throw "Intel compiler directory not found."
    }

    $script:IntelLibPath = Join-Path $IntelCompilerDir.FullName "lib"
    $script:IntelBinPath = Join-Path $IntelCompilerDir.FullName "bin"
    $script:IFX = Join-Path $script:IntelBinPath "ifx.exe"

    if (-not (Test-Path $script:IFX)) {
        throw "Intel Fortran compiler not found at: $script:IFX"
    }

    # Find MSVC tools path
    $MSVCToolsDir = Get-ChildItem -Path "$script:VSInstallPath\VC\Tools\MSVC" -Directory |
        Sort-Object Name -Descending |
        Select-Object -First 1
    $script:MSVCBinPath = Join-Path $MSVCToolsDir.FullName "bin\Hostx64\x64"

    # Capture environment from batch files
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
                [Environment]::SetEnvironmentVariable($matches[1], $matches[2], "Process")
            }
        }
        Remove-Item $TempEnv -Force -ErrorAction SilentlyContinue
    }
    Remove-Item $TempBatch -Force -ErrorAction SilentlyContinue

    $env:LIB  = "$script:IntelLibPath;$env:LIB"
    $env:PATH = "$script:MSVCBinPath;$script:IntelBinPath;$env:PATH"

    Write-Host "  Visual Studio:  $script:VSInstallPath" -ForegroundColor Gray
    Write-Host "  Intel Compiler: $($IntelCompilerDir.Name)" -ForegroundColor Gray
    Write-Host "  ifx:            $script:IFX" -ForegroundColor Gray
    Write-Host "Environment ready." -ForegroundColor Green
    Write-Host ""
}

# ── Macro Resolution ──────────────────────────────────────────────────────────
function Resolve-VSMacros {
    param(
        [string]$Text,
        [string]$ProjectDir,
        [string]$ConfigName,
        [string]$TargetName,
        [string]$TargetExt
    )
    $result = $Text
    $result = $result -replace '\$\(ProjectDir\)',       $ProjectDir
    $result = $result -replace '\$\(PlatformName\)',     'x64'
    $result = $result -replace '\$\(ConfigurationName\)',$ConfigName
    $result = $result -replace '\$\(TargetName\)',       $TargetName
    $result = $result -replace '\$\(TargetExt\)',        $TargetExt
    # $(OutDir) = $(ProjectDir)$(PlatformName)\$(ConfigurationName)\
    $outDir = "${ProjectDir}x64\${ConfigName}\"
    $result = $result -replace '\$\(OutDir\)', $outDir
    return $result
}

# ── Parse .vfproj ─────────────────────────────────────────────────────────────
function Parse-VFProj {
    param(
        [string]$VFProjFile,
        [string]$Config
    )

    [xml]$xml = Get-Content $VFProjFile -Raw
    $root = $xml.VisualStudioProject

    # Determine project type
    $projectType = $root.GetAttribute("ProjectType")
    $isDLL = ($projectType -eq "typeDynamicLibrary")

    $ProjectDir = (Split-Path -Parent (Resolve-Path $VFProjFile)).TrimEnd('\') + '\'

    # Find the matching configuration
    $configName = "${Config}|x64"
    $configNode = $root.Configurations.Configuration | Where-Object { $_.Name -eq $configName }

    if (-not $configNode) {
        throw "Configuration '$configName' not found in $VFProjFile"
    }

    # Check ConfigurationType on the config node itself
    $configType = $configNode.GetAttribute("ConfigurationType")
    if ($configType -eq "typeDynamicLibrary") { $isDLL = $true }

    $targetName = $configNode.TargetName
    $targetExt  = if ($isDLL) { ".dll" } else { ".exe" }
    $intDir     = Resolve-VSMacros -Text $configNode.IntermediateDirectory `
                    -ProjectDir $ProjectDir -ConfigName $Config -TargetName $targetName -TargetExt $targetExt

    # If IntermediateDirectory is not set, use default
    if (-not $intDir -or $intDir -eq $configNode.IntermediateDirectory) {
        $intDir = "${ProjectDir}x64\${Config}"
    }

    # Normalize to absolute path
    if (-not [System.IO.Path]::IsPathRooted($intDir)) {
        $intDir = Join-Path $ProjectDir $intDir
    }
    $intDir = [System.IO.Path]::GetFullPath($intDir)

    # ── Compiler flags ──
    $compilerTool = $configNode.Tool | Where-Object { $_.Name -eq "VFFortranCompilerTool" }
    $compilerFlags = @()

    # /nologo
    if ($compilerTool.SuppressStartupBanner -eq "true") {
        $compilerFlags += "/nologo"
    }

    # Preprocessing
    if ($compilerTool.Preprocess -eq "preprocessYes") {
        $compilerFlags += "/fpp"
    }

    # F2003 semantics
    if ($compilerTool.F2003Semantics -eq "true") {
        $compilerFlags += "/standard-semantics"
    }

    # OpenMP
    if ($compilerTool.OpenMP -eq "OpenMPParallelCode") {
        $compilerFlags += "/Qopenmp"
    }

    # Optimization
    $optAttr = $compilerTool.Optimization
    switch ($optAttr) {
        "optimizeDisabled" { $compilerFlags += "/Od" }
        "optimizeFull"     { $compilerFlags += "/O2" }
        default {
            if ($Config -eq "Release") { $compilerFlags += "/O2" }
        }
    }

    # Debug info
    if ($compilerTool.DebugInformationFormat -eq "debugEnabled") {
        $compilerFlags += "/debug:full"
    }

    # Traceback
    if ($compilerTool.Traceback -eq "true") {
        $compilerFlags += "/traceback"
    }

    # Bounds check
    if ($compilerTool.BoundsCheck -eq "true") {
        $compilerFlags += "/check:bounds"
    }

    # Stack frame check
    if ($compilerTool.StackFrameCheck -eq "true") {
        $compilerFlags += "/Gs0"
    }

    # Runtime library
    $rtLib = $compilerTool.RuntimeLibrary
    switch ($rtLib) {
        "rtMultiThreadedDebug" { $compilerFlags += "/libs:static", "/threads", "/dbglibs" }
        default {
            # Release default: static multithreaded
            $compilerFlags += "/libs:static", "/threads"
        }
    }

    # Heap arrays
    $heapArrays = $compilerTool.HeapArrays
    if ($heapArrays) {
        $compilerFlags += "/heap-arrays:$heapArrays"
    }

    # Warn interfaces
    if ($compilerTool.WarnInterfaces -eq "true") {
        $compilerFlags += "/warn:interfaces"
    }

    # Warn unused variables
    if ($compilerTool.WarnUnusedVariables -eq "true") {
        $compilerFlags += "/warn:unused"
    }

    # Warn uncalled
    if ($compilerTool.WarnUncalled -eq "true") {
        $compilerFlags += "/warn:uncalled"
    }

    # Disable specific diagnostics
    $diagDisable = $compilerTool.DisableSpecificDiagnostics
    if ($diagDisable) {
        foreach ($diag in ($diagDisable -split '[;,]')) {
            $diag = $diag.Trim()
            if ($diag) {
                $compilerFlags += "/Qdiag-disable:$diag"
            }
        }
    }

    # Additional options (passthrough verbatim)
    $additionalOpts = $compilerTool.AdditionalOptions
    if ($additionalOpts) {
        $compilerFlags += ($additionalOpts -split '\s+' | Where-Object { $_ })
    }

    # Include directories
    $includeDirs = @()
    $rawIncludes = $compilerTool.AdditionalIncludeDirectories
    if ($rawIncludes) {
        foreach ($inc in ($rawIncludes -split ';')) {
            $inc = $inc.Trim()
            if ($inc) {
                $resolved = Resolve-VSMacros -Text $inc -ProjectDir $ProjectDir `
                    -ConfigName $Config -TargetName $targetName -TargetExt $targetExt
                # Resolve relative path
                if (-not [System.IO.Path]::IsPathRooted($resolved)) {
                    $resolved = Join-Path $ProjectDir $resolved
                }
                $resolved = [System.IO.Path]::GetFullPath($resolved)
                $includeDirs += $resolved
            }
        }
    }

    # ── Linker flags ──
    $linkerTool = $configNode.Tool | Where-Object { $_.Name -eq "VFLinkerTool" }
    $linkerFlags = @()

    if ($linkerTool.SuppressStartupBanner -eq "true") {
        $linkerFlags += "/nologo"
    }

    # Subsystem
    $subSys = $linkerTool.SubSystem
    switch ($subSys) {
        "subSystemConsole" { $linkerFlags += "/SUBSYSTEM:CONSOLE" }
        "subSystemWindows" { $linkerFlags += "/SUBSYSTEM:WINDOWS" }
    }

    # Stack
    $stackSize = $linkerTool.StackReserveSize
    if ($stackSize) {
        $linkerFlags += "/STACK:$stackSize"
    }

    # Debug
    if ($linkerTool.GenerateDebugInformation -eq "true") {
        $linkerFlags += "/DEBUG"
    }

    # Incremental
    if ($linkerTool.LinkIncremental -eq "linkIncrementalNo") {
        $linkerFlags += "/INCREMENTAL:NO"
    }

    # DLL
    if ($linkerTool.LinkDLL -eq "true" -or $isDLL) {
        $linkerFlags += "/DLL"
    }

    # Library directories
    $libDirs = @()
    $rawLibDirs = $linkerTool.AdditionalLibraryDirectories
    if ($rawLibDirs) {
        foreach ($ld in ($rawLibDirs -split ';')) {
            $ld = $ld.Trim()
            if ($ld) {
                $resolved = Resolve-VSMacros -Text $ld -ProjectDir $ProjectDir `
                    -ConfigName $Config -TargetName $targetName -TargetExt $targetExt
                if (-not [System.IO.Path]::IsPathRooted($resolved)) {
                    $resolved = Join-Path $ProjectDir $resolved
                }
                $resolved = [System.IO.Path]::GetFullPath($resolved)
                $libDirs += $resolved
            }
        }
    }

    # Additional dependencies (libraries)
    $libs = @()
    $rawLibs = $linkerTool.AdditionalDependencies
    if ($rawLibs) {
        $libs = ($rawLibs -split '\s+' | Where-Object { $_ })
    }

    # Post-build command
    $postBuild = ""
    $postBuildTool = $configNode.Tool | Where-Object { $_.Name -eq "VFPostBuildEventTool" }
    if ($postBuildTool -and $postBuildTool.CommandLine) {
        $postBuild = Resolve-VSMacros -Text $postBuildTool.CommandLine -ProjectDir $ProjectDir `
            -ConfigName $Config -TargetName $targetName -TargetExt $targetExt
    }

    # ── Source files (collect from vfproj XML) ──
    $sourceFiles = [System.Collections.Generic.List[string]]::new()

    function Collect-Files($node, $fileList, $projDir) {
        foreach ($child in $node.ChildNodes) {
            if ($child.LocalName -eq "File") {
                $relPath = $child.GetAttribute("RelativePath")
                if ($relPath -and $relPath -match '\.f90$') {
                    $absPath = Join-Path $projDir $relPath
                    $absPath = [System.IO.Path]::GetFullPath($absPath)
                    if (-not $fileList.Contains($absPath)) {
                        $fileList.Add($absPath)
                    }
                }
            }
            elseif ($child.LocalName -eq "Filter") {
                Collect-Files $child $fileList $projDir
            }
        }
    }
    Collect-Files $root.Files $sourceFiles $ProjectDir

    return @{
        TargetName    = $targetName
        TargetExt     = $targetExt
        IsDLL         = $isDLL
        IntDir        = $intDir
        CompilerFlags = $compilerFlags
        IncludeDirs   = $includeDirs
        LinkerFlags   = $linkerFlags
        LibDirs       = $libDirs
        Libs          = $libs
        SourceFiles   = @($sourceFiles)
        PostBuild     = $postBuild
        ProjectDir    = $ProjectDir
    }
}

# ── Scan a file for MODULE definitions and USE dependencies ───────────────────
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

# ── Build module→file index for entire source tree ───────────────────────────
function Build-SourceTreeIndex {
    param([string]$SourceRoot)
    $index = @{}  # module_name (lower) -> absolute file path
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

# ── Resolve missing dependencies and topologically sort ───────────────────────
function Resolve-AndSort {
    param(
        [string[]]$Files,
        [string]$SourceRoot
    )

    # Intrinsic/external modules to ignore (not provided by our source files)
    $externalModules = [System.Collections.Generic.HashSet[string]]::new(
        [string[]]@("iso_fortran_env", "iso_c_binding", "omp_lib", "omp_lib_kinds",
                     "ifport", "ifcore", "ieee_arithmetic", "ieee_exceptions", "ieee_features"),
        [System.StringComparer]::OrdinalIgnoreCase)

    # Build source tree index for discovering missing files
    Write-Host "  Indexing source tree for module discovery..." -ForegroundColor Gray
    $treeIndex = Build-SourceTreeIndex -SourceRoot $SourceRoot

    # Iteratively discover missing dependencies
    $fileSet = [System.Collections.Generic.HashSet[string]]::new(
        [string[]]$Files, [System.StringComparer]::OrdinalIgnoreCase)
    $fileList = [System.Collections.Generic.List[string]]::new([string[]]$Files)

    $changed = $true
    while ($changed) {
        $changed = $false
        # Scan all current files
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
        # Find missing modules and add providing files from source tree
        foreach ($f in @($fileList)) {
            foreach ($mod in $fileUses[$f]) {
                if ($externalModules.Contains($mod)) { continue }
                if ($moduleProvider.ContainsKey($mod)) { continue }
                # Module not provided by any file in our set — search source tree
                if ($treeIndex.ContainsKey($mod)) {
                    $provider = $treeIndex[$mod]
                    if (-not $fileSet.Contains($provider)) {
                        $fileSet.Add($provider) | Out-Null
                        $fileList.Add($provider)
                        Write-Host "  Auto-added: $([System.IO.Path]::GetFileName($provider)) (provides module '$mod')" -ForegroundColor Gray
                        $changed = $true
                    }
                }
            }
        }
    }

    # Final scan for topological sort
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

    # Build adjacency
    $deps = @{}
    foreach ($f in @($fileList)) {
        $deps[$f] = [System.Collections.Generic.List[string]]::new()
        foreach ($mod in $fileUses[$f]) {
            if ($moduleProvider.ContainsKey($mod)) {
                $provider = $moduleProvider[$mod]
                if ($provider -ne $f -and $fileSet.Contains($provider)) {
                    if (-not $deps[$f].Contains($provider)) {
                        $deps[$f].Add($provider)
                    }
                }
            }
        }
    }

    # Kahn's algorithm
    $allFiles = @($fileList)
    $inDeg = @{}
    $dependents = @{}
    foreach ($f in $allFiles) {
        $inDeg[$f] = $deps[$f].Count
        $dependents[$f] = [System.Collections.Generic.List[string]]::new()
    }
    foreach ($f in $allFiles) {
        foreach ($dep in $deps[$f]) {
            $dependents[$dep].Add($f)
        }
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
            if ($inDeg[$dependent] -eq 0) {
                $queue.Enqueue($dependent)
            }
        }
    }

    # Append any unsorted files (cycles)
    if ($sorted.Count -lt $allFiles.Count) {
        foreach ($f in $allFiles) {
            if (-not $sorted.Contains($f)) {
                $sorted.Add($f)
            }
        }
    }

    return @($sorted)
}

# ── Main Build ─────────────────────────────────────────────────────────────────
try {
    Initialize-BuildEnvironment

    # ── Resolve Dependencies ──────────────────────────────────────────────────
    $AbsDepsDir = if ([System.IO.Path]::IsPathRooted($DepsDir)) { $DepsDir } else { Join-Path $ScriptDir $DepsDir }

    if ($BuildDeps) {
        Write-Host "Building dependencies via build-deps.ps1..." -ForegroundColor Yellow
        & (Join-Path $ScriptDir "build-deps.ps1") -DepsDir $AbsDepsDir
        if ($LASTEXITCODE -ne 0) { throw "build-deps.ps1 failed" }
        Write-Host ""
    }

    # Verify pre-built dependencies exist
    $script:HDF5Dir   = Join-Path $AbsDepsDir "hdf5-install"
    $script:HeclibDir = Join-Path $AbsDepsDir "heclib-install"
    $script:ZlibDir   = Join-Path $AbsDepsDir "zlib-install"

    $missingDeps = @()
    if (-not (Test-Path "$script:HDF5Dir\lib\libhdf5_fortran.lib")) { $missingDeps += "HDF5 ($script:HDF5Dir)" }
    if (-not (Test-Path "$script:HeclibDir\lib\heclib.lib"))        { $missingDeps += "heclib ($script:HeclibDir)" }
    if (-not (Test-Path "$script:ZlibDir\lib\zlibstatic.lib"))      { $missingDeps += "zlib ($script:ZlibDir)" }

    if ($missingDeps.Count -gt 0) {
        Write-Host "ERROR: Pre-built dependencies not found:" -ForegroundColor Red
        foreach ($dep in $missingDeps) {
            Write-Host "  - $dep" -ForegroundColor Red
        }
        Write-Host ""
        Write-Host "Run 'build-deps.ps1' first, or use '-BuildDeps' to build automatically:" -ForegroundColor Yellow
        Write-Host "  .\Build-IWFM-VS.ps1 -BuildDeps" -ForegroundColor White
        exit 1
    }

    Write-Host "  Dependencies: $AbsDepsDir" -ForegroundColor Gray

    # Parse project
    Write-Host "Parsing $VFProjRelPath ..." -ForegroundColor Yellow
    $proj = Parse-VFProj -VFProjFile $VFProjPath -Config $Configuration

    Write-Host "  Target:       $($proj.TargetName)$($proj.TargetExt)" -ForegroundColor Gray
    Write-Host "  Source files: $($proj.SourceFiles.Count) (from vfproj)" -ForegroundColor Gray
    Write-Host "  IntDir:       $($proj.IntDir)" -ForegroundColor Gray
    Write-Host ""

    # Resolve missing dependencies from source tree and topologically sort
    Write-Host "Resolving module dependencies..." -ForegroundColor Yellow
    $sourceRoot = Join-Path $ScriptDir "SourceCode"
    $proj.SourceFiles = Resolve-AndSort -Files $proj.SourceFiles -SourceRoot $sourceRoot
    Write-Host "  Dependency order resolved: $($proj.SourceFiles.Count) files total." -ForegroundColor Gray
    Write-Host ""

    # Clean
    if ($Clean -and (Test-Path $proj.IntDir)) {
        Write-Host "Cleaning: $($proj.IntDir)" -ForegroundColor Yellow
        Remove-Item -Path $proj.IntDir -Recurse -Force
        Write-Host ""
    }

    # Ensure intermediate directory exists
    if (-not (Test-Path $proj.IntDir)) {
        New-Item -ItemType Directory -Path $proj.IntDir -Force | Out-Null
    }

    # Ensure Bin directory exists
    if (-not (Test-Path $BinDir)) {
        New-Item -ItemType Directory -Path $BinDir -Force | Out-Null
    }

    # ── Compile ────────────────────────────────────────────────────────────────
    Write-Host "=== Compiling ($($proj.SourceFiles.Count) files) ===" -ForegroundColor Cyan
    $stopwatch = [System.Diagnostics.Stopwatch]::StartNew()
    $objFiles = @()
    $fileNum = 0
    $totalFiles = $proj.SourceFiles.Count

    foreach ($srcFile in $proj.SourceFiles) {
        $fileNum++
        $baseName = [System.IO.Path]::GetFileNameWithoutExtension($srcFile)
        $objFile  = Join-Path $proj.IntDir "$baseName.obj"
        $objFiles += $objFile

        # Build compiler command
        $args = @("/c")
        $args += $proj.CompilerFlags
        foreach ($inc in $proj.IncludeDirs) {
            # Replace bundled HDF5 include path with pre-built deps path
            if ($inc -match 'IWFM-kernel[\\/]HDF5') {
                $args += "/I`"$script:HDF5Dir\include\static`""
            } else {
                $args += "/I`"$inc`""
            }
        }
        # Module output and search in intermediate dir
        $args += "/module:`"$($proj.IntDir)`""
        $args += "/I`"$($proj.IntDir)`""
        $args += "/object:`"$objFile`""
        $args += "`"$srcFile`""

        $argsStr = $args -join " "

        if ($ShowCommands) {
            Write-Host "[$fileNum/$totalFiles] ifx $argsStr" -ForegroundColor Gray
        } else {
            $pct = [math]::Floor(($fileNum / $totalFiles) * 100)
            Write-Host "  [$fileNum/$totalFiles] ($pct%) $baseName.f90" -ForegroundColor White -NoNewline
            Write-Host "" # newline
        }

        # Execute compiler
        $psi = New-Object System.Diagnostics.ProcessStartInfo
        $psi.FileName = $script:IFX
        $psi.Arguments = $argsStr
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
            Write-Host ""
            Write-Host "ERROR: Compilation failed for $baseName.f90 (exit code $($process.ExitCode))" -ForegroundColor Red
            exit 1
        }
    }

    $compileTime = $stopwatch.Elapsed
    Write-Host ""
    Write-Host "Compilation complete: $($compileTime.Minutes)m $($compileTime.Seconds)s" -ForegroundColor Green
    Write-Host ""

    # ── Link ───────────────────────────────────────────────────────────────────
    Write-Host "=== Linking ===" -ForegroundColor Cyan
    $stopwatch.Restart()

    $outDir = Join-Path $proj.ProjectDir "x64\$Configuration"
    if (-not (Test-Path $outDir)) {
        New-Item -ItemType Directory -Path $outDir -Force | Out-Null
    }

    $outputFile = Join-Path $outDir "$($proj.TargetName)$($proj.TargetExt)"

    $linkArgs = @("/nologo")

    # Pass OpenMP to linker if it was a compiler flag
    if ($proj.CompilerFlags -contains "/Qopenmp") {
        $linkArgs += "/Qopenmp"
    }

    # Object files
    foreach ($obj in $objFiles) {
        $linkArgs += "`"$obj`""
    }

    # Output file
    $linkArgs += "/Fe:`"$outputFile`""

    # Linker flags go after /link
    $linkArgs += "/link"
    $linkArgs += $proj.LinkerFlags

    foreach ($ld in $proj.LibDirs) {
        # Replace bundled paths with pre-built deps
        if ($ld -match 'IWFM-kernel[\\/]HDF5') {
            $linkArgs += "/LIBPATH:`"$script:HDF5Dir\lib`""
        } elseif ($ld -match 'IWFM-kernel[\\/]heclib') {
            $linkArgs += "/LIBPATH:`"$script:HeclibDir\lib`""
        } else {
            $linkArgs += "/LIBPATH:`"$ld`""
        }
    }

    # Use pre-built dependencies from build-deps.ps1
    # Override .vfproj library paths with pre-built deps
    $filteredLibs = $proj.Libs | Where-Object { $_ -notmatch 'heclib|libhdf5' }
    foreach ($lib in $filteredLibs) {
        $linkArgs += $lib
    }
    # HDF5 libraries
    $linkArgs += "`"$script:HDF5Dir\lib\libhdf5_fortran.lib`""
    $linkArgs += "`"$script:HDF5Dir\lib\libhdf5_f90cstub.lib`""
    $linkArgs += "`"$script:HDF5Dir\lib\libhdf5.lib`""
    # heclib (HEC-DSS 7)
    $linkArgs += "`"$script:HeclibDir\lib\heclib.lib`""
    # zlib
    $linkArgs += "`"$script:ZlibDir\lib\zlibstatic.lib`""
    # System libraries required by HEC-DSS 7
    $linkArgs += "ws2_32.lib"
    $linkArgs += "shlwapi.lib"

    $linkArgsStr = $linkArgs -join " "

    if ($ShowCommands) {
        Write-Host "ifx $linkArgsStr" -ForegroundColor Gray
    }

    $psi = New-Object System.Diagnostics.ProcessStartInfo
    $psi.FileName = $script:IFX
    $psi.Arguments = $linkArgsStr
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
        Write-Host ""
        Write-Host "ERROR: Linking failed (exit code $($process.ExitCode))" -ForegroundColor Red
        exit 1
    }

    $linkTime = $stopwatch.Elapsed
    Write-Host "Linking complete: $($linkTime.Seconds)s" -ForegroundColor Green
    Write-Host ""

    # ── Post-build: copy to Bin ────────────────────────────────────────────────
    Write-Host "=== Post-build ===" -ForegroundColor Cyan

    if (Test-Path $outputFile) {
        $destFile = Join-Path $BinDir "$($proj.TargetName)$($proj.TargetExt)"
        Copy-Item -Path $outputFile -Destination $destFile -Force
        $size = [math]::Round((Get-Item $destFile).Length / 1MB, 1)
        Write-Host "  Copied to: $destFile ($size MB)" -ForegroundColor White
    } else {
        Write-Host "  WARNING: Output file not found: $outputFile" -ForegroundColor Yellow
    }

    Write-Host ""
    Write-Host "Build successful." -ForegroundColor Green
    Write-Host "  Output: $destFile" -ForegroundColor White
    Write-Host "  Compile time: $($compileTime.Minutes)m $($compileTime.Seconds)s" -ForegroundColor Gray
    Write-Host "  Link time:    $($linkTime.Seconds)s" -ForegroundColor Gray
    Write-Host ""
}
catch {
    Write-Host ""
    Write-Host "ERROR: $_" -ForegroundColor Red
    Write-Host $_.ScriptStackTrace -ForegroundColor DarkGray
    Write-Host ""
    $script:ExitCode = 1
}

exit $script:ExitCode
