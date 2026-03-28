# Debug environment setup
$VSWhere = "${env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"
$VSPath = & $VSWhere -latest -property installationPath
$VCVars = Join-Path $VSPath "VC\Auxiliary\Build\vcvars64.bat"
$OneAPI = "${env:ProgramFiles(x86)}\Intel\oneAPI\setvars.bat"

$TempBatch = Join-Path $env:TEMP "iwfm_env_test.bat"
$TempEnv = Join-Path $env:TEMP "iwfm_env_out.txt"

# Clean up from previous runs
if (Test-Path $TempEnv) { Remove-Item $TempEnv -Force }

$bc = "@echo off`r`n"
$bc += "call `"$VCVars`" >nul 2>&1`r`n"
$bc += "if errorlevel 1 echo VCVARS FAILED >> `"$TempEnv`"`r`n"
$bc += "call `"$OneAPI`" intel64 vs2022 >nul 2>&1`r`n"
$bc += "if errorlevel 1 echo SETVARS FAILED >> `"$TempEnv`"`r`n"
$bc += "set > `"$TempEnv`"`r`n"
[IO.File]::WriteAllText($TempBatch, $bc)

Write-Host "Batch file: $TempBatch"
Write-Host "Env output: $TempEnv"
Write-Host "Running..."

$p = Start-Process cmd.exe -ArgumentList "/c `"$TempBatch`"" -Wait -NoNewWindow -PassThru
Write-Host "Exit code: $($p.ExitCode)"

if (Test-Path $TempEnv) {
    $lineCount = (Get-Content $TempEnv).Count
    Write-Host "Env lines captured: $lineCount"

    # Check for LIB
    $libLine = Get-Content $TempEnv | Where-Object { $_ -match '^LIB=' }
    if ($libLine) {
        Write-Host "LIB found with $($libLine.Split(';').Count) entries"
        # Check for Intel
        $libLine.Split(';') | Where-Object { $_ -match 'Intel|intel' } | ForEach-Object { Write-Host "  $_" }
    } else {
        Write-Host "LIB not found in env output!"
        # Show first few lines
        Get-Content $TempEnv | Select-Object -First 5 | ForEach-Object { Write-Host "  $_" }
    }
} else {
    Write-Host "ERROR: $TempEnv not created!"
}

# Cleanup
Remove-Item $TempBatch -Force -EA SilentlyContinue
