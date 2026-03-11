$staging = "C:\Users\hatch\OneDrive\Desktop\iwfm-2025.0.1747\src\release-staging"
$binDir = "C:\Users\hatch\OneDrive\Desktop\iwfm-2025.0.1747\src\Bin"

New-Item -ItemType Directory -Force -Path $staging | Out-Null

$releaseFiles = @(
    "Simulation_x64.exe",
    "Simulation_PLL_x64.exe",
    "Simulation_MM_x64.exe",
    "PreProcessor_x64.exe",
    "Budget_x64.exe",
    "ZBudget_x64.exe",
    "IWFM_C_x64.dll",
    "IWFM2OBS_x64.exe",
    "CalcTypeHyd_x64.exe"
)

$debugFiles = @(
    "Simulation_x64_D.exe",
    "Simulation_PLL_x64_D.exe",
    "Simulation_MM_x64_D.exe",
    "PreProcessor_x64_D.exe",
    "Budget_x64_D.exe",
    "ZBudget_x64_D.exe",
    "IWFM_C_x64_D.dll",
    "IWFM2OBS_x64_D.exe",
    "CalcTypeHyd_x64_D.exe"
)

function Package-Files($fileList, $zipName) {
    $zipPath = Join-Path $staging $zipName
    if (Test-Path $zipPath) { Remove-Item $zipPath -Force }
    $paths = $fileList | ForEach-Object { Join-Path $binDir $_ } | Where-Object { Test-Path $_ }
    if ($paths.Count -eq 0) {
        Write-Host "  No files found for $zipName" -ForegroundColor Red
        return
    }
    Compress-Archive -Path $paths -DestinationPath $zipPath
    $size = [math]::Round((Get-Item $zipPath).Length / 1MB, 1)
    Write-Host "  $zipName ($size MB) - $($paths.Count) files" -ForegroundColor Green
}

Write-Host "Packaging Windows Release..." -ForegroundColor Cyan
Package-Files $releaseFiles "IWFM-2025.0.1747-Windows-x64-Release.zip"

Write-Host "Packaging Windows Debug..." -ForegroundColor Cyan
Package-Files $debugFiles "IWFM-2025.0.1747-Windows-x64-Debug.zip"

Write-Host ""
Write-Host "Done. Archives in: $staging" -ForegroundColor Yellow
