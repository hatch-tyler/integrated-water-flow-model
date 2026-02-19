# PowerShell script to build IWFM in Linux Docker container
# Usage: .\docker\build-linux.ps1 [-IWFM2OBS] [-CalcTypeHyd]

$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$SrcDir = Split-Path -Parent $ScriptDir

Write-Host "=== IWFM Linux Docker Build ===" -ForegroundColor Cyan
Write-Host "Source directory: $SrcDir"

# Change to source directory
Push-Location $SrcDir

try {
    # Build the Docker image
    Write-Host "`n=== Building Docker image ===" -ForegroundColor Yellow
    docker build -t iwfm-build:latest -f docker/Dockerfile .

    if ($LASTEXITCODE -ne 0) {
        throw "Docker build failed"
    }

    # Run the build and tests
    Write-Host "`n=== Running build and tests in container ===" -ForegroundColor Yellow
    docker run --rm `
        -v "${SrcDir}:/src" `
        -v "${SrcDir}/../samplemodel:/src/samplemodel:ro" `
        iwfm-build:latest

    if ($LASTEXITCODE -ne 0) {
        throw "Container build/test failed"
    }

    Write-Host "`n=== Build completed successfully ===" -ForegroundColor Green

    # List built executables
    Write-Host "`nBuilt Linux executables:"
    if (Test-Path "$SrcDir/build-linux/Bin") {
        Get-ChildItem "$SrcDir/build-linux/Bin" | Format-Table Name, Length
    } elseif (Test-Path "$SrcDir/Bin-Linux") {
        Get-ChildItem "$SrcDir/Bin-Linux" | Format-Table Name, Length
    }
}
finally {
    Pop-Location
}
