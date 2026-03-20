@echo off
set SRCDIR=C:\Users\hatch\OneDrive\Desktop\iwfm-2025.0.1747\src\SourceCode\IWFM-kernel\Package_Matrix\cpp
set BUILDDIR=%SRCDIR%\build_test

call "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvars64.bat" >nul 2>&1
cd /d "%SRCDIR%"

if exist "%BUILDDIR%" rmdir /s /q "%BUILDDIR%"
mkdir "%BUILDDIR%"
if not exist "%SRCDIR%\Bin" mkdir "%SRCDIR%\Bin"

cmake -S "%SRCDIR%" -B "%BUILDDIR%" -G Ninja -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=cl -DCMAKE_C_COMPILER=cl -DIWFM_BUILD_TESTS=ON -Wno-dev
if errorlevel 1 (
    echo CMake configuration FAILED
    exit /b 1
)
echo --- CMake configure OK ---

cmake --build "%BUILDDIR%" --verbose
if errorlevel 1 (
    echo Build FAILED
    exit /b 1
)
echo --- Build OK ---
