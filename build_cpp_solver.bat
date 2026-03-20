@echo off
REM Build IWFM with C++ solver enabled
REM Uses Intel ifx for Fortran, icx for C/C++

set SRCDIR=C:\Users\hatch\OneDrive\Desktop\iwfm-2025.0.1747\src
set BUILDDIR=%SRCDIR%\build-cpp-solver
set IFX=C:\Program Files (x86)\Intel\oneAPI\compiler\2025.3\bin\ifx.exe
set ICX=C:\Program Files (x86)\Intel\oneAPI\compiler\2025.3\bin\icx.exe

echo === Setting up build environment ===
call "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvars64.bat" >nul 2>&1
call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" intel64 vs2022 >nul 2>&1

echo.
echo === Configuring CMake (C++ solver enabled) ===
if not exist "%BUILDDIR%" mkdir "%BUILDDIR%"
cd /d "%BUILDDIR%"

cmake "%SRCDIR%" -G Ninja ^
    -DCMAKE_BUILD_TYPE=Release ^
    -DCMAKE_Fortran_COMPILER="%IFX%" ^
    -DCMAKE_C_COMPILER="%ICX%" ^
    -DCMAKE_CXX_COMPILER="%ICX%" ^
    -DIWFM_USE_CPP_SOLVER=ON ^
    -DIWFM_BUILD_TESTS=ON ^
    -Wno-dev

if errorlevel 1 (
    echo.
    echo CMake configuration FAILED
    exit /b 1
)

echo.
echo === Building IWFM with C++ solver ===
cmake --build . --parallel
if errorlevel 1 (
    echo.
    echo Build FAILED
    exit /b 1
)

echo.
echo === Build successful ===
echo Binaries in: %SRCDIR%\Bin\
dir "%SRCDIR%\Bin\*.exe" /b 2>nul

echo.
echo === Running C++ solver unit tests ===
"%SRCDIR%\Bin\test_iwfm_solver.exe"

echo.
echo === Done ===
