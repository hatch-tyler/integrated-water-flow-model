@echo off
REM Build and test the C++ solver library standalone
REM Sets up Visual Studio environment automatically

set SRCDIR=%~dp0
if "%SRCDIR:~-1%"=="\" set SRCDIR=%SRCDIR:~0,-1%
set BUILDDIR=%SRCDIR%\build_test

REM Set up VS environment (provides cl.exe, link.exe, Ninja via PATH)
call "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvars64.bat" >nul 2>&1

REM Try Intel oneAPI if available (provides icx, OpenMP runtime)
if exist "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" (
    call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" intel64 vs2022 >nul 2>&1
)

if exist "%BUILDDIR%" rmdir /s /q "%BUILDDIR%"
mkdir "%BUILDDIR%"

REM Create Bin directory for test output
if not exist "%SRCDIR%\Bin" mkdir "%SRCDIR%\Bin"

REM Try Intel icx first, fall back to MSVC cl
where icx >nul 2>&1
if %errorlevel%==0 (
    echo Using Intel icx compiler
    cmake -S "%SRCDIR%" -B "%BUILDDIR%" -G Ninja -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=icx -DCMAKE_C_COMPILER=icx -DIWFM_BUILD_TESTS=ON -Wno-dev
) else (
    echo Using MSVC cl compiler
    cmake -S "%SRCDIR%" -B "%BUILDDIR%" -G Ninja -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=cl -DCMAKE_C_COMPILER=cl -DIWFM_BUILD_TESTS=ON -Wno-dev
)
if errorlevel 1 (
    echo CMake configuration failed
    exit /b 1
)

cmake --build "%BUILDDIR%" --verbose
if errorlevel 1 (
    echo Build failed
    exit /b 1
)

echo.
echo === Running Unit Tests ===
echo.
cd /d "%BUILDDIR%"
ctest -V
