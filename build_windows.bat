@echo off
REM Windows Build Script for IWFM
REM Run this from a native Windows Command Prompt (cmd.exe), NOT Git Bash
REM
REM Usage: build_windows.bat [configure|build|clean]

setlocal enabledelayedexpansion

REM Check if running from Git Bash (which causes linker issues)
if defined MSYSTEM (
    echo ERROR: This script must be run from Windows Command Prompt, not Git Bash.
    echo        Git Bash converts /flag paths incorrectly for the MSVC linker.
    echo        Please open cmd.exe and run this script again.
    exit /b 1
)

REM Set Intel oneAPI environment if not already set
if not defined ONEAPI_ROOT (
    set "ONEAPI_ROOT=C:\Program Files (x86)\Intel\oneAPI"
)

if exist "%ONEAPI_ROOT%\setvars.bat" (
    call "%ONEAPI_ROOT%\setvars.bat" intel64
) else (
    echo Warning: Intel oneAPI setvars.bat not found at %ONEAPI_ROOT%
    echo Assuming compiler is already in PATH
)

REM Navigate to source directory
cd /d "%~dp0"

REM Parse command line argument
set "ACTION=%~1"
if "%ACTION%"=="" set "ACTION=build"

if /i "%ACTION%"=="configure" goto :configure
if /i "%ACTION%"=="build" goto :build
if /i "%ACTION%"=="clean" goto :clean
if /i "%ACTION%"=="rebuild" goto :rebuild

echo Unknown action: %ACTION%
echo Usage: %~nx0 [configure^|build^|clean^|rebuild]
exit /b 1

:configure
echo.
echo === Configuring IWFM Build ===
echo.
if not exist build mkdir build
cd build
cmake .. -G "Ninja" -DCMAKE_BUILD_TYPE=Release -DCMAKE_Fortran_COMPILER=ifx -DCMAKE_C_COMPILER=icx
if errorlevel 1 (
    echo Configuration failed!
    exit /b 1
)
echo Configuration complete.
goto :end

:build
echo.
echo === Building IWFM ===
echo.
if not exist build (
    echo Build directory not found. Running configure first...
    call :configure
    cd ..
)
cd build
cmake --build . --parallel
if errorlevel 1 (
    echo Build failed!
    exit /b 1
)
echo Build complete.
goto :end

:clean
echo.
echo === Cleaning Build Directory ===
echo.
if exist build (
    rmdir /s /q build
    echo Build directory removed.
) else (
    echo Build directory does not exist.
)
goto :end

:rebuild
echo.
echo === Rebuilding IWFM ===
echo.
call :clean
call :configure
cd ..
call :build
goto :end

:end
echo.
echo Done.
exit /b 0
