@echo off

REM 
REM   Path to calling location and to the location of this batch file
REM

set callpath=%CD%
set mypath=%~dp0

cd %mypath%
for /f %%a in ('git tag --points-at') do set tag=%%a
for /f %%a in ('git branch --show-current') do set branch=%%a
for /f %%a in ('git rev-parse HEAD') do set sha1=%%a

REM
REM   %1 is the parameter provided to the batch file as the path where 
REM   the Fortran include file will be generated
REM

echo CHARACTER(LEN=40),PARAMETER :: f_cTag    = '%tag%'      > %mypath%IWFMVersion.fi
echo CHARACTER(LEN=40),PARAMETER :: f_cBranch = '%branch%'  >> %mypath%IWFMVersion.fi
echo CHARACTER(LEN=40),PARAMETER :: f_cHash   = '%sha1%'    >> %mypath%IWFMVersion.fi

cd %callpath%