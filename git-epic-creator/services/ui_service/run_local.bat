@echo off
setlocal ENABLEEXTENSIONS

cd /d "%~dp0"

REM Usage:
REM   run_local.bat           -> run UI
REM   run_local.bat --install -> install/update deps, then run
REM   run_local.bat -i        -> same as --install

set DO_INSTALL=
if /I "%1"=="--install" set DO_INSTALL=1
if /I "%1"=="-i" set DO_INSTALL=1
if /I "%1"=="--help" goto :help
if /I "%1"=="-h" goto :help

if defined DO_INSTALL (
  echo Installing/updating dependencies...
  python -m pip install -e .
)

REM Ports and endpoints exposed by docker-compose on the host
set API_PORT=8000
set PM_API_BASE=http://localhost:8003
set REDIS_URL=redis://localhost:6379

echo Starting UI Service on port %API_PORT% with PM_API_BASE=%PM_API_BASE%
python -m src.main

endlocal


:help
echo.
echo run_local.bat [--install]
echo    --install, -i   Install/update dependencies before start
echo.
goto :eof

