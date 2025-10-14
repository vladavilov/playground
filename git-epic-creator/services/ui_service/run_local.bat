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
set PROJECT_MANAGEMENT_SERVICE_URL=http://localhost:8003
set REDIS_URL=redis://localhost:6379
set AI_WORKFLOW_SERVICE_URL=http://localhost:8009

REM Auth: use localhost for local runs
set AZURE_AD_AUTHORITY=https://localhost:8005
set AZURE_TENANT_ID=e7963c3a-3b3a-43b6-9426-89e433d07e69
set AZURE_CLIENT_ID=a9e304a9-5b6c-4ef7-9b37-23a579a6d7be
set AZURE_AD_VERIFY_SSL=false

echo Starting UI Service on port %API_PORT% with PROJECT_MANAGEMENT_SERVICE_URL=%PROJECT_MANAGEMENT_SERVICE_URL%
python -m src.main

endlocal


:help
echo.
echo run_local.bat [--install]
echo    --install, -i   Install/update dependencies before start
echo.
goto :eof

