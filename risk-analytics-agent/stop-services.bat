@echo off
setlocal enabledelayedexpansion

REM Check if PowerShell is available
powershell -Command "Write-Host 'PowerShell is available'" >nul 2>&1
if errorlevel 1 (
    echo PowerShell is not available or not in PATH
    pause
    exit /b 1
)

REM Display usage if help is requested
if "%~1"=="help" (
    echo Usage: stop-services.bat [stop^|down^|clean^|purge] [timeout]
    echo.
    echo Actions:
    echo   stop  - Stop all running services (containers remain)
    echo   down  - Stop and remove containers and networks
    echo   clean - Stop and remove containers, networks, and volumes
    echo   purge - Stop and remove containers, networks, volumes, and images
    echo.
    echo Examples:
    echo   stop-services.bat              - Stop all services (default)
    echo   stop-services.bat down         - Stop and remove containers
    echo   stop-services.bat clean        - Stop and remove containers with volumes
    echo   stop-services.bat stop 30      - Stop with 30 second timeout
    echo.
    pause
    exit /b 0
)

REM Set default action if none provided
set "action=%~1"
if "%action%"=="" set "action=stop"

REM Build PowerShell command
set "ps_command=.\stop-services.ps1 -Action %action%"

REM Add timeout parameter if provided
if not "%~2"=="" (
    set "ps_command=!ps_command! -Timeout %~2"
)

REM Execute PowerShell script
powershell -ExecutionPolicy Bypass -Command "!ps_command!"

REM Keep window open if there was an error
if errorlevel 1 (
    echo.
    echo An error occurred. Press any key to exit...
    pause >nul
) 