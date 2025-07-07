@echo off
setlocal enabledelayedexpansion

REM Check if PowerShell is available
powershell -Command "Write-Host 'PowerShell is available'" >nul 2>&1
if errorlevel 1 (
    echo PowerShell is not available or not in PATH
    pause
    exit /b 1
)

REM Display usage if no parameters provided
if "%~1"=="" (
    echo Usage: start-services.bat [cosmos^|servicebus^|emulators^|apps^|all] [build] [detach]
    echo.
    echo Examples:
    echo   start-services.bat cosmos        - Start only Cosmos DB services
    echo   start-services.bat emulators     - Start all Azure emulators
    echo   start-services.bat all build     - Start all services with rebuild
    echo   start-services.bat apps detach   - Start app services in detached mode
    echo.
    pause
    exit /b 1
)

REM Build PowerShell command
set "ps_command=.\start-services.ps1 -Group %1"

REM Check for additional parameters
if "%~2"=="build" set "ps_command=!ps_command! -Build"
if "%~2"=="detach" set "ps_command=!ps_command! -Detach"
if "%~3"=="build" set "ps_command=!ps_command! -Build"
if "%~3"=="detach" set "ps_command=!ps_command! -Detach"

REM Execute PowerShell script
powershell -ExecutionPolicy Bypass -Command "!ps_command!"

REM Keep window open if there was an error
if errorlevel 1 (
    echo.
    echo An error occurred. Press any key to exit...
    pause >nul
) 