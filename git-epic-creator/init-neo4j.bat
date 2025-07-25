@echo off
REM Neo4j Initialization Script
REM Sends a POST request to initialize Neo4j Graph RAG schema

echo ========================================
echo Neo4j Initialization Script
echo ========================================
echo.

REM Check if curl is available
curl --version >nul 2>&1
if %errorlevel% neq 0 (
    echo ERROR: curl is not available on this system.
    echo Please install curl or use Windows 10/11 which includes curl by default.
    pause
    exit /b 1
)

echo Checking Neo4j Maintenance Service health...
curl -s -o nul -w "%%{http_code}" http://localhost:8002/health > temp_status.txt
set /p status=<temp_status.txt
del temp_status.txt

if "%status%"=="200" (
    echo ✓ Neo4j Maintenance Service is healthy
    echo.
) else (
    echo ✗ Neo4j Maintenance Service is not responding (HTTP %status%)
    echo Please ensure the service is running with: docker-compose up -d
    echo.
    set /p choice="Continue anyway? (y/N): "
    if /i not "%choice%"=="y" (
        echo Operation cancelled.
        pause
        exit /b 1
    )
    echo.
)

echo Initializing Neo4j Graph RAG schema...
echo Sending POST request to http://localhost:8002/init-neo4j
echo.

REM Send the POST request and capture response
curl -X POST -H "Content-Type: application/json" -w "\nHTTP Status: %%{http_code}\nResponse Time: %%{time_total}s\n" http://localhost:8002/init-neo4j

if %errorlevel% equ 0 (
    echo.
    echo ✓ Request completed successfully
) else (
    echo.
    echo ✗ Request failed with error code: %errorlevel%
    echo Please check if the Neo4j Maintenance Service is running
)

echo.
echo ========================================
echo Script completed
echo ========================================
pause