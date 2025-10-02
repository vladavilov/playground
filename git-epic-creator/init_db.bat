@echo off
echo ========================================
echo Database Initialization Service
echo ========================================
echo.

echo Checking if db-init-service is running...
curl -s -f http://localhost:8001/health >nul 2>&1
if %errorlevel% neq 0 (
    echo ERROR: db-init-service is not running or not accessible on port 8001
    echo Please ensure the service is started with: docker-compose up db-init-service
    echo.
    pause
    exit /b 1
)

echo db-init-service is running. Proceeding with database initialization...
echo.

echo Making request to initialize database...
echo Endpoint: http://localhost:8001/db/init
echo.

set AUTH_HEADER=
if not "%LOCAL_JWT_TOKEN%"=="" set AUTH_HEADER=-H "Authorization: Bearer %LOCAL_JWT_TOKEN%"

curl -X POST http://localhost:8001/db/init ^
    -H "Content-Type: application/json" ^
    -H "Accept: application/json" ^
    %AUTH_HEADER% ^
    --connect-timeout 30 ^
    --max-time 60

echo.
echo.
echo ========================================
echo Database initialization request completed
echo ========================================
echo.
pause
