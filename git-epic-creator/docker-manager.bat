@echo off
setlocal enabledelayedexpansion

:: Docker Compose Management Script
:: Provides easy management of different service groups

:MAIN_MENU
cls
echo ========================================
echo    Docker Compose Service Manager
echo ========================================
echo.
echo Select an option:
echo.
echo 1. Infrastructure Services (postgres, redis, neo4j, azurite, db-init, neo4j-maintenance)
echo 2. Full Environment (all services)
echo 3. Application Services Only (excludes infrastructure)
echo 4. Single Service Management
echo 5. Stop All Services
echo 6. View Service Status
echo 7. View Service Logs
echo 8. Clean Up (remove containers and volumes)
echo 9. Exit
echo.
set /p choice="Enter your choice (1-9): "

if "%choice%"=="1" goto INFRASTRUCTURE
if "%choice%"=="2" goto FULL_ENVIRONMENT
if "%choice%"=="3" goto APP_SERVICES
if "%choice%"=="4" goto SINGLE_SERVICE
if "%choice%"=="5" goto STOP_ALL
if "%choice%"=="6" goto VIEW_STATUS
if "%choice%"=="7" goto VIEW_LOGS
if "%choice%"=="8" goto CLEANUP
if "%choice%"=="9" goto EXIT

echo Invalid choice. Please try again.
pause
goto MAIN_MENU

:INFRASTRUCTURE
cls
echo ========================================
echo    Infrastructure Services Management
echo ========================================
echo.
echo Services: postgres, redis, neo4j, azurite, db-init-service, neo4j-maintenance-service
echo.
echo 1. Start/Restart
echo 2. Start/Restart with Rebuild
echo 3. Stop
echo 4. Back to Main Menu
echo.
set /p infra_choice="Enter your choice (1-4): "

set INFRA_SERVICES=postgres redis neo4j azurite db-init-service neo4j-maintenance-service

if "%infra_choice%"=="1" (
    echo Starting infrastructure services...
    docker-compose up -d %INFRA_SERVICES%
    echo.
    echo Infrastructure services started successfully!
    pause
    goto MAIN_MENU
)

if "%infra_choice%"=="2" (
    echo Rebuilding and starting infrastructure services...
    docker-compose up -d --build %INFRA_SERVICES%
    echo.
    echo Infrastructure services rebuilt and started successfully!
    pause
    goto MAIN_MENU
)

if "%infra_choice%"=="3" (
    echo Stopping infrastructure services...
    docker-compose stop %INFRA_SERVICES%
    echo.
    echo Infrastructure services stopped successfully!
    pause
    goto MAIN_MENU
)

if "%infra_choice%"=="4" goto MAIN_MENU

echo Invalid choice. Please try again.
pause
goto INFRASTRUCTURE

:FULL_ENVIRONMENT
cls
echo ========================================
echo    Full Environment Management
echo ========================================
echo.
echo This will manage ALL services in docker-compose.yml
echo.
echo 1. Start/Restart All
echo 2. Start/Restart All with Rebuild
echo 3. Stop All
echo 4. Back to Main Menu
echo.
set /p full_choice="Enter your choice (1-4): "

if "%full_choice%"=="1" (
    echo Starting all services...
    docker-compose up -d
    echo.
    echo All services started successfully!
    pause
    goto MAIN_MENU
)

if "%full_choice%"=="2" (
    echo Rebuilding and starting all services...
    docker-compose up -d --build
    echo.
    echo All services rebuilt and started successfully!
    pause
    goto MAIN_MENU
)

if "%full_choice%"=="3" (
    echo Stopping all services...
    docker-compose down
    echo.
    echo All services stopped successfully!
    pause
    goto MAIN_MENU
)

if "%full_choice%"=="4" goto MAIN_MENU

echo Invalid choice. Please try again.
pause
goto FULL_ENVIRONMENT

:APP_SERVICES
cls
echo ========================================
echo    Application Services Management
echo ========================================
echo.
echo Services: project-management-service, document-processing-service, neo4j-ingestion-service, mock-auth-service
echo.
echo 1. Start/Restart
echo 2. Start/Restart with Rebuild
echo 3. Stop
echo 4. Back to Main Menu
echo.
set /p app_choice="Enter your choice (1-4): "

set APP_SERVICES=project-management-service document-processing-service neo4j-ingestion-service mock-auth-service

if "%app_choice%"=="1" (
    echo Starting application services...
    docker-compose up -d %APP_SERVICES%
    echo.
    echo Application services started successfully!
    pause
    goto MAIN_MENU
)

if "%app_choice%"=="2" (
    echo Rebuilding and starting application services...
    docker-compose up -d --build %APP_SERVICES%
    echo.
    echo Application services rebuilt and started successfully!
    pause
    goto MAIN_MENU
)

if "%app_choice%"=="3" (
    echo Stopping application services...
    docker-compose stop %APP_SERVICES%
    echo.
    echo Application services stopped successfully!
    pause
    goto MAIN_MENU
)

if "%app_choice%"=="4" goto MAIN_MENU

echo Invalid choice. Please try again.
pause
goto APP_SERVICES

:SINGLE_SERVICE
cls
echo ========================================
echo    Single Service Management
echo ========================================
echo.
echo Available services:
echo 1. postgres
echo 2. redis
echo 3. neo4j
echo 4. azurite
echo 5. db-init-service
echo 6. neo4j-maintenance-service
echo 7. project-management-service
echo 8. document-processing-service
echo 9. neo4j-ingestion-service
echo 10. mock-auth-service
echo 11. pgadmin
echo 12. e2e-tests
echo 13. Custom service name
echo 14. Back to Main Menu
echo.
set /p service_choice="Enter your choice (1-14): "

set SERVICE_NAME=
if "%service_choice%"=="1" set SERVICE_NAME=postgres
if "%service_choice%"=="2" set SERVICE_NAME=redis
if "%service_choice%"=="3" set SERVICE_NAME=neo4j
if "%service_choice%"=="4" set SERVICE_NAME=azurite
if "%service_choice%"=="5" set SERVICE_NAME=db-init-service
if "%service_choice%"=="6" set SERVICE_NAME=neo4j-maintenance-service
if "%service_choice%"=="7" set SERVICE_NAME=project-management-service
if "%service_choice%"=="8" set SERVICE_NAME=document-processing-service
if "%service_choice%"=="9" set SERVICE_NAME=neo4j-ingestion-service
if "%service_choice%"=="10" set SERVICE_NAME=mock-auth-service
if "%service_choice%"=="11" set SERVICE_NAME=pgadmin
if "%service_choice%"=="12" set SERVICE_NAME=e2e-tests
if "%service_choice%"=="13" (
    set /p SERVICE_NAME="Enter service name: "
)
if "%service_choice%"=="14" goto MAIN_MENU

if "%SERVICE_NAME%"=="" (
    echo Invalid choice. Please try again.
    pause
    goto SINGLE_SERVICE
)

:SINGLE_SERVICE_ACTION
cls
echo ========================================
echo    Managing Service: %SERVICE_NAME%
echo ========================================
echo.
echo 1. Start/Restart
echo 2. Start/Restart with Rebuild
echo 3. Stop
echo 4. View Logs
echo 5. Execute Shell
echo 6. Back to Service Selection
echo.
set /p action_choice="Enter your choice (1-6): "

if "%action_choice%"=="1" (
    echo Starting service %SERVICE_NAME%...
    docker-compose up -d %SERVICE_NAME%
    echo.
    echo Service %SERVICE_NAME% started successfully!
    pause
    goto SINGLE_SERVICE_ACTION
)

if "%action_choice%"=="2" (
    echo Rebuilding and starting service %SERVICE_NAME%...
    docker-compose up -d --build %SERVICE_NAME%
    echo.
    echo Service %SERVICE_NAME% rebuilt and started successfully!
    pause
    goto SINGLE_SERVICE_ACTION
)

if "%action_choice%"=="3" (
    echo Stopping service %SERVICE_NAME%...
    docker-compose stop %SERVICE_NAME%
    echo.
    echo Service %SERVICE_NAME% stopped successfully!
    pause
    goto SINGLE_SERVICE_ACTION
)

if "%action_choice%"=="4" (
    echo Showing logs for %SERVICE_NAME%...
    echo Press Ctrl+C to exit logs view
    docker-compose logs -f %SERVICE_NAME%
    goto SINGLE_SERVICE_ACTION
)

if "%action_choice%"=="5" (
    echo Executing shell in %SERVICE_NAME%...
    docker-compose exec %SERVICE_NAME% /bin/bash
    goto SINGLE_SERVICE_ACTION
)

if "%action_choice%"=="6" goto SINGLE_SERVICE

echo Invalid choice. Please try again.
pause
goto SINGLE_SERVICE_ACTION

:STOP_ALL
cls
echo ========================================
echo    Stop All Services
echo ========================================
echo.
echo This will stop all running services but keep containers and volumes.
echo.
set /p confirm="Are you sure? (y/N): "
if /i "%confirm%"=="y" (
    echo Stopping all services...
    docker-compose down
    echo.
    echo All services stopped successfully!
) else (
    echo Operation cancelled.
)
pause
goto MAIN_MENU

:VIEW_STATUS
cls
echo ========================================
echo    Service Status
echo ========================================
echo.
docker-compose ps
echo.
echo Press any key to continue...
pause >nul
goto MAIN_MENU

:VIEW_LOGS
cls
echo ========================================
echo    View Service Logs
echo ========================================
echo.
echo 1. All services
echo 2. Infrastructure services only
echo 3. Application services only
echo 4. Specific service
echo 5. Back to Main Menu
echo.
set /p log_choice="Enter your choice (1-5): "

if "%log_choice%"=="1" (
    echo Showing logs for all services...
    echo Press Ctrl+C to exit logs view
    docker-compose logs -f
    goto MAIN_MENU
)

if "%log_choice%"=="2" (
    echo Showing logs for infrastructure services...
    echo Press Ctrl+C to exit logs view
    docker-compose logs -f postgres redis neo4j azurite db-init-service neo4j-maintenance-service
    goto MAIN_MENU
)

if "%log_choice%"=="3" (
    echo Showing logs for application services...
    echo Press Ctrl+C to exit logs view
    docker-compose logs -f project-management-service document-processing-service neo4j-ingestion-service mock-auth-service
    goto MAIN_MENU
)

if "%log_choice%"=="4" (
    set /p log_service="Enter service name: "
    echo Showing logs for !log_service!...
    echo Press Ctrl+C to exit logs view
    docker-compose logs -f !log_service!
    goto MAIN_MENU
)

if "%log_choice%"=="5" goto MAIN_MENU

echo Invalid choice. Please try again.
pause
goto VIEW_LOGS

:CLEANUP
cls
echo ========================================
echo    Clean Up Environment
echo ========================================
echo.
echo WARNING: This will remove all containers, networks, and volumes!
echo This action cannot be undone!
echo.
echo 1. Remove containers and networks only
echo 2. Remove containers, networks, and volumes
echo 3. Remove everything including images
echo 4. Back to Main Menu
echo.
set /p cleanup_choice="Enter your choice (1-4): "

if "%cleanup_choice%"=="1" (
    set /p confirm="Remove containers and networks? (y/N): "
    if /i "!confirm!"=="y" (
        echo Removing containers and networks...
        docker-compose down
        echo Cleanup completed!
    ) else (
        echo Operation cancelled.
    )
    pause
    goto MAIN_MENU
)

if "%cleanup_choice%"=="2" (
    set /p confirm="Remove containers, networks, and volumes? (y/N): "
    if /i "!confirm!"=="y" (
        echo Removing containers, networks, and volumes...
        docker-compose down -v
        echo Cleanup completed!
    ) else (
        echo Operation cancelled.
    )
    pause
    goto MAIN_MENU
)

if "%cleanup_choice%"=="3" (
    set /p confirm="Remove everything including images? (y/N): "
    if /i "!confirm!"=="y" (
        echo Removing containers, networks, volumes, and images...
        docker-compose down -v --rmi all
        echo Full cleanup completed!
    ) else (
        echo Operation cancelled.
    )
    pause
    goto MAIN_MENU
)

if "%cleanup_choice%"=="4" goto MAIN_MENU

echo Invalid choice. Please try again.
pause
goto CLEANUP

:EXIT
echo.
echo Thank you for using Docker Compose Service Manager!
echo.
pause
exit /b 0