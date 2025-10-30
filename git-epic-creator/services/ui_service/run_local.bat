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

REM ============================================================================
REM API Configuration
REM ============================================================================
set API_PORT=8000

REM ============================================================================
REM Service URLs (pointing to docker-compose exposed ports on localhost)
REM ============================================================================
set PROJECT_MANAGEMENT_SERVICE_URL=http://localhost:8003
set AI_REQUIREMENTS_SERVICE_URL=http://localhost:8009
set AI_TASKS_SERVICE_URL=http://localhost:8013
set GITLAB_CLIENT_SERVICE_URL=http://localhost:8012
set GRAPH_RAG_SERVICE_URL=http://localhost:8008

REM ============================================================================
REM Mock Mode Configuration (for UI testing without AI services)
REM ============================================================================
REM Set to true/1/yes to enable mock responses for ai-requirements and ai-tasks
set MOCK_AI_SERVICES=true

REM ============================================================================
REM HTTP Client Configuration
REM ============================================================================
set HTTP_CONNECTION_TIMEOUT=30.0
set HTTP_READ_TIMEOUT=180.0
set HTTP_MAX_RETRIES=3
set HTTP_RETRY_BACKOFF_FACTOR=2.0
set HTTP_MAX_CONNECTIONS=100
set HTTP_MAX_KEEPALIVE_CONNECTIONS=20

REM ============================================================================
REM Redis Configuration
REM ============================================================================
set REDIS_URL=redis://localhost:6379
set REDIS_PASSWORD=
set REDIS_DB=0
set REDIS_MAX_CONNECTIONS=10
set REDIS_RETRY_ON_TIMEOUT=true
set REDIS_SOCKET_CONNECT_TIMEOUT=5.0
set REDIS_SOCKET_TIMEOUT=5.0

REM Redis Pub/Sub Configuration
set REDIS_PUBSUB_CHANNEL_PREFIX=project_progress
set REDIS_PUBSUB_MAX_CONNECTIONS=5
set REDIS_PUBSUB_CONNECTION_TIMEOUT=10.0

REM Common Retry/Backoff Settings
set RETRY_MAX_ATTEMPTS=3
set RETRY_BACKOFF_BASE_SEC=2
set RETRY_BACKOFF_FACTOR=2
set RETRY_BACKOFF_MAX_SEC=60

REM ============================================================================
REM Azure AD Authentication Configuration
REM ============================================================================
set AZURE_AD_AUTHORITY=https://localhost:8005
set AZURE_TENANT_ID=e7963c3a-3b3a-43b6-9426-89e433d07e69
set AZURE_CLIENT_ID=a9e304a9-5b6c-4ef7-9b37-23a579a6d7be
set AZURE_CLIENT_SECRET=mock-secret
set AZURE_AD_VERIFY_SSL=false

REM ============================================================================
REM Session and JWT Configuration
REM ============================================================================
set SESSION_SECRET_KEY=change-me-strong-secret
set SESSION_COOKIE_NAME=ui_session
set SESSION_MAX_AGE=1209600
set SESSION_SAME_SITE=lax
set ALLOW_INSECURE_SESSION=true
set LOCAL_JWT_SECRET=dev-local-jwt-secret

echo Starting UI Service on port %API_PORT% with PROJECT_MANAGEMENT_SERVICE_URL=%PROJECT_MANAGEMENT_SERVICE_URL%
python -m src.main

endlocal


:help
echo.
echo run_local.bat [--install]
echo    --install, -i   Install/update dependencies before start
echo.
goto :eof

