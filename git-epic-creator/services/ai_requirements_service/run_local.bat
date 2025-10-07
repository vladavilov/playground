@echo off
setlocal ENABLEEXTENSIONS

cd /d "%~dp0"

REM Usage:
REM   run_local.bat           -> run AI Workflow Service
REM   run_local.bat --install -> install/update deps, then run
REM   run_local.bat -i        -> same as --install
REM   run_local.bat --help    -> show help

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
set API_PORT=8009

REM Upstream services in docker-compose (reachable via localhost due to published ports)
set REDIS_URL=redis://localhost:6379

REM GraphRAG retrieval service (neo4j-retrieval-service -> host 8008)
set GRAPH_RAG_BASE_URL=http://localhost:8008
set HTTP_TIMEOUT_SEC=5.0
set RETRIEVAL_TOP_K=2
set RETRY_MAX_ATTEMPTS=3
set RETRIEVAL_BACKOFF_BASE_SEC=0.2

REM OpenAI-compatible mock service (openai-mock-service -> host 8010)
set OAI_KEY=KEY
set OAI_MODEL=gpt-4.1
set OAI_EMBED_MODEL=text-embedding-3-small
set OAI_BASE_URL=http://localhost:8010/v1

REM Optional: Project Management Service (if used by any HTTP client utilities)
set PROJECT_MANAGEMENT_SERVICE_URL=http://localhost:8003

REM Auth: use host.docker.internal so issuer matches containers
set AZURE_AD_AUTHORITY=http://host.docker.internal:8005
set AZURE_TENANT_ID=e7963c3a-3b3a-43b6-9426-89e433d07e69
set AZURE_CLIENT_ID=a9e304a9-5b6c-4ef7-9b37-23a579a6d7be

echo Starting AI Workflow Service on port %API_PORT%
python -m src.main

endlocal

:help
echo.
echo run_local.bat [--install]
echo    --install, -i   Install/update dependencies before start
echo.
goto :eof


