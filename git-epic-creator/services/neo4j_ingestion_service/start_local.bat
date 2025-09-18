@echo off
REM Local start script for Neo4j Ingestion Service
REM This script starts the microservice locally with environment variables from docker-compose.env

echo Starting Neo4j Ingestion Service locally...

REM Set the working directory to the service directory
cd /d "%~dp0"

REM Check if virtual environment exists
if not exist "venv312\Scripts\activate.bat" (
    echo Error: Virtual environment not found. Please run 'python -m venv venv312' first.
    pause
    exit /b 1
)

REM Activate virtual environment
call venv312\Scripts\activate.bat

REM Ensure shared package and service are installed/updated (editable)
echo Installing/Updating shared package...
pip install -e ..\shared
if errorlevel 1 (
    echo Error: Failed to install shared package
    pause
    exit /b 1
)

echo Installing/Updating service package...
pip install -e .
if errorlevel 1 (
    echo Error: Failed to install service package
    pause
    exit /b 1
)

REM Create logs directory if it doesn't exist
if not exist "logs" mkdir logs

REM Set log file path with timestamp
for /f "tokens=2 delims==" %%a in ('wmic OS Get localdatetime /value') do set "dt=%%a"
set "YY=%dt:~2,2%" & set "YYYY=%dt:~0,4%" & set "MM=%dt:~4,2%" & set "DD=%dt:~6,2%"
set "HH=%dt:~8,2%" & set "Min=%dt:~10,2%" & set "Sec=%dt:~12,2%"
set "timestamp=%YYYY%-%MM%-%DD%_%HH%-%Min%-%Sec%"
set "LOG_FILE=logs\neo4j_ingestion_service_%timestamp%.log"

echo Log file: %LOG_FILE%

REM Set environment variables explicitly
echo Setting environment variables...

REM PostgreSQL Configuration
set POSTGRES_HOST=localhost
set POSTGRES_PORT=5432
set POSTGRES_USER=postgres
set POSTGRES_PASSWORD=postgres123
set POSTGRES_DB=requirementsdb
set POSTGRES_SCHEMA=public
set POSTGRES_POOL_SIZE=5
set POSTGRES_MAX_OVERFLOW=10
set POSTGRES_POOL_TIMEOUT=30
set POSTGRES_POOL_RECYCLE=1800

REM Neo4j Configuration
set NEO4J_URI=bolt://localhost:7687
set NEO4J_USERNAME=neo4j
set NEO4J_PASSWORD=neo4j123
set NEO4J_DATABASE=neo4j
set NEO4J_CONNECTION_TIMEOUT=30.0
set NEO4J_MAX_RETRY_ATTEMPTS=3
set NEO4J_RETRY_DELAY=2.0
set NEO4J_MAX_CONNECTION_POOL_SIZE=50
set NEO4J_MAX_TRANSACTION_RETRY_TIME=30.0

REM Neo4j Vector Index Configuration
set CHUNK_VECTOR_INDEX_NAME=graphrag_chunk_index
set CHUNK_VECTOR_INDEX_LABEL=Chunk
set VECTOR_INDEX_PROPERTY=embedding
set VECTOR_INDEX_DIMENSIONS=1536
set VECTOR_INDEX_SIMILARITY=cosine

REM Redis Configuration
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
set REDIS_PUBSUB_RETRY_ATTEMPTS=3
set REDIS_PUBSUB_RETRY_DELAY=2.0

REM Celery Configuration
set CELERY_BROKER_URL=redis://localhost:6379/0
set CELERY_RESULT_BACKEND=redis://localhost:6379/0

REM Common Retry/Backoff Settings
set RETRY_MAX_ATTEMPTS=3
set RETRY_BACKOFF_BASE_SEC=2
set RETRY_BACKOFF_FACTOR=2
set RETRY_BACKOFF_MAX_SEC=60

REM HTTP Client Configuration
set PROJECT_MANAGEMENT_SERVICE_URL=http://localhost:8003

REM OpenAI Configuration
set OAI_KEY=KEY
set OAI_MODEL=gpt-4.1
set OAI_EMBED_MODEL=text-embedding-3-small
set OAI_BASE_URL=http://localhost:8010/v1
set OAI_API_VERSION=2024-02-15-preview

REM AI Workflow / GraphRAG Client Configuration
set GRAPH_RAG_BASE_URL=http://localhost:8008
set GRAPH_RAG_TIMEOUT_SEC=5.0
set RETRIEVAL_MAX_ATTEMPTS=3
set RETRIEVAL_BACKOFF_BASE_SEC=0.2

REM GraphRAG Concurrency/Throttling
set GRAPHRAG_ASYNC_MODE=threaded
set GRAPHRAG_LLM_THREAD_COUNT=16
set GRAPHRAG_LLM_THREAD_STAGGER=0.2
set GRAPHRAG_LLM_CONCURRENT_REQUESTS=12
set GRAPHRAG_LLM_TOKENS_PER_MINUTE=100000
set GRAPHRAG_LLM_REQUESTS_PER_MINUTE=60
set GRAPHRAG_EMBEDDING_THREAD_COUNT=16
set GRAPHRAG_EMBEDDING_THREAD_STAGGER=0.2
set GRAPHRAG_EMBEDDING_CONCURRENT_REQUESTS=12
set GRAPHRAG_EMBEDDING_TOKENS_PER_MINUTE=60000
set GRAPHRAG_EMBEDDING_REQUESTS_PER_MINUTE=2000
set GRAPHRAG_EXTRACT_MAX_GLEANINGS=1

REM Azure AD Configuration
set AZURE_AD_AUTHORITY=http://localhost:8005
set AZURE_TENANT_ID=e7963c3a-3b3a-43b6-9426-89e433d07e69
set AZURE_CLIENT_ID=a9e304a9-5b6c-4ef7-9b37-23a579a6d7be

REM Mock Authentication Configuration
set MOCK_CLIENT_SECRET=mock-secret
set MOCK_SCOPE=api://a9e304a9-5b6c-4ef7-9b37-23a579a6d7be

REM Azure Blob Storage Configuration
set AZURE_STORAGE_CONNECTION_STRING=DefaultEndpointsProtocol=http;AccountName=devstoreaccount1;AccountKey=Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw==;BlobEndpoint=http://localhost:10000/devstoreaccount1;
set AZURE_STORAGE_CONTAINER_NAME=documents
set AZURE_STORAGE_ACCOUNT_NAME=devstoreaccount1
set AZURE_STORAGE_ACCOUNT_KEY=Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw==
set AZURE_STORAGE_BLOB_ENDPOINT=http://localhost:10000/devstoreaccount1
set AZURE_STORAGE_MAX_SINGLE_PUT_SIZE=67108864
set AZURE_STORAGE_MAX_BLOCK_SIZE=4194304

REM Tika Configuration
set TIKA_SERVER_JAR=/opt/tika-server/tika-server.jar
set TIKA_SERVER_ENDPOINT=http://localhost:9998
set TIKA_LOG_PATH=/tmp/tika-logs
set TIKA_SERVER_TIMEOUT=300
set TIKA_CLIENT_TIMEOUT=120
set TIKA_VERSION=3.1.0
set TIKA_CLIENT_ONLY=true
set TIKA_SERVER_AUTO_START=true
set TIKA_SERVER_STARTUP_TIMEOUT=60

REM Community Summarization Configuration
set COMM_SUMM_MAX_WORKERS=4
set COMM_SUMM_MAX_ATTEMPTS=3
set COMM_SUMM_BASE_DELAY_MS=250

REM Backfill Description Configuration
set BACKFILL_BATCH=100
set BACKFILL_MAX_WORKERS=4

REM Set additional environment variables for local development
set PYTHONPATH=%CD%\src;%CD%\..\shared\src
set PYTHONUNBUFFERED=1

echo Starting service...
echo Press Ctrl+C to stop the service
echo.

REM Start the service and redirect output to log file
python src\main.py > "%LOG_FILE%" 2>&1

REM If the service exits, show the last few lines of the log
if errorlevel 1 (
    echo.
    echo Service exited with error. Last 20 lines of log:
    echo ==========================================
    powershell "Get-Content '%LOG_FILE%' | Select-Object -Last 20"
    echo ==========================================
)

echo Service stopped.
pause
