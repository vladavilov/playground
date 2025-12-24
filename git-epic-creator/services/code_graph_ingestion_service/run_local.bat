@echo off
setlocal

REM Builds and starts the Rust service locally.

cd /d "%~dp0"

set API_PORT=8015

REM Optional:
REM set WORKSPACE_ROOT=workspace
REM set DATABASE_URL=postgres://postgres:postgres@localhost:5432/postgres
REM set NEO4J_REPOSITORY_SERVICE_URL=http://localhost:8080

cargo run --release

endlocal
