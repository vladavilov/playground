@echo off
setlocal

REM Creates/uses a local venv and starts the service.
REM Note: src.main:app is added in Task 01.

cd /d "%~dp0"

if not exist venv (
  python -m venv venv
)

call ".\venv\Scripts\activate.bat"

python -m pip install -U pip
pip install -e ".[dev]"

REM Will work once Task 01 is implemented:
REM uvicorn src.main:app --reload --port 8015
python -c "print('Environment ready. Implement Task 01 to run uvicorn src.main:app --reload --port 8015')"

endlocal
