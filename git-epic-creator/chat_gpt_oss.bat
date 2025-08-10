@echo off
REM Chat with local gpt-oss service via OpenAI SDK
REM Defaults: base_url=http://localhost:8010/v1, api_key=EMPTY, model=gpt-oss:20b

setlocal ENABLEDELAYEDEXPANSION

set "BASE_URL=%OPENAI_BASE_URL%"
if "%BASE_URL%"=="" set "BASE_URL=http://localhost:8010/v1"

set "API_KEY=%OPENAI_API_KEY%"
if "%API_KEY%"=="" set "API_KEY=EMPTY"

set "MODEL=%GPT_OSS_MODEL%"
if "%MODEL%"=="" set "MODEL=gpt-oss:20b"

REM Build message arg string
set "ARGSTR="
if not "%~1"=="" set "ARGSTR=-m %*"

REM Call the Python helper script in the same folder
python "%~dp0chat_gpt_oss.py" %ARGSTR% --base-url "%BASE_URL%" --api-key "%API_KEY%" --model "%MODEL%"

endlocal

