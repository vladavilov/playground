
@echo off
setlocal

set "SERVICES_DIR=%~dp0services"
set "PYTHON_EXE=python"

for /d %%s in ("%SERVICES_DIR%\*") do (
    echo ===============================================================================
    echo Running tests for: %%~ns
    echo ===============================================================================
    
    pushd "%%s"
    
    if exist "pyproject.toml" (
        echo Found pyproject.toml, setting up virtual environment and running tests...
        
        rem Create virtual environment
        if not exist "venv" (
            echo Creating virtual environment...
            %PYTHON_EXE% -m venv venv
            if %errorlevel% neq 0 (
                echo Failed to create virtual environment for %%~ns.
                goto :next_service
            )
        )
        
        rem Activate virtual environment and run commands
        call "venv\Scripts\activate.bat"
        
        rem Install dependencies
        echo Installing dependencies...
        pip install -e .[dev] --quiet
        if %errorlevel% neq 0 (
            echo Failed to install dependencies for %%~ns.
            call "venv\Scripts\deactivate.bat"
            goto :next_service
        )
        
        rem Run tests
        echo Running tests...
        pytest
        if %errorlevel% neq 0 (
            echo Tests failed for %%~ns.
        ) else (
            echo Tests passed for %%~ns.
        )
        
        rem Deactivate virtual environment
        call "venv\Scripts\deactivate.bat"
        
    ) else (
        echo No pyproject.toml found in %%~ns. Skipping.
    )
    
    :next_service
    popd
    echo.
)

echo ===============================================================================
echo All tests have been run.
echo ===============================================================================

endlocal 