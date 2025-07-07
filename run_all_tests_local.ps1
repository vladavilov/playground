
$ScriptDir = $PSScriptRoot
$VenvDir = Join-Path $ScriptDir ".test_venv2"
$CoverageFile = Join-Path $ScriptDir ".coverage"
$env:COVERAGE_FILE = $CoverageFile

$ServiceDirs = @(
    "risk-analytics-agent\src\news_sentiment_service\common",
    "risk-analytics-agent\src\news_sentiment_service\data_ingestion\adapters\benzinga_adapter",
    "risk-analytics-agent\src\news_sentiment_service\data_ingestion\orchestrator_service",
    "risk-analytics-agent\src\news_sentiment_service\news_processor_service",
    "risk-analytics-agent\src\news_sentiment_service\sentiment_api_service"
)
$FailedServices = New-Object System.Collections.ArrayList

try {
    Write-Host "--- Preparing for Test Execution ---" -ForegroundColor Yellow

    Write-Host "Creating a shared virtual environment at $VenvDir..."
    python -m venv $VenvDir
    . (Join-Path $VenvDir "Scripts\activate.ps1")

    Write-Host "Upgrading pip and installing testing tools..."
    python -m pip install --upgrade pip -q
    pip install pytest pytest-cov pytest-asyncio -q

    Write-Host "Installing all service dependencies..."
    foreach ($servicePath in $ServiceDirs) {
        $serviceDirAbsolute = Join-Path $ScriptDir $servicePath
        if (Test-Path $serviceDirAbsolute) {
            Write-Host "Installing dependencies for: $servicePath"
            Set-Location $serviceDirAbsolute
            pip install -e ".[dev]" -q
            Set-Location $ScriptDir
        } else {
            Write-Host "Warning: Directory not found, skipping dependency installation for: $servicePath" -ForegroundColor Red
        }
    }

    Write-Host "Removing old coverage data..."
    if (Test-Path $CoverageFile) { Remove-Item $CoverageFile }
    if (Test-Path (Join-Path $ScriptDir "htmlcov")) { Remove-Item -Recurse -Force (Join-Path $ScriptDir "htmlcov") }

    Set-Location $ScriptDir

    foreach ($servicePath in $ServiceDirs) {
        $serviceDirAbsolute = Join-Path $ScriptDir $servicePath
        
        Write-Host ""
        Write-Host "======================================================================" -ForegroundColor Cyan
        Write-Host "             Testing Service: $servicePath" -ForegroundColor Cyan
        Write-Host "======================================================================" -ForegroundColor Cyan

        if (-not (Test-Path $serviceDirAbsolute)) {
            Write-Host "Warning: Directory not found, skipping tests for: $serviceDirAbsolute" -ForegroundColor Red
            continue
        }

        Write-Host "Running pytest on $serviceDirAbsolute and appending to coverage report..."
        $covPath = Join-Path $servicePath "src"
        pytest --cov=$covPath --cov-append $serviceDirAbsolute
        if ($LASTEXITCODE -ne 0) {
            Write-Host "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" -ForegroundColor Red
            Write-Host "!!! Tests FAILED for service: $servicePath" -ForegroundColor Red
            Write-Host "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" -ForegroundColor Red
            $FailedServices.Add($servicePath) | Out-Null
        }
    }

    Write-Host ""
    Write-Host "======================================================================" -ForegroundColor Green
    Write-Host "           Aggregated Project Coverage Report" -ForegroundColor Green
    Write-Host "======================================================================" -ForegroundColor Green

    Write-Host "Generating combined text and HTML coverage reports..."
    coverage report -m
    coverage html -d (Join-Path $ScriptDir "htmlcov")
    Write-Host "HTML report generated at: file://$ScriptDir/htmlcov/index.html"
}
finally {
    Write-Host ""
    Write-Host "--- Cleaning up ---" -ForegroundColor Yellow
    if (Get-Command 'deactivate' -ErrorAction SilentlyContinue) {
        deactivate
    }
    if (Test-Path $VenvDir) {
        Write-Host "Removing virtual environment at $VenvDir..."
        Remove-Item -Recurse -Force $VenvDir
    }
    Write-Host "Cleanup complete."
}

if ($FailedServices.Count -ne 0) {
    Write-Host ""
    Write-Host "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" -ForegroundColor Red
    Write-Host "The following services had test failures:" -ForegroundColor Red
    foreach ($failedService in $FailedServices) {
        Write-Host "  - $failedService" -ForegroundColor Red
    }
    Write-Host "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" -ForegroundColor Red
    exit 1
} else {
    Write-Host ""
    Write-Host "======================================================================" -ForegroundColor Green
    Write-Host "           All test cycles completed successfully!           " -ForegroundColor Green
    Write-Host "======================================================================" -ForegroundColor Green
} 