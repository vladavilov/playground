#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
VENV_DIR="$SCRIPT_DIR/.test_venv"

export COVERAGE_FILE="$SCRIPT_DIR/.coverage"

SERVICE_DIRS=(
    "risk-analytics-agent/src/news_sentiment_service/common"
    "risk-analytics-agent/src/news_sentiment_service/data_ingestion/adapters/benzinga_adapter"
    "risk-analytics-agent/src/news_sentiment_service/data_ingestion/orchestrator_service"
    "risk-analytics-agent/src/news_sentiment_service/news_processor_service"
    "risk-analytics-agent/src/news_sentiment_service/sentiment_api_service"
)

FAILED_SERVICES=()

cleanup() {
  echo "--- Cleaning up ---"
  deactivate &>/dev/null || true
  if [ -d "$VENV_DIR" ]; then
    echo "Removing virtual environment at $VENV_DIR..."
    rm -rf "$VENV_DIR"
  fi
  echo "Cleanup complete."
}

trap cleanup EXIT

if [ ! -d "$SCRIPT_DIR/risk-analytics-agent" ]; then
    echo "Error: This script must be located in the root of the 'playground' directory."
    exit 1
fi

echo "--- Preparing for Test Execution ---"

echo "Creating a shared virtual environment at $VENV_DIR..."
python -m venv "$VENV_DIR"
source "$VENV_DIR/bin/activate"

echo "Upgrading pip and installing testing tools..."
pip install --upgrade pip -q
pip install pytest pytest-cov pytest-asyncio -q

echo "Installing all service dependencies..."
for service_path in "${SERVICE_DIRS[@]}"; do
    service_dir_absolute="$SCRIPT_DIR/$service_path"
    if [ -d "$service_dir_absolute" ]; then
        echo "Installing dependencies for: ${service_path}"
        cd "$service_dir_absolute"
        pip install -e ".[dev]" -q
        cd "$SCRIPT_DIR"
    else
        echo "Warning: Directory not found, skipping dependency installation for: $service_path"
    fi
done

echo "Removing old coverage data..."
rm -f "$COVERAGE_FILE"
rm -rf "$SCRIPT_DIR/htmlcov"

cd "$SCRIPT_DIR"

for service_path in "${SERVICE_DIRS[@]}"; do
    service_dir_absolute="$SCRIPT_DIR/$service_path"
    
    echo ""
    echo "======================================================================"
    echo "             Testing Service: ${service_path}"
    echo "======================================================================"

    if [ ! -d "$service_dir_absolute" ]; then
        echo "Warning: Directory not found, skipping tests for: $service_dir_absolute"
        continue
    fi

    echo "Running pytest on $service_dir_absolute and appending to coverage report..."
    if ! pytest --cov="${service_path}/src" --cov-append "$service_dir_absolute"; then
        echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        echo "!!! Tests FAILED for service: ${service_path}"
        echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        FAILED_SERVICES+=("${service_path}")
    fi
done

echo ""
echo "======================================================================"
echo "           Aggregated Project Coverage Report"
echo "======================================================================"

echo "Generating combined text and HTML coverage reports..."
coverage report -m
coverage html -d "$SCRIPT_DIR/htmlcov"
echo "HTML report generated at: file://$SCRIPT_DIR/htmlcov/index.html"


if [ ${#FAILED_SERVICES[@]} -ne 0 ]; then
    echo ""
    echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    echo "The following services had test failures:"
    for failed_service in "${FAILED_SERVICES[@]}"; do
        echo "  - $failed_service"
    done
    echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    exit 1
else
    echo ""
    echo "======================================================================"
    echo "           All test cycles completed successfully!           "
    echo "======================================================================"
fi 