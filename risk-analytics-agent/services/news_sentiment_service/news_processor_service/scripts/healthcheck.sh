#!/bin/bash

# Enhanced health check script for news-processor-service
# Checks both basic connectivity and service health status

set -e

# Configuration
SERVICE_URL="http://localhost:8000"
HEALTH_ENDPOINT="${SERVICE_URL}/health"
DETAILED_HEALTH_ENDPOINT="${SERVICE_URL}/health/detailed"
TIMEOUT=10

# Function to check if service responds
check_connectivity() {
    if ! curl -f -s --max-time "$TIMEOUT" "$HEALTH_ENDPOINT" > /dev/null 2>&1; then
        echo "FAILED: Service not responding at $HEALTH_ENDPOINT"
        exit 1
    fi
}

# Function to check service health status
check_health_status() {
    local response
    response=$(curl -f -s --max-time "$TIMEOUT" "$HEALTH_ENDPOINT" 2>/dev/null)
    
    if [ $? -ne 0 ]; then
        echo "FAILED: Could not get health status"
        exit 1
    fi
    
    # Check if response contains "healthy" status
    if echo "$response" | grep -q '"status":\s*"healthy"'; then
        echo "SUCCESS: Service is healthy"
        return 0
    elif echo "$response" | grep -q '"status":\s*"degraded"'; then
        echo "WARNING: Service is degraded but functional"
        return 0
    else
        echo "FAILED: Service reports unhealthy status"
        echo "Response: $response"
        exit 1
    fi
}

# Function to check detailed health (optional, non-fatal)
check_detailed_health() {
    local response
    response=$(curl -f -s --max-time "$TIMEOUT" "$DETAILED_HEALTH_ENDPOINT" 2>/dev/null)
    
    if [ $? -eq 0 ]; then
        # Log any warnings about dependencies
        if echo "$response" | grep -q '"warning"'; then
            local warning
            warning=$(echo "$response" | grep -o '"warning":\s*"[^"]*"' | cut -d'"' -f4)
            echo "WARNING: $warning"
        fi
    fi
}

# Main health check logic
echo "Starting health check for news-processor-service..."

# Step 1: Basic connectivity check
check_connectivity

# Step 2: Health status check
check_health_status

# Step 3: Detailed health check (non-fatal)
check_detailed_health

echo "Health check completed successfully"
exit 0 