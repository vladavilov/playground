#!/bin/bash

echo "=== Debugging Docker Health Check ==="

echo "1. Checking current service status..."
docker-compose ps news-processor-service

echo ""
echo "2. Checking Docker health status..."
health_status=$(docker inspect --format='{{.State.Health.Status}}' news-processor-service 2>/dev/null)
echo "   Health Status: $health_status"

echo ""
echo "3. Checking health check logs..."
docker inspect --format='{{range .State.Health.Log}}{{.Start}}: Exit Code {{.ExitCode}} - {{.Output}}{{end}}' news-processor-service

echo ""
echo "4. Testing healthcheck script manually inside container..."
echo "   Running: docker-compose exec news-processor-service ./healthcheck.sh"
docker-compose exec news-processor-service ./healthcheck.sh
manual_exit_code=$?
echo "   Manual execution exit code: $manual_exit_code"

echo ""
echo "5. Testing health endpoint directly..."
response=$(curl -s http://localhost:8000/health)
curl_exit_code=$?
echo "   Curl exit code: $curl_exit_code"
echo "   Response: $response"

echo ""
echo "6. Checking if health endpoint returns proper status..."
if [ $curl_exit_code -eq 0 ]; then
    status=$(echo "$response" | python3 -c "import sys, json; data = json.load(sys.stdin); print(data.get('status', 'unknown'))" 2>/dev/null)
    echo "   Parsed status: $status"
    
    if [ "$status" = "healthy" ] || [ "$status" = "degraded" ]; then
        echo "   ✓ Status should pass health check"
    else
        echo "   ✗ Status would fail health check"
    fi
else
    echo "   ✗ Cannot reach health endpoint"
fi

echo ""
echo "7. Testing healthcheck script with verbose output..."
echo "   Running healthcheck with set -x for debugging..."
docker-compose exec news-processor-service bash -c "set -x; ./healthcheck.sh"
verbose_exit_code=$?
echo "   Verbose execution exit code: $verbose_exit_code"

echo ""
echo "8. Checking Docker daemon health check configuration..."
docker inspect news-processor-service | jq '.Config.Healthcheck'

echo ""
echo "=== Summary ==="
echo "- Service Status: $(docker-compose ps --format 'table {{.State}}' news-processor-service | tail -1)"
echo "- Docker Health Status: $health_status"
echo "- Manual Script Exit Code: $manual_exit_code"
echo "- Health Endpoint Available: $([ $curl_exit_code -eq 0 ] && echo "Yes" || echo "No")"

if [ "$health_status" = "healthy" ]; then
    echo "✅ Service is healthy!"
elif [ "$health_status" = "unhealthy" ]; then
    echo "❌ Service is unhealthy - check health check logs above"
elif [ "$health_status" = "starting" ]; then
    echo "⏳ Service is still starting - wait for health check to complete"
else
    echo "⚠️  Health status unknown: $health_status"
fi 