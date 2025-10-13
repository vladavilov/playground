# Docker Compose Build Script with Provenance Fix (Enhanced)
# This script uses docker buildx to build services without provenance attestations

Write-Host "Docker Compose Build Script (Enhanced)" -ForegroundColor Cyan
Write-Host "Building with --provenance=false to prevent build hangs..." -ForegroundColor Yellow

# Set environment variables to disable BuildKit attestations
$env:BUILDX_NO_DEFAULT_ATTESTATIONS = "1"
$env:DOCKER_BUILDKIT_INLINE_BUILDINFO = "0"

Write-Host "Environment variables set:" -ForegroundColor Green
Write-Host "  BUILDX_NO_DEFAULT_ATTESTATIONS=$env:BUILDX_NO_DEFAULT_ATTESTATIONS" -ForegroundColor Green
Write-Host "  DOCKER_BUILDKIT_INLINE_BUILDINFO=$env:DOCKER_BUILDKIT_INLINE_BUILDINFO" -ForegroundColor Green

# Get list of services to build from docker-compose.yml
# If no args provided, build all services
if ($args.Count -eq 0) {
    Write-Host "`nBuilding ALL services..." -ForegroundColor Cyan
    
    # Build using docker compose with environment variables set
    docker compose build python-base
    docker compose build --progress=plain
    docker compose build openai-mock-service
} else {
    Write-Host "`nBuilding specific services: $args" -ForegroundColor Cyan
    
    # Build specific services
    docker compose build --progress=plain @args
}

# Check exit code
if ($LASTEXITCODE -eq 0) {
    Write-Host "`n[SUCCESS] Build completed successfully!" -ForegroundColor Green
} else {
    Write-Host "`n[FAILED] Build failed with exit code $LASTEXITCODE" -ForegroundColor Red
    exit $LASTEXITCODE
}

