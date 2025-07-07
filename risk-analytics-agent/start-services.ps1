#!/usr/bin/env pwsh

<#
.SYNOPSIS
    Start specific groups of services from the Docker Compose configuration.

.DESCRIPTION
    This script allows you to start different combinations of services:
    - cosmos: Cosmos DB emulator and initialization
    - servicebus: Service Bus emulator with SQL Edge dependency
    - emulators: All Azure emulators (Cosmos DB + Service Bus)
    - apps: Application services (benzinga-adapter, dis-orchestrator-service)
    - all: All services

.PARAMETER Group
    The group of services to start. Valid values: cosmos, servicebus, emulators, apps, all

.PARAMETER Build
    Force rebuild of images before starting services

.PARAMETER Detach
    Run services in detached mode

.EXAMPLE
    .\start-services.ps1 -Group cosmos
    Start only Cosmos DB related services

.EXAMPLE
    .\start-services.ps1 -Group all -Build
    Start all services with forced rebuild
#>

param(
    [Parameter(Mandatory = $true)]
    [ValidateSet("cosmos", "servicebus", "emulators", "apps", "all")]
    [string]$Group,
    
    [switch]$Build,
    
    [switch]$Detach
)

# Check if docker-compose.yml exists
if (-not (Test-Path "docker-compose.yml")) {
    Write-Error "docker-compose.yml not found. Please run this script from the playground/risk-analytics-agent directory."
    exit 1
}

# Define service groups
$ServiceGroups = @{
    cosmos = @("cosmosdb-emulator", "cosmos-init", "dis-orchestrator-service")
    servicebus = @("sqledge", "servicebus-emulator", "servicebus-healthcheck")
    emulators = @("cosmosdb-emulator", "cosmos-init", "dis-orchestrator-service", "sqledge", "servicebus-emulator", "servicebus-healthcheck")
    apps = @("benzinga-adapter", "dis-orchestrator-service")
    all = @("cosmosdb-emulator", "cosmos-init", "sqledge", "servicebus-emulator", "servicebus-healthcheck", "benzinga-adapter", "dis-orchestrator-service")
}

# Get the services to start
$ServicesToStart = $ServiceGroups[$Group]

if (-not $ServicesToStart) {
    Write-Error "Invalid group: $Group"
    exit 1
}

# Build the docker-compose command
$DockerComposeArgs = @("up")

if ($Build) {
    $DockerComposeArgs += "--build"
}

if ($Detach) {
    $DockerComposeArgs += "-d"
}

# Add the services
$DockerComposeArgs += $ServicesToStart

# Display what we're about to run
Write-Host "Starting services: $($ServicesToStart -join ', ')" -ForegroundColor Green
Write-Host "Command: docker-compose $($DockerComposeArgs -join ' ')" -ForegroundColor Yellow

# Confirm before running
$confirmation = Read-Host "Do you want to continue? (y/N)"
if ($confirmation -ne 'y' -and $confirmation -ne 'Y') {
    Write-Host "Operation cancelled." -ForegroundColor Yellow
    exit 0
}

# Run the command
try {
    & docker-compose $DockerComposeArgs
} catch {
    Write-Error "Failed to start services: $_"
    exit 1
}

Write-Host "`nServices started successfully!" -ForegroundColor Green

# Display useful information based on the group
switch ($Group) {
    "cosmos" {
        Write-Host "`nCosmos DB Emulator:" -ForegroundColor Cyan
        Write-Host "  - Web UI: https://localhost:8081/_explorer/index.html" -ForegroundColor White
        Write-Host "  - API Endpoint: https://localhost:8081 (HTTPS with SSL bypass)" -ForegroundColor White
        Write-Host "  - Key: C2y6yDjf5/R+ob0N8A7Cgv30VRDJIWEHLM+4QDU5DE2nQ9nDuVTqobD4b8mGGyPMbIZnqyMsEcaGQy67XIw/Jw==" -ForegroundColor White
        Write-Host "`nDIS Orchestrator:" -ForegroundColor Cyan
        Write-Host "  - Service: http://localhost:8002" -ForegroundColor White
    }
    "servicebus" {
        Write-Host "`nService Bus Emulator:" -ForegroundColor Cyan
        Write-Host "  - AMQP: amqp://localhost:5672" -ForegroundColor White
        Write-Host "  - AMQPS: amqps://localhost:5671" -ForegroundColor White
        Write-Host "  - Management: http://localhost:9090" -ForegroundColor White
        Write-Host "  - Health: http://localhost:5300/health" -ForegroundColor White
        Write-Host "`nSQL Edge:" -ForegroundColor Cyan
        Write-Host "  - Server: localhost,1433" -ForegroundColor White
        Write-Host "  - Username: sa" -ForegroundColor White
        Write-Host "  - Password: YourStrong!Passw0rd" -ForegroundColor White
    }
    "emulators" {
        Write-Host "`nCosmos DB Emulator:" -ForegroundColor Cyan
        Write-Host "  - Web UI: https://localhost:8081/_explorer/index.html" -ForegroundColor White
        Write-Host "  - API Endpoint: https://localhost:8081 (HTTPS with SSL bypass)" -ForegroundColor White
        Write-Host "`nService Bus Emulator:" -ForegroundColor Cyan
        Write-Host "  - AMQP: amqp://localhost:5672" -ForegroundColor White
        Write-Host "  - Management: http://localhost:9090" -ForegroundColor White
        Write-Host "  - Health: http://localhost:5300/health" -ForegroundColor White
        Write-Host "`nDIS Orchestrator:" -ForegroundColor Cyan
        Write-Host "  - Service: http://localhost:8002" -ForegroundColor White
    }
    "apps" {
        Write-Host "`nApplication Services:" -ForegroundColor Cyan
        Write-Host "  - Benzinga Adapter: http://localhost:8001" -ForegroundColor White
        Write-Host "  - DIS Orchestrator: http://localhost:8002" -ForegroundColor White
    }
    "all" {
        Write-Host "`nAll Services Started:" -ForegroundColor Cyan
        Write-Host "  - Cosmos DB Web UI: https://localhost:8081/_explorer/index.html" -ForegroundColor White
        Write-Host "  - Service Bus Management: http://localhost:9090" -ForegroundColor White
        Write-Host "  - Benzinga Adapter: http://localhost:8001" -ForegroundColor White
        Write-Host "  - DIS Orchestrator: http://localhost:8002" -ForegroundColor White
    }
}

Write-Host "`nTo stop services, run: docker-compose down" -ForegroundColor Yellow
Write-Host "To view logs, run: docker-compose logs -f [service-name]" -ForegroundColor Yellow 