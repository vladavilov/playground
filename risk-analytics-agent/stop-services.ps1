#!/usr/bin/env pwsh

<#
.SYNOPSIS
    Stop Docker Compose services with various options.

.DESCRIPTION
    This script provides convenient ways to stop Docker Compose services:
    - Stop all services
    - Stop and remove containers
    - Stop and remove containers with volumes
    - Stop and remove everything including images

.PARAMETER Action
    The action to perform. Valid values: stop, down, clean, purge

.PARAMETER Timeout
    Timeout in seconds for stopping services (default: 10)

.EXAMPLE
    .\stop-services.ps1 -Action stop
    Stop all running services

.EXAMPLE
    .\stop-services.ps1 -Action clean
    Stop services and remove containers with volumes
#>

param(
    [Parameter(Mandatory = $false)]
    [ValidateSet("stop", "down", "clean", "purge")]
    [string]$Action = "stop",
    
    [Parameter(Mandatory = $false)]
    [int]$Timeout = 10
)

# Check if docker-compose.yml exists
if (-not (Test-Path "docker-compose.yml")) {
    Write-Error "docker-compose.yml not found. Please run this script from the playground/risk-analytics-agent directory."
    exit 1
}

# Define actions
$ActionDescriptions = @{
    stop = "Stop all running services (containers remain)"
    down = "Stop and remove containers and networks"
    clean = "Stop and remove containers, networks, and volumes"
    purge = "Stop and remove containers, networks, volumes, and images"
}

# Build the docker-compose command based on action
$DockerComposeArgs = @()

switch ($Action) {
    "stop" {
        $DockerComposeArgs = @("stop", "--timeout", $Timeout.ToString())
    }
    "down" {
        $DockerComposeArgs = @("down")
    }
    "clean" {
        $DockerComposeArgs = @("down", "-v")
    }
    "purge" {
        $DockerComposeArgs = @("down", "-v", "--rmi", "all")
    }
}

# Display what we're about to run
Write-Host "Action: $($ActionDescriptions[$Action])" -ForegroundColor Green
Write-Host "Command: docker-compose $($DockerComposeArgs -join ' ')" -ForegroundColor Yellow

# Confirm before running for destructive actions
if ($Action -in @("clean", "purge")) {
    Write-Host "`nWARNING: This action will remove data and/or images!" -ForegroundColor Red
    $confirmation = Read-Host "Are you sure you want to continue? (y/N)"
    if ($confirmation -ne 'y' -and $confirmation -ne 'Y') {
        Write-Host "Operation cancelled." -ForegroundColor Yellow
        exit 0
    }
}

# Run the command
try {
    Write-Host "`nStopping services..." -ForegroundColor Cyan
    & docker-compose $DockerComposeArgs
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host "`nServices stopped successfully!" -ForegroundColor Green
        
        # Show current status
        Write-Host "`nCurrent status:" -ForegroundColor Cyan
        & docker-compose ps
        
        # Additional cleanup suggestions
        if ($Action -eq "stop") {
            Write-Host "`nTo completely remove containers and networks, run:" -ForegroundColor Yellow
            Write-Host "  .\stop-services.ps1 -Action down" -ForegroundColor White
        }
        
        if ($Action -eq "down") {
            Write-Host "`nTo also remove volumes, run:" -ForegroundColor Yellow
            Write-Host "  .\stop-services.ps1 -Action clean" -ForegroundColor White
        }
    }
} catch {
    Write-Error "Failed to stop services: $_"
    exit 1
} 