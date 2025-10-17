# Docker Compose Build Script with Provenance Fix (Enhanced)
# This script uses docker buildx to build services without provenance attestations
#
# Parameters:
#   -VlmMode: 'local' or 'remote' for Document Processing Service (default: 'local')
#   -SkipDocumentProcessingBase: Skip building document-processing-base image
#   -Services: Specific services to build (if not provided, builds all)
#
# Examples:
#   .\build-all.ps1                                    # Build all with local VLM
#   .\build-all.ps1 -VlmMode remote                    # Build all with remote VLM
#   .\build-all.ps1 -SkipDocumentProcessingBase        # Skip base, rebuild service only
#   .\build-all.ps1 -Services "ui-service", "neo4j"    # Build specific services

param(
    [Parameter(Mandatory=$false)]
    [ValidateSet('local', 'remote')]
    [string]$VlmMode = 'local',

    [Parameter(Mandatory=$false)]
    [switch]$SkipDocumentProcessingBase,

    [Parameter(Mandatory=$false, ValueFromRemainingArguments=$true)]
    [string[]]$Services
)

$ErrorActionPreference = 'Stop'

Write-Host "===========================================================" -ForegroundColor Cyan
Write-Host " Docker Compose Build Script (Enhanced)" -ForegroundColor Cyan
Write-Host "===========================================================" -ForegroundColor Cyan
Write-Host ""

# Set environment variables to disable BuildKit attestations
$env:BUILDX_NO_DEFAULT_ATTESTATIONS = "1"
$env:DOCKER_BUILDKIT_INLINE_BUILDINFO = "0"

Write-Host "Environment variables set:" -ForegroundColor Green
Write-Host "  BUILDX_NO_DEFAULT_ATTESTATIONS=$env:BUILDX_NO_DEFAULT_ATTESTATIONS" -ForegroundColor Green
Write-Host "  DOCKER_BUILDKIT_INLINE_BUILDINFO=$env:DOCKER_BUILDKIT_INLINE_BUILDINFO" -ForegroundColor Green
Write-Host ""

# Function to check if document-processing-base image needs to be built
function Test-DocumentProcessingBaseImage {
    $baseImageExists = docker images --format "{{.Repository}}:{{.Tag}}" | Select-String -Pattern "^document-processing-base:latest$" -Quiet
    return $baseImageExists
}

# Function to verify RapidOCR models exist
function Test-RapidOCRModels {
    $modelsPath = ".\services\document_processing_service\plugins\rapidocr-models"
    $requiredModels = @(
        "ch_PP-OCRv3_det_infer.onnx",
        "ch_PP-OCRv3_rec_infer.onnx",
        "ch_ppocr_mobile_v2.0_cls_infer.onnx"
    )
    
    $missingModels = @()
    foreach ($model in $requiredModels) {
        $modelPath = Join-Path $modelsPath $model
        if (-not (Test-Path $modelPath)) {
            $missingModels += $model
        }
    }
    
    return @{
        AllPresent = ($missingModels.Count -eq 0)
        Missing = $missingModels
    }
}

# Function to verify Docling layout models exist
function Test-DoclingModels {
    $modelsPath = ".\services\document_processing_service\plugins\docling-models\ds4sd--docling-layout-heron"
    $requiredFiles = @(
        "config.json",
        "model.safetensors",
        "preprocessor_config.json"
    )
    
    # Check if directory exists
    if (-not (Test-Path $modelsPath)) {
        return @{
            AllPresent = $false
            Missing = @("Directory 'ds4sd--docling-layout-heron' not found")
            DirectoryMissing = $true
        }
    }
    
    $missingFiles = @()
    foreach ($file in $requiredFiles) {
        $filePath = Join-Path $modelsPath $file
        if (-not (Test-Path $filePath)) {
            $missingFiles += $file
        }
    }
    
    return @{
        AllPresent = ($missingFiles.Count -eq 0)
        Missing = $missingFiles
        DirectoryMissing = $false
    }
}

# Function to build python-service-base image
function Build-PythonServiceBase {
    Write-Host "-----------------------------------------------------------" -ForegroundColor Yellow
    Write-Host " Building Python Service Base Image" -ForegroundColor Yellow
    Write-Host "-----------------------------------------------------------" -ForegroundColor Yellow
    Write-Host ""
    
    $startTime = Get-Date
    
    docker compose build python-base
    
    if ($LASTEXITCODE -ne 0) {
        throw "Failed to build python-service-base image"
    }
    
    $duration = (Get-Date) - $startTime
    $baseSize = docker images --format "{{.Size}}" python-service-base:latest
    
    Write-Host ""
    Write-Host "[OK] Python base image built successfully in $($duration.ToString('mm\:ss'))" -ForegroundColor Green
    Write-Host "   Image size: $baseSize" -ForegroundColor Cyan
    Write-Host ""
}

# Function to build document-processing-base image
function Build-DocumentProcessingBase {
    param(
        [string]$VlmMode
    )
    
    Write-Host "-----------------------------------------------------------" -ForegroundColor Yellow
    Write-Host " Building Document Processing Base Image" -ForegroundColor Yellow
    Write-Host "-----------------------------------------------------------" -ForegroundColor Yellow
    Write-Host "  VLM Mode: $VlmMode" -ForegroundColor Cyan
    Write-Host "  This may take 30-40 min (local) or 5-10 min (remote)..." -ForegroundColor Yellow
    Write-Host ""
    
    $startTime = Get-Date
    
    Push-Location services
    try {
        docker build `
            -f ./document_processing_service/Dockerfile.base `
            --build-arg DOCLING_VLM_MODE=$VlmMode `
            -t document-processing-base:latest `
            .
        
        if ($LASTEXITCODE -ne 0) {
            throw "Failed to build document-processing-base image"
        }
        
        $duration = (Get-Date) - $startTime
        $baseSize = docker images --format "{{.Size}}" document-processing-base:latest
        
        Write-Host ""
        Write-Host "[OK] Base image built successfully in $($duration.ToString('mm\:ss'))" -ForegroundColor Green
        Write-Host "   Image size: $baseSize" -ForegroundColor Cyan
        Write-Host ""
    } finally {
        Pop-Location
    }
}

# Check if document-processing-service is in the build list
$buildDocumentProcessing = $false
if ($Services.Count -eq 0) {
    # Building all services
    $buildDocumentProcessing = $true
} elseif ($Services -contains "document-processing-service") {
    $buildDocumentProcessing = $true
}

# Handle document-processing-base image if needed
if ($buildDocumentProcessing) {
    Write-Host "Checking Document Processing Service prerequisites..." -ForegroundColor Yellow
    Write-Host ""
    
    # Check RapidOCR models
    $rapidocrCheck = Test-RapidOCRModels
    if (-not $rapidocrCheck.AllPresent) {
        Write-Host ""
        Write-Host "================================================================" -ForegroundColor Red
        Write-Host "  ERROR: Required RapidOCR models NOT FOUND                    " -ForegroundColor Red
        Write-Host "================================================================" -ForegroundColor Red
        Write-Host ""
        Write-Host "Missing models in services/document_processing_service/plugins/rapidocr-models/:" -ForegroundColor Red
        foreach ($model in $rapidocrCheck.Missing) {
            Write-Host "  - $model" -ForegroundColor Red
        }
        Write-Host ""
        Write-Host "Please download the required models before building." -ForegroundColor Yellow
        exit 1
    }
    Write-Host "[OK] All RapidOCR models found" -ForegroundColor Green
    
    # Check Docling layout models
    $doclingCheck = Test-DoclingModels
    if (-not $doclingCheck.AllPresent) {
        Write-Host ""
        Write-Host "================================================================" -ForegroundColor Red
        Write-Host "  ERROR: Required Docling layout models NOT FOUND              " -ForegroundColor Red
        Write-Host "================================================================" -ForegroundColor Red
        Write-Host ""
        if ($doclingCheck.DirectoryMissing) {
            Write-Host "Directory not found:" -ForegroundColor Red
            Write-Host "  plugins/docling-models/ds4sd--docling-layout-heron/" -ForegroundColor Red
            Write-Host ""
            Write-Host "NOTE: The folder name MUST be 'ds4sd--docling-layout-heron' (with double hyphen)" -ForegroundColor Yellow
            Write-Host "      This matches the Hugging Face repository pattern 'ds4sd/docling-layout-heron'" -ForegroundColor Yellow
        } else {
            Write-Host "Missing files in plugins/docling-models/ds4sd--docling-layout-heron/:" -ForegroundColor Red
            foreach ($file in $doclingCheck.Missing) {
                Write-Host "  - $file" -ForegroundColor Red
            }
        }
        Write-Host ""
        Write-Host "To download models, run:" -ForegroundColor Yellow
        Write-Host "  cd services/document_processing_service" -ForegroundColor Yellow
        Write-Host "  pip install huggingface_hub" -ForegroundColor Yellow
        Write-Host "  python src/scripts/download_docling_models.py" -ForegroundColor Yellow
        Write-Host ""
        exit 1
    }
    Write-Host "[OK] All Docling layout models found" -ForegroundColor Green
    Write-Host ""
    
    # Check if python-service-base exists (required for document-processing-base)
    $pythonBaseExists = docker images --format "{{.Repository}}:{{.Tag}}" | Select-String -Pattern "^python-service-base:latest$" -Quiet
    if (-not $pythonBaseExists) {
        Write-Host "python-service-base:latest not found" -ForegroundColor Yellow
        Write-Host "   Building python base image first (required for document-processing-base)..." -ForegroundColor Yellow
        Write-Host ""
        
        Build-PythonServiceBase
    } else {
        Write-Host "[OK] python-service-base:latest found" -ForegroundColor Green
    }
    Write-Host ""
    
    # Check if base image exists
    if (-not $SkipDocumentProcessingBase) {
        $baseExists = Test-DocumentProcessingBaseImage
        
        if (-not $baseExists) {
            Write-Host "WARNING: document-processing-base:latest not found" -ForegroundColor Yellow
            Write-Host "   Building base image first..." -ForegroundColor Yellow
            Write-Host ""
            
            Build-DocumentProcessingBase -VlmMode $VlmMode
        } else {
            Write-Host "[OK] document-processing-base:latest found" -ForegroundColor Green
            Write-Host "   Use -SkipDocumentProcessingBase to skip rebuilding base image" -ForegroundColor Cyan
            Write-Host ""
            
            # Ask if user wants to rebuild base
            $rebuild = Read-Host "Rebuild base image? (y/N)"
            if ($rebuild -eq 'y' -or $rebuild -eq 'Y') {
                Build-DocumentProcessingBase -VlmMode $VlmMode
            } else {
                Write-Host "Skipping base image rebuild" -ForegroundColor Yellow
                Write-Host ""
            }
        }
    } else {
        Write-Host "Skipping document-processing-base build (--SkipDocumentProcessingBase flag)" -ForegroundColor Yellow
        
        # Verify base image exists when skipping
        $baseExists = Test-DocumentProcessingBaseImage
        if (-not $baseExists) {
            Write-Host ""
            Write-Host "ERROR: Base image document-processing-base:latest not found" -ForegroundColor Red
            Write-Host "   Cannot skip base build when base image doesn't exist." -ForegroundColor Red
            Write-Host "   Run without -SkipDocumentProcessingBase flag first." -ForegroundColor Yellow
            exit 1
        }
        Write-Host "[OK] Base image document-processing-base:latest found" -ForegroundColor Green
        Write-Host ""
    }
}

# Main docker compose build
Write-Host "-----------------------------------------------------------" -ForegroundColor Yellow
Write-Host " Building Services with Docker Compose" -ForegroundColor Yellow
Write-Host "-----------------------------------------------------------" -ForegroundColor Yellow
Write-Host ""

if ($Services.Count -eq 0) {
    Write-Host "Building ALL services..." -ForegroundColor Cyan
    Write-Host ""
    
    # Check if python-base needs to be built (only if not already built earlier)
    $pythonBaseExists = docker images --format "{{.Repository}}:{{.Tag}}" | Select-String -Pattern "^python-service-base:latest$" -Quiet
    if (-not $pythonBaseExists) {
        Write-Host "Building python-base..." -ForegroundColor Cyan
        docker compose build python-base
        if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
    }
    
    docker compose build --progress=plain
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
    
    docker compose build openai-mock-service
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
} else {
    Write-Host "Building specific services: $($Services -join ', ')" -ForegroundColor Cyan
    Write-Host ""
    
    # Build specific services
    docker compose build --progress=plain $Services
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
}

# Success message
Write-Host ""
Write-Host "===========================================================" -ForegroundColor Green
Write-Host " Build Completed Successfully!" -ForegroundColor Green
Write-Host "===========================================================" -ForegroundColor Green
Write-Host ""

if ($buildDocumentProcessing) {
    Write-Host "Document Processing Service Tips:" -ForegroundColor Cyan
    Write-Host "  - For code changes only: .\build-all.ps1 -SkipDocumentProcessingBase" -ForegroundColor White
    Write-Host "  - To rebuild base: Remove -SkipDocumentProcessingBase flag" -ForegroundColor White
    Write-Host "  - For remote VLM: .\build-all.ps1 -VlmMode remote" -ForegroundColor White
    Write-Host ""
}

