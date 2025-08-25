# Run Neo4j Ingestion Service locally (host) against Docker Compose services
# Usage:
#   Start: powershell -ExecutionPolicy Bypass -File .\scripts\run_local.ps1
#   Stop:  powershell -ExecutionPolicy Bypass -File .\scripts\run_local.ps1 -Stop

param(
    [switch]$Stop,
    [int[]]$Ports = @(8011, 8011)
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

# Root path (service directory)
$serviceDir = Split-Path -Parent $MyInvocation.MyCommand.Path | Split-Path -Parent
$repoRoot = (Split-Path -Parent $serviceDir) | Split-Path -Parent

Push-Location $serviceDir

try {
    if ($Stop) {
        # Try to stop process running this service's main.py
        $stopped = $false
        $targets = Get-CimInstance Win32_Process |
            Where-Object { $_.CommandLine -match 'neo4j_ingestion_service\\src\\main.py' }
        foreach ($p in $targets) {
            try { Stop-Process -Id $p.ProcessId -Force -ErrorAction Stop; $stopped = $true } catch {}
        }

        if (-not $stopped) {
            # Fallback: stop any python.exe running from this service venv
            $venvFragment = 'neo4j_ingestion_service\\venv\'
            $pyTargets = Get-Process python -ErrorAction SilentlyContinue |
                Where-Object { $_.Path -like "*${venvFragment}*" }
            foreach ($p in $pyTargets) {
                try { Stop-Process -Id $p.Id -Force -ErrorAction Stop; $stopped = $true } catch {}
            }
        }

        if (-not $stopped -and $Ports.Count -gt 0) {
            # Fallback: kill any process listening on the specified ports (IPv4 and IPv6)
            foreach ($port in $Ports) {
                try {
                    # IPv4 / any address family (first approach)
                    $conns = Get-NetTCPConnection -LocalPort $port -State Listen -ErrorAction SilentlyContinue
                    if ($conns) {
                        $pids = $conns | Select-Object -ExpandProperty OwningProcess -Unique
                        foreach ($procId in $pids) {
                            try { Stop-Process -Id $procId -Force -ErrorAction SilentlyContinue; $stopped = $true } catch {}
                        }
                    }
                } catch {}
                try {
                    # IPv6-specific listeners (second approach)
                    $conns6 = Get-NetTCPConnection -LocalPort $port -State Listen -AddressFamily IPv6 -ErrorAction SilentlyContinue
                    if ($conns6) {
                        $pids6 = $conns6 | Select-Object -ExpandProperty OwningProcess -Unique
                        foreach ($procId in $pids6) {
                            try { Stop-Process -Id $procId -Force -ErrorAction SilentlyContinue; $stopped = $true } catch {}
                        }
                    }
                } catch {}
            }
        }

        # No proxy override to stop/remove

        if ($stopped) { Write-Host 'Neo4j Ingestion Service stopped.' } else { Write-Host 'No running service process found.' }
        return
    }

    # Ensure Python
    $python = Get-Command python -ErrorAction SilentlyContinue
    if (-not $python) { throw 'Python is required on PATH' }

    # Prefer venv under the service root: neo4j_ingestion_service\venv
    $serviceVenvActivate = Join-Path $serviceDir 'venv\Scripts\Activate.ps1'
    if (Test-Path $serviceVenvActivate) {
        & $serviceVenvActivate
    } elseif (-not $env:VIRTUAL_ENV) {
        throw "No active virtualenv detected and '$serviceVenvActivate' not found. Please create/activate the venv in the service root."
    }

    # Export host→compose environment
    $env:API_HOST = '127.0.0.1'
    $env:API_PORT = '8011'

    $env:NEO4J_URI = 'bolt://localhost:7687'
    $env:NEO4J_USERNAME = 'neo4j'
    $env:NEO4J_PASSWORD = 'neo4j123'
    $env:NEO4J_DATABASE = 'neo4j'

    $env:REDIS_URL = 'redis://localhost:6379'
    $env:CELERY_BROKER_URL = 'redis://localhost:6379/0'
    $env:CELERY_RESULT_BACKEND = 'redis://localhost:6379/0'

    $env:PROJECT_MANAGEMENT_SERVICE_URL = 'http://localhost:8003'
    $env:HTTP_ENABLE_AZURE_AUTH = 'false'

    $env:AZURE_STORAGE_CONNECTION_STRING = 'DefaultEndpointsProtocol=http;AccountName=devstoreaccount1;AccountKey=Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw==;BlobEndpoint=http://localhost:10000/devstoreaccount1;'
    $env:AZURE_STORAGE_CONTAINER_NAME = 'documents'
    $env:AZURE_STORAGE_ACCOUNT_NAME = 'devstoreaccount1'
    $env:AZURE_STORAGE_ACCOUNT_KEY = 'Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw=='
    $env:AZURE_STORAGE_BLOB_ENDPOINT = 'http://localhost:10000/devstoreaccount1'
    $env:AZURE_STORAGE_MAX_SINGLE_PUT_SIZE = '67108864'
    $env:AZURE_STORAGE_MAX_BLOCK_SIZE = '4194304'

    $env:RAG_WORKSPACE_ROOT = './graphrag'
    $env:GRAPHRAG_INDEX_MODEL_TYPE = 'OPENAI'

    $env:OAI_BASE_URL = 'http://localhost:8010'
    $env:OAI_KEY = 'KEY'
    $env:OAI_MODEL = 'gpt-4.1'
    $env:OAI_EMBED_MODEL = 'text-embedding-3-large'

    # Ensure unique worker suffix for Celery hostname
    $env:WORKER_SUFFIX = [guid]::NewGuid().ToString()

    # Python path
    $env:PYTHONPATH = 'src'

    Write-Host "Starting Neo4j Ingestion Service on http://$($env:API_HOST):$($env:API_PORT) ..."
    # Start local python service in background so we can attach a proxy container in the compose network
    $proc = Start-Process -FilePath "python" -ArgumentList ".\src\main.py" -WorkingDirectory $serviceDir -PassThru

    # Proxy override and COMPOSE_FILE not used anymore
    Write-Host "Local service started (PID=$($proc.Id))."
}
finally {
    Pop-Location
}


