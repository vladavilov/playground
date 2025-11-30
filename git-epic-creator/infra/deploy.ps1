<#
.SYNOPSIS
Deploys the Azure infrastructure for a specific environment using Bicep.

.DESCRIPTION
This script deploys the simplified Bicep templates. Provisions:
- AKS cluster with OIDC/Workload Identity
- Azure Cache for Redis
- Azure OpenAI
- Storage Account
- Key Vault + App Configuration
- Log Analytics + Application Insights

NOT provisioned (run in AKS as containers):
- PostgreSQL (StatefulSet in AKS)
- Neo4j (StatefulSet in AKS)
- Application services

.EXAMPLE
./deploy.ps1 -SubscriptionId "your-sub-id" -Environment "dev" -AadClientId "client-id" -TenantId "tenant-id" -AdminGroupObjectId "group-id" -ContainerRegistryLoginServer "myregistry.azurecr.io"

.NOTES
- Requires Azure CLI to be installed and authenticated (`az login`).
#>
param(
    [Parameter(Mandatory=$true)]
    [string]$SubscriptionId,
    
    [Parameter(Mandatory=$true)]
    [string]$Environment,
    
    [Parameter(Mandatory=$true)]
    [string]$AadClientId,
    
    [Parameter(Mandatory=$true)]
    [string]$TenantId,
    
    [Parameter(Mandatory=$true)]
    [string]$AdminGroupObjectId,
    
    [Parameter(Mandatory=$true)]
    [string]$ContainerRegistryLoginServer,
    
    [Parameter(Mandatory=$false)]
    [securestring]$SessionSecretKey,
    
    [Parameter(Mandatory=$false)]
    [securestring]$LocalJwtSecret,
    
    [Parameter(Mandatory=$false)]
    [securestring]$Neo4jPassword,
    
    [Parameter(Mandatory=$false)]
    [securestring]$PostgresPassword,
    
    [Parameter(Mandatory=$false)]
    [securestring]$GitlabOAuthClientSecret
)

# Generate default secrets if not provided
function ConvertTo-PlainText([securestring]$secure) {
    if ($null -eq $secure) { return "" }
    return [System.Runtime.InteropServices.Marshal]::PtrToStringAuto(
        [System.Runtime.InteropServices.Marshal]::SecureStringToBSTR($secure))
}

$sessionSecretPlain = if ($SessionSecretKey) { ConvertTo-PlainText $SessionSecretKey } else { [System.Guid]::NewGuid().ToString() + [System.Guid]::NewGuid().ToString() }
$localJwtPlain = if ($LocalJwtSecret) { ConvertTo-PlainText $LocalJwtSecret } else { [System.Guid]::NewGuid().ToString() }
$neo4jPlain = if ($Neo4jPassword) { ConvertTo-PlainText $Neo4jPassword } else { 'neo4j' + [System.Guid]::NewGuid().ToString().Substring(0, 8) }
$postgresPlain = if ($PostgresPassword) { ConvertTo-PlainText $PostgresPassword } else { 'postgres' + [System.Guid]::NewGuid().ToString().Substring(0, 8) }
$gitlabPlain = if ($GitlabOAuthClientSecret) { ConvertTo-PlainText $GitlabOAuthClientSecret } else { '' }

az account set --subscription $SubscriptionId

Write-Host "Fetching Object ID for the signed-in user..."
$deploymentPrincipalId = az ad signed-in-user show --query "id" --output tsv
if ($LASTEXITCODE -ne 0) {
    Write-Host "Trying service principal..."
    $deploymentPrincipalId = az ad sp show --id $env:AZURE_CLIENT_ID --query "id" --output tsv
    if ($LASTEXITCODE -ne 0) {
        Write-Error "Failed to get principal ID. Please ensure you are logged in."
        exit 1
    }
}
Write-Host "Principal ID: $deploymentPrincipalId"

$templateFile = "./main.bicep"
$deploymentName = "agentic-ai-${Environment}-$(Get-Date -Format 'yyyyMMdd-HHmm')"
$location = "eastus2"

Write-Host ""
Write-Host "=== Deployment Configuration ==="
Write-Host "Environment: $Environment"
Write-Host "Container Registry: $ContainerRegistryLoginServer"
Write-Host "Template: $templateFile"
Write-Host "Deployment: $deploymentName"
Write-Host ""

az deployment sub create `
    --name $deploymentName `
    --location $location `
    --template-file $templateFile `
    --parameters environment=$Environment `
    --parameters aadClientId=$AadClientId `
    --parameters tenantId=$TenantId `
    --parameters adminGroupObjectId=$AdminGroupObjectId `
    --parameters deploymentPrincipalId=$deploymentPrincipalId `
    --parameters containerRegistryLoginServer=$ContainerRegistryLoginServer `
    --parameters sessionSecretKey=$sessionSecretPlain `
    --parameters localJwtSecret=$localJwtPlain `
    --parameters neo4jPassword=$neo4jPlain `
    --parameters postgresPassword=$postgresPlain `
    --parameters gitlabOAuthClientSecret=$gitlabPlain

if ($LASTEXITCODE -ne 0) {
    Write-Error "Deployment failed. Please check the Azure deployment logs for details."
    exit 1
}

Write-Host ""
Write-Host "✅ Deployment completed successfully for environment '$Environment'."
Write-Host ""
Write-Host "=== Deployment Outputs ==="
az deployment sub show --name $deploymentName --query "properties.outputs" --output table

Write-Host ""
Write-Host "=== Next Steps ==="
Write-Host "1. Get AKS credentials:"
Write-Host "   az aks get-credentials --resource-group agentic-ai-${Environment}-rg --name agentic-ai-${Environment}-aks"
Write-Host ""
Write-Host "2. Install NGINX Ingress Controller:"
Write-Host "   helm repo add ingress-nginx https://kubernetes.github.io/ingress-nginx"
Write-Host "   helm install nginx-ingress ingress-nginx/ingress-nginx --namespace ingress-nginx --create-namespace"
Write-Host ""
Write-Host "3. Deploy application using GitLab CI/CD or:"
Write-Host "   cd k8s/overlays/${Environment}"
Write-Host "   kustomize build . | kubectl apply -f -"
