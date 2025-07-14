<#
.SYNOPSIS
Deploys the Azure infrastructure for a specific environment using Bicep.

.DESCRIPTION
This script deploys the Bicep templates defined in the 'infra' directory.
It creates a dedicated resource group and all necessary resources for a single environment (e.g., dev, qa, prod).

.EXAMPLE
./deploy.ps1 -SubscriptionId "your-sub-id" -Environment "dev" -AdminGroupObjectId "your-group-id" -PostgresCredential (Get-Credential)

.EXAMPLE
./deploy.ps1 -SubscriptionId "your-sub-id" -Environment "prod" -AdminGroupObjectId "your-group-id" -PostgresCredential (Get-Credential) -AppGatewayBackendIpsJson '[{"ipAddress":"10.0.1.10"},{"ipAddress":"10.0.1.11"}]'

.NOTES
- Requires Azure CLI to be installed and authenticated (`az login`).
- Set your active subscription using `az account set --subscription <Your-Subscription-ID>` before running.
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
    [string]$AppGwSslCertSecretId,
    [Parameter(Mandatory=$true)]
    [System.Management.Automation.PSCredential]$PostgresCredential,
    [Parameter(Mandatory=$true)]
    [string]$PublisherEmail,
    [Parameter(Mandatory=$false)]
    [string]$AppGatewayBackendIpsJson = '[]'
)

$PostgresAdminUser = $PostgresCredential.UserName

# Set the context to the correct subscription
az account set --subscription $SubscriptionId

# Get the Object ID of the currently signed-in user or service principal for Key Vault access
Write-Host "Fetching Object ID for the signed-in user..."
$deploymentPrincipalId = az ad signed-in-user show --query "id" --output tsv
if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to get signed-in user's object ID. Please ensure you are logged in with 'az login'."
    exit 1
}
Write-Host "Signed-in user Principal ID: $deploymentPrincipalId"


$templateFile = "./main.bicep"
$deploymentName = "agentic-ai-${Environment}-$(Get-Date -Format 'yyyyMMdd-HHmm')"
$location = "eastus2" # The location for the deployment metadata

Write-Host "Starting Azure infrastructure deployment for environment '$Environment'..."
Write-Host "Template file: $templateFile"
Write-Host "Deployment name: $deploymentName"

# Execute the Bicep deployment at the subscription scope
az deployment sub create --name $deploymentName --location $location --template-file $templateFile --parameters "environment=$Environment" "aadClientId=$AadClientId" "tenantId=$TenantId" "adminGroupObjectId=$AdminGroupObjectId" "appGwSslCertSecretId=$AppGwSslCertSecretId" "deploymentPrincipalId=$deploymentPrincipalId" "postgresAdminUser=$PostgresAdminUser" "publisherEmail=$PublisherEmail" "appGatewayBackendAddresses=$AppGatewayBackendIpsJson"

if ($LASTEXITCODE -ne 0) {
    Write-Error "Deployment failed. Please check the Azure deployment logs for details."
} else {
    Write-Host "✅ Deployment completed successfully for environment '$Environment'."
} 