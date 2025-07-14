<#
.SYNOPSIS
Deploys tenant-level resources, specifically the Azure AD application registration.

.DESCRIPTION
This script deploys the 'auth.bicep' template to the tenant scope.
It should be run once by a user with sufficient permissions (e.g., Global Administrator)
to create an AAD application registration. The outputs of this script are
required inputs for the environment-specific infrastructure deployments.

.EXAMPLE
./deploy-tenant.ps1

.NOTES
- Requires Azure CLI to be installed and authenticated (`az login`).
- The user running this script needs permissions to create AAD applications.
#>
param (
    [Parameter(Mandatory=$false)]
    [string]$Location = 'eastus2'
)

$templateFile = "./modules/auth.bicep"
$deploymentName = "agentic-ai-tenant-auth-$(Get-Date -Format 'yyyyMMdd-HHmm')"

Write-Host "Starting Azure AD application deployment at tenant scope..."
Write-Host "Template file: $templateFile"
Write-Host "Deployment name: $deploymentName"
Write-Host "Deployment metadata location: $Location"

# Execute the Bicep deployment at the tenant scope
az deployment tenant create --name $deploymentName --location $Location --template-file $templateFile --parameters "applicationName=AgenticAIRequirementsEngine"

if ($LASTEXITCODE -ne 0) {
    Write-Error "Tenant-level deployment failed. Please check the Azure deployment logs for details."
} else {
    Write-Host "✅ Tenant-level deployment completed successfully."
    Write-Host "Please use the outputs from this deployment (clientId, tenantId) as parameters for your environment deployments."
} 