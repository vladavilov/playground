@description('The name of the AKS cluster.')
param aksClusterName string

@description('The resource group name where the AKS cluster is located.')
param resourceGroupName string

@description('The name of the App Configuration store.')
param appConfigStoreName string

@description('The name of the Key Vault.')
param keyVaultName string

@description('The Azure AD application client ID for workload identity.')
param aadClientId string

@description('The Kubernetes namespace where the workload identity will be used.')
param kubernetesNamespace string = 'agentic-ai'

@description('The Kubernetes service account name for workload identity.')
param kubernetesServiceAccount string = 'agentic-ai-workload-identity'

// Get references to existing resources
resource existingAksCluster 'Microsoft.ContainerService/managedClusters@2024-05-01' existing = {
  name: aksClusterName
  scope: resourceGroup(resourceGroupName)
}

resource existingAppConfig 'Microsoft.AppConfiguration/configurationStores@2023-03-01' existing = {
  name: appConfigStoreName
  scope: resourceGroup(resourceGroupName)
}

resource existingKeyVault 'Microsoft.KeyVault/vaults@2023-07-01' existing = {
  name: keyVaultName
  scope: resourceGroup(resourceGroupName)
}

// Create federated identity credential for the workload identity
resource federatedIdentityCredential 'Microsoft.ManagedIdentity/userAssignedIdentities/federatedIdentityCredentials@2023-07-31-preview' = {
  name: 'agentic-ai-federated-credential'
  parent: workloadIdentity
  properties: {
    issuer: existingAksCluster.properties.oidcIssuerProfile.issuerURL
    subject: 'system:serviceaccount:${kubernetesNamespace}:${kubernetesServiceAccount}'
    audiences: [
      'api://AzureADTokenExchange'
    ]
  }
}

// Create user-assigned managed identity for workload identity
resource workloadIdentity 'Microsoft.ManagedIdentity/userAssignedIdentities@2023-07-31-preview' = {
  name: 'agentic-ai-workload-identity'
  location: resourceGroup().location
  tags: {
    Purpose: 'Workload Identity for Agentic AI services'
    Environment: 'Multi-environment'
  }
}

// Grant App Configuration Data Reader role to the workload identity
module appConfigRoleAssignment 'role-assignment.bicep' = {
  name: 'workload-identity-appconfig-rbac'
  scope: existingAppConfig
  params: {
    principalId: workloadIdentity.properties.principalId
    principalType: 'ServicePrincipal'
    roleDefinitionId: subscriptionResourceId('Microsoft.Authorization/roleDefinitions', '516239f1-63e1-4d78-a4de-a74fb236a071') // App Configuration Data Reader
    roleAssignmentName: guid(existingAppConfig.id, workloadIdentity.id, 'AppConfigDataReader')
  }
}

// Grant Key Vault Secrets User role to the workload identity
module keyVaultRoleAssignment 'role-assignment.bicep' = {
  name: 'workload-identity-keyvault-rbac'
  scope: existingKeyVault
  params: {
    principalId: workloadIdentity.properties.principalId
    principalType: 'ServicePrincipal'
    roleDefinitionId: subscriptionResourceId('Microsoft.Authorization/roleDefinitions', '4633458b-17de-408a-b874-0445c86b69e6') // Key Vault Secrets User
    roleAssignmentName: guid(existingKeyVault.id, workloadIdentity.id, 'KeyVaultSecretsUser')
  }
}

@description('The client ID of the workload identity.')
output workloadIdentityClientId string = workloadIdentity.properties.clientId

@description('The principal ID of the workload identity.')
output workloadIdentityPrincipalId string = workloadIdentity.properties.principalId

@description('The resource ID of the workload identity.')
output workloadIdentityId string = workloadIdentity.id 