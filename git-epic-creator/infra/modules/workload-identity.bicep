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

resource existingAksCluster 'Microsoft.ContainerService/managedClusters@2024-05-01' existing = {
  name: aksClusterName
}

resource existingAppConfig 'Microsoft.AppConfiguration/configurationStores@2023-03-01' existing = {
  name: appConfigStoreName
}

resource existingKeyVault 'Microsoft.KeyVault/vaults@2023-07-01' existing = {
  name: keyVaultName
}

// User-assigned managed identity for workload identity
resource workloadIdentity 'Microsoft.ManagedIdentity/userAssignedIdentities@2023-07-31-preview' = {
  name: 'agentic-ai-workload-identity'
  location: resourceGroup().location
  tags: {
    Purpose: 'Workload Identity for Agentic AI services'
  }
}

// Federated identity credential linking K8s service account to Azure identity
resource federatedIdentityCredential 'Microsoft.ManagedIdentity/userAssignedIdentities/federatedIdentityCredentials@2023-07-31-preview' = {
  parent: workloadIdentity
  name: 'agentic-ai-federated-credential'
  properties: {
    issuer: existingAksCluster.properties.oidcIssuerProfile.issuerURL
    subject: 'system:serviceaccount:${kubernetesNamespace}:${kubernetesServiceAccount}'
    audiences: ['api://AzureADTokenExchange']
  }
}

// App Configuration Data Reader role
resource appConfigRoleAssignment 'Microsoft.Authorization/roleAssignments@2022-04-01' = {
  name: guid(existingAppConfig.id, workloadIdentity.id, 'AppConfigDataReader')
  scope: existingAppConfig
  properties: {
    principalId: workloadIdentity.properties.principalId
    principalType: 'ServicePrincipal'
    roleDefinitionId: subscriptionResourceId('Microsoft.Authorization/roleDefinitions', '516239f1-63e1-4d78-a4de-a74fb236a071')
  }
}

// Key Vault Secrets User role
resource keyVaultRoleAssignment 'Microsoft.Authorization/roleAssignments@2022-04-01' = {
  name: guid(existingKeyVault.id, workloadIdentity.id, 'KeyVaultSecretsUser')
  scope: existingKeyVault
  properties: {
    principalId: workloadIdentity.properties.principalId
    principalType: 'ServicePrincipal'
    roleDefinitionId: subscriptionResourceId('Microsoft.Authorization/roleDefinitions', '4633458b-17de-408a-b874-0445c86b69e6')
  }
}

output workloadIdentityClientId string = workloadIdentity.properties.clientId
output workloadIdentityPrincipalId string = workloadIdentity.properties.principalId
output workloadIdentityId string = workloadIdentity.id
