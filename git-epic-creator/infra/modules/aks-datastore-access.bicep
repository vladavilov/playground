@description('The name of the Key Vault to grant access to.')
param keyVaultName string

@description('The name of the App Configuration store to grant access to.')
param appConfigStoreName string

@description('The principal ID of the AKS managed identity.')
param aksPrincipalId string

@description('The name of the resource group where the resources exist.')
param resourceGroupName string

// --- Existing Resources ---
resource existingKeyVault 'Microsoft.KeyVault/vaults@2023-07-01' existing = {
  name: keyVaultName
  scope: resourceGroup(resourceGroupName)
}

resource existingAppConfig 'Microsoft.AppConfiguration/configurationStores@2023-03-01' existing = {
  name: appConfigStoreName
  scope: resourceGroup(resourceGroupName)
}

// --- Role Assignments ---

// Grant AKS access to App Configuration data
module aksToAppConfigRoleAssignment 'role-assignment.bicep' = {
  name: 'aks-to-appconfig-rbac'
  scope: existingAppConfig
  params: {
    principalId: aksPrincipalId
    principalType: 'ServicePrincipal'
    roleDefinitionId: subscriptionResourceId('Microsoft.Authorization/roleDefinitions', '516239f1-63e1-4d78-a4de-a74fb236a071') // App Configuration Data Reader
    roleAssignmentName: guid(existingAppConfig.id, aksPrincipalId, 'AppConfigDataReader')
  }
} 