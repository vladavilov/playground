@description('The location for the Key Vault.')
param location string

@description('The name of the Key Vault.')
param keyVaultName string

@description('The object ID of the principal running the deployment to grant initial access.')
param deploymentPrincipalId string

@description('Tags to apply to the resources.')
param tags object = {}

@description('Determines if the Key Vault is publicly accessible.')
param publicNetworkAccess string = 'Enabled'

resource keyVault 'Microsoft.KeyVault/vaults@2023-07-01' = {
  name: keyVaultName
  location: location
  tags: tags
  properties: {
    sku: {
      family: 'A'
      name: 'standard'
    }
    tenantId: subscription().tenantId
    enableRbacAuthorization: true // Use RBAC for data plane access instead of access policies.
    publicNetworkAccess: publicNetworkAccess
  }
}

// Grant the deployment principal Key Vault Secrets Officer role to manage secrets.
module deploymentPrincipalAccess 'role-assignment.bicep' = {
  name: 'deployment-principal-rbac'
  scope: keyVault
  params: {
    principalId: deploymentPrincipalId
    principalType: 'User' // The deployment script uses 'az ad signed-in-user show', which is a user principal.
    roleDefinitionId: subscriptionResourceId('Microsoft.Authorization/roleDefinitions', 'b86a8fe4-44ce-4948-aee5-eccb2c12d543') // Key Vault Secrets Officer
    roleAssignmentName: guid(keyVault.id, deploymentPrincipalId, 'KeyVaultSecretsOfficer')
  }
}

output name string = keyVault.name
output id string = keyVault.id
output uri string = keyVault.properties.vaultUri 