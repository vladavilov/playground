@description('Specifies the principal ID of the identity to assign the role to.')
param principalId string

@description('Specifies the principal type of the identity.')
@allowed([
  'User'
  'Group'
  'ServicePrincipal'
  'ForeignGroup'
  'Device'
])
param principalType string = 'ServicePrincipal'

@description('The role definition ID to assign. See https://docs.microsoft.com/azure/role-based-access-control/built-in-roles for a list of built-in role IDs.')
param roleDefinitionId string

@description('The scope at which the role assignment is created. Can be a resource group, subscription, or a specific resource.')
param scope string = resourceGroup().id

resource roleAssignment 'Microsoft.Authorization/roleAssignments@2022-04-01' = {
  name: guid(principalId, roleDefinitionId, scope)
  scope: scope
  properties: {
    principalId: principalId
    principalType: principalType
    roleDefinitionId: tenantResourceId('Microsoft.Authorization/roleDefinitions', roleDefinitionId)
  }
} 