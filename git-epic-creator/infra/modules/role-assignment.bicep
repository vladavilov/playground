
@description('The principal ID to assign the role to.')
param principalId string

@description('The principal type of the principal ID.')
@allowed([
  'User'
  'Group'
  'ServicePrincipal'
  'ForeignGroup'
  'Device'
])
param principalType string

@description('The resource ID of the role definition to assign.')
param roleDefinitionId string

@description('The unique name for the role assignment, typically a GUID.')
param roleAssignmentName string = guid(resourceGroup().id, principalId, roleDefinitionId)

resource roleAssignment 'Microsoft.Authorization/roleAssignments@2024-04-01' = {
  name: roleAssignmentName
  properties: {
    roleDefinitionId: roleDefinitionId
    principalId: principalId
    principalType: principalType
  }
} 