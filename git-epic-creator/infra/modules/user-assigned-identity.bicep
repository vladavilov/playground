@description('Specifies the Azure location where the Managed Identity should be created.')
param location string

@description('Specifies the name of the Managed Identity.')
param name string

@description('Specifies the tags that should be assigned to the Managed Identity.')
param tags object = {}

resource managedIdentity 'Microsoft.ManagedIdentity/userAssignedIdentities@2023-07-31-preview' = {
  name: name
  location: location
  tags: tags
}

@description('The resource ID of the User Assigned Managed Identity.')
output id string = managedIdentity.id

@description('The principal ID of the User Assigned Managed Identity.')
output principalId string = managedIdentity.properties.principalId 