@description('Specifies the name of the Key Vault.')
param name string

@description('Specifies the location for the resources.')
param location string = resourceGroup().location

@description('Specifies the tags for the resources.')
param tags object = {}

@description('Specifies the Azure Active Directory tenant ID that should be used for authenticating requests to the key vault.')
param tenantId string = subscription().tenantId

@description('Specifies the SKU of the Key Vault.')
param sku object = {
  name: 'standard'
  family: 'A'
}

resource keyVault 'Microsoft.KeyVault/vaults@2023-07-01' = {
  name: name
  location: location
  tags: tags
  properties: {
    sku: sku
    tenantId: tenantId
    enableRbacAuthorization: true
    networkAcls: {
      bypass: 'AzureServices'
      defaultAction: 'Deny'
    }
  }
}

resource benzingaApiTokenSecret 'Microsoft.KeyVault/vaults/secrets@2023-07-01' = {
  name: 'benzinga-api-token'
  parent: keyVault
  properties: {
    value: 'PLEASE_UPDATE_THIS_VALUE_IN_AZURE_PORTAL'
  }
}

@description('The URI of the Key Vault.')
output uri string = keyVault.properties.vaultUri

@description('The name of the Key Vault.')
output name string = keyVault.name

@description('The ID of the Key Vault.')
output id string = keyVault.id 