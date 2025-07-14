@description('The location for the App Configuration store.')
param location string

@description('The name of the App Configuration store.')
param appConfigStoreName string

@description('An array of key-value pairs to create in the App Configuration store.')
param keyValues array = []

@description('An array of Key Vault references to create in the App Configuration store.')
param keyVaultReferences array = []

@description('Tags to apply to the resources.')
param tags object = {}

@description('The SKU for the App Configuration store.')
@allowed([
  'Free'
  'Standard'
])
param sku string = 'Free' // Free tier is sufficient for dev/qa

@description('Determines if the App Configuration store is publicly accessible.')
param publicNetworkAccess string = 'Enabled'

resource appConfigStore 'Microsoft.AppConfiguration/configurationStores@2023-03-01' = {
  name: appConfigStoreName
  location: location
  tags: tags
  sku: {
    name: sku
  }
  // Enable System Assigned Managed Identity
  identity: {
    type: 'SystemAssigned'
  }
  properties: {
    // Enabling soft delete is a good practice
    enablePurgeProtection: false // Can be enabled for production
    publicNetworkAccess: publicNetworkAccess
  }
}

// Create standard key-value pairs
resource configKeyValue 'Microsoft.AppConfiguration/configurationStores/keyValues@2023-03-01' = [for item in keyValues: {
  parent: appConfigStore
  name: item.name
  properties: {
    value: item.value
  }
}]

// Create Key Vault references
resource configKeyVaultReference 'Microsoft.AppConfiguration/configurationStores/keyValues@2023-03-01' = [for item in keyVaultReferences: {
  parent: appConfigStore
  name: item.name
  properties: {
    contentType: 'application/vnd.microsoft.appconfig.keyvaultref+json;charset=utf-8'
    value: json(concat('{"uri":"', item.secretUri, '"}'))
  }
}]

@description('The ID of the App Configuration store.')
output id string = appConfigStore.id

@description('The principal ID of the App Configuration store\'s managed identity.')
output principalId string = appConfigStore.identity.principalId

@description('The endpoint of the App Configuration store.')
output endpoint string = appConfigStore.properties.endpoint 