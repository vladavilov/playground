@description('Specifies the name of the App Configuration store.')
param name string

@description('The URI of the Key Vault to reference secrets from.')
param keyVaultUri string

@description('Specifies the location for the resources.')
param location string = resourceGroup().location

@description('Specifies the tags for the resources.')
param tags object = {}

@description('Specifies the SKU for the App Configuration store.')
param sku string = 'standard'

resource appConfig 'Microsoft.AppConfiguration/configurationStores@2023-03-01' = {
  name: name
  location: location
  tags: tags
  sku: {
    name: sku
  }
  identity: {
    type: 'SystemAssigned'
  }
}

resource apiToken 'Microsoft.AppConfiguration/configurationStores/keyValues@2023-03-01' = {
  parent: appConfig
  name: 'BenzingaAdapter:ApiToken'
  properties: {
    contentType: 'application/vnd.microsoft.appconfig.keyvaultref+json;charset=utf-8'
    value: json({
      uri: '${keyVaultUri}secrets/benzinga-api-token'
    })
  }
}

@description('The principal ID of the System-Assigned Managed Identity.')
output identityPrincipalId string = appConfig.identity.principalId

@description('The endpoint of the App Configuration store.')
output endpoint string = appConfig.properties.endpoint 