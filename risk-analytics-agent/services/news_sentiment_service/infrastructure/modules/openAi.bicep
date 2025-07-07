@description('Specifies the name of the Azure OpenAI account.')
param name string

@description('Specifies the location for the resources.')
param location string = resourceGroup().location

@description('Specifies the tags for the resources.')
param tags object = {}

@description('Specifies the SKU for the Azure OpenAI account.')
param sku object = {
  name: 'S0'
}

resource openAiAccount 'Microsoft.CognitiveServices/accounts@2023-05-01' = {
  name: name
  location: location
  tags: tags
  sku: sku
  kind: 'OpenAI'
  properties: {
    customSubDomainName: toLower(name)
    publicNetworkAccess: 'Enabled'
  }
}

@description('The endpoint of the Azure OpenAI account.')
output endpoint string = openAiAccount.properties.endpoint

@description('The ID of the Azure OpenAI account.')
output id string = openAiAccount.id 