param location string = resourceGroup().location
param env string = 'dev'
param principalId string = ''
param principalType string = 'ServicePrincipal'
param serviceBusSku string = 'Standard'
param cosmosDbThroughput int = 400
param cosmosDbPublicNetworkAccess string = 'Enabled'

var resourceToken = toLower('${uniqueString(resourceGroup().id)}-${env}')
var appConfigName = 'appconfig-${resourceToken}'
var keyVaultName = 'kv-${resourceToken}'
var serviceBusNamespaceName = 'sb-${resourceToken}'
var cosmosDbAccountName = 'cosmos-${resourceToken}'

var keyVaultSecretsUserRoleId = '4633458b-17de-408a-b874-0445c86b69e6'

var tags = {
  environment: env
  'managed-by': 'bicep'
}

module appConfig 'modules/appConfig.bicep' = {
  name: 'appConfigDeployment'
  params: {
    location: location
    name: appConfigName
    tags: tags
    keyVaultUri: keyVault.outputs.uri
  }
}

module keyVault 'modules/keyVault.bicep' = {
  name: 'keyVaultDeployment'
  params: {
    location: location
    name: keyVaultName
    tags: tags
    principalId: principalId
    principalType: principalType
  }
}

module appConfigRoleAssignment 'modules/roleAssignments.bicep' = {
  name: 'appConfigToKeyVaultRoleAssignment'
  params: {
    principalId: appConfig.outputs.identityPrincipalId
    roleDefinitionId: keyVaultSecretsUserRoleId
    scope: keyVault.outputs.id
  }
}

module serviceBus 'modules/serviceBus.bicep' = {
  name: 'serviceBusDeployment'
  params: {
    location: location
    name: serviceBusNamespaceName
    tags: tags
    skuName: serviceBusSku
  }
}

module cosmosDb 'modules/cosmosDb.bicep' = {
  name: 'cosmosDbDeployment'
  params: {
    location: location
    name: cosmosDbAccountName
    tags: tags
    throughput: cosmosDbThroughput
    publicNetworkAccess: cosmosDbPublicNetworkAccess
  }
}

output appConfigName string = appConfig.outputs.name
output appConfigEndpoint string = appConfig.outputs.endpoint
output keyVaultName string = keyVault.outputs.name
output keyVaultUri string = keyVault.outputs.uri
output keyVaultId string = keyVault.outputs.id
output serviceBusNamespaceName string = serviceBus.outputs.name
output cosmosDbAccountName string = cosmosDb.outputs.name
output cosmosDbId string = cosmosDb.outputs.id 