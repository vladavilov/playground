using '../main.bicep'

param env = 'dev'
param principalId = ''
param serviceBusSku = 'Basic'
param cosmosDbThroughput = 400
param cosmosDbPublicNetworkAccess = 'Enabled'
