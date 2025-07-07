using '../main.bicep'

param env = 'prod'
param principalId = ''
param serviceBusSku = 'Standard'
param cosmosDbThroughput = 1000
param cosmosDbPublicNetworkAccess = 'Disabled' 