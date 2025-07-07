@description('Specifies the name of the Cosmos DB account.')
param accountName string

@description('Specifies the location for the resources.')
param location string = resourceGroup().location

@description('Specifies the tags for the resources.')
param tags object = {}

@description('Specifies the name of the Cosmos DB database.')
param databaseName string = 'news-sentiment-db'

@description('Specifies the name of the Cosmos DB container.')
param containerName string = 'enriched-news-events'

@description('Specifies the partition key path for the container.')
param partitionKeyPath string = '/published_at'

@description('Specifies the throughput for the container (RU/s). Set to -1 for serverless.')
param throughput int = 400

@description('Specifies whether public network access is enabled. Should be Disabled for production.')
@allowed([
  'Enabled'
  'Disabled'
])
param publicNetworkAccess string = 'Enabled'

@description('Specifies the indexing policy for the container. Refine to only include queried paths for production.')
param indexingPolicy object = {
  indexingMode: 'consistent'
  includedPaths: [
    {
      path: '/*'
    }
  ]
  excludedPaths: [
    {
      path: '/"_etag"/?'
    }
    {
      path: '/raw_text_hash/?'
    }
    {
      path: '/sentiment/summary/?'
    }
  ]
}

resource cosmosDbAccount 'Microsoft.DocumentDB/databaseAccounts@2023-04-15' = {
  name: accountName
  location: location
  tags: tags
  kind: 'GlobalDocumentDB'
  properties: {
    databaseAccountOfferType: 'Standard'
    locations: [
      {
        locationName: location
        failoverPriority: 0
      }
    ]
    publicNetworkAccess: publicNetworkAccess
  }
}

resource cosmosDbSqlDatabase 'Microsoft.DocumentDB/databaseAccounts/sqlDatabases@2023-04-15' = {
  parent: cosmosDbAccount
  name: databaseName
  properties: {
    resource: {
      id: databaseName
    }
  }
}

resource cosmosDbSqlContainer 'Microsoft.DocumentDB/databaseAccounts/sqlDatabases/containers@2023-04-15' = {
  parent: cosmosDbSqlDatabase
  name: containerName
  properties: {
    resource: {
      id: containerName
      partitionKey: {
        paths: [
          partitionKeyPath
        ]
        kind: 'Hash'
      }
      indexingPolicy: indexingPolicy
    }
  }
  options: throughput != -1 ? {
    throughput: throughput
  } : null
}

@description('The name of the Cosmos DB account.')
output name string = cosmosDbAccount.name

@description('The endpoint of the Cosmos DB account.')
output endpoint string = cosmosDbAccount.properties.documentEndpoint

@description('The ID of the Cosmos DB account.')
output id string = cosmosDbAccount.id 