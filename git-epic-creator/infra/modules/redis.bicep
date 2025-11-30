@description('The name of the Redis cache.')
param redisName string

@description('The location for Redis.')
param location string

@description('The SKU name.')
@allowed(['Basic', 'Standard', 'Premium'])
param skuName string = 'Basic'

@description('The SKU family.')
@allowed(['C', 'P'])
param skuFamily string = 'C'

@description('The SKU capacity (0-6 for C family, 1-5 for P family).')
param skuCapacity int = 0

@description('Key Vault name for storing connection string.')
param keyVaultName string

@description('Tags to apply.')
param tags object = {}

resource redis 'Microsoft.Cache/redis@2023-08-01' = {
  name: redisName
  location: location
  tags: tags
  properties: {
    sku: {
      name: skuName
      family: skuFamily
      capacity: skuCapacity
    }
    enableNonSslPort: false
    minimumTlsVersion: '1.2'
    publicNetworkAccess: 'Enabled'
    redisConfiguration: {
      'maxmemory-policy': 'allkeys-lru'
    }
  }
}

resource keyVault 'Microsoft.KeyVault/vaults@2023-07-01' existing = {
  name: keyVaultName
}

resource redisConnectionStringSecret 'Microsoft.KeyVault/vaults/secrets@2023-07-01' = {
  parent: keyVault
  name: 'Redis-ConnectionString'
  properties: {
    value: '${redis.properties.hostName}:${redis.properties.sslPort},password=${redis.listKeys().primaryKey},ssl=True,abortConnect=False'
  }
}

resource redisPasswordSecret 'Microsoft.KeyVault/vaults/secrets@2023-07-01' = {
  parent: keyVault
  name: 'Redis-Password'
  properties: {
    value: redis.listKeys().primaryKey
  }
}

output redisId string = redis.id
output redisHostName string = redis.properties.hostName
output redisSslPort int = redis.properties.sslPort
output redisConnectionStringSecretUri string = redisConnectionStringSecret.properties.secretUri
output redisPasswordSecretUri string = redisPasswordSecret.properties.secretUri


