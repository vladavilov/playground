@description('Key Vault name for storing secrets.')
param keyVaultName string

@description('Session secret key for UI service.')
@secure()
param sessionSecretKey string

@description('Local JWT secret for inter-service auth.')
@secure()
param localJwtSecret string

@description('GitLab OAuth client secret.')
@secure()
param gitlabOAuthClientSecret string = ''

@description('Neo4j password.')
@secure()
param neo4jPassword string

@description('PostgreSQL password (for containerized PostgreSQL).')
@secure()
param postgresPassword string = ''

resource keyVault 'Microsoft.KeyVault/vaults@2023-07-01' existing = {
  name: keyVaultName
}

resource sessionSecretKeySecret 'Microsoft.KeyVault/vaults/secrets@2023-07-01' = {
  parent: keyVault
  name: 'Session-SecretKey'
  properties: {
    value: sessionSecretKey
  }
}

resource localJwtSecretSecret 'Microsoft.KeyVault/vaults/secrets@2023-07-01' = {
  parent: keyVault
  name: 'Local-JwtSecret'
  properties: {
    value: localJwtSecret
  }
}

resource gitlabOAuthSecret 'Microsoft.KeyVault/vaults/secrets@2023-07-01' = if (!empty(gitlabOAuthClientSecret)) {
  parent: keyVault
  name: 'GitLab-OAuthClientSecret'
  properties: {
    value: gitlabOAuthClientSecret
  }
}

resource neo4jPasswordSecret 'Microsoft.KeyVault/vaults/secrets@2023-07-01' = {
  parent: keyVault
  name: 'Neo4j-Password'
  properties: {
    value: neo4jPassword
  }
}

resource postgresPasswordSecret 'Microsoft.KeyVault/vaults/secrets@2023-07-01' = if (!empty(postgresPassword)) {
  parent: keyVault
  name: 'Postgres-Password'
  properties: {
    value: postgresPassword
  }
}

output sessionSecretKeyUri string = sessionSecretKeySecret.properties.secretUri
output localJwtSecretUri string = localJwtSecretSecret.properties.secretUri
output gitlabOAuthSecretUri string = !empty(gitlabOAuthClientSecret) ? gitlabOAuthSecret.properties.secretUri : ''
output neo4jPasswordSecretUri string = neo4jPasswordSecret.properties.secretUri
output postgresPasswordSecretUri string = !empty(postgresPassword) ? postgresPasswordSecret.properties.secretUri : ''


