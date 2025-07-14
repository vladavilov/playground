@description('The location for the PostgreSQL server.')
param location string

@description('The name of the PostgreSQL server.')
param postgresServerName string

@description('The administrator login name for the PostgreSQL server.')
param administratorLogin string

@description('The name of the Key Vault to store the admin password in.')
param keyVaultName string

@description('The High Availability mode for the server. Disabled for non-prod to save costs.')
@allowed([
  'Disabled'
  'ZoneRedundant'
])
param highAvailabilityMode string = 'Disabled'

@description('Specifies if geo-redundant backup is enabled. Disabled for non-prod to save costs.')
@allowed([
  'Enabled'
  'Disabled'
])
param backupGeoRedundant string = 'Disabled'

@description('The PostgreSQL version.')
param postgresVersion string = '15'

@description('The SKU for the PostgreSQL server.')
param sku object = {
  name: 'Standard_D4ds_v4'
  tier: 'GeneralPurpose'
}

@description('The storage size in GB for the PostgreSQL server.')
param storageSizeGB int = 128

@description('The ID of the subnet to delegate to the PostgreSQL server.')
param delegatedSubnetId string

@description('The ID of the virtual network to link the Private DNS Zone to.')
param virtualNetworkId string

@description('Tags to apply to the resources.')
param tags object = {}

var administratorLoginPassword = guid(resourceGroup().id, postgresServerName)
var passwordSecretName = '${postgresServerName}-admin-password'

resource postgresServer 'Microsoft.DBforPostgreSQL/flexibleServers@2023-12-01-preview' = {
  name: postgresServerName
  location: location
  tags: tags
  sku: sku
  properties: {
    version: postgresVersion
    administratorLogin: administratorLogin
    administratorLoginPassword: administratorLoginPassword
    network: {
      delegatedSubnetResourceId: delegatedSubnetId
      privateDnsZoneArmResourceId: privateDnsZone.id
    }
    highAvailability: {
      mode: highAvailabilityMode
    }
    backup: {
      backupRetentionDays: (highAvailabilityMode == 'ZoneRedundant') ? 35 : 7
      geoRedundantBackup: backupGeoRedundant
    }
    storage: {
      storageSizeGB: storageSizeGB
    }
  }
}

resource privateDnsZone 'Microsoft.Network/privateDnsZones@2020-06-01' = {
  name: '${postgresServerName}.private.postgres.database.azure.com'
  location: 'global'
}

resource privateDnsZoneLink 'Microsoft.Network/privateDnsZones/virtualNetworkLinks@2020-06-01' = {
  parent: privateDnsZone
  name: '${postgresServerName}-vnet-link'
  location: 'global'
  properties: {
    registrationEnabled: false
    virtualNetwork: {
      id: virtualNetworkId
    }
  }
}

resource passwordSecret 'Microsoft.KeyVault/vaults/secrets@2023-07-01' = {
  name: passwordSecretName
  parent: existingKeyVault
  properties: {
    value: administratorLoginPassword
    contentType: 'PostgreSQL Admin Password'
    attributes: {
      enabled: true
    }
  }
}

// Reference to the existing Key Vault created in main.bicep
resource existingKeyVault 'Microsoft.KeyVault/vaults@2023-07-01' existing = {
  name: keyVaultName
}

output serverName string = postgresServer.name
output serverFqdn string = postgresServer.properties.fullyQualifiedDomainName
output serverId string = postgresServer.id
output adminPasswordSecretName string = passwordSecret.name
output adminPasswordSecretUri string = passwordSecret.properties.secretUri 