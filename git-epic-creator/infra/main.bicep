@description('The base name for the project, used to generate resource names.')
param projectName string = 'agentic-ai'

targetScope = 'subscription'

@description('The environment name (e.g., dev, qa, prod).')
param environment string

@description('The Azure region where the resources will be deployed.')
param location string = 'eastus2'

@description('The Client ID of the AAD application registration to be used for authentication.')
@secure()
param aadClientId string

@description('The Tenant ID where the AAD application is registered.')
@secure()
param tenantId string

@description('The object ID of the principal running the deployment, to be granted Key Vault access.')
@secure()
param deploymentPrincipalId string

@description('The secret URI for the Application Gateway SSL certificate stored in Key Vault.')
@secure()
param appGwSslCertSecretId string

@description('The object ID of the Azure AD group to be assigned as AKS cluster administrators.')
@secure()
param adminGroupObjectId string

@description('The administrator login name for the PostgreSQL server.')
param postgresAdminUser string = 'psqladmin'

@description('The email address of the API publisher.')
param publisherEmail string

@description('An array of backend IP addresses for the Application Gateway.')
param appGatewayBackendAddresses array = []

// --- Global Naming and Tagging ---
var rgName = '${projectName}-${environment}-rg'
var tags = {
  Project: projectName
  Environment: environment
}

// --- Network Configuration ---
var vnetName = '${projectName}-${environment}-vnet'
var nsgName = '${projectName}-${environment}-nsg'
var routeTableName = '${projectName}-${environment}-rt'
var vnetAddressPrefix = '10.0.0.0/16'

// --- Resource Names ---
var logAnalyticsWorkspaceName = '${projectName}-${environment}-logs'
var keyVaultName = '${projectName}-${environment}-kv'
var appConfigStoreName = '${projectName}-${environment}-appconfig'
var postgresServerName = '${projectName}-${environment}-psql'
var openAiAccountName = '${projectName}-${environment}-oai'
var storageAccountName = '${projectName}-${environment}-storage'
var appInsightsName = '${projectName}-${environment}-ai'
var dashboardName = '${projectName}-${environment}-dashboard'
var actionGroupName = '${projectName}-${environment}-ag'
var appGwIdentityName = '${projectName}-${environment}-appgw-id'
var apiManagementName = '${projectName}-${environment}-apim'
var kvPrivateEndpointName = '${keyVaultName}-pe'
var appConfigPrivateEndpointName = '${appConfigStoreName}-pe'
var kvPrivateDnsZoneName = 'privatelink.vaultcore.azure.net'
var appConfigPrivateDnsZoneName = 'privatelink.azconfig.io'


// --- Module: Resource Group ---
module rg 'modules/resourceGroup.bicep' = {
  name: 'rg-deployment'
  params: {
    name: rgName
    location: location
  }
}

// --- Module: Core Network ---
module network 'modules/network.bicep' = {
  name: 'network-deployment'
  scope: resourceGroup(rgName)
  params: {
    location: location
    vnetName: vnetName
    vnetAddressPrefix: vnetAddressPrefix
    subnets: [
      { name: 'ApplicationSubnet', addressPrefix: '10.0.1.0/24' }
      { name: 'GatewaySubnet', addressPrefix: '10.0.2.0/24' }
      { name: 'AppGatewaySubnet', addressPrefix: '10.0.3.0/24' }
      { name: 'DatabaseSubnet', addressPrefix: '10.0.4.0/24' }
      { name: 'PrivateEndpointsSubnet', addressPrefix: '10.0.5.0/24' }
    ]
    nsgName: nsgName
    routeTableName: routeTableName
  }
}

// --- Module: User Assigned Identity for App Gateway ---
module appGwIdentity 'modules/user-assigned-identity.bicep' = {
  name: 'appgw-identity-deployment'
  scope: resourceGroup(rgName)
  params: {
    name: appGwIdentityName
    location: location
    tags: tags
  }
}

// --- Module: Gateways ---
module gateways 'modules/gateways.bicep' = {
  name: 'gateways-deployment'
  scope: resourceGroup(rgName)
  params: {
    location: location
    projectName: projectName
    vnetName: network.outputs.vnetName
    appGwSubnetId: network.outputs.appGwSubnetId
    gwSubnetId: network.outputs.gwSubnetId
    backendAddresses: appGatewayBackendAddresses
    appGwIdentityId: appGwIdentity.outputs.id
    appGwSslCertSecretId: appGwSslCertSecretId
  }
  dependsOn: [appGwIdentity]
}

// --- Module: Log Analytics ---
module logAnalytics 'modules/loganalytics.bicep' = {
  name: 'logAnalytics-deployment'
  scope: resourceGroup(rgName)
  params: {
    location: location
    workspaceName: logAnalyticsWorkspaceName
  }
}

// --- Module: Application Insights ---
module appInsights 'modules/appinsights.bicep' = {
  name: 'appInsights-deployment'
  scope: resourceGroup(rgName)
  params: {
    location: location
    appInsightsName: appInsightsName
    logAnalyticsWorkspaceId: logAnalytics.outputs.id
    tags: tags
  }
  dependsOn: [logAnalytics]
}

// --- Module: Monitoring Alerts ---
module alerts 'modules/monitoring_alerts.bicep' = {
  name: 'monitoring-alerts-deployment'
  scope: resourceGroup(rgName)
  params: {
    location: location
    targetResourceId: appInsights.outputs.id
    actionGroupName: actionGroupName
    alertName: 'High-Server-Errors'
    alertDescription: 'Alert when the server responds with an excessive number of HTTP 5xx errors.'
    severity: 1
    metricName: 'requests/failed'
    timeAggregation: 'Count'
    operator: 'GreaterThan'
    threshold: 5
    tags: tags
  }
  dependsOn: [appInsights]
}

// --- Module: Dashboard ---
module dashboard 'modules/dashboard.bicep' = {
  name: 'dashboard-deployment'
  scope: resourceGroup(rgName)
  params: {
    location: location
    dashboardName: dashboardName
    appInsightsName: appInsightsName
    tags: tags
  }
  dependsOn: [appInsights]
}

// --- Module: Key Vault (Centralized) ---
module keyvault 'modules/keyvault.bicep' = {
  name: 'keyvault-deployment'
  scope: resourceGroup(rgName)
  params: {
    location: location
    keyVaultName: keyVaultName
    deploymentPrincipalId: deploymentPrincipalId
    publicNetworkAccess: 'Disabled'
    tags: tags
  }
}

// --- Module: PostgreSQL ---
module postgres 'modules/postgres.bicep' = {
  name: 'postgres-deployment'
  scope: resourceGroup(rgName)
  params: {
    location: location
    postgresServerName: postgresServerName
    administratorLogin: postgresAdminUser
    delegatedSubnetId: network.outputs.dbSubnetId
    virtualNetworkId: network.outputs.vnetId
    keyVaultName: keyvault.outputs.name
    highAvailabilityMode: (environment == 'prod') ? 'ZoneRedundant' : 'Disabled'
    backupGeoRedundant: (environment == 'prod') ? 'Enabled' : 'Disabled'
    sku: {
      name: (environment == 'prod') ? 'Standard_D4ds_v4' : 'Standard_B2s'
      tier: 'GeneralPurpose'
    }
    tags: tags
  }
  dependsOn: [network, keyvault]
}

// --- Module: Azure OpenAI ---
module openAi 'modules/openai.bicep' = {
  name: 'openai-deployment'
  scope: resourceGroup(rgName)
  params: {
    location: location
    openAiAccountName: openAiAccountName
    keyVaultName: keyvault.outputs.name
    logAnalyticsWorkspaceId: logAnalytics.outputs.id
  }
  dependsOn: [logAnalytics, keyvault]
}

// --- Centralized App Configuration Values ---
// These are defined after their dependent resources are created to ensure outputs are available.
var appConfigKeyValues = [
  {
    name: 'ApiManagement:Jwt:IssuerUrl'
    value: 'https://sts.windows.net/${tenantId}/'
  }
  {
    name: 'ApiManagement:Jwt:Audience'
    value: aadClientId
  }
  {
    name: 'ApplicationInsights:ConnectionString'
    value: appInsights.outputs.connectionString
  }
]

var appConfigKeyVaultReferences = [
  {
    name: 'Postgres:Password'
    secretUri: postgres.outputs.adminPasswordSecretUri
  }
  {
    name: 'OpenAI:ApiKey'
    secretUri: openAi.outputs.openAiKeySecretUri
  }
]

// --- Module: App Configuration ---
module appconfig 'modules/appconfiguration.bicep' = {
  name: 'appconfig-deployment'
  scope: resourceGroup(rgName)
  params: {
    location: location
    appConfigStoreName: appConfigStoreName
    tags: tags
    sku: (environment == 'prod') ? 'Standard' : 'Free'
    keyValues: appConfigKeyValues
    keyVaultReferences: appConfigKeyVaultReferences
    publicNetworkAccess: 'Disabled'
  }
  dependsOn: [
    // Ensure secrets and dependent resources are created before references are made
    postgres,
    openAi,
    appInsights
  ]
}

// --- Private Endpoints and DNS ---

// Key Vault Private DNS Zone
resource kvPrivateDnsZone 'Microsoft.Network/privateDnsZones@2020-06-01' = {
  name: kvPrivateDnsZoneName
  location: 'global'
  scope: resourceGroup(rgName)
}

resource kvDnsVnetLink 'Microsoft.Network/privateDnsZones/virtualNetworkLinks@2020-06-01' = {
  parent: kvPrivateDnsZone
  name: '${kvPrivateDnsZoneName}-link'
  location: 'global'
  properties: {
    registrationEnabled: false
    virtualNetwork: {
      id: network.outputs.vnetId
    }
  }
}

// Key Vault Private Endpoint
resource kvPrivateEndpoint 'Microsoft.Network/privateEndpoints@2023-11-01' = {
  name: kvPrivateEndpointName
  location: location
  scope: resourceGroup(rgName)
  properties: {
    subnet: {
      id: network.outputs.pepSubnetId
    }
    privateLinkServiceConnections: [
      {
        name: kvPrivateEndpointName
        properties: {
          privateLinkServiceId: keyvault.outputs.id
          groupIds: [ 'vault' ]
        }
      }
    ]
  }
  dependsOn: [keyvault]
}

resource kvPrivateDnsZoneGroup 'Microsoft.Network/privateEndpoints/privateDnsZoneGroups@2023-11-01' = {
  name: 'default'
  parent: kvPrivateEndpoint
  properties: {
    privateDnsZoneConfigs: [
      {
        name: kvPrivateDnsZoneName
        properties: {
          privateDnsZoneId: kvPrivateDnsZone.id
        }
      }
    ]
  }
}

// App Configuration Private DNS Zone
resource appConfigPrivateDnsZone 'Microsoft.Network/privateDnsZones@2020-06-01' = {
  name: appConfigPrivateDnsZoneName
  location: 'global'
  scope: resourceGroup(rgName)
}

resource appConfigDnsVnetLink 'Microsoft.Network/privateDnsZones/virtualNetworkLinks@2020-06-01' = {
  parent: appConfigPrivateDnsZone
  name: '${appConfigPrivateDnsZoneName}-link'
  location: 'global'
  properties: {
    registrationEnabled: false
    virtualNetwork: {
      id: network.outputs.vnetId
    }
  }
}

// App Configuration Private Endpoint
resource appConfigPrivateEndpoint 'Microsoft.Network/privateEndpoints@2023-11-01' = {
  name: appConfigPrivateEndpointName
  location: location
  scope: resourceGroup(rgName)
  properties: {
    subnet: {
      id: network.outputs.pepSubnetId
    }
    privateLinkServiceConnections: [
      {
        name: appConfigPrivateEndpointName
        properties: {
          privateLinkServiceId: appconfig.outputs.id
          groupIds: [ 'configurationStore' ]
        }
      }
    ]
  }
  dependsOn: [appconfig]
}

resource appConfigPrivateDnsZoneGroup 'Microsoft.Network/privateEndpoints/privateDnsZoneGroups@2023-11-01' = {
  name: 'default'
  parent: appConfigPrivateEndpoint
  properties: {
    privateDnsZoneConfigs: [
      {
        name: appConfigPrivateDnsZoneName
        properties: {
          privateDnsZoneId: appConfigPrivateDnsZone.id
        }
      }
    ]
  }
}


// --- Data Plane Role Assignments ---

// Grant App Configuration's Managed Identity access to Key Vault
module appConfigToKvRoleAssignment 'modules/role-assignment.bicep' = {
  name: 'appconfig-to-kv-rbac'
  scope: keyvault
  params: {
    principalId: appconfig.outputs.principalId
    principalType: 'ServicePrincipal'
    roleDefinitionId: subscriptionResourceId('Microsoft.Authorization/roleDefinitions', '4633458b-17de-408a-b874-0445c86b69e6') // Key Vault Secrets User
    roleAssignmentName: guid(keyvault.outputs.name, appconfig.outputs.id, 'KeyVaultSecretsUser')
  }
  dependsOn: [appconfig] // Depends on appconfig to get the principalId
}

// Grant App Gateway Identity access to Key Vault for SSL Cert
module appGwToKvRoleAssignment 'modules/role-assignment.bicep' = {
  name: 'appgw-to-kv-rbac'
  scope: keyvault
  params: {
    principalId: appGwIdentity.outputs.principalId
    principalType: 'ServicePrincipal'
    roleDefinitionId: subscriptionResourceId('Microsoft.Authorization/roleDefinitions', '4633458b-17de-408a-b874-0445c86b69e6') // Key Vault Secrets User
    roleAssignmentName: guid(keyvault.outputs.name, appGwIdentity.outputs.id, 'KeyVaultSecretsUser')
  }
  dependsOn: [appGwIdentity]
}

// --- Module: AKS Cluster ---
module aks 'modules/aks.bicep' = {
  name: 'aks-deployment'
  scope: resourceGroup(rgName)
  params: {
    projectName: projectName
    location: location
    environment: environment
    vnetSubnetId: network.outputs.appSubnetId
    logAnalyticsWorkspaceId: logAnalytics.outputs.id
    adminGroupObjectId: adminGroupObjectId
    minNodeCount: (environment == 'prod') ? 2 : 1
    maxNodeCount: (environment == 'prod') ? 5 : 3
  }
  dependsOn: [network, logAnalytics]
}

// --- Module: Storage Account ---
module storage 'modules/storage.bicep' = {
  name: 'storage-deployment'
  scope: resourceGroup(rgName)
  params: {
    location: location
    storageAccountName: storageAccountName
    tags: tags
  }
}

// --- Module: API Management ---
module apiManagement 'modules/api_management.bicep' = {
  name: 'apim-deployment'
  scope: resourceGroup(rgName)
  params: {
    apiManagementName: apiManagementName
    location: location
    publisherEmail: publisherEmail
    allowedOrigins: 'https://portal.azure.com' // Example: Lock down to the Azure portal, should be frontend URL
    sku: {
      name: (environment == 'prod') ? 'Standard_v2' : 'Developer'
      capacity: (environment == 'prod') ? 2 : 1
    }
    appInsightsInstrumentationKey: appInsights.outputs.instrumentationKey
    jwtIssuerUrl: 'https://sts.windows.net/${tenantId}/'
    jwtAudience: aadClientId
  }
  dependsOn: [
    appInsights
  ]
}

// --- Granting Data Plane Access for AKS ---
module aksDataPlaneAccess 'modules/aks-datastore-access.bicep' = {
  name: 'aks-datastore-access-deployment'
  scope: resourceGroup(rgName)
  params: {
    keyVaultName: keyVaultName
    appConfigStoreName: appConfigStoreName
    aksPrincipalId: aks.outputs.clusterPrincipalId
    resourceGroupName: rgName
  }
  dependsOn: [keyvault, appconfig, aks]
}


// --- Outputs ---
@description('The name of the created resource group.')
output resourceGroupName string = rg.outputs.name

@description('The FQDN of the PostgreSQL server.')
output postgresFqdn string = postgres.outputs.serverFqdn

@description('The Gateway URL of the API Management service.')
output apimGatewayUrl string = apiManagement.outputs.gatewayUrl

@description('The secret URI for the PostgreSQL admin password.')
output postgresPasswordSecretUri string = postgres.outputs.adminPasswordSecretUri

@description('The primary endpoints for the storage account.')
output storageEndpoints object = storage.outputs.primaryEndpoints

@description('The endpoint for the Azure OpenAI service.')
output openAiEndpoint string = openAi.outputs.openAiEndpoint

@description('The secret URI for the Azure OpenAI API key.')
output openAiKeySecretUri string = openAi.outputs.openAiKeySecretUri

@description('The endpoint for the App Configuration store.')
output appConfigEndpoint string = appconfig.outputs.endpoint

@description('The connection string for Application Insights.')
output appInsightsConnectionString string = appInsights.outputs.connectionString 

@description('The ID of the Key Vault resource.')
output keyVaultId string = keyvault.outputs.id 