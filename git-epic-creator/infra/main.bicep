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

@description('The object ID of the Azure AD group to be assigned as AKS cluster administrators.')
@secure()
param adminGroupObjectId string

@description('Session secret key for UI service.')
@secure()
param sessionSecretKey string

@description('Local JWT secret for inter-service auth.')
@secure()
param localJwtSecret string

@description('Neo4j password.')
@secure()
param neo4jPassword string

@description('PostgreSQL password (for containerized PostgreSQL in AKS).')
@secure()
param postgresPassword string

@description('GitLab OAuth client secret (optional).')
@secure()
param gitlabOAuthClientSecret string = ''

@description('Corporate container registry login server (e.g., myregistry.azurecr.io).')
param containerRegistryLoginServer string

// --- Global Naming and Tagging ---
var rgName = '${projectName}-${environment}-rg'
var tags = {
  Project: projectName
  Environment: environment
}

// --- Network Configuration (Simplified) ---
var vnetName = '${projectName}-${environment}-vnet'
var nsgName = '${projectName}-${environment}-nsg'
var routeTableName = '${projectName}-${environment}-rt'
var vnetAddressPrefix = '10.0.0.0/16'

// --- Resource Names ---
var logAnalyticsWorkspaceName = '${projectName}-${environment}-logs'
var keyVaultName = '${projectName}-${environment}-kv'
var appConfigStoreName = '${projectName}-${environment}-appconfig'
var openAiAccountName = '${projectName}-${environment}-oai'
var storageAccountName = replace('${projectName}${environment}storage', '-', '')
var appInsightsName = '${projectName}-${environment}-ai'
var dashboardName = '${projectName}-${environment}-dashboard'
var actionGroupName = '${projectName}-${environment}-ag'
var redisName = '${projectName}-${environment}-redis'
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

// --- Module: Core Network (Simplified - removed gateway subnets) ---
module network 'modules/network.bicep' = {
  name: 'network-deployment'
  scope: resourceGroup(rgName)
  params: {
    location: location
    vnetName: vnetName
    vnetAddressPrefix: vnetAddressPrefix
    subnets: [
      { name: 'ApplicationSubnet', addressPrefix: '10.0.1.0/24' }
      { name: 'PrivateEndpointsSubnet', addressPrefix: '10.0.5.0/24' }
    ]
    nsgName: nsgName
    routeTableName: routeTableName
  }
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

// --- Module: Key Vault ---
module keyvault 'modules/keyvault.bicep' = {
  name: 'keyvault-deployment'
  scope: resourceGroup(rgName)
  params: {
    location: location
    keyVaultName: keyVaultName
    deploymentPrincipalId: deploymentPrincipalId
    publicNetworkAccess: 'Enabled' // Enabled for AKS access; private endpoint optional
    tags: tags
  }
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

// --- Module: Azure Cache for Redis ---
module redis 'modules/redis.bicep' = {
  name: 'redis-deployment'
  scope: resourceGroup(rgName)
  params: {
    redisName: redisName
    location: location
    skuName: (environment == 'prod') ? 'Standard' : 'Basic'
    skuFamily: 'C'
    skuCapacity: (environment == 'prod') ? 1 : 0
    keyVaultName: keyvault.outputs.name
    tags: tags
  }
  dependsOn: [keyvault]
}

// --- Module: Application Secrets (includes PostgreSQL password for containerized instance) ---
module secrets 'modules/secrets.bicep' = {
  name: 'secrets-deployment'
  scope: resourceGroup(rgName)
  params: {
    keyVaultName: keyvault.outputs.name
    sessionSecretKey: sessionSecretKey
    localJwtSecret: localJwtSecret
    neo4jPassword: neo4jPassword
    postgresPassword: postgresPassword
    gitlabOAuthClientSecret: gitlabOAuthClientSecret
  }
  dependsOn: [keyvault]
}

// --- Centralized App Configuration Values ---
// Note: PostgreSQL runs as container in AKS, host is K8s service name
var appConfigKeyValues = [
  { name: 'ApplicationInsights:ConnectionString', value: appInsights.outputs.connectionString }
  { name: 'Postgres:Host', value: 'postgresql-service' } // K8s service name
  { name: 'Postgres:Port', value: '5432' }
  { name: 'Postgres:User', value: 'postgres' }
  { name: 'Postgres:Database', value: 'requirementsdb' }
  { name: 'Redis:Host', value: redis.outputs.redisHostName }
  { name: 'Redis:Port', value: string(redis.outputs.redisSslPort) }
  { name: 'Redis:Ssl', value: 'true' }
  { name: 'OpenAI:Endpoint', value: openAi.outputs.openAiEndpoint }
  { name: 'Storage:BlobEndpoint', value: storage.outputs.primaryEndpoints.blob }
  { name: 'Storage:ContainerName', value: 'documents' }
  { name: 'ContainerRegistry:LoginServer', value: containerRegistryLoginServer }
  { name: 'Azure:TenantId', value: tenantId }
  { name: 'Azure:ClientId', value: aadClientId }
]

var appConfigKeyVaultReferences = [
  { name: 'Postgres:Password', secretUri: secrets.outputs.postgresPasswordSecretUri }
  { name: 'OpenAI:ApiKey', secretUri: openAi.outputs.openAiKeySecretUri }
  { name: 'Redis:Password', secretUri: redis.outputs.redisPasswordSecretUri }
  { name: 'Session:SecretKey', secretUri: secrets.outputs.sessionSecretKeyUri }
  { name: 'Jwt:LocalSecret', secretUri: secrets.outputs.localJwtSecretUri }
  { name: 'Neo4j:Password', secretUri: secrets.outputs.neo4jPasswordSecretUri }
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
    publicNetworkAccess: 'Enabled' // Enabled for AKS access
  }
  dependsOn: [openAi, appInsights, redis, secrets]
}

// --- Private Endpoints (Optional - Key Vault) ---
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
          groupIds: ['vault']
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

// --- Private Endpoints (Optional - App Configuration) ---
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
          groupIds: ['configurationStore']
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
module appConfigToKvRoleAssignment 'modules/role-assignment.bicep' = {
  name: 'appconfig-to-kv-rbac'
  scope: keyvault
  params: {
    principalId: appconfig.outputs.principalId
    principalType: 'ServicePrincipal'
    roleDefinitionId: subscriptionResourceId('Microsoft.Authorization/roleDefinitions', '4633458b-17de-408a-b874-0445c86b69e6')
    roleAssignmentName: guid(keyvault.outputs.name, appconfig.outputs.id, 'KeyVaultSecretsUser')
  }
  dependsOn: [appconfig]
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

// --- Workload Identity for AKS Services ---
module workloadIdentity 'modules/workload-identity.bicep' = {
  name: 'workload-identity-deployment'
  scope: resourceGroup(rgName)
  params: {
    aksClusterName: aks.outputs.clusterName
    resourceGroupName: rgName
    appConfigStoreName: appConfigStoreName
    keyVaultName: keyVaultName
    aadClientId: aadClientId
  }
  dependsOn: [aks, keyvault, appconfig]
}

// --- Outputs ---
@description('The name of the created resource group.')
output resourceGroupName string = rg.outputs.name

@description('The primary endpoints for the storage account.')
output storageEndpoints object = storage.outputs.primaryEndpoints

@description('The endpoint for the Azure OpenAI service.')
output openAiEndpoint string = openAi.outputs.openAiEndpoint

@description('The endpoint for the App Configuration store.')
output appConfigEndpoint string = appconfig.outputs.endpoint

@description('The connection string for Application Insights.')
output appInsightsConnectionString string = appInsights.outputs.connectionString

@description('The ID of the Key Vault resource.')
output keyVaultId string = keyvault.outputs.id

@description('The Key Vault name.')
output keyVaultName string = keyvault.outputs.name

@description('The Redis hostname.')
output redisHostName string = redis.outputs.redisHostName

@description('The AKS cluster name.')
output aksClusterName string = aks.outputs.clusterName

@description('The AKS OIDC issuer URL.')
output aksOidcIssuerUrl string = aks.outputs.oidcIssuerUrl

@description('The workload identity client ID.')
output workloadIdentityClientId string = workloadIdentity.outputs.workloadIdentityClientId
