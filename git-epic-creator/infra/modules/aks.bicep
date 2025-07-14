@description('The base name of the project.')
param projectName string

@description('The location for the AKS cluster.')
param location string

@description('The environment name (e.g., dev, qa, prod).')
param environment string

@description('The Kubernetes version for the AKS cluster.')
param kubernetesVersion string = '1.28.5'

@description('The VM size for the AKS node pool.')
param nodeVmSize string = 'Standard_DS2_v2'

@description('The minimum number of nodes for the node pool.')
param minNodeCount int = 1

@description('The maximum number of nodes for the node pool.')
param maxNodeCount int = 3

@description('The resource ID of the subnet to deploy the AKS cluster into.')
param vnetSubnetId string

@description('The resource ID of the Log Analytics Workspace for monitoring.')
param logAnalyticsWorkspaceId string

@description('The object ID of the Azure AD group for AKS cluster administrators.')
param adminGroupObjectId string

@description('The SKU tier for the AKS cluster.')
@allowed([
  'Free'
  'Paid'
])
param clusterSkuTier string = 'Free'

var clusterName = '${projectName}-${environment}-aks'
var nodePoolName = 'default'

resource aksCluster 'Microsoft.ContainerService/managedClusters@2024-05-01' = {
  name: clusterName
  location: location
  identity: {
    type: 'SystemAssigned'
  }
  sku: {
    name: 'Basic'
    tier: clusterSkuTier // For cost-saving in non-prod. Can be 'Paid' for production with uptime SLA.
  }
  properties: {
    kubernetesVersion: kubernetesVersion
    dnsPrefix: '${projectName}-${environment}-dns'
    agentPoolProfiles: [
      {
        name: nodePoolName
        count: 1 // Start with 1, will scale out
        vmSize: nodeVmSize
        osType: 'Linux'
        mode: 'System'
        enableAutoScaling: true
        minCount: minNodeCount
        maxCount: maxNodeCount
        vnetSubnetID: vnetSubnetId
      }
    ]
    networkProfile: {
      networkPlugin: 'azure'
      networkPolicy: 'azure' // enables network policies
    }
    // Enable Azure AD integration with RBAC
    aadProfile: {
      managed: true
      enableAzureRBAC: true
      adminGroupObjectIDs: [
        adminGroupObjectId
      ]
    }
    // Enable Monitoring with Azure Monitor for containers
    addonProfiles: {
      omsagent: {
        enabled: true
        config: {
          logAnalyticsWorkspaceResourceID: logAnalyticsWorkspaceId
        }
      }
    }
    // As per task: "Network policies for VPN-only access"
    // Making the cluster private makes the API server not publicly accessible.
    // It can be accessed from within the VNet, peered networks, or via the VPN.
    apiServerAccessProfile: {
      enablePrivateCluster: true
    }
  }
}

// Granting the AKS managed identity Network Contributor role on the subnet.
// This is necessary for the cluster to create internal load balancers and manage networking resources.
resource existingSubnet 'Microsoft.Network/virtualNetworks/subnets@2023-11-01' existing = {
  name: last(split(vnetSubnetId, '/'))
  parent: resourceId('Microsoft.Network/virtualNetworks', split(vnetSubnetId, '/')[8])
}

module subnetRoleAssignment 'role-assignment.bicep' = {
  name: 'aks-subnet-rbac'
  scope: existingSubnet
  params: {
    principalId: aksCluster.identity.principalId
    principalType: 'ServicePrincipal'
    roleDefinitionId: subscriptionResourceId('Microsoft.Authorization/roleDefinitions', '4d97b98b-1d4f-4787-a291-c67834d212e7') // Network Contributor
    roleAssignmentName: guid(aksCluster.id, vnetSubnetId, 'NetworkContributor')
  }
}

output clusterName string = aksCluster.name
output clusterPrincipalId string = aksCluster.identity.principalId 