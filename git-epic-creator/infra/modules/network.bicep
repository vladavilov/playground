@description('The location for the network resources.')
param location string

@description('The name of the virtual network.')
param vnetName string

@description('The address prefix for the virtual network.')
param vnetAddressPrefix string

@description('An array of subnet configurations.')
param subnets array

@description('The name of the Network Security Group.')
param nsgName string

@description('The name of the Route Table.')
param routeTableName string

resource nsg 'Microsoft.Network/networkSecurityGroups@2023-11-01' = {
  name: nsgName
  location: location
  properties: {
    securityRules: []
  }
}

resource routeTable 'Microsoft.Network/routeTables@2023-11-01' = {
  name: routeTableName
  location: location
  properties: {
    routes: []
  }
}

resource vnet 'Microsoft.Network/virtualNetworks@2023-11-01' = {
  name: vnetName
  location: location
  properties: {
    addressSpace: {
      addressPrefixes: [
        vnetAddressPrefix
      ]
    }
    subnets: [for subnet in subnets: {
      name: subnet.name
      properties: {
        addressPrefix: subnet.addressPrefix
        // Associate NSG and Route Table only to the application subnet.
        networkSecurityGroup: subnet.name == 'ApplicationSubnet' ? {
          id: nsg.id
        } : null
        routeTable: subnet.name == 'ApplicationSubnet' ? {
          id: routeTable.id
        } : null
        // Add delegation for the database subnet
        delegations: subnet.name == 'DatabaseSubnet' ? [
          {
            name: 'postgresql-delegation'
            properties: {
              serviceName: 'Microsoft.DBforPostgreSQL/flexibleServers'
            }
          }
        ] : []
        // Private Endpoints require this policy to be disabled on the subnet.
        privateEndpointNetworkPolicies: subnet.name == 'PrivateEndpointsSubnet' ? 'Disabled' : 'Enabled'
      }
    }]
  }
}

func getSubnetId(vnetName string, subnetName string) string {
  return resourceId('Microsoft.Network/virtualNetworks/subnets', vnetName, subnetName)
}

output vnetName string = vnet.name
output appSubnetId string = getSubnetId(vnet.name, 'ApplicationSubnet')
output appGwSubnetId string = getSubnetId(vnet.name, 'AppGatewaySubnet')
output gwSubnetId string = getSubnetId(vnet.name, 'GatewaySubnet')
output dbSubnetId string = getSubnetId(vnet.name, 'DatabaseSubnet')
output pepSubnetId string = getSubnetId(vnet.name, 'PrivateEndpointsSubnet')
output vnetId string = vnet.id