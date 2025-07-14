@description('The location for the gateway resources.')
param location string

@description('The base name of the project.')
param projectName string

@description('The name of the virtual network to connect the gateways to.')
param vnetName string

@description('The resource ID of the Application Gateway subnet.')
param appGwSubnetId string

@description('The resource ID of the VPN Gateway subnet.')
param gwSubnetId string

@description('The resource ID of the User Assigned Managed Identity for the Application Gateway.')
param appGwIdentityId string

@description('The secret ID of the SSL certificate stored in Key Vault. The App Gateway identity must have access to it.')
param appGwSslCertSecretId string

@description('The SKU for the VPN Gateway.')
param vpnGatewaySku string = 'VpnGw1'

@description('An array of objects containing the IP addresses for the backend pool. Example: [{ipAddress: "10.0.1.10"}]')
param backendAddresses array = []

// --- VPN Gateway Resources ---
resource vpnGwPip 'Microsoft.Network/publicIPAddresses@2023-11-01' = {
  name: '${projectName}-vpn-gw-pip'
  location: location
  sku: {
    name: 'Standard'
  }
  properties: {
    publicIPAllocationMethod: 'Static'
  }
}

resource vpnGateway 'Microsoft.Network/virtualNetworkGateways@2023-11-01' = {
  name: '${projectName}-vpn-gw'
  location: location
  properties: {
    ipConfigurations: [
      {
        name: 'default'
        properties: {
          publicIPAddress: {
            id: vpnGwPip.id
          }
          subnet: {
            id: gwSubnetId
          }
        }
      }
    ]
    gatewayType: 'Vpn'
    vpnType: 'RouteBased'
    sku: {
      name: vpnGatewaySku
      tier: vpnGatewaySku
    }
  }
}

// --- Application Gateway Resources ---
// A Public IP is no longer needed as the App Gateway will be internal.
/* resource appGwPip 'Microsoft.Network/publicIPAddresses@2023-05-01' = {
  name: '${projectName}-appgw-pip'
  location: location
  sku: {
    name: 'Standard'
  }
  properties: {
    publicIPAllocationMethod: 'Static'
  }
} */

resource wafPolicy 'Microsoft.Network/ApplicationGatewayWebApplicationFirewallPolicies@2023-11-01' = {
  name: '${projectName}-waf-policy'
  location: location
  properties: {
    managedRules: {
      managedRuleSets: [
        {
          ruleSetType: 'OWASP'
          ruleSetVersion: '3.2'
        }
      ]
    }
    policySettings: {
      state: 'Enabled'
      mode: 'Prevention'
    }
  }
}

resource appGateway 'Microsoft.Network/applicationGateways@2023-11-01' = {
  name: '${projectName}-appgw'
  location: location
  identity: {
    type: 'UserAssigned'
    userAssignedIdentities: {
      '${appGwIdentityId}': {}
    }
  }
  properties: {
    sku: {
      name: 'WAF_v2'
      tier: 'WAF_v2'
    }
    gatewayIPConfigurations: [
      {
        name: 'appGatewayIpConfig'
        properties: {
          subnet: {
            id: appGwSubnetId
          }
        }
      }
    ]
    frontendIPConfigurations: [
      {
        name: 'appGwPrivateFrontendIp'
        properties: {
          privateIPAllocationMethod: 'Dynamic'
          subnet: {
            id: appGwSubnetId
          }
        }
      }
    ]
    frontendPorts: [
      {
        name: 'port_80'
        properties: {
          port: 80
        }
      }
      {
        name: 'port_443'
        properties: {
          port: 443
        }
      }
    ]
    backendAddressPools: [
      {
        name: 'defaultBackendPool'
        properties: {
          backendAddresses: backendAddresses
        }
      }
    ]
    backendHttpSettingsCollection: [
      {
        name: 'defaultHttpSettings'
        properties: {
          port: 80
          protocol: 'Http'
          cookieBasedAffinity: 'Disabled'
          requestTimeout: 20
        }
      }
    ]
    sslCertificates: [
      {
        name: 'appgw-ssl-cert'
        properties: {
          keyVaultSecretId: appGwSslCertSecretId
        }
      }
    ]
    httpListeners: [
      {
        name: 'http-listener'
        properties: {
          frontendIPConfiguration: {
            id: resourceId('Microsoft.Network/applicationGateways/frontendIPConfigurations', '${projectName}-appgw', 'appGwPrivateFrontendIp')
          }
          frontendPort: {
            id: resourceId('Microsoft.Network/applicationGateways/frontendPorts', '${projectName}-appgw', 'port_80')
          }
          protocol: 'Http'
          requireServerNameIndication: false
        }
      }
      {
        name: 'https-listener'
        properties: {
          frontendIPConfiguration: {
            id: resourceId('Microsoft.Network/applicationGateways/frontendIPConfigurations', '${projectName}-appgw', 'appGwPrivateFrontendIp')
          }
          frontendPort: {
            id: resourceId('Microsoft.Network/applicationGateways/frontendPorts', '${projectName}-appgw', 'port_443')
          }
          protocol: 'Https'
          sslCertificate: {
            id: resourceId('Microsoft.Network/applicationGateways/sslCertificates', '${projectName}-appgw', 'appgw-ssl-cert')
          }
          requireServerNameIndication: false
        }
      }
    ]
    requestRoutingRules: [
      {
        name: 'https-routing-rule'
        properties: {
          ruleType: 'Basic'
          httpListener: {
            id: resourceId('Microsoft.Network/applicationGateways/httpListeners', '${projectName}-appgw', 'https-listener')
          }
          backendAddressPool: {
            id: resourceId('Microsoft.Network/applicationGateways/backendAddressPools', '${projectName}-appgw', 'defaultBackendPool')
          }
          backendHttpSettings: {
            id: resourceId('Microsoft.Network/applicationGateways/backendHttpSettingsCollection', '${projectName}-appgw', 'defaultHttpSettings')
          }
        }
      }
      {
        name: 'http-redirect-rule'
        properties: {
          ruleType: 'Basic'
          httpListener: {
            id: resourceId('Microsoft.Network/applicationGateways/httpListeners', '${projectName}-appgw', 'http-listener')
          }
          redirectConfiguration: {
            id: resourceId('Microsoft.Network/applicationGateways/redirectConfigurations', '${projectName}-appgw', 'http-to-https-redirect')
          }
        }
      }
    ]
    redirectConfigurations: [
      {
        name: 'http-to-https-redirect'
        properties: {
          redirectType: 'Permanent'
          targetListener: {
            id: resourceId('Microsoft.Network/applicationGateways/httpListeners', '${projectName}-appgw', 'https-listener')
          }
          includePath: true
          includeQueryString: true
        }
      }
    ]
    firewallPolicy: {
      id: wafPolicy.id
    }
  }
} 