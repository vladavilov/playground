@description('The location for the Log Analytics Workspace.')
param location string

@description('The name of the Log Analytics Workspace.')
param workspaceName string

resource logAnalyticsWorkspace 'Microsoft.OperationalInsights/workspaces@2023-09-01' = {
  name: workspaceName
  location: location
  properties: {
    sku: {
      name: 'PerGB2018'
    }
    retentionInDays: 30
  }
}

output id string = logAnalyticsWorkspace.id 