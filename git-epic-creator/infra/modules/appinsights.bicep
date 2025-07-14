
@description('The location for the Application Insights resource.')
param location string

@description('The name of the Application Insights resource.')
param appInsightsName string

@description('The ID of the Log Analytics Workspace to associate with Application Insights.')
param logAnalyticsWorkspaceId string

@description('Tags to apply to the resource.')
param tags object = {}

resource appInsights 'Microsoft.Insights/components@2020-02-02' = {
  name: appInsightsName
  location: location
  kind: 'web'
  properties: {
    Application_Type: 'web'
    WorkspaceResourceId: logAnalyticsWorkspaceId
  }
  tags: tags
}

@description('The connection string for the Application Insights resource.')
output connectionString string = appInsights.properties.ConnectionString

@description('The instrumentation key for the Application Insights resource.')
output instrumentationKey string = appInsights.properties.InstrumentationKey

@description('The ID of the Application Insights resource.')
output id string = appInsights.id 