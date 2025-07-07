@description('Specifies the name of the Application Insights component.')
param name string

@description('Specifies the location for the resources.')
param location string = resourceGroup().location

@description('Specifies the tags for the resources.')
param tags object = {}

resource appInsights 'Microsoft.Insights/components@2020-02-02' = {
  name: name
  location: location
  tags: tags
  kind: 'web'
  properties: {
    Application_Type: 'web'
    WorkspaceResourceId: null
  }
}

@description('The Connection String for the Application Insights component.')
output connectionString string = appInsights.properties.ConnectionString

@description('The Instrumentation Key for the Application Insights component.')
output instrumentationKey string = appInsights.properties.InstrumentationKey 