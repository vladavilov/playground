
@description('The Azure region where the dashboard will be deployed.')
param location string

@description('The name of the dashboard.')
param dashboardName string

@description('The name of the Application Insights resource to use as a data source.')
param appInsightsName string

@description('Tags to apply to the resources.')
param tags object = {}

resource dashboard 'Microsoft.Portal/dashboards@2020-09-01-preview' = {
  name: dashboardName
  location: location
  properties: {
    lenses: [
      {
        order: 0
        parts: [
          // Part 1: Application Map
          // Provides a visual layout of the service dependencies.
          {
            position: {
              x: 0
              y: 0
              colSpan: 4
              rowSpan: 4
            }
            metadata: {
              inputs: [
                {
                  name: 'ComponentId'
                  value: {
                    subscriptionId: subscription().subscriptionId
                    resourceGroup: resourceGroup().name
                    name: appInsightsName
                    resourceType: 'microsoft.insights/components'
                  }
                }
              ]
              type: 'Extension/AppInsightsExtension/PartType/AppMap'
            }
          }
          // Part 2: Requests Failed Chart
          // Displays a pie chart of failed requests, grouped by result code.
          {
            position: {
              x: 4
              y: 0
              colSpan: 6
              rowSpan: 4
            }
            metadata: {
              inputs: [
                {
                  name: 'ComponentId'
                  value: {
                    subscriptionId: subscription().subscriptionId
                    resourceGroup: resourceGroup().name
                    name: appInsightsName
                    resourceType: 'microsoft.insights/components'
                  }
                }
                {
                  name: 'Query'
                  value: 'requests | where success == false | summarize count() by resultCode | render piechart'
                }
              ]
              type: 'Extension/AppInsightsExtension/PartType/AnalyticsChart/query'
            }
          }
        ]
      }
    ]
    metadata: {
      model: {
        // Sets the default time range for the dashboard to the last 24 hours.
        timeRange: {
          value: {
            relative: {
              duration: 24
              timeUnit: 1
            }
          }
          type: 'MsPortalFx.Composition.Configuration.ValueTypes.TimeRange'
        }
      }
    }
  }
  tags: tags
}

@description('The ID of the created dashboard.')
output id string = dashboard.id 