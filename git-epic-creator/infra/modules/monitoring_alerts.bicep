
@description('The location for the alert rule resources. Defaults to the resource group location.')
param location string = resourceGroup().location

@description('The ID of the target resource to create the alert for (e.g., Application Insights instance).')
param targetResourceId string

@description('The name of the action group to be notified.')
param actionGroupName string

@description('A descriptive name for the metric alert.')
param alertName string

@description('A detailed description for the metric alert.')
param alertDescription string

@description('The severity of the alert. 0=critical, 1=error, 2=warning, 3=informational, 4=verbose.')
@allowed([0, 1, 2, 3, 4])
param severity int

@description('The name of the metric to monitor (e.g., requests/failed).')
param metricName string

@description('The aggregation type for the metric (e.g., Count, Average, Total).')
param timeAggregation string

@description('The operator to use for the threshold comparison (e.g., GreaterThan, LessThan).')
param operator string

@description('The threshold value to trigger the alert.')
param threshold int

@description('The frequency to evaluate the alert rule (e.g., PT1M for 1 minute).')
param evaluationFrequency string = 'PT1M'

@description('The time window over which to evaluate the metric (e.g., PT5M for 5 minutes).')
param windowSize string = 'PT5M'

@description('Tags to apply to the resources.')
param tags object = {}

resource actionGroup 'Microsoft.Insights/actionGroups@2023-09-01' = {
  name: actionGroupName
  location: 'Global'
  properties: {
    groupShortName: actionGroupName
    enabled: true
  }
  tags: tags
}

resource metricAlert 'Microsoft.Insights/metricAlerts@2018-03-01' = {
  name: alertName
  location: 'Global'
  tags: tags
  properties: {
    description: alertDescription
    severity: severity
    enabled: true
    scopes: [
      targetResourceId
    ]
    evaluationFrequency: evaluationFrequency
    windowSize: windowSize
    criteria: {
      'odata.type': 'Microsoft.Azure.Monitor.SingleResourceMultipleMetricCriteria'
      allOf: [
        {
          criterionType: 'StaticThresholdCriterion'
          name: 'Metric_Threshold'
          metricName: metricName
          operator: operator
          threshold: threshold
          timeAggregation: timeAggregation
        }
      ]
    }
    actions: [
      {
        actionGroupId: actionGroup.id
      }
    ]
  }
}

@description('The ID of the created action group.')
output actionGroupId string = actionGroup.id 