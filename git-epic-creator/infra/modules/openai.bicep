@description('The location for the Azure OpenAI resources.')
param location string

@description('The name of the Azure OpenAI account to create.')
param openAiAccountName string

@description('The name of the Key Vault to store the OpenAI API key in.')
param keyVaultName string

@description('The ID of the Log Analytics workspace to send diagnostic data to.')
param logAnalyticsWorkspaceId string

@description('The SKU for the Azure OpenAI account.')
param openAiSku object = {
  name: 'S0'
}

@description('The name for the GPT-4 model deployment.')
param gptDeploymentName string = 'gpt4'

@description('The name for the text embedding model deployment.')
param embeddingDeploymentName string = 'text-embedding-ada-002'

var openAiKeySecretName = '${openAiAccountName}-key'

// Reference to the existing Key Vault created in main.bicep
resource existingKeyVault 'Microsoft.KeyVault/vaults@2023-07-01' existing = {
  name: keyVaultName
}

resource openAiAccount 'Microsoft.CognitiveServices/accounts@2023-10-01-preview' = {
  name: openAiAccountName
  location: location
  sku: openAiSku
  kind: 'OpenAI'
  properties: {
    customSubDomainName: openAiAccountName
    publicNetworkAccess: 'Enabled'
  }
}

resource gpt4Deployment 'Microsoft.CognitiveServices/accounts/deployments@2024-02-01' = {
  parent: openAiAccount
  name: gptDeploymentName
  properties: {
    model: {
      format: 'OpenAI'
      name: 'gpt-4'
      version: '0613'
    }
  }
  sku: {
    name: 'Standard'
    capacity: 20
  }
}

resource embeddingDeployment 'Microsoft.CognitiveServices/accounts/deployments@2024-02-01' = {
  parent: openAiAccount
  name: embeddingDeploymentName
  properties: {
    model: {
      format: 'OpenAI'
      name: 'text-embedding-ada-002'
      version: '2'
    }
  }
  sku: {
    name: 'Standard'
    capacity: 50
  }
}

resource openAiKeySecret 'Microsoft.KeyVault/vaults/secrets@2023-07-01' = {
  parent: existingKeyVault
  name: openAiKeySecretName
  properties: {
    value: openAiAccount.listKeys().key1
    contentType: 'Azure OpenAI API Key'
    attributes: {
      enabled: true
    }
  }
}

resource diagnostics 'Microsoft.Insights/diagnosticSettings@2021-05-01-preview' = {
  name: '${openAiAccountName}-diagnostics'
  scope: openAiAccount
  properties: {
    workspaceId: logAnalyticsWorkspaceId
    logs: [
      {
        category: 'RequestResponse'
        enabled: true
      }
      {
        category: 'Audit'
        enabled: true
      }
    ]
    metrics: [
      {
        category: 'AllMetrics'
        enabled: true
      }
    ]
  }
}

output openAiEndpoint string = openAiAccount.properties.endpoint
output openAiKeySecretName string = openAiKeySecret.name
output openAiKeySecretUri string = openAiKeySecret.properties.secretUri 