// Enables the Microsoft Graph provider to manage AAD resources.
provider graph

targetScope = 'tenant'

@description('The display name for the Azure AD application.')
param applicationName string

// Define unique and static GUIDs for the application roles to ensure idempotency.
var adminRoleId = 'd83296c2-0b1a-42b7-8dd3-0d50937c16c2'
var projectManagerRoleId = 'a73f721c-8066-410a-9e46-1339d1b0d18b'
var contributorRoleId = 'f3c8e426-38d5-4720-b3e7-8833c62f275e'

var appRoles = [
  {
    allowedMemberTypes: [
      'User'
    ]
    description: 'Administrators can manage all aspects of the system.'
    displayName: 'Admin'
    id: adminRoleId
    isEnabled: true
    value: 'Admin'
  }
  {
    allowedMemberTypes: [
      'User'
    ]
    description: 'Project Managers can create projects and manage contributors.'
    displayName: 'ProjectManager'
    id: projectManagerRoleId
    isEnabled: true
    value: 'ProjectManager'
  }
  {
    allowedMemberTypes: [
      'User'
    ]
    description: 'Contributors can view and edit requirements and tasks.'
    displayName: 'Contributor'
    id: contributorRoleId
    isEnabled: true
    value: 'Contributor'
  }
]

// Create the Azure AD Application registration.
resource appRegistration 'Microsoft.Graph/applications@v1.0' = {
  displayName: applicationName
  signInAudience: 'AzureADMyOrg'
  appRoles: appRoles
  spa: {
    redirectUris: [] // Redirect URIs should be configured based on environment needs.
  }
}

// Create the Service Principal for the application, which is required for role assignments.
resource appSp 'Microsoft.Graph/servicePrincipals@v1.0' = {
  appId: appRegistration.appId
}

// Create Security Groups for each application role.
resource adminGroup 'Microsoft.Graph/groups@v1.0' = {
  displayName: '${applicationName}-Admins'
  mailEnabled: false
  securityEnabled: true
  mailNickname: toLower('${replace(applicationName, ' ', '')}Admins${uniqueString(subscription().id, applicationName)}')
}

resource projectManagerGroup 'Microsoft.Graph/groups@v1.0' = {
  displayName: '${applicationName}-ProjectManagers'
  mailEnabled: false
  securityEnabled: true
  mailNickname: toLower('${replace(applicationName, ' ', '')}ProjectManagers${uniqueString(subscription().id, applicationName)}')
}

resource contributorGroup 'Microsoft.Graph/groups@v1.0' = {
  displayName: '${applicationName}-Contributors'
  mailEnabled: false
  securityEnabled: true
  mailNickname: toLower('${replace(applicationName, ' ', '')}Contributors${uniqueString(subscription().id, applicationName)}')
}

// Assign the defined application roles to the corresponding security groups.
resource adminRoleAssignment 'Microsoft.Graph/appRoleAssignments@v1.0' = {
  appRoleId: adminRoleId
  principalId: adminGroup.id
  resourceId: appSp.id
}

resource projectManagerRoleAssignment 'Microsoft.Graph/appRoleAssignments@v1.0' = {
  appRoleId: projectManagerRoleId
  principalId: projectManagerGroup.id
  resourceId: appSp.id
}

resource contributorRoleAssignment 'Microsoft.Graph/appRoleAssignments@v1.0' = {
  appRoleId: contributorRoleId
  principalId: contributorGroup.id
  resourceId: appSp.id
}

@description('The Client ID of the registered AAD application.')
output clientId string = appRegistration.appId

@description('The Tenant ID where the application is registered.')
output tenantId string = tenant().tenantId

@description('The Object ID of the Admins security group.')
output adminGroupId string = adminGroup.id

@description('The Object ID of the Project Managers security group.')
output projectManagerGroupId string = projectManagerGroup.id

@description('The Object ID of the Contributors security group.')
output contributorGroupId string = contributorGroup.id 