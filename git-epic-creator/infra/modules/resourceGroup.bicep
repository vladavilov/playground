@description('The name of the resource group.')
param name string

@description('The location of the resource group.')
param location string

resource rg 'Microsoft.Resources/resourceGroups@2024-03-01' = {
  name: name
  location: location
}

output name string = rg.name 