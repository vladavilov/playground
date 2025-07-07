# Before running, ensure you have Pester and Azure CLI/Bicep CLI installed.
# Install-Module Pester -Force -SkipPublisherCheck

$scriptPath = $MyInvocation.MyCommand.Path
$infrastructurePath = Resolve-Path -Path (Join-Path $scriptPath '..')
$mainBicepPath = Join-Path $infrastructurePath 'main.bicep'
$testOutputPath = Join-Path $infrastructurePath 'tests/output'

if (-not (Test-Path $testOutputPath)) {
    New-Item -Path $testOutputPath -ItemType Directory | Out-Null
}

$environments = @(
    @{
        name = 'dev'
        paramsPath = Join-Path $infrastructurePath 'environments/dev.bicepparam'
        expectedServiceBusSku = 'Basic'
        expectedCosmosDbPublicNetworkAccess = 'Enabled'
    }
    @{
        name = 'prod'
        paramsPath = Join-Path $infrastructurePath 'environments/prod.bicepparam'
        expectedServiceBusSku = 'Standard'
        expectedCosmosDbPublicNetworkAccess = 'Disabled'
    }
)

Describe "Bicep Template Tests: main.bicep" -ForEach $environments {
    $envName = $_.name
    $paramsPath = $_.paramsPath
    $outputJsonPath = Join-Path $testOutputPath "main.arm.$envName.json"
    $template = $null

    BeforeAll {
        # Build the bicep file into an ARM template for the specific environment
        az bicep build --file $mainBicepPath --params $paramsPath --outfile $outputJsonPath
        $template = Get-Content -Path $outputJsonPath | ConvertFrom-Json
    }

    AfterAll {
        if (Test-Path $outputJsonPath) {
            Remove-Item $outputJsonPath
        }
    }

    Context "[$envName] Resource Validation" {
        It 'should contain 8 resource definitions' {
            $template.resources.Count | Should -Be 8
        }

        $expectedResourceTypes = @(
            'Microsoft.AppConfiguration/configurationStores'
            ,'Microsoft.KeyVault/vaults'
            ,'Microsoft.Authorization/roleAssignments'
            ,'Microsoft.ServiceBus/namespaces'
            ,'Microsoft.ServiceBus/namespaces/queues'
            ,'Microsoft.DocumentDB/databaseAccounts'
            ,'Microsoft.DocumentDB/databaseAccounts/sqlDatabases'
            ,'Microsoft.DocumentDB/databaseAccounts/sqlDatabases/containers'
        )
        It "should contain the correct resource types: $($expectedResourceTypes -join ', ')" {
            $actualResourceTypes = $template.resources.type
            $actualResourceTypes | Should -BeExactly $expectedResourceTypes
        }
    }

    Context "[$envName] Naming Convention Validation" {
        It 'should have resource names referencing the resourceToken variable' {
            foreach ($resource in $template.resources) {
                # The name is an expression, e.g., "[format('appconfig-{0}', variables('resourceToken'))]"
                # We can do a basic check on the expression string itself for the variable reference
                # This excludes the role assignment which uses a guid, and nested resources that have fixed names
                if ($resource.type -notin @('Microsoft.Authorization/roleAssignments', 'Microsoft.ServiceBus/namespaces/queues', 'Microsoft.DocumentDB/databaseAccounts/sqlDatabases', 'Microsoft.DocumentDB/databaseAccounts/sqlDatabases/containers')) {
                    $resource.name | Should -Match 'resourceToken'
                }
            }
        }
    }

    Context "[$envName] Property Validation" {
        It "should have all resources tagged with the '$envName' environment" {
            foreach ($resource in $template.resources) {
                # Role assignments do not support tags
                if ($resource.type -ne 'Microsoft.Authorization/roleAssignments') {
                    $resource.tags.environment | Should -Be $envName
                }
            }
        }

        It 'should have Key Vault configured with RBAC authorization' {
            $keyVault = $template.resources | Where-Object { $_.type -eq 'Microsoft.KeyVault/vaults' }
            $keyVault.properties.enableRbacAuthorization | Should -Be $true
        }

        It "should have Service Bus Sku set to '$($_.expectedServiceBusSku)'" {
            $serviceBus = $template.resources | Where-Object { $_.type -eq 'Microsoft.ServiceBus/namespaces' }
            $serviceBus.sku.name | Should -Be $_.expectedServiceBusSku
        }

        It "should have Cosmos DB publicNetworkAccess set to '$($_.expectedCosmosDbPublicNetworkAccess)'" {
            $cosmosDb = $template.resources | Where-Object { $_.type -eq 'Microsoft.DocumentDB/databaseAccounts' }
            $cosmosDb.properties.publicNetworkAccess | Should -Be $_.expectedCosmosDbPublicNetworkAccess
        }

        It 'should have App Configuration configured with a System-Assigned Identity' {
            $appConfig = $template.resources | Where-Object { $_.type -eq 'Microsoft.AppConfiguration/configurationStores' }
            $appConfig.identity.type | Should -Be 'SystemAssigned'
        }
    }

    Context "[$envName] Outputs Validation" {
        It 'should have 8 outputs' {
            ($template.outputs | Get-Member -MemberType NoteProperty).Count | Should -Be 8
        }

        It 'should have the correct output names' {
            $expectedOutputs = @(
                'appConfigName'
                ,'appConfigEndpoint'
                ,'keyVaultName'
                ,'keyVaultUri'
                ,'keyVaultId'
                ,'serviceBusNamespaceName'
                ,'cosmosDbAccountName'
                ,'cosmosDbId'
            )
            $actualOutputs = ($template.outputs | Get-Member -MemberType NoteProperty).Name
            $actualOutputs | Should -BeExactly $expectedOutputs
        }
    }
} 