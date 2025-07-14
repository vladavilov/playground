
@description('The name of the API Management service.')
param apiManagementName string

@description('The Azure region where the resources will be deployed.')
param location string

@description('The name of the publisher.')
param publisherName string = 'Agentic AI'

@description('The email address of the publisher.')
param publisherEmail string

@description('A comma-separated list of allowed origins for CORS.')
param allowedOrigins string = '*'

@description('The SKU for the API Management service.')
param sku object = {
  name: 'Developer'
  capacity: 1
}

@description('The Application Insights instrumentation key for logging.')
param appInsightsInstrumentationKey string

@description('The JWT issuer URL for token validation.')
param jwtIssuerUrl string

@description('The JWT audience for token validation.')
param jwtAudience string

var apimLoggerName = '${apiManagementName}-logger'
var apimServiceName = '${apiManagementName}-service'

resource apimService 'Microsoft.ApiManagement/service@2023-09-01-preview' = {
  name: apimServiceName
  location: location
  sku: sku
  properties: {
    publisherEmail: publisherEmail
    publisherName: publisherName
  }
}

resource apimLogger 'Microsoft.ApiManagement/service/loggers@2023-09-01-preview' = {
  parent: apimService
  name: apimLoggerName
  properties: {
    loggerType: 'applicationInsights'
    credentials: {
      instrumentationKey: appInsightsInstrumentationKey
    }
  }
}

resource globalPolicy 'Microsoft.ApiManagement/service/policies@2023-09-01-preview' = {
  parent: apimService
  name: 'policy'
  properties: {
    format: 'xml'
    value: '''
<policies>
    <inbound>
        <cors>
            <allowed-origins>
                <origin>${replace(allowedOrigins, ',', '</origin><origin>')}</origin>
            </allowed-origins>
            <allowed-methods>
                <method>*</method>
            </allowed-methods>
            <allowed-headers>
                <header>*</header>
            </allowed-headers>
        </cors>
        <validate-jwt header-name="Authorization" failed-validation-httpcode="401" failed-validation-error-message="Unauthorized. Access token is missing or invalid.">
            <openid-config url="${jwtIssuerUrl}" />
            <audiences>
                <audience>${jwtAudience}</audience>
            </audiences>
        </validate-jwt>
        <rate-limit calls="1000" renewal-period="3600" />
        <rate-limit-by-key calls="100" renewal-period="60" counter-key="@(context.Request.Headers.GetValueOrDefault("Authorization", "").Split(' ')[1].AsJwt()?.Claims.GetValueOrDefault("oid", "anonymous"))" />
        <trace source="api-management-policy" severity="information">
            <message>@(String.Format("Inbound: RequestId={0}, Method={1}, Url={2}", context.RequestId, context.Request.Method, context.Request.Url.ToString()))</message>
            <metadata name="request-ip" value="@(context.Request.IpAddress)" />
        </trace>
        <circuit-breaker max-failure-count="10" interval-seconds="60" max-trip-duration-seconds="600">
             <when condition="@(context.Response.StatusCode >= 500)" />
        </circuit-breaker>
    </inbound>
    <backend>
        <forward-request />
    </backend>
    <outbound>
        <set-header name="X-Powered-By" exists-action="delete" />
        <trace source="api-management-policy" severity="information">
            <message>@(String.Format("Outbound: RequestId={0}, StatusCode={1}", context.RequestId, context.Response.StatusCode))</message>
            <metadata name="content-length" value="@(context.Response.Body.As<string>(true)?.Length.ToString())" />
        </trace>
    </outbound>
    <on-error>
        <trace source="api-management-policy" severity="error">
            <message>@(String.Format("OnError: RequestId={0}, Source={1}, Reason={2}, Message={3}", context.RequestId, context.LastError.Source, context.LastError.Reason, context.LastError.Message))</message>
            <metadata name="section" value="@(context.LastError.Section)" />
            <metadata name="path" value="@(context.LastError.Path)" />
            <metadata name="policy-id" value="@(context.LastError.PolicyId)" />
        </trace>
    </on-error>
</policies>
'''
  }
}

@description('The gateway URL of the API Management service.')
output gatewayUrl string = apimService.properties.gatewayUrl 