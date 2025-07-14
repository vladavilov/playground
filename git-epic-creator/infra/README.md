# Agentic AI - Azure Infrastructure as Code

## Architecture Diagrams

To understand the system, let's look at it from a few different perspectives.

### 1. High-Level System Architecture

This diagram provides a top-down overview of the major components, illustrating the flow from the user down to the backend services and security components.

```mermaid
graph TD
    %% === Styles: Modern, Accessible & Calm Palette ===
    classDef default fill:#fdfdfd,stroke:#bbb,stroke-width:1px,color:#333;
    classDef user fill:#e3f2fd,stroke:#64b5f6,color:#1e88e5;
    classDef ingress fill:#e0f2f1,stroke:#4db6ac,color:#00796b;
    classDef service fill:#ede7f6,stroke:#9575cd,color:#5e35b1;
    classDef data fill:#fff3e0,stroke:#ffb74d,color:#e65100;
    classDef security fill:#fbe9e7,stroke:#ff8a65,color:#d84315;
    classDef config fill:#eceff1,stroke:#90a4ae,color:#455a64;

    %% === Structure ===
    subgraph User Space
        User("fa:fa-user User"):::user
    end

    subgraph "Azure Cloud Environment"
        subgraph "Tenant Scope"
            AAD("fa:fa-id-card Azure AD"):::security
        end
        
        subgraph "Subscription Scope / Resource Group"
            subgraph Ingress
                direction LR
                AppGateway("fa:fa-shield-alt App Gateway"):::ingress
                APIM("fa:fa-sitemap API Mgmt"):::ingress
            end
            
            subgraph "Application Logic"
                AKS("fa:fa-cogs AKS Cluster"):::service
            end
            
            subgraph "Data & AI Services"
                direction LR
                Postgres("fa:fa-database PostgreSQL"):::data
                Storage("fa:fa-hdd Storage Account"):::data
                OpenAI("fa:fa-robot OpenAI"):::data
            end

            subgraph "Configuration & Security"
                direction LR
                AppConfig("fa:fa-tasks App Config"):::config
                KeyVault("fa:fa-key Key Vault"):::security
            end
        end
    end
    
    %% === Connections ===
    User -- "HTTPS" --> AppGateway
    AppGateway --> APIM
    APIM -- "Routes to" --> AKS
    
    AKS -- "Reads/Writes Data" --> Postgres
    AKS -- "Accesses Files" --> Storage
    AKS -- "Calls AI Models" --> OpenAI
    
    AKS -- "Reads App Settings" --> AppConfig
    AppConfig -- "Resolves Secret URIs" --> KeyVault
    AppGateway -- "Reads SSL Certificate" --> KeyVault
    APIM -- "Validates JWT Token" --> AAD
```

### 2. Network Layout and Traffic Flow

This diagram focuses on the network segmentation and how traffic flows into and within the environment. With all users being internal, the architecture is simplified to a single, private entry path via the corporate VPN, enhancing security by removing public internet exposure.

```mermaid
graph LR
    %% === Styles: Modern, Accessible & Calm Palette ===
    classDef default fill:#fdfdfd,stroke:#bbb,stroke-width:1px,color:#333;
    classDef user fill:#e3f2fd,stroke:#64b5f6,color:#1e88e5;
    classDef subnet fill:#f5f5f5,stroke:#b0bec5;
    classDef config fill:#eceff1,stroke:#90a4ae,color:#455a64;
    
    %% === Diagram ===
    subgraph "Corporate Network"
        CorpUser("fa:fa-building Corporate User"):::user
    end

    subgraph "Azure Virtual Network"
        direction LR
        subgraph "Gateway Subnet"
            VpnGw("fa:fa-lock VPN Gateway"):::subnet
        end

        subgraph "Ingress Subnet"
            AppGw("fa:fa-shield-alt App Gateway (Internal)"):::subnet
        end

        subgraph "Application Subnet"
            AKS("fa:fa-cogs AKS Cluster"):::subnet
        end
        
        subgraph "Database Subnet"
            Postgres("fa:fa-database PostgreSQL"):::subnet
            Neo4j("fa:fa-project-diagram Neo4j"):::subnet
        end

        subgraph "Private Endpoints Subnet"
            AppConfigPE("fa:fa-tasks App Config"):::config
            KeyVaultPE("fa:fa-key Key Vault"):::config
        end
    end

    %% === Connections ===
    CorpUser -- "Secure VPN Tunnel" --> VpnGw
    VpnGw -- "Enters VNet" --> AppGw
    AppGw -- "Inspects & Forwards Traffic" --> AKS
    
    subgraph "Application Connections"
        AKS -- "Connects via Private Link" --> Postgres
        AKS -- "Connects to Graph DB" --> Neo4j
        AKS -- "Reads Config via PE" --> AppConfigPE
        AppConfigPE -- "Reads Secrets via PE" --> KeyVaultPE
    end
```

### 3. Security and Authentication Flow

This diagram details the authentication flow and the centralized configuration access pattern using a sequence diagram for clarity.

```mermaid
sequenceDiagram
    actor User
    
    %% Participant Definitions
    participant ClientApp as Client (SPA)
    participant AAD as Azure AD
    participant APIM as API Management
    participant AKS as Backend Service (AKS)
    participant AppConfig as App Configuration
    participant KeyVault as Key Vault

    %% Authentication Flow
    User->>+ClientApp: 1. Initiate Login
    ClientApp->>+AAD: 2. Request ID Token (OIDC)
    AAD-->>-ClientApp: 3. Issue ID Token (JWT)
    
    ClientApp->>+APIM: 4. Call API with Bearer Token
    APIM->>+AAD: 5. Validate token signature & claims
    AAD-->>-APIM: Confirm token validity
    APIM->>+AKS: 6. Forward authorized request
    
    %% Configuration & Secret Retrieval
    AKS->>+AppConfig: 7. Read configuration value
    Note right of AppConfig: Authenticates via Managed Identity
    AppConfig-->>-AKS: Return simple key-value
    
    alt Secret value needed (Key Vault Reference)
        AKS->>+AppConfig: 8. Request secret (e.g., 'Postgres:Password')
        AppConfig->>+KeyVault: 9. Resolve reference & fetch secret<br>using its own Managed Identity
        KeyVault-->>-AppConfig: Return secret value
        AppConfig-->>-AKS: Return secret to service
    end

    %% Response Flow
    AKS-->>-APIM: 10. Respond to request
    APIM-->>-ClientApp: 11. Return final response
```

## How to Deploy

The infrastructure is deployed in a two-step process to correctly handle resources at different Azure scopes (Tenant and Subscription).

### Step 1: Deploy Tenant-Level Resources (One-Time)

This step deploys the Azure Active Directory application registration. It only needs to be run once for the entire tenant by an administrator with permissions to create AAD applications.

```powershell
./deploy-tenant.ps1
```

After the script completes, note the `clientId` and `tenantId` from the output. These are required for the environment deployments.

### Step 2: Deploy Environment-Specific Infrastructure

This step deploys all the resources for a specific environment (e.g., dev, qa, prod) into a dedicated resource group.

#### Prerequisites

1.  **Azure CLI**: Ensure you have the Azure CLI installed and authenticated.
    ```powershell
    az login
    az account set --subscription <Your-Subscription-ID>
    ```
2.  **PowerShell**: The deployment scripts are designed to be run in a PowerShell environment.
    **Example:**

    ```powershell
    ./deploy.ps1 `
        -SubscriptionId "your-subscription-id" `
        -Environment "dev" `
        -AadClientId "client-id-from-tenant-deployment" `
        -TenantId "tenant-id-from-tenant-deployment" `
        -AdminGroupObjectId "azure-ad-group-object-id-for-aks-admins" `
        -AppGwSslCertSecretId "your-key-vault-secret-id-for-ssl-cert" `
        -PostgresCredential (Get-Credential) `
        -PublisherEmail "your.email@example.com"
    ```

    ### Parameters for `deploy.ps1`

    -   `SubscriptionId` (Mandatory): The ID of the Azure subscription to deploy to.
    -   `Environment` (Mandatory): The target environment name (e.g., `dev`, `qa`, `prod`).
    -   `AadClientId` (Mandatory): The Client ID of the AAD Application from the tenant deployment.
    -   `TenantId` (Mandatory): The Tenant ID where the AAD Application is registered.
    -   `AdminGroupObjectId` (Mandatory): The Object ID of the Azure AD group that will be granted administrator access to the AKS cluster.
    -   `AppGwSslCertSecretId` (Mandatory): The secret identifier (URI) for the SSL certificate stored in Key Vault.
    -   `PostgresCredential` (Mandatory): A PowerShell credential object for the PostgreSQL administrator.
    -   `PublisherEmail` (Mandatory): The email address to be associated with the API Management service publisher.
    -   `AppGatewayBackendIpsJson` (Optional): A JSON string representing an array of IP addresses for the Application Gateway's backend pool. Defaults to an empty array.

3.  **SSL Certificate**: You must have an SSL certificate uploaded to an Azure Key Vault as a secret. The secret's URI is required for the deployment.
    ```powershell
    az keyvault secret set --vault-name <your-key-vault-name> --name <your-secret-name> --value <your-certificate-content>
    ```