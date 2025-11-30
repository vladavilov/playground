# Agentic AI - Azure Infrastructure (Simplified)

Azure infrastructure provisioning using Bicep for the Git Epic Creator microservices platform.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              Azure Cloud                                     │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌────────────────────────────────────────────────────────────────────────┐ │
│  │                         Resource Group                                  │ │
│  │                                                                         │ │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌──────────────┐  │ │
│  │  │     AKS     │  │   Redis     │  │   OpenAI    │  │  Key Vault   │  │ │
│  │  │  + OIDC +   │  │   Cache     │  │   Service   │  │  (Secrets)   │  │ │
│  │  │  Workload   │  │             │  │             │  │              │  │ │
│  │  │  Identity   │  │             │  │             │  │              │  │ │
│  │  └──────┬──────┘  └─────────────┘  └─────────────┘  └──────────────┘  │ │
│  │         │                                                              │ │
│  │         │  Contains (as containers):                                   │ │
│  │         │  • PostgreSQL (StatefulSet)                                  │ │
│  │         │  • Neo4j (StatefulSet)                                       │ │
│  │         │  • All 11 microservices                                      │ │
│  │         │  • NGINX Ingress Controller                                  │ │
│  │         │                                                              │ │
│  │  ┌──────┴──────┐  ┌─────────────┐  ┌────────────────────────────────┐ │ │
│  │  │   Storage   │  │ Log Analytics│  │      App Configuration        │ │ │
│  │  │   Account   │  │ + Insights  │  │    (Centralized Settings)     │ │ │
│  │  └─────────────┘  └─────────────┘  └────────────────────────────────┘ │ │
│  │                                                                         │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│                                                                              │
│  External (Corporate):                                                       │
│  ┌────────────────────────────────────────────────────────────────────────┐ │
│  │  Container Registry (provided by organization)                         │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

## What's Provisioned

| Resource | Purpose | Notes |
|----------|---------|-------|
| Resource Group | Container for all resources | `agentic-ai-{env}-rg` |
| Virtual Network | Network isolation | Simplified: 2 subnets only |
| AKS Cluster | Container orchestration | OIDC + Workload Identity enabled |
| Azure Cache for Redis | Caching, Celery broker, pub/sub | Basic (dev) / Standard (prod) |
| Azure OpenAI | AI/ML services | GPT-4, embeddings |
| Storage Account | Blob storage for documents | |
| Key Vault | Secrets management | All secrets stored here |
| App Configuration | Centralized configuration | Free (dev) / Standard (prod) |
| Log Analytics | Monitoring and logging | |
| Application Insights | APM and telemetry | |
| Workload Identity | AKS ↔ Azure AD federation | For Key Vault access |

## What's NOT Provisioned (Runs in AKS)

| Component | Why Container? |
|-----------|----------------|
| **PostgreSQL** | Small data (2 tables, 100 projects). Cost savings ~$50/month |
| **Neo4j** | Graph database best run as container with persistence |
| **Container Registry** | Uses corporate registry (cost savings, centralized) |
| **Application Gateway** | NGINX Ingress in AKS is sufficient |
| **API Management** | ui-service handles auth/proxying |

## Prerequisites

1. **Azure CLI** installed and authenticated
2. **PowerShell** 7.0+
3. **Azure Subscription** with Contributor access
4. **Corporate Container Registry** credentials
5. **Azure AD permissions** for app registration (one-time)

## Deployment

### Step 1: Tenant-Level Resources (One-Time)

```powershell
cd infra
./deploy-tenant.ps1
```

Save the `clientId` and `tenantId` from output.

### Step 2: Environment Infrastructure

```powershell
./deploy.ps1 `
    -SubscriptionId "your-subscription-id" `
    -Environment "dev" `
    -AadClientId "client-id-from-step-1" `
    -TenantId "your-tenant-id" `
    -AdminGroupObjectId "azure-ad-group-object-id" `
    -ContainerRegistryLoginServer "myregistry.azurecr.io"
```

### Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `SubscriptionId` | Yes | Azure subscription ID |
| `Environment` | Yes | Environment name (dev, qa, prod) |
| `AadClientId` | Yes | AAD application client ID |
| `TenantId` | Yes | Azure AD tenant ID |
| `AdminGroupObjectId` | Yes | AAD group for AKS admins |
| `ContainerRegistryLoginServer` | Yes | Corporate registry URL |
| `SessionSecretKey` | No | Auto-generated |
| `LocalJwtSecret` | No | Auto-generated |
| `Neo4jPassword` | No | Auto-generated |
| `PostgresPassword` | No | Auto-generated |

## Outputs

```powershell
az deployment sub show --name "agentic-ai-dev" --query "properties.outputs"
```

| Output | Description |
|--------|-------------|
| `resourceGroupName` | Resource group name |
| `aksClusterName` | AKS cluster name |
| `aksOidcIssuerUrl` | OIDC issuer for workload identity |
| `keyVaultName` | Key Vault name |
| `redisHostName` | Redis cache hostname |
| `appConfigEndpoint` | App Configuration endpoint |
| `workloadIdentityClientId` | Managed identity client ID |
| `openAiEndpoint` | Azure OpenAI endpoint |
| `storageEndpoints` | Storage account endpoints |

## Security Features

- **Private AKS Cluster**: API server not publicly accessible
- **Workload Identity**: Passwordless Azure AD auth for pods
- **Key Vault Integration**: Secrets via CSI driver
- **NGINX Ingress**: SSL termination, rate limiting
- **Network Policies**: Pod-to-pod traffic control

## Environment Configurations

| Setting | Dev | Prod |
|---------|-----|------|
| AKS Nodes | 1-3 | 2-5 |
| Redis SKU | Basic C0 | Standard C1 |
| App Config SKU | Free | Standard |
| PostgreSQL | Container | Container |
| Neo4j | Container | Container |

## Cost Comparison

| Component | Previous | Simplified | Savings |
|-----------|----------|------------|---------|
| PostgreSQL | ~$52/month (Flexible B2s) | $0 (container) | $52 |
| App Gateway | ~$150/month (Basic) | $0 (NGINX) | $150 |
| APIM | ~$50/month (Developer) | $0 (ui-service) | $50 |
| ACR | ~$5-25/month | $0 (corporate) | $5-25 |
| **Total Savings** | | | **~$260/month** |

## Next Steps After Deployment

1. **Get AKS credentials**:
   ```bash
   az aks get-credentials --resource-group agentic-ai-dev-rg --name agentic-ai-dev-aks
   ```

2. **Install NGINX Ingress**:
   ```bash
   helm repo add ingress-nginx https://kubernetes.github.io/ingress-nginx
   helm install nginx-ingress ingress-nginx/ingress-nginx \
     --namespace ingress-nginx --create-namespace \
     --set controller.service.annotations."service\.beta\.kubernetes\.io/azure-load-balancer-internal"="true"
   ```

3. **Deploy application**:
   ```bash
   cd k8s/overlays/dev
   kustomize build . | kubectl apply -f -
   ```

Or use GitLab CI/CD pipeline for automated deployment.
