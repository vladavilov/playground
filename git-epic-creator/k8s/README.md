# Agentic AI - Kubernetes Manifests

Kubernetes deployment manifests using Kustomize for the Git Epic Creator platform.

## Architecture

```
â”Œâ”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”
â”‚                              AKS Cluster                                     â”‚
â”œâ”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”¤
â”‚                                                                              â”‚
â”‚  â”Œâ”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”â”‚
â”‚  â”‚  NGINX Ingress Controller (LoadBalancer - Internal)                     â”‚â”‚
â”‚  â”‚  â””â”€â”€ Routes traffic to envoy-gateway (and / â†’ frontend-service)         â”‚â”‚
â”‚  â””â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”˜â”‚
â”‚                              â”‚                                               â”‚
â”‚  â”Œâ”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”´â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”  â”‚
â”‚  â”‚                       envoy-gateway (Envoy)                             â”‚  â”‚
â”‚  â”‚  â€¢ Routing / retries / timeouts                                         â”‚  â”‚
â”‚  â”‚  â€¢ Calls gateway-control-plane-service /authz (ext_authz)               â”‚  â”‚
â”‚  â””â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”˜  â”‚
â”‚                              â”‚                                               â”‚
â”‚  â”Œâ”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”´â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”  â”‚
â”‚  â”‚                 gateway-control-plane-service                           â”‚  â”‚
â”‚  â”‚  â€¢ Azure AD Authentication (MSAL)                                      â”‚  â”‚
â”‚  â”‚  â€¢ Session management (Redis-backed)                                   â”‚  â”‚
â”‚  â”‚  â€¢ /authz policy enforcement + S2S token minting                        â”‚  â”‚
â”‚  â”‚  â€¢ /events                                                       â”‚  â”‚
â”‚  â””â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”˜  â”‚
â”‚                              â”‚                                               â”‚
â”‚         â”Œâ”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”¼â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”                         â”‚
â”‚         â”‚                    â”‚                    â”‚                         â”‚
â”‚         â–¼                    â–¼                    â–¼                         â”‚
â”‚  â”Œâ”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”     â”Œâ”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”     â”Œâ”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”                   â”‚
â”‚  â”‚  project-   â”‚     â”‚    ai-      â”‚     â”‚   gitlab-   â”‚   ... 8 more     â”‚
â”‚  â”‚  management â”‚     â”‚ requirementsâ”‚     â”‚   client    â”‚   services       â”‚
â”‚  â”‚   service   â”‚     â”‚   service   â”‚     â”‚   service   â”‚                   â”‚
â”‚  â””â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”˜     â””â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”˜     â””â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”˜                   â”‚
â”‚         â”‚                    â”‚                                              â”‚
â”‚         â–¼                    â–¼                                              â”‚
â”‚  â”Œâ”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”     â”Œâ”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”                                       â”‚
â”‚  â”‚ PostgreSQL  â”‚     â”‚    Neo4j    â”‚     (StatefulSets with PVC)          â”‚
â”‚  â”‚(StatefulSet)â”‚     â”‚(StatefulSet)â”‚                                       â”‚
â”‚  â””â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”˜     â””â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”˜                                       â”‚
â”‚                                                                              â”‚
â””â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”˜
```

## Structure

```
k8s/
â”œâ”€â”€ base/                           # Base manifests
â”‚   â”œâ”€â”€ kustomization.yaml          # Base kustomization
â”‚   â”œâ”€â”€ namespace.yaml              # Namespace definition
â”‚   â”œâ”€â”€ service-account.yaml        # Workload identity service account
â”‚   â”œâ”€â”€ configmap.yaml              # Application configuration
â”‚   â”œâ”€â”€ secret-provider.yaml        # CSI secrets store provider
â”‚   â”œâ”€â”€ network-policy.yaml         # Network isolation policies
â”‚   â”œâ”€â”€ ingress.yaml                # NGINX Ingress (routes to envoy-gateway and frontend-service)
â”‚   â”œâ”€â”€ services/                   # Service deployments (11 services)
â”‚   â”‚   â”œâ”€â”€ db-init-service.yaml
â”‚   â”‚   â”œâ”€â”€ project-management-service.yaml
â”‚   â”‚   â”œâ”€â”€ neo4j-maintanance-service.yaml
â”‚   â”‚   â”œâ”€â”€ neo4j-ingestion-service.yaml
â”‚   â”‚   â”œâ”€â”€ neo4j-retrieval-service.yaml
â”‚   â”‚   â”œâ”€â”€ gateway-control-plane-service.yaml
â”‚   â”‚   â”œâ”€â”€ envoy-gateway.yaml
â”‚   â”‚   â”œâ”€â”€ document-processing-service.yaml
â”‚   â”‚   â”œâ”€â”€ ai-requirements-service.yaml
â”‚   â”‚   â”œâ”€â”€ ai-tasks-service.yaml
â”‚   â”‚   â”œâ”€â”€ gitlab-client-service.yaml
â”‚   â”‚   â””â”€â”€ mock-auth-service.yaml
â”‚   â”œâ”€â”€ statefulsets/               # Stateful databases
â”‚   â”‚   â”œâ”€â”€ neo4j.yaml
â”‚   â”‚   â””â”€â”€ postgresql.yaml
â”‚   â””â”€â”€ jobs/                       # One-time init jobs
â”‚       â”œâ”€â”€ postgresdb-schema-init.yaml
â”‚       â””â”€â”€ neo4j-schema-init.yaml
â””â”€â”€ overlays/
    â”œâ”€â”€ dev/                        # Development (1 replica)
    â””â”€â”€ prod/                       # Production (3 replicas)
```

## Services

| Service | Replicas (Dev/Prod) | Purpose |
|---------|---------------------|---------|
| `gateway-control-plane-service` | 1/3 | **API Gateway control plane** (MSAL, sessions, `/authz`, `/events`, ``) |
| `project-management-service` | 1/3 | Project CRUD |
| `ai-requirements-service` | 1/3 | AI requirement generation |
| `ai-tasks-service` | 1/3 | AI task breakdown |
| `neo4j-retrieval-service` | 1/3 | Knowledge graph queries |
| `neo4j-ingestion-service` | 1/1 | GraphRAG ingestion |
| `neo4j-maintanance-service` | 1/1 | Neo4j admin |
| `document-processing-service` | 1/3 | Document parsing |
| `gitlab-client-service` | 1/2 | GitLab integration |
| `authentication-service` | 1/2 | Azure token exchange + gateway service-token minting |
| `db-init-service` | 1/1 | PostgreSQL schema |
| `mock-auth-service` | 1/1 | Dev auth mock |
| `postgresql` (StatefulSet) | 1/1 | Relational data |
| `neo4j` (StatefulSet) | 1/1 | Graph database |

## Request Flow

```
Browser â†’ NGINX Ingress â†’ envoy-gateway (Envoy) â†’ Backend Services
                               â”‚
                               â”œâ”€â”€ ext_authz: gateway-control-plane-service /authz (session + RBAC â†’ injected headers)
                               â”œâ”€â”€ /auth/*,  /events â†’ gateway-control-plane-service
                               â”œâ”€â”€ /project/* â†’ project-management-service
                               â”œâ”€â”€ /workflow/* â†’ ai-requirements-service
                               â”œâ”€â”€ /tasks/* â†’ ai-tasks-service
                               â”œâ”€â”€ /gitlab/* â†’ gitlab-client-service
                               â””â”€â”€ /* â†’ Static UI files (frontend-service)
```

**Why Envoy + gateway control plane?**
- Envoy owns routing/retries/timeouts and calls `gateway-control-plane-service /authz` (`ext_authz`) for centralized policy enforcement.
- gateway-control-plane-service keeps Azure AD login (MSAL), session management, `/events`, and ``.

## Prerequisites

1. **Azure infrastructure deployed** (see `infra/README.md`)
2. **kubectl** configured for AKS cluster
3. **Kustomize** v5.0+ installed
4. **Helm** (for NGINX Ingress)

## Deployment

### 1. Install NGINX Ingress Controller

```bash
helm repo add ingress-nginx https://kubernetes.github.io/ingress-nginx
helm repo update
helm install nginx-ingress ingress-nginx/ingress-nginx \
  --namespace ingress-nginx --create-namespace \
  --set controller.service.type=LoadBalancer \
  --set controller.service.annotations."service\.beta\.kubernetes\.io/azure-load-balancer-internal"="true"
```

### 2. Create Image Pull Secret (Corporate Registry)

```bash
kubectl create secret docker-registry regcred \
  --docker-server=<your-registry.azurecr.io> \
  --docker-username=<username> \
  --docker-password=<password> \
  -n agentic-ai
```

### 3. Set Environment Variables

```bash
export ENVIRONMENT="dev"
export CONTAINER_REGISTRY="your-registry.azurecr.io"
export DOCKER_IMAGE_TAG="latest"
export WORKLOAD_IDENTITY_CLIENT_ID="..." # From infra deployment
export KEY_VAULT_NAME="agentic-ai-dev-kv"
export REDIS_HOST="agentic-ai-dev-redis.redis.cache.windows.net"
export OAI_ENDPOINT="https://agentic-ai-dev-oai.openai.azure.com"
export AZURE_TENANT_ID="..."
export AZURE_CLIENT_ID="..."
```

### 4. Deploy with Kustomize

```bash
cd k8s/overlays/dev

# Set images
kustomize edit set image \
  gateway-control-plane-service=${CONTAINER_REGISTRY}/gateway-control-plane-service:${DOCKER_IMAGE_TAG} \
  # ... other images

# Apply with variable substitution
kustomize build . | envsubst | kubectl apply -f -
```

Or use the GitLab CI/CD pipeline for automated deployment.

## Local Kubernetes (Docker Desktop / kind)

Use the local overlay at `k8s/overlays/local` to run the same K8s topology locally, without Azure KeyVault CSI:

- Local overlay docs: `k8s/overlays/local/README.md`
- Apply:

```bash
kubectl apply -k k8s/overlays/local
```

Access locally via port-forward (Ingress is deleted in the local overlay):

```bash
kubectl -n agentic-ai port-forward svc/frontend-service 8080:80
kubectl -n agentic-ai port-forward svc/envoy-gateway 8000:80
```

## Configuration

### Environment Variables (ConfigMap)

| Variable | Value | Description |
|----------|-------|-------------|
| `POSTGRES_HOST` | `postgresql-service` | K8s service name |
| `NEO4J_URI` | `bolt://neo4j-service:7687` | K8s service name |
| `REDIS_HOST` | `${REDIS_HOST}` | Azure Redis (from infra) |
| `OAI_BASE_URL` | `${OAI_ENDPOINT}` | Azure OpenAI (from infra) |

### Secrets (from Key Vault via CSI Driver)

| Secret | Key Vault Name | Description |
|--------|----------------|-------------|
| `POSTGRES_PASSWORD` | `Postgres-Password` | PostgreSQL auth |
| `NEO4J_PASSWORD` | `Neo4j-Password` | Neo4j auth |
| `REDIS_PASSWORD` | `Redis-Password` | Azure Redis auth |
| `SESSION_SECRET_KEY` | `Session-SecretKey` | UI sessions |
| `LOCAL_JWT_SECRET` | `Local-JwtSecret` | S2S auth |
| `API_GATEWAY_MINT_SECRET` | `Api-Gateway-MintSecret` | Gateway â†” auth-service mint authorization |
| `OAI_KEY` | `OpenAI-ApiKey` | Azure OpenAI |

## Storage

| StatefulSet | PVC Size | Storage Class |
|-------------|----------|---------------|
| PostgreSQL | 10Gi | managed-premium |
| Neo4j data | 50Gi | managed-premium |
| Neo4j logs | 10Gi | managed-premium |

## Network Policies

| Policy | Description |
|--------|-------------|
| `default-deny-ingress` | Deny all ingress by default |
| `allow-ingress-from-gateway` | NGINX â†’ envoy-gateway |
| `allow-frontend-service-ingress-from-gateway` | NGINX â†’ frontend-service |
| `allow-gateway-control-plane-service-ingress-from-envoy` | envoy-gateway â†’ gateway-control-plane-service |
| `allow-authentication-service-ingress-from-gateway-control-plane` | gateway-control-plane-service â†’ authentication-service |
| `allow-backend-communication` | envoy-gateway â†” backend tier + backend â†” backend |
| `allow-data-tier-access` | Backend â†’ PostgreSQL/Neo4j |

## Troubleshooting

### Check pod status
```bash
kubectl get pods -n agentic-ai
kubectl describe pod <pod-name> -n agentic-ai
```

### View logs
```bash
kubectl logs -f deployment/gateway-control-plane-service -n agentic-ai
```

### Check secrets mounting
```bash
kubectl exec -it deployment/gateway-control-plane-service -n agentic-ai -- ls /mnt/secrets-store
```

### Check NGINX Ingress
```bash
kubectl get ingress -n agentic-ai
kubectl logs -n ingress-nginx -l app.kubernetes.io/name=ingress-nginx
```

### PostgreSQL connection
```bash
kubectl exec -it statefulset/postgresql -n agentic-ai -- psql -U postgres -d requirementsdb
```
