# Agentic AI - Kubernetes Manifests

Kubernetes deployment manifests using Kustomize for the Git Epic Creator platform.

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              AKS Cluster                                     │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌─────────────────────────────────────────────────────────────────────────┐│
│  │  NGINX Ingress Controller (LoadBalancer - Internal)                     ││
│  │  └── Routes all traffic to ui-service (API Gateway)                     ││
│  └─────────────────────────────────────────────────────────────────────────┘│
│                              │                                               │
│  ┌───────────────────────────┴───────────────────────────────────────────┐  │
│  │                         ui-service                                     │  │
│  │  • Azure AD Authentication (MSAL)                                      │  │
│  │  • Session management (Redis-backed)                                   │  │
│  │  • S2S JWT token minting                                               │  │
│  │  • API proxying to all backend services                                │  │
│  └───────────────────────────────────────────────────────────────────────┘  │
│                              │                                               │
│         ┌────────────────────┼────────────────────┐                         │
│         │                    │                    │                         │
│         ▼                    ▼                    ▼                         │
│  ┌─────────────┐     ┌─────────────┐     ┌─────────────┐                   │
│  │  project-   │     │    ai-      │     │   gitlab-   │   ... 8 more     │
│  │  management │     │ requirements│     │   client    │   services       │
│  │   service   │     │   service   │     │   service   │                   │
│  └─────────────┘     └─────────────┘     └─────────────┘                   │
│         │                    │                                              │
│         ▼                    ▼                                              │
│  ┌─────────────┐     ┌─────────────┐                                       │
│  │ PostgreSQL  │     │    Neo4j    │     (StatefulSets with PVC)          │
│  │(StatefulSet)│     │(StatefulSet)│                                       │
│  └─────────────┘     └─────────────┘                                       │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Structure

```
k8s/
├── base/                           # Base manifests
│   ├── kustomization.yaml          # Base kustomization
│   ├── namespace.yaml              # Namespace definition
│   ├── service-account.yaml        # Workload identity service account
│   ├── configmap.yaml              # Application configuration
│   ├── secret-provider.yaml        # CSI secrets store provider
│   ├── network-policy.yaml         # Network isolation policies
│   ├── ingress.yaml                # NGINX Ingress (routes to ui-service)
│   ├── services/                   # Service deployments (11 services)
│   │   ├── db-init-service.yaml
│   │   ├── project-management-service.yaml
│   │   ├── neo4j-maintanance-service.yaml
│   │   ├── neo4j-ingestion-service.yaml
│   │   ├── neo4j-retrieval-service.yaml
│   │   ├── ui-service.yaml
│   │   ├── document-processing-service.yaml
│   │   ├── ai-requirements-service.yaml
│   │   ├── ai-tasks-service.yaml
│   │   ├── gitlab-client-service.yaml
│   │   └── mock-auth-service.yaml
│   ├── statefulsets/               # Stateful databases
│   │   ├── neo4j.yaml
│   │   └── postgresql.yaml
│   └── jobs/                       # One-time init jobs
│       ├── postgresdb-schema-init.yaml
│       └── neo4j-schema-init.yaml
└── overlays/
    ├── dev/                        # Development (1 replica)
    └── prod/                       # Production (3 replicas)
```

## Services

| Service | Replicas (Dev/Prod) | Purpose |
|---------|---------------------|---------|
| `ui-service` | 1/3 | **API Gateway** + Web UI |
| `project-management-service` | 1/3 | Project CRUD |
| `ai-requirements-service` | 1/3 | AI requirement generation |
| `ai-tasks-service` | 1/3 | AI task breakdown |
| `neo4j-retrieval-service` | 1/3 | Knowledge graph queries |
| `neo4j-ingestion-service` | 1/1 | GraphRAG ingestion |
| `neo4j-maintanance-service` | 1/1 | Neo4j admin |
| `document-processing-service` | 1/3 | Document parsing |
| `gitlab-client-service` | 1/2 | GitLab integration |
| `db-init-service` | 1/1 | PostgreSQL schema |
| `mock-auth-service` | 1/1 | Dev auth mock |
| `postgresql` (StatefulSet) | 1/1 | Relational data |
| `neo4j` (StatefulSet) | 1/1 | Graph database |

## Request Flow

```
Browser → NGINX Ingress → ui-service → Backend Services
                              │
                              ├── /project/* → project-management-service
                              ├── /workflow/* → ai-requirements-service
                              ├── /tasks/* → ai-tasks-service
                              ├── /gitlab/* → gitlab-client-service
                              └── /* → Static UI files
```

**Why ui-service as API Gateway?**
- Already handles Azure AD authentication (MSAL)
- Already implements S2S JWT token minting
- Already does API proxying with error handling
- No need for separate App Gateway or APIM (~$200/month savings)

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
  ui-service=${CONTAINER_REGISTRY}/ui-service:${DOCKER_IMAGE_TAG} \
  # ... other images

# Apply with variable substitution
kustomize build . | envsubst | kubectl apply -f -
```

Or use the GitLab CI/CD pipeline for automated deployment.

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
| `allow-ingress-from-gateway` | NGINX → ui-service |
| `allow-backend-communication` | Backend ↔ Backend |
| `allow-data-tier-access` | Backend → PostgreSQL/Neo4j |

## Troubleshooting

### Check pod status
```bash
kubectl get pods -n agentic-ai
kubectl describe pod <pod-name> -n agentic-ai
```

### View logs
```bash
kubectl logs -f deployment/ui-service -n agentic-ai
```

### Check secrets mounting
```bash
kubectl exec -it deployment/ui-service -n agentic-ai -- ls /mnt/secrets-store
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
