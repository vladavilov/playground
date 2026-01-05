# Local Kubernetes overlay (`k8s/overlays/local`)

This overlay lets you run the **same K8s architecture** locally as in AKS, with minimal local-only changes:

- **Removes AKS-only KeyVault CSI** and uses a normal Kubernetes `Secret` (`agentic-ai-secrets`).
- Adds local replacements for cloud-managed dependencies:
  - `redis` (no TLS)
  - `azurite` (Azure Storage emulator)
  - `openai-mock-service`
  - `gitlab-mock-service`
- Disables base `Ingress` (local flow uses `kubectl port-forward`).

## Prereqs (Windows)

- Docker Desktop with **Kubernetes enabled** (recommended) or another local cluster (kind/minikube)
- `kubectl` in PATH

## 1) Build images into your local Docker daemon

These tags are what the K8s manifests expect.

```powershell
cd playground\git-epic-creator

# Base images
docker build -t python-service-base:latest -f services/base/Dockerfile services/base

# Service images (context is ./services, matching docker-compose)
docker build -t db-init-service:latest -f services/db_init_service/Dockerfile services
docker build -t project-management-service:latest -f services/project_management_service/Dockerfile services
docker build -t neo4j-repository-service:latest -f services/neo4j_repository_service/Dockerfile services/neo4j_repository_service
docker build -t neo4j-ingestion-service:latest -f services/neo4j_ingestion_service/Dockerfile services
docker build -t neo4j-retrieval-service:latest -f services/neo4j_retrieval_service/Dockerfile services
docker build -t frontend-service:latest -f services/frontend_service/Dockerfile services
docker build -t sse-bridge-service:latest -f services/sse_bridge_service/Dockerfile services
docker build -t document-processing-service:latest -f services/document_processing_service/Dockerfile services
docker build -t ai-requirements-service:latest -f services/ai_requirements_service/Dockerfile services
docker build -t ai-tasks-service:latest -f services/ai_tasks_service/Dockerfile services
docker build -t gitlab-client-service:latest -f services/gitlab_client_service/Dockerfile services
docker build -t authentication-service:latest -f services/authentication_service/Dockerfile services
docker build -t mock-auth-service:latest -f services/mock_auth_service/Dockerfile services

# Extra local-only deps
docker build -t openai-mock-service:latest -f services/openai_mock_service/Dockerfile services
docker build -t gitlab-mock-service:latest -f services/gitlab_mock_service/Dockerfile services

# Neo4j baked image
docker build -t neo4j-baked:latest -f services/neo4j_baked/Dockerfile services
```

## 2) Apply the local overlay

```powershell
cd playground\git-epic-creator
kubectl apply -k k8s/overlays/local
```

## Notes on `neo4j-repository-service` locally

The repository contains `services/neo4j_repository_service` (Rust) which exposes:

- `POST /v1/maintenance/init-schema` (requires S2S `Authorization` bearer token)

The base K8s manifests deploy this as `neo4j-repository-service`. For local K8s, build
`neo4j_repository_service` and tag it as `neo4j-repository-service:latest`.

## 3) Access the system

The local overlay deletes the `Ingress` on purpose; use port-forward.

In one terminal:

```powershell
kubectl -n agentic-ai port-forward svc/frontend-service 8080:80
```

Then open `http://localhost:8080`.

If you want to hit APIs through the gateway data plane:

```powershell
kubectl -n agentic-ai port-forward svc/envoy-gateway 8000:80
```

Now `http://localhost:8000/*` routes through Envoy.

## 4) Secrets

Local secrets live in `resources/agentic-ai-secrets.local.yaml`.

- For real AKS, secrets come from KeyVault CSI (`k8s/base/secret-provider.yaml`).
- For local K8s, we keep the same secret keys, but store them in-cluster.

If you want to use your own local secrets, edit that file (or replace it with your own sealed secret workflow).


