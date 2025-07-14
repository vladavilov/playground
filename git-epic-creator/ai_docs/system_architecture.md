# System Architecture Document
# Agentic AI Requirements Engineering System

## 1. Executive Summary

This document presents the detailed system architecture for the Agentic AI Requirements Engineering System, designed for UBS enterprise environment. The architecture implements a cloud-native, microservices-based platform that transforms high-level feature descriptions into structured requirements using orchestrated AI experts and Graph RAG capabilities.

### 1.1 Architecture Principles
- **Cloud-Native Design**: Containerized microservices with Kubernetes orchestration
- **Event-Driven Architecture**: Asynchronous communication with reliable message queues
- **API-First Approach**: RESTful APIs with OpenAPI 3.0 specifications
- **Zero-Trust Security**: Azure AD integration with role-based access control
- **Scalable Design**: Built for 100 concurrent users with horizontal scaling capabilities
- **Data Privacy**: No Customer Identification Data (CID) storage compliance

### 1.2 System Constraints
- **Region**: Azure East US 2 (primary and only region)
- **Scale**: 100 concurrent users maximum, 100 projects with 10 concurrent processing
- **AI Load**: 1000 requests per hour
- **Deployment**: Monthly deployments (last Friday of month)
- **Environments**: Dev/QA/Prod with isolated configurations

## 2. High-Level Architecture

### 2.1 System Context Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                UBS Enterprise Environment (East US 2)           │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────┐    ┌─────────────────────────────────────────┐  │
│  │   Azure AD  │    │      Agentic AI Requirements System     │  │
│  │     SSO     │◄──►│            (VPN Network)               │  │
│  └─────────────┘    │  ┌─────────────┐  ┌─────────────────┐  │  │
│                     │  │  Frontend   │  │   API Gateway   │  │  │
│  ┌─────────────┐    │  │   Service   │◄─┤     Service     │  │  │
│  │    GitLab   │◄──►│  └─────────────┘  └─────────────────┘  │  │
│  │  Internal   │    │                                         │  │
│  └─────────────┘    │  ┌─────────────┐  ┌─────────────────┐  │  │
│                     │  │  Business   │  │   Data Layer    │  │  │
│  ┌─────────────┐    │  │  Services   │◄─┤    Services     │  │  │
│  │Azure OpenAI │◄──►│  └─────────────┘  └─────────────────┘  │  │
│  │   GPT-4.1   │    └─────────────────────────────────────────┘  │
│  └─────────────┘                                                │
└─────────────────────────────────────────────────────────────────┘
```

### 2.2 Microservices Architecture

The system consists of 8 core microservices organized into three logical layers:

#### Presentation Layer
- **Frontend Service**: React.js SPA with real-time updates
- **API Gateway Service**: Azure API Management with authentication

#### Business Logic Layer
- **Requirements Processing Service**: AI expert orchestration
- **Project Management Service**: Project CRUD operations
- **GitLab Integration Service**: Epic/issue creation and sync

#### Data Layer
- **Document Processing Service**: File processing and structuring
- **Graph RAG Context Service**: Neo4j proxy for knowledge retrieval
- **Graph RAG Ingestion Service**: Data ingestion and graph construction

## 3. Graph RAG Implementation Strategy

### 3.1 Vector Embedding Recommendations

**Recommended Configuration:**
- **Embedding Dimensions:** 1536 (Azure OpenAI text-embedding-ada-002)
- **Similarity Algorithm:** Cosine Similarity
- **Embedding Model:** text-embedding-ada-002 (Azure OpenAI)

### 3.2 Neo4j Vector Index Configuration

```cypher
// Vector index for semantic search
CREATE VECTOR INDEX requirement_embeddings IF NOT EXISTS
FOR (r:Requirement) ON (r.embedding)
OPTIONS {indexConfig: {
    'vector.dimensions': 1536,
    'vector.similarity_function': 'cosine'
}};

CREATE VECTOR INDEX entity_embeddings IF NOT EXISTS
FOR (e:Entity) ON (e.embedding)
OPTIONS {indexConfig: {
    'vector.dimensions': 1536,
    'vector.similarity_function': 'cosine'
}};
```

## 4. Detailed Service Architecture

### 4.1 Frontend Service

**Technology Stack:**
- React 18+
- Redux
- React Toolkit
- React-Redux
- Redux-Saga
- Material-UI v5 for enterprise UX

**Features:**
- Azure AD SSO integration
- Real-time progress tracking
- Rich text editor for requirements and tasks list
- View for CRUD operations for project entity
- Document upload with drag-and-drop
- **CID Compliance**: No customer identification data handling

### 4.2 API Gateway Service

**Technology Stack:**
- Azure API Management
- Azure Application Gateway for L7 load balancing
- VPN Gateway integration

**Responsibilities:**
- JWT token validation (Azure AD)
- Rate limiting (100 req/min per user, 1000 req/hour system-wide)
- Request routing and transformation
- API versioning and documentation
- Metrics collection and monitoring

**Security Features:**
- VPN network access control
- Request/response filtering
- API throttling and circuit breaker
- CID data filtering and validation

### 4.3 Requirements Processing Service

**Technology Stack:**
- Python 3.11+ with FastAPI
- Celery with Redis for async processing
- Pydantic for data validation
- Azure OpenAI SDK

**AI Expert Orchestration:**
```python
class ExpertOrchestrator:
    def __init__(self):
        self.experts = [
            BusinessAnalyst(),
            TechnicalArchitect(),
            UXDesigner(),
            QASpecialist(),
            DomainExpert()
        ]
    
    async def process_requirements(self, feature_description: str, context: Dict) -> RequirementsResult:
        # Sequential processing with consensus scoring
        # CID filtering and bias detection
        pass
```

**Processing Pipeline:**
1. **Input Validation**: CID detection and filtering
2. **Feature Analysis**: Context-aware analysis with Graph RAG
3. **Gap Analysis**: Refinement and validation
4. **Expert Consensus**: 70% threshold validation with bias detection
5. **Clarifying Questions**: Domain-specific question generation
6. **Requirements Structuring**: Structured output generation

### 4.4 Project Management Service

**Technology Stack:**
- Python 3.11+ with FastAPI
- SQLAlchemy with PostgreSQL
- Azure AD integration

**Data Model (CID-Free):**
```sql
-- Core entities (no CID storage)
CREATE TABLE projects (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    name VARCHAR(255) NOT NULL,
    description TEXT,
    gitlab_url TEXT, -- URL for GitLab project with issue tracking
    gitlab_repository_url TEXT,
    status VARCHAR(50) DEFAULT 'active',
    created_by VARCHAR(255) NOT NULL, -- Azure AD user ID only
    created_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW()
);

-- No personal or customer data stored
```

**RBAC Implementation:**
- Azure AD group-based permissions
- Project-level access control
- Row-level security in PostgreSQL
- CID access prevention

### 4.5 GitLab Integration Service

**Technology Stack:**
- Python 3.11+ with aiohttp
- GitLab REST API v4
- OAuth2 token management

**Integration Features:**
- Epic creation with structured templates
- Issue creation with labels and assignments
- Bidirectional synchronization
- Rate limiting compliance
- CID filtering in GitLab data

**API Endpoints:**
```python
@router.post("/projects/{project_id}/create-epic")
async def create_epic(project_id: UUID, epic_data: EpicCreateRequest):
    # Create epic in GitLab with CID-free structured description
    logger.info("Creating GitLab epic", project_id=str(project_id), epic_title=epic_data.title)
    pass

@router.post("/projects/{project_id}/create-tasks")
async def create_tasks(project_id: UUID, tasks: List[TaskCreateRequest]):
    # Batch create issues with dependencies, CID filtering
    logger.info("Creating GitLab tasks", project_id=str(project_id), task_count=len(tasks))
    pass

@router.get("/projects/permitted")
async def get_permitted_projects(current_user: User = Depends(get_current_user)):
    """
    Get all GitLab projects the current user is permitted to access.
    Returns a list of project metadata (id, name, gitlab_url, etc).
    """
    logger.info("Fetching permitted GitLab projects", user_id=current_user.id)
    # Implementation: Query GitLab API for permitted projects for current_user
    pass
```

### 4.6 Document Processing Service

**Technology Stack:**
- Python 3.11+ with FastAPI
- Apache Tika for document extraction
- Tesseract OCR for image processing
- Celery for background processing
- Azure Blob Storage client

**Processing Pipeline:**
```python
class DocumentProcessor:
    def __init__(self):
        self.cid_filter = CIDDetectionFilter()
        
    async def process_document(self, file_path: str) -> StructuredDocument:
        # 1. File type detection
        # 2. Text extraction (Tika/OCR)
        # 3. CID detection and filtering
        # 4. Structure analysis
        # 5. Metadata extraction
        # 6. JSON serialization
        logger.info("Document processing completed", 
                   file_path=file_path, 
                   processing_time=processing_time,
                   cid_detected=cid_detected)
        pass
    
    async def process_jira_archive(self, zip_path: str) -> List[JiraTicket]:
        # 1. Extract zip archive
        # 2. Parse JSON/XML files
        # 3. CID filtering and anonymization
        # 4. Normalize data structure
        # 5. Extract relationships
        # 6. Structured output generation
        pass
```

**Supported Formats:**
- PDF (including scanned documents)
- DOCX/DOC (Microsoft Word)
- XLSX/XLS (Excel spreadsheets)
- TXT (plain text)
- ZIP (JIRA archives)

### 4.7 Graph RAG Context Service

**Technology Stack:**
- Python 3.11+ with Neo4j driver
- FastAPI with async support
- Cypher query optimization
- Vector similarity search (1536 dimensions, cosine similarity)

**Responsibilities:**
- Operates as a Neo4j read-only service (no write operations to the graph).
- Receives a user prompt and optional context as input.
- Extracts entities from the prompt and context using NLP/LLM techniques.
- Executes multiple graph RAG retrieval strategies (e.g., semantic search, graph traversal, entity expansion) against the Neo4j knowledge graph.
- Aggregates and returns the relevant RAG context as a structured response to the caller.
- Supports flexible retrieval logic to optimize for different use cases (e.g., direct entity match, related requirements, evidence aggregation).

**Neo4j Schema:**
```cypher
// Core entity types (CID-free)
CREATE CONSTRAINT requirement_id IF NOT EXISTS FOR (r:Requirement) REQUIRE r.id IS UNIQUE;
CREATE CONSTRAINT entity_id IF NOT EXISTS FOR (e:Entity) REQUIRE e.id IS UNIQUE;
CREATE CONSTRAINT document_id IF NOT EXISTS FOR (d:Document) REQUIRE d.id IS UNIQUE;
CREATE CONSTRAINT jticket_id IF NOT EXISTS FOR (j:JiraTicket) REQUIRE j.id IS UNIQUE;

// Relationships
// Entity -> Requirement
(:Entity)-[:REFERENCED_BY]->(:Requirement)
// Requirement -> Document/JiraTicket (evidence)
(:Requirement)-[:EVIDENCED_BY]->(:Document)
(:Requirement)-[:EVIDENCED_BY]->(:JiraTicket)
// Requirement deduplication/merging
(:Requirement)-[:MERGED_FROM]->(:Requirement)
```

**Bidirectional Navigation:**
The `(:Entity)-[:REFERENCED_BY]->(:Requirement)` relationship enables both:
- Finding all entities referenced by a requirement
- Iterating through all entities and, for each, retrieving all related requirements

**Example Cypher Queries:**
- Iterate through all entities:
  ```cypher
  MATCH (e:Entity)
  RETURN e
  ```
- For each entity, find related requirements:
  ```cypher
  MATCH (e:Entity)-[:REFERENCED_BY]->(r:Requirement)
  RETURN e, collect(r) AS requirements
  ```
- For a specific entity:
  ```cypher
  MATCH (e:Entity {name: "SomeEntity"})-[:REFERENCED_BY]->(r:Requirement)
  RETURN r
  ```

**Query Optimization:**
- Semantic search with 1536-dimensional cosine similarity on requirements and entities
- Requirement-centric retrieval: all evidence and entities are accessed via requirements
- Deduplication and merge history tracked via `MERGED_FROM` relationships

### 4.8 Graph RAG Ingestion Service (Requirement-Centric)

**Ingestion Pipeline:**
1. Get parsed documents or JIRA tickets as input.
2. Extract requirements from text using NLP/LLM.
3. For each requirement, extract referenced entities.
4. For each requirement, compute embedding and search for similar requirements (vector similarity).
5. If similar requirements are found, review and merge, keeping the latest as canonical and linking older via `MERGED_FROM`.
6. Create relationships:
   - `(:Entity)-[:REFERENCED_BY]->(:Requirement)`
   - `(:Requirement)-[:EVIDENCED_BY]->(:Document)`
   - `(:Requirement)-[:EVIDENCED_BY]->(:JiraTicket)`
   - `(:Requirement)-[:MERGED_FROM]->(:Requirement)` (if merged)
7. Store deduplicated, requirement-centric graph in Neo4j.

**Supported Formats:**
- PDF, DOCX, XLSX, TXT (for document evidence)
- JIRA archives (for ticket evidence)

## 5. Data Architecture

### 5.1 Neo4j Graph Schema
```cypher
// Project knowledge graph (no customer data)
(:Entity)-[:REFERENCED_BY]->(:Requirement)
(:Requirement)-[:EVIDENCED_BY]->(:Document)
(:Requirement)-[:EVIDENCED_BY]->(:JiraTicket)
(:Requirement)-[:MERGED_FROM]->(:Requirement)
// Temporal and provenance relationships can be added as needed
```

### 5.2 Data Flow Architecture

#### A. User Interaction & Project Lifecycle
- User authenticates via Azure AD SSO.
- Project is created and managed via the Frontend and Project Management Service (PostgreSQL).
- Documents/JIRA archives are uploaded and stored in Azure Blob Storage.

#### B. Document & JIRA Processing
- Document Processing Service extracts and structures content from uploaded files.
- Structured data is CID-filtered and sent to the Graph RAG Ingestion Service.

#### C. Requirements & Entity Extraction
- Graph RAG Ingestion Service extracts requirements and entities from structured data.
- Deduplication and merging of requirements is performed, including already existing in Neo4j requirements.
- Embeddings are generated and the requirement-centric graph is constructed and stored in Neo4j.

#### D. RAG Context Retrieval
- User submits a prompt via the Frontend.
- Graph RAG Context Service extracts entities and retrieves relevant context from Neo4j using semantic and graph-based queries.
- Aggregated RAG context is enhancing user prompt and sent to agents orchestrator for further processing.
- Agents orchestrator returns the generated requirements with score to the user.

#### E. Task Breakdown & GitLab Integration
- Requirements Processing Service generates tasks from requirements.
- User reviews tasks; on approval, tasks/epics are created in GitLab via the GitLab Integration Service.
- Task/epic status is synchronized back to the system upon creation.

#### F. Monitoring, Logging, and Audit
- All services log to Azure Log Analytics.

#### G. Backup & Recovery
- Automated backups for Neo4j (monthly) and PostgreSQL (daily) to Azure Blob Storage.

## 6. Security Architecture

### 6.1 Authentication & Authorization

**Azure AD Integration:**
- Single Sign-On (SSO) with SAML 2.0
- JWT token validation
- Role-based access control
- Group-based permissions

**Security Layers:**
```
Internet → VPN Gateway → Azure Application Gateway → API Gateway → Microservices
    ↓           ↓              ↓                      ↓              ↓
 VPN Access  WAF Protection  TLS 1.3             JWT Validation   RBAC + CID Filter
```

### 6.2 Data Protection

**CID Compliance:**
- No Customer Identification Data storage
- CID detection and filtering at ingestion

**Encryption:**
- TLS 1.3 for data in transit
- AES-256 encryption at rest
- Azure Key Vault for secrets
- Transparent data encryption for databases

### 6.3 Network Security

**VPN Network Configuration:**
- Azure VPN Gateway for secure access
- Application Gateway with WAF

**Network Topology:**
```
Internet → VPN Gateway → Private Virtual Network (East US 2)
                              ↓
                         Application Subnet
                         (All services: Frontend, API Gateway, Microservices, Data Layer)
```

## 7. Deployment Architecture

### 7.1 Multi-Environment Configuration

**Environment Structure:**
```yaml
# Development Environment
apiVersion: v1
kind: Namespace
metadata:
  name: agentic-ai-dev
  labels:
    environment: development

---
# QA Environment
apiVersion: v1
kind: Namespace
metadata:
  name: agentic-ai-qa
  labels:
    environment: qa

---
# Production Environment
apiVersion: v1
kind: Namespace
metadata:
  name: agentic-ai-prod
  labels:
    environment: production
```

**Environment-Specific Configurations:**
- **Dev**: Single replica, smaller resource limits, mock AI services
- **QA**: Production-like setup, full AI integration, automated testing
- **Prod**: Multiple replicas, full resource allocation, monitoring

### 7.2 Kubernetes Configuration

**Resource Specifications:**
```yaml
# Production service configuration
apiVersion: apps/v1
kind: Deployment
metadata:
  name: requirements-processing-service
  namespace: agentic-ai-prod
spec:
  replicas: 3
  selector:
    matchLabels:
      app: requirements-processing
  template:
    spec:
      containers:
      - name: requirements-processing
        image: requirements-processing:latest
        resources:
          requests:
            memory: "512Mi"
            cpu: "500m"
          limits:
            memory: "1Gi"
            cpu: "1000m"
        env:
        - name: ENVIRONMENT
          value: "production"
        - name: AZURE_OPENAI_ENDPOINT
          valueFrom:
            secretKeyRef:
              name: azure-openai-secret
              key: endpoint
```

### 7.3 CI/CD Pipeline

**Monthly Deployment Strategy:**
```yaml
# Azure DevOps Pipeline
trigger:
  - release/*

schedules:
- cron: "0 9 * * 5"  # Last Friday of month
  displayName: Monthly Production Deployment
  branches:
    include:
    - main
  always: false

stages:
- stage: Build
  jobs:
  - job: BuildServices
    steps:
    - task: Docker@2
      displayName: 'Build and Push Images'
      
- stage: DeployDev
  jobs:
  - job: DeployToAKS
    steps:
    - task: KubernetesManifest@0
      displayName: 'Deploy to Dev Environment'
      
- stage: DeployQA
  dependsOn: DeployDev
  jobs:
  - job: DeployToAKS
    steps:
    - task: KubernetesManifest@0
      displayName: 'Deploy to QA Environment'
      
- stage: DeployProd
  dependsOn: DeployQA
  jobs:
  - job: DeployToAKS
    steps:
    - task: KubernetesManifest@0
      displayName: 'Deploy to Production'
```

## 8. Monitoring & Observability

### 8.1 Azure Standard Monitoring Stack

**Application Monitoring:**
- Azure Application Insights for application performance
- Azure Monitor for infrastructure metrics
- Azure Log Analytics for centralized logging
- Azure Alerts for proactive monitoring

**Key Metrics:**
- Request latency (P50, P95, P99)
- Error rates by service
- AI processing times (target: < 2 minutes)
- Database query performance
- Resource utilization (100 user scale)

### 8.2 Common Logging Configuration Rule for Microservices

All microservices must use a unified logging configuration as defined in the shared `logging_config.py` module. This standard ensures consistent, secure, and traceable logging across the system.

**Requirements:**
- **Sensitive Data Filtering:**
  - Fields such as `password`, `security_key`, `secret`, and `token` must be filtered from all logs
- **Correlation IDs:**
  - All log entries must include correlation IDs (e.g., `request_id`, `project_id`, `user_id`) for traceability
- **Log Rotation:**
  - Log files must use rotation: maximum 10MB per file, up to 5 backup files
  - Both file and console handlers must be configured
- **Log Format:**
  - Use the current format as defined in the architecture
- **Logs should be collected by Azure Log Analytics:**
  - Logs should be shipped to centralized aggregation systems

**Implementation:**
- Use the provided `logging_config.py` module in each microservice:

```python
import logging
from logging.handlers import RotatingFileHandler
import structlog
import os
from azure.monitor.opentelemetry import configure_azure_monitor

# Configure Azure Monitor integration
configure_azure_monitor(
    connection_string="InstrumentationKey=your-app-insights-key"
)

SENSITIVE_FIELDS = ['password', 'security_key', 'secret', 'token']

class SensitiveDataFilter(logging.Filter):
    def filter(self, record):
        for field in SENSITIVE_FIELDS:
            if hasattr(record, field):
                setattr(record, field, '[FILTERED]')
        return True

def configure_logging(log_file='/var/log/app.log', log_level=logging.INFO):
    handlers = [
        RotatingFileHandler(log_file, maxBytes=10*1024*1024, backupCount=5),
        logging.StreamHandler()
    ]
    logging.basicConfig(
        level=log_level,
        format='%(asctime)s - %(name)s - %(levelname)s - %(funcName)s:%(lineno)d - %(message)s',
        handlers=handlers
    )
    for handler in handlers:
        handler.addFilter(SensitiveDataFilter())
    structlog.configure(
        wrapper_class=structlog.make_filtering_bound_logger(log_level),
        processors=[
            structlog.processors.TimeStamper(fmt="iso"),
            structlog.processors.add_log_level,
            structlog.processors.StackInfoRenderer(),
            structlog.processors.format_exc_info,
            structlog.processors.JSONRenderer()
        ]
    )
```

- All microservices must import and call `configure_logging()` at startup.
- All log statements must include correlation IDs.
- This rule is mandatory for all new and existing microservices.

## 9. Performance & Scalability

### 9.1 Performance Targets

**Response Time SLAs:**
- UI operations: < 1 second
- API calls: < 2 seconds
- AI processing: < 2 minutes
- Document processing: < 5 minutes per document
- Vector similarity search: < 500ms

**Scalability Targets:**
- 100 concurrent users (maximum)
- 100 projects with 10 concurrent processing
- 1000 AI requests per hour
- 10,000 requirements
- 100,000 tasks

### 9.2 Scaling Strategy

**Horizontal Scaling:**
- Kubernetes Horizontal Pod Autoscaler
- PostgreSQL read replicas
- Neo4j clustering (if needed)
- Azure Application Gateway load balancing

## 10. Backup & Recovery

### 10.1 Backup Strategy

**Neo4j Monthly Backups:**
- Automated monthly backups (first Friday after deployment)
- Point-in-time recovery capability
- Backup verification and testing
- Azure Blob Storage for backup retention

**PostgreSQL Backups:**
- Daily automated backups
- Point-in-time recovery
- Cross-region backup replication
- 30-day retention policy