# Business Requirements Document
# Agentic AI Requirements Engineering System
## 1. Executive Summary

### 1.1 Project Overview
The Agentic AI Requirements Engineering System is a cloud-native, microservices-based platform designed for UBS enterprise environment that transforms high-level feature descriptions into structured requirements and actionable task breakdowns. The system leverages orchestrated AI experts to validate, enrich, and structure requirements while maintaining project-specific context through Retrieval-Augmented Generation (RAG) capabilities. Built specifically for UBS infrastructure with Azure AD authentication and internal GitLab integration, the system handles large-scale document processing with background job execution to build knowledge base for providing sharp requirements.

### 1.2 Business Value Proposition
- **Accelerated Requirements Engineering**: Reduce requirements analysis time from weeks to hours
- **Consistency & Quality**: Ensure standardized, well-structured requirements across projects
- **Context Awareness**: Leverage existing project knowledge for coherent, system-aware requirements
- **Seamless Workflow Integration**: Direct integration with GitLab for epic and task creation
- **Expert Knowledge Orchestration**: Simulate multiple domain experts for comprehensive requirements validation

### 1.3 Success Metrics
- 80% reduction in requirements analysis time (from weeks to hours)
- 95% user satisfaction with generated requirements quality
- 90% of generated tasks require minimal manual editing
- 100% successful GitLab integration for supported project types

## 2. Business Requirements

### 2.1 Core Functional Requirements

#### FR-001: High-Level Feature Input Processing
**Priority:** Critical  
**Description:** The system shall accept high-level feature descriptions and generate comprehensive clarifying questions.

**Acceptance Criteria:**
- Users can input feature descriptions via text interface (minimum 10 words, maximum 5000 words)
- System generates 5-15 relevant clarifying questions within 30 seconds
- Questions cover functional, non-functional, technical, and business aspects
- Questions are categorized by domain (Business Logic, Technical Architecture, User Experience, Compliance, Integration)
- System provides context-aware questions based on project RAG data when available

#### FR-002: AI Expert Orchestration Engine
**Priority:** Critical  
**Description:** The system shall orchestrate multiple AI experts to validate, enrich, and structure requirements.

**Acceptance Criteria:**
- Minimum 5 AI expert personas: Business Analyst, Technical Architect, UX Designer, QA Specialist, Domain Expert (configurable: Financial Advisor, Risk Manager, Fixed Income Trader, etc.)
- Sequential expert processing: Business Analyst → Technical Architect → UX Designer → QA Specialist → Domain Expert
- Expert consensus scoring system (minimum 70% agreement threshold)
- Audit trail of expert contributions stored in logs
- If consensus score < 70%, system prompts user with gap-filling questions
- Configurable and extensible Domain Expert personas based on project needs
- Results returned in markdown format

#### FR-003: Requirements Editing and Refinement
**Priority:** High  
**Description:** Users shall be able to edit and refine AI-generated requirements through an intuitive interface.

**Acceptance Criteria:**
- Rich text editor for adjusting proposed requirements
- Display expert persona scores for proposed requirements
- If score < 70%, show gap-filling questions to improve score and give user a possibility to re-run the orchestration engine with answers to the questions
- User can submit requirements and proceed to task breakdown

#### FR-004: Task Breakdown Generation
**Priority:** Critical  
**Description:** The system shall generate detailed task lists with requirement references and comprehensive descriptions.

**Acceptance Criteria:**
- Automatic technical task breakdown from finalized requirements
- Each task includes: title, description, acceptance criteria, requirement references, priority, dependencies
- Automatic test task creation with proposed test cases
- Task dependency identification and visualization
- Task prioritization recommendations
- User-editable task list, priorities, and descriptions
- Note: Wireframes and UI mocks to be added manually
- "Create in GitLab" button to proceed to GitLab integration

#### FR-005: GitLab Integration for Epic/Task Creation
**Priority:** High  
**Description:** The system shall create epics and tasks directly in internal UBS GitLab repositories.

**Acceptance Criteria:**
- Integration with internal UBS GitLab instance only
- Azure AD-based authentication for GitLab access (leveraging existing UBS SSO)
- Epic creation with structured description and metadata
- Task creation as GitLab issues with proper labels and assignments
- Support for UBS GitLab project templates and custom fields

#### FR-006: Project Management and Context Building
**Priority:** High  
**Description:** The system shall provide comprehensive project management with possibility to add RAG-enhanced context.

**Acceptance Criteria:**
- Project creation with name, description, and metadata (admin-only)
- Internal UBS GitLab repository attachment and synchronization
- Multi-format file upload support (PDF, DOCX, XLSX, TXT) with large-scale processing (up to 2,500 pages per project)
- Graph RAG index building from uploaded documents and GitLab data (background processing)
- Project-scoped Graph RAG with high-level cross-project references
- Context-aware requirement generation using project-specific knowledge
- Project-level user access control via Azure AD groups
- **Optional Data Sources**:
  - UBS internal Confluence pages (automatic scraping)
  - JIRA archive as zip file
  - Code repositories (automatic processing)
    
### 2.2 Non-Functional Requirements

#### NFR-001: Performance Requirements
- **Response Time:** UI operations complete within 1 second; AI processing executed asynchronously in background (up to 2 minutes per request for requirements processing or task breakdown)
- **Throughput:** System supports 100 concurrent users with responsive UI interactions
- **Scalability:** Horizontal scaling architecture to handle large-scale document processing (up to 50 documents × 50 pages per project = 2,500 pages maximum)
- **Background Processing:** Long-running operations (Graph RAG creation, document processing, AI orchestration) executed asynchronously with real-time progress tracking and user navigation capability

#### NFR-003: Security Requirements
- **Authentication:** Azure Active Directory (Azure AD) integration with SSO - MANDATORY and ONLY authentication mechanism
- **Authorization:** Role-based access control (Admin, Project Manager, Contributor) via Azure AD groups
- **Data Encryption:** TLS 1.3 encryption for data in transit


### 2.3 Data Requirements

#### DR-001: Data Storage and Management
- **Requirements Repository:** Neo4j Graph RAG with vector indexing for all requirements with full history and recency-based scoring
- **Project Context Store:** Project-specific Graph RAG knowledge bases with cross-project high-level references
- **User Data Management:** No user or client data stored in application (Azure AD handles authentication)
- **Integration Data:** GitLab tokens stored in Azure Key Vault via Azure App Configuration
- **File Storage Policy:** No files stored after processing - only structured data and Graph RAG representations stored in the database

#### DR-002: Data Integration Requirements
- **GitLab API Integration:** Full UBS GitLab REST API v4 support with Azure AD authentication
- **File Processing Pipeline:** Automated extraction and indexing of uploaded documents with structured JSON output
- **Data Flow:** Document Processing → Structured JSON → Graph RAG Ingestion → Neo4j → RAG Context Service

## 3. System Architecture Overview

### 3.1 High-Level Architecture Principles
- **Cloud-Native Design:** Containerized microservices with Kubernetes orchestration
- **Event-Driven Architecture:** Asynchronous communication between services
- **API-First Approach:** RESTful APIs with OpenAPI 3.0 specifications
- **Resilience Patterns:** Circuit breakers, retry mechanisms, and graceful degradation

### 3.2 Core Microservices

#### 3.2.1 Frontend Service
- **Technology:** React.js Single Page Application with JavaScript, modern UI components (Material-UI or similar)
- **Responsibilities:** Web application interface, user management, project management, requirements management, task management, document upload, JIRA archive upload, GitLab authentication, background job progress tracking
- **Authentication:** Azure AD integration with SSO
- **Deployment:** Dockerized Node.js application
- **Features:** Responsive design, real-time updates

#### 3.2.2 API Gateway Service  
- **Technology:** Azure API Management or Kong
- **Responsibilities:** Azure AD authentication, rate limiting, request routing, API versioning
- **Security:** Azure AD JWT token validation, request/response filtering

#### 3.2.3 Requirements Processing Service
- **Technology:** Python with FastAPI, Celery for async processing
- **Responsibilities:** Feature description analysis, AI expert orchestration, requirements generation, integrated with (3.2.7) Graph RAG Context Service (Neo4j proxy) to enrich requirements with project-specific context
- **AI Integration:** Azure OpenAI GPT-4.1 on private Azure infrastructure
- **Deployment:** Dockerized python application
- **Graph RAG Integration:** Consume context data via RAG Context Service API calls to Neo4j-backed knowledge graph

#### 3.2.4 Project Management Service
- **Technology:** Python with FastAPI
- **Responsibilities:** Project CRUD operations, get projects list, get project details, RBAC validation for project access by current user
- **Deployment:** Dockerized python application
- **Database:** PostgreSQL with row-level security

#### 3.2.5 GitLab Integration Service
- **Technology:** Python with aiohttp for async operations
- **Responsibilities:** GitLab API integration, epic/issue creation, bidirectional sync
- **Security:** OAuth2 token management, rate limiting
- **Deployment:** Dockerized python application

#### 3.2.6 Document Processing Service
- **Technology:** Python with Apache Tika, OCR capabilities, async processing queues, zip file extraction
- **Responsibilities:** 
  - **Document Processing**: Large-scale file upload processing (PDF, DOCX, XLSX, TXT), text extraction, OCR, document indexing
  - **Document Structuring**: Convert unstructured documents to structured JSON format containing metadata, source, content, relationships, and parsed table/image text
  - **JIRA Archive Processing**: 
    - Extract and parse JIRA tickets, comments, attachments, and metadata from zip files
    - Support standard JIRA export formats (JSON, XML within zip archives)
    - Preserve ticket relationships (blocks, relates to, duplicates, hierarchies) in structured format
    - Parse JIRA custom fields, labels, components, and project-specific configurations
    - Data normalization: merge duplicates, filter cancelled/deleted issues, anonymize users for privacy compliance
    - Output structured JIRA data in standardized JSON format for Graph RAG processing
- **Storage:** Azure Blob Storage with file versioning and large file support (temporary zip storage during processing)
- **Processing:** Background job processing for large document collections (up to 2500 pages per project) and JIRA archive extraction

#### 3.2.7 Graph RAG Context Service (Neo4j Proxy)
- **Technology:** Python with Neo4j driver, FastAPI, async processing, Cypher query optimization
- **Database:** Neo4j Graph Database for Graph RAG implementation
- **Responsibilities:** 
  - Neo4j proxy layer: Translate microservice requests to Cypher queries
  - Graph RAG data retrieval: Execute semantic search and knowledge retrieval from Neo4j, combining with Drift Search
  - Query optimization: Transform complex business queries into efficient Cypher statements
  - Cross-reference resolution: Navigate relationships between main terms in Neo4j Graph, using Drift Search
  - Context aggregation: Combine related graph nodes for comprehensive context delivery
- **Neo4j Integration Features:**
  - Graph schema management for project-specific knowledge structures
  - Vector similarity search integration with Neo4j vector indexes
  - Real-time query performance optimization
  - Graph traversal algorithms for contextual data discovery
- **API Layer:** RESTful endpoints for other microservices to query Graph RAG data
- **Scalability:** Neo4j clustering support for high-availability and horizontal scaling

#### 3.2.8 Graph RAG Ingestion Service
- **Technology:** Python with Neo4j driver, Celery for background processing, Azure OpenAI embeddings
- **Responsibilities:**
  - Data ingestion orchestration: Load structured data from Document Processing Service into Neo4j
  - Graph construction: Create nodes and relationships from JIRA, GitLab, and document data, creating entities and relationships, but having metadata (ducoment names, jira numbers, etc.) only as evidence. Entities and relatonships creation should be done using clearly prompted LLM.
  - Multi-source data merging: Combine structured (JIRA, GitLab) and unstructured (documents) content
  - Vector embedding generation: Create embeddings for semantic search using Azure OpenAI
  - Schema evolution: Manage Neo4j schema updates as new data sources are added
  - Data deduplication: Handle duplicate content across different data sources
  - Data timestamps: Store data timestamps to configure higher weights for recent data and provide evidences for the retrieval.
- **Processing Pipeline:**
  - Consume structured JSON from Document Processing Service
  - Transform data into Neo4j graph nodes and relationships
  - Generate and store vector embeddings for semantic search
  - Create cross-references between different data types

### 3.3 Data Layer Architecture

#### 3.3.1 Primary Database
- **Technology:** PostgreSQL 15+ with read replicas
- **Schema:** Projects
- **Backup Strategy:** Point-in-time recovery with automated backups

#### 3.3.2 Graph Database
- **Technology:** Neo4j with vector index capabilities for Graph RAG implementation
- **Purpose:** Graph RAG storage, semantic search, relationship traversal, knowledge discovery
- **Features:** Native vector similarity search, Cypher query language, ACID compliance
- **Scaling:** Neo4j clustering for high-availability and horizontal scaling
- **Integration:** Direct integration with RAG Context Service and Graph RAG Ingestion Service as primary data store

#### 3.3.4 Object Storage
- **Technology:** Azure Blob Storage
- **Usage:** Large-scale document storage
- **Security:** Azure encryption at rest, SAS tokens for secure access
- **Scalability:** Optimized for large file collections and high-volume document processing

### 3.4 Integration Architecture

#### 3.4.1 External AI Services
- **Primary:** OpenAI GPT-4 API
- **Rate Limiting:** Intelligent queuing and retry mechanisms

#### 3.4.2 Internal UBS GitLab Integration
- **API Version:** UBS GitLab REST API v4
- **Authentication:** Azure AD-based authentication (leveraging existing UBS SSO)
- **Webhooks:** Real-time notifications for project changes
- **Error Handling:** Exponential backoff, dead letter queues
- **Scope:** Internal UBS GitLab instance only (no external GitLab support)

## 4. User Workflow and Experience

### 4.1 Primary User Journey

#### 4.1.1 Project Setup
1. User creates new project with name and description
2. System prompts for GitLab repository connection
3. User uploads relevant documentation (optional)
4. System builds Graph RAG context in background
5. Project dashboard displays with ready status

#### 4.1.2 Requirements Generation
1. User enters high-level feature description
2. System generates clarifying questions within 30 seconds
3. User answers questions or modifies as needed
4. AI experts process responses and generate structured requirements
5. User reviews and edits requirements using built-in editor
6. User approves final requirements version

#### 4.1.3 Task Creation
1. System generates task breakdown from approved requirements
2. User reviews task list with descriptions and dependencies
3. User edits tasks, adds estimates, modifies priorities
4. User clicks "Create in GitLab" button
5. System creates epic and individual issues in GitLab
6. User receives confirmation with GitLab links

### 4.2 User Roles and Permissions

#### 4.2.1 System Administrator
- **Permissions:** Full system access, system configuration
- **Responsibilities:** System maintenance, security monitoring, user support

#### 4.2.2 Project Manager
- **Permissions:** Project creation, user assignment, requirements approval, document upload, JIRA archive upload, GitLab integration
- **Responsibilities:** Project oversight

#### 4.2.3 Contributor
- **Permissions:** Requirements creation and editing, task creation, review and submit
- **Responsibilities:** Requirements quality, stakeholder alignment, acceptance criteria

## 5. Technical Constraints and Assumptions

### 5.1 Technology Constraints
- **Container Platform:** Kubernetes 1.25+ required
- **Database:** PostgreSQL 15+ for ACID compliance, Neo4j for Graph RAG implementation
- **AI Services:** Azure OpenAI GPT-4.1 on private Azure infrastructure
- **Authentication:** Azure Active Directory (mandatory - no alternatives)
- **GitLab:** Internal UBS GitLab instance only
- **File Formats:** PDF, DOCX, XLSX, TXT only
- **Browser Support:** Modern browsers with ES2020 support

### 5.3 Assumptions
- **Azure Infrastructure:** Access to UBS private Azure environment with appropriate resource allocation
- **GitLab Access:** Users have appropriate UBS GitLab permissions via Azure AD
- **File Formats:** Large document collections in PDF, DOCX, XLSX, TXT formats
- **User Training:** Users familiar with basic project management concepts and Azure AD authentication
- **Data Policies:** Data retention, governance, and compliance policies to be defined separately (deferred)

### 5.4 References
- GitLab API Documentation v4
- OpenAI API Documentation
- Kubernetes Best Practices Guide
- OWASP Security Guidelines