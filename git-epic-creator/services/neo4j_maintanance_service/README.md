# Neo4j Graph RAG Schema Initialization Service

A one-call microservice to initialize the Neo4j Graph RAG schema for the Agentic AI Requirements Engineering System. This service creates the foundational graph database schema including nodes, relationships, and constraints required for the Graph RAG implementation.

## System Context

### Role in Agentic AI Architecture
This service is part of the **Database Architecture** component within the Agentic AI system. It supports the complete Graph RAG data flow:

```
Document Processing → Graph RAG Ingestion → Neo4j Schema ← Graph RAG Context Service
                                              ↑
                                    [This Service Initializes]
```

### Business Requirements Supported
- **Graph RAG Foundation**: Enables storage of requirement-centric graph data with semantic search capabilities
- **Schema Consistency**: Ensures proper constraints and indexes for data integrity
- **Vector Search**: Creates necessary indexes for embeddings (dimension size configurable via `VECTOR_INDEX_DIMENSIONS`) with cosine similarity

## Technical Specifications

### Neo4j Schema Initialization
Creates and configures:
- **Node Types**: Entity, Requirement, Document, JiraTicket with unique constraints
- **Relationships**: REFERENCED_BY, EVIDENCED_BY, MERGED_FROM
- **Constraints**: Unique constraints on ID properties for all node types
- **Vector Indexes**: Support for semantic search (configured separately in DB-003)

### Graph Schema Design
Initializes storage for the requirement-centric graph structure:
```cypher
// Node Types with Constraints
(:Entity {id: "string", name: "string", embedding: [float]})
(:Requirement {id: "string", text: "string", embedding: [float]})
(:Document {id: "string", name: "string", content: "string"})
(:JiraTicket {id: "string", key: "string", summary: "string"})

// Relationship Structure
(:Entity)-[:REFERENCED_BY]->(:Requirement)
(:Requirement)-[:EVIDENCED_BY]->(:Document)
(:Requirement)-[:EVIDENCED_BY]->(:JiraTicket)
(:Requirement)-[:MERGED_FROM]->(:Requirement)
```

## Architecture & Implementation

### Microservice Design Pattern
- **Language**: Python 3.11+
- **Framework**: FastAPI for HTTP API endpoint
- **Dependencies**: Neo4j Python driver, shared configuration library
- **Execution Model**: One-time schema initialization with idempotent operations
- **Container Strategy**: Dockerized service following established patterns

## Configuration

### Core Settings
| Parameter | Default | Description |
|-----------|---------|-------------|
| `NEO4J_URI` | `bolt://localhost:7687` | Neo4j connection URI |
| `NEO4J_USERNAME` | `neo4j` | Database username |
| `NEO4J_PASSWORD` | `password` | Database password |
| `NEO4J_DATABASE` | `neo4j` | Target database name |

### Environment-Specific Configuration
| Environment | Setting | Value |
|-------------|---------|-------|
| **Local Development** | `NEO4J_URI` | `bolt://localhost:7687` |
| **Local Development** | `NEO4J_USERNAME` | `neo4j` |
| **Production** | `NEO4J_URI` | Azure-managed Neo4j URI |

## Service Dependencies & Integration

### Upstream Dependencies
- **Neo4j Database**: Must be accessible and ready for connections
- **Network connectivity**: Service must be able to reach Neo4j instance

### Downstream Dependents  
Services that require this schema initialization to complete:
- **Graph RAG Ingestion Service**: Requires schema to store extracted data
- **Graph RAG Context Service**: Requires schema for semantic queries  

### Kubernetes Integration
```yaml
# Dependency chain in Kubernetes deployment
neo4j → neo4j-init-service → [graph-rag-ingestion, graph-rag-context]
```

## Operational Characteristics

### Startup Behavior
1. **Connection Test**: Verifies Neo4j database accessibility
2. **Schema Creation**: Creates nodes, relationships, and constraints
3. **Idempotent Operations**: Uses `IF NOT EXISTS` to prevent errors on re-runs
4. **Validation**: Confirms schema elements were created successfully

### API Endpoints
- **POST /init-neo4j**: Initialize the Neo4j Graph RAG schema
- **GET /health**: Health check endpoint

### Error Handling
- Connection failures with retry logic
- Constraint creation conflicts (idempotent handling)
- Comprehensive logging for troubleshooting

### Security & Compliance
- **CID-Free Schema**: No customer identification data in schema design
- **Secure Connections**: Support for encrypted Neo4j connections
- **Credential Management**: Integration with Azure Key Vault via shared configuration

## Implementation Details

### Schema Operations Performed
1. **Create Node Constraints**:
   - `Entity.id` unique constraint
   - `Requirement.id` unique constraint  
   - `Document.id` unique constraint
   - `JiraTicket.id` unique constraint

2. **Validate Schema**: Confirm all constraints and schema elements exist

3. **Error Recovery**: Handle existing constraints gracefully

### Performance Characteristics
- **Execution Time**: < 10 seconds for complete schema initialization
- **Resource Usage**: Minimal CPU/memory footprint
- **Scalability**: Designed for single execution per environment

This service ensures the Neo4j Graph RAG schema is properly initialized before any data ingestion or context retrieval operations begin. 