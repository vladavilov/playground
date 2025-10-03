# AI Tasks Service

Agentic epic/task generation service that transforms validated requirements into a structured, actionable backlog.

## Overview

The AI Tasks Service specializes in:
- Backlog synthesis from requirements
- **Optimized duplicate detection** against existing GitLab epics/issues (reuses cached embeddings)
- Iterative improvement with self-evaluation loop
- Real-time progress updates via Redis Pub/Sub

## Architecture

Mirrors `ai_workflow_service` architecture:
- **LangGraph orchestration** with ensemble-of-experts pattern
- **Pure expert classes** with explicit I/O for testability
- **Redis pub/sub** for UI progress updates
- **FastAPI** with shared utilities and configuration

## API Endpoints

### POST /tasks/generate
Single chat-style endpoint for both initial generation and follow-up refinement.

**Headers:**
```
Authorization: Bearer <jwt_token>
X-GitLab-Access-Token: <gitlab_token>  (optional, forwarded from UI proxy)
```

**Request:**
```json
    {
      "project_id": "{uuid}",
  "prompt_id": "{uuid}",  // Optional: omit for new conversation
  "message": "Generate epics and tasks for user authentication system",
  "options": {
    "top_k": 2,
    "similarity_threshold": 0.83,
    "max_iters": 3
  }
}
```

**Response:**
```json
{
      "prompt_id": "{uuid}",
  "project_id": "{uuid}",
  "epics": [...],
  "score": 0.81,
  "clarification_questions": [...]  // If score < target
}
```

### GET /health
Liveness/readiness checks including Redis and GraphRAG connectivity.

## Integration Points

### GraphRAG Retrieval Service
- `POST {GRAPH_RAG_BASE_URL}/retrieve`
- Provides technical context for epic/task synthesis

### GitLab Client Service  
- `GET {GITLAB_INGESTION_BASE_URL}/gitlab/projects/{project_id}/backlog`
- Fetches existing epics/issues with cached embeddings for duplicate detection
- **Performance**: Reuses embeddings from Redis cache, only computes embeddings for new items

## Configuration

See `src/config.py` for all environment variables. Key settings:

```env
# Service URLs
GRAPH_RAG_BASE_URL=http://neo4j-retrieval-service:8000
GITLAB_INGESTION_BASE_URL=http://gitlab-client-service:8000
REDIS_URL=redis://redis:6379

# Azure OpenAI (required)
OAI_BASE_URL=https://your-resource.openai.azure.com
OAI_KEY=your-azure-openai-key
OAI_API_VERSION=2024-02-01
OAI_MODEL=gpt-4
OAI_EMBED_MODEL=text-embedding-3-small

# Workflow parameters
CLARIFICATION_SCORE_TARGET=0.75
SIMILARITY_THRESHOLD=0.83
MAX_AGENT_ITERS=3
```

## Development

```bash
cd services/ai_tasks_service
pip install -e .[dev]
pytest tests/
```

## Docker

```bash
docker-compose up ai-tasks-service
```
