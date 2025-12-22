# Database Initialization Service

A FastAPI microservice for initializing and managing PostgreSQL database schema.

## Features

- Database schema initialization from SQLAlchemy models
- Automatic table creation based on shared model definitions
- Schema synchronization: drops and recreates tables to match current models

## API Endpoints

- `POST /db/init` - Initialize database schema (drops and recreates tables)

## Database Initialization

The `/db/init` endpoint initializes the database by:

1. **Dropping existing tables** if they exist (to ensure schema matches current models)
2. **Creating all tables** defined in the shared models

This ensures the database schema always matches the latest model definitions from the shared library.

### Current Schema

The service creates the following tables:

- **`projects`** - Project management table with fields:
  - `id` (UUID, primary key)
  - `name` (String 255, required)
  - `description` (Text, optional)
  - `gitlab_repository_url` (Text, optional) - Single GitLab repository URL for source code
  - `gitlab_backlog_project_ids` (ARRAY[String], optional) - Resolved GitLab project IDs for backlog
  - `gitlab_backlog_project_urls` (ARRAY[Text], optional) - GitLab project URLs for backlog
  - `status` (String 50, default: 'active')
  - `processed_pct` (Float, default: 0.0)
  - `created_by` (String 255, required)
  - `created_at` (DateTime)
  - `updated_at` (DateTime)

- **`project_members`** - Project member assignments table with fields:
  - `id` (UUID, primary key)
  - `project_id` (UUID, required)
  - `user_id` (String 255, required)
  - `role` (String 50, required)
  - `created_by` (String 255, required)
  - `created_at` (DateTime)
  - `updated_at` (DateTime)

- **`project_repo_indexes`** - Deterministic repo index (manifest) table with fields:
  - `id` (UUID, primary key)
  - `project_id` (UUID, required)
  - `repo_fingerprint` (Text, required)
  - `repo_index_json` (JSONB, required) - Deterministic repo manifest used for canonical naming/dedup support
  - `content_sha256` (Text, required) - sha256 over canonical JSON bytes (determinism gate)
  - `created_by` (String 255, required)
  - `created_at` (DateTime)
  - `updated_at` (DateTime)

## Authentication

All endpoints require local authentication via `get_local_user_verified` dependency.

## Usage

```bash
# Initialize database schema
curl -X POST http://localhost:PORT/db/init \
  -H "Authorization: Bearer <token>"
```

## Development

```bash
# Install dependencies
pip install -e ".[dev]"

# Run tests
pytest

# Start service
uvicorn main:app --reload --port PORT
```

## Notes

- **Destructive Operation**: The initialization endpoint drops existing tables before recreating them. All data will be lost.
- **Schema Synchronization**: This approach ensures the database schema always matches the current model definitions without requiring migration scripts.
- **No Backward Compatibility**: This service does not support schema migrations or backward compatibility. Use for development and fresh database setup only.
