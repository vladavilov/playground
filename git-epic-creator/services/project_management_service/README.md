# Project Management Service

A FastAPI microservice for managing projects in the Agentic AI Requirements Engineering System.

## Features

- Project CRUD operations with Azure AD authentication
- Role-based access control (Admin, Project Manager, Contributor)
- Project member management
- PostgreSQL integration with connection pooling
- Comprehensive API documentation

## API Endpoints

- `GET /projects` - List projects (filtered by user access)
- `POST /projects` - Create new project (Admin only)
- `GET /projects/{project_id}` - Get project details
- `PUT /projects/{project_id}` - Update project
- `DELETE /projects/{project_id}` - Delete project (Admin only)
- `GET /projects/{project_id}/members` - List project members
- `POST /projects/{project_id}/members` - Add project member
- `DELETE /projects/{project_id}/members/{user_id}` - Remove project member

## Authentication

All endpoints require Azure AD authentication. Role-based access control is enforced based on Azure AD groups.

## Development

```bash
# Install dependencies
pip install -e ".[dev]"

# Run tests
pytest

# Start service
uvicorn main:app --reload --port 8001
```