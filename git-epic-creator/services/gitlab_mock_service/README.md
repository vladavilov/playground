# GitLab Mock Service

Mock GitLab service for local development and testing. Simulates GitLab OAuth authentication and REST API endpoints.

## Features

- **OAuth 2.0 Flow**: Complete OAuth authorization code flow with PKCE support
- **API v4 Endpoints**: Mock GitLab REST API endpoints for epics and issues
- **Realistic Responses**: Returns properly formatted JSON responses matching GitLab API structure

## Supported Endpoints

### OAuth Endpoints

- `GET /oauth/authorize` - OAuth authorization endpoint
- `POST /oauth/token` - Token exchange endpoint

### API v4 Endpoints

- `GET /api/v4/user` - Get current user information
- `GET /api/v4/groups/{group_id}/epics` - List group epics
- `GET /api/v4/groups/{group_id}/epics/{epic_iid}/issues` - List epic issues
- `GET /api/v4/projects/{project_id}/issues` - List project issues

### Health Check

- `GET /health` - Service health check

## Configuration

Environment variables:

- `GITLAB_CLIENT_ID` - OAuth client ID (default: `mock-gitlab-client-id`)
- `GITLAB_CLIENT_SECRET` - OAuth client secret (default: `mock-gitlab-secret`)
- `HOST` - Service host (default: `0.0.0.0`)
- `PORT` - Service port (default: `8000`)

## Running Locally

```bash
cd services/gitlab_mock_service
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate
pip install -e .
python src/main.py
```

## Running with Docker

```bash
docker build -t gitlab-mock-service .
docker run -p 8011:8000 gitlab-mock-service
```

## Mock Data

The service provides mock data for:

- 2 epics (Migration Planning Epic, Risk Analytics Modernization)
- 2 issues per epic (Analysis tasks)
- 1 mock user

All mock data includes realistic GitLab API response structures.

## Integration

Configure your application to use:

```
GITLAB_BASE_URL=http://gitlab-mock-service:8000
GITLAB_OAUTH_CLIENT_ID=mock-gitlab-client-id
GITLAB_OAUTH_CLIENT_SECRET=mock-gitlab-secret
```

## Development

The service is designed to be minimal and stateless, with in-memory storage for OAuth flows. Perfect for:

- Local development without external GitLab instance
- E2E testing of GitLab integration
- CI/CD pipelines

