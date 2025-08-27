# UI Service

Serves a Tailwind HTML/JS UI and exposes an SSE endpoint `/events` that bridges Redis pubsub channel `ui:project_progress` to the browser.

- Configure `PROJECT_MANAGEMENT_SERVICE_URL` env var to point to Project Management Service base URL used by the server (default in Docker: `http://project-management-service:8000`, local: `http://localhost:8003`).
- The browser calls the UI service only; all PM-service requests are proxied via `GET/POST/PUT/DELETE /api/*`.
- Endpoints consumed:
  - GET /projects
  - POST /projects
  - PUT /projects/{id}
  - DELETE /projects/{id}
  - POST /projects/{id}/documents/upload (multipart form data)

Run locally

```
pip install -e .
python -m ui_service.main
```


