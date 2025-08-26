# UI Service

Serves a Tailwind HTML/JS UI and exposes an SSE endpoint `/events` that bridges Redis pubsub channel `ui:project_progress` to the browser.

- Configure `PM_API_BASE` env var to point to Project Management Service base URL (default: `http://localhost:8080`).
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


