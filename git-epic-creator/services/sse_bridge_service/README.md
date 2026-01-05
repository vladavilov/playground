# Gateway Control Plane Service

This service is an **SSE-only** gateway component:

- Bridges **Redis Pub/Sub → Server-Sent Events (SSE)** on `/events`
- Contains **no authentication**, **no sessions**, and **no ext_authz** logic

Authentication is enforced by **Envoy `ext_authz`**, which calls `authentication_service /authz`.

## Endpoints

- **Realtime**
  - `GET /events`: SSE stream (authentication enforced by Envoy)
- **Health**
  - `GET /health`

## Environment variables

This service relies on the shared Redis configuration (`REDIS_URL`, `REDIS_PASSWORD`, etc.) provided via the shared config map / env file.


