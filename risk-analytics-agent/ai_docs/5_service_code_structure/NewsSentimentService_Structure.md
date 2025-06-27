# Project Boilerplate: News Sentiment Component

This document outlines the proposed folder and file structure for the News Sentiment microservices.

```
risk-analytics-agent/
│
├── services/
│   │
│   ├── data-ingestion-service/
│   │   ├── app/
│   │   │   ├── __init__.py
│   │   │   ├── main.py             # Entrypoint for the cron job
│   │   │   ├── core/
│   │   │   │   ├── __init__.py
│   │   │   │   └── logic.py        # Core ingestion and deduplication logic
│   │   │   └── clients/
│   │   │       ├── __init__.py
│   │   │       ├── cosmos_client.py
│   │   │       └── service_bus_client.py
│   │   ├── tests/
│   │   │   ├── __init__.py
│   │   │   └── test_logic.py
│   │   ├── Dockerfile
│   │   └── requirements.txt
│   │
│   ├── news-processor-service/
│   │   ├── app/
│   │   │   ├── __init__.py
│   │   │   ├── main.py             # FastAPI application entrypoint
│   │   │   ├── api/
│   │   │   │   └── v1/
│   │   │   │       └── ... (if exposing any utility endpoints)
│   │   │   ├── core/
│   │   │   │   ├── __init__.py
│   │   │   │   └── processing.py   # Two-step AI enrichment logic
│   │   │   └── services/
│   │   │       ├── __init__.py
│   │   │       ├── cosmos_db.py
│   │   │       └── service_bus_listener.py
│   │   ├── tests/
│   │   │   ├── __init__.py
│   │   │   └── test_processing.py
│   │   ├── Dockerfile
│   │   └── requirements.txt
│   │
│   └── sentiment-score-api/
│       ├── app/
│       │   ├── __init__.py
│       │   ├── main.py             # FastAPI application entrypoint
│       │   ├── api/
│       │   │   ├── __init__.py
│       │   │   └── v1/
│       │   │       ├── __init__.py
│       │   │       └── endpoints.py # /realtime and /historical endpoints
│       │   ├── core/
│       │   │   ├── __init__.py
│       │   │   └── calculation.py  # Aggregated Sentiment Score (ASS) formula
│       │   └── services/
│       │       ├── __init__.py
│       │       └── cosmos_db.py
│       ├── tests/
│       │   ├── __init__.py
│       │   └── test_api.py
│       ├── Dockerfile
│       └── requirements.txt
│
├── shared/
│   └── models/
│       ├── __init__.py
│       └── pydantic_models.py      # Shared data models (RawNewsArticle, etc.)
│
├── .gitlab-ci.yml                  # GitLab CI/CD pipeline configuration
├── docker-compose.yml              # For local development and testing
└── README.md
```

### Key Components Explained

-   **`services/`**: This directory acts as a mono-repo for all microservices related to the News Sentiment component.
    -   **`data-ingestion-service/`**: A standalone application for the cron job (DIS).
    -   **`news-processor-service/`**: The FastAPI service for AI enrichment (NPS).
    -   **`sentiment-score-api/`**: The public-facing FastAPI for score calculation (NSSS).
-   **`shared/`**: This directory will be installable as a package. It contains code that is common across multiple services, starting with the Pydantic data models to ensure consistency.
-   **`Dockerfile`**: Each service has its own Dockerfile, defining how to build its container image. This is essential for both local development with `docker-compose` and deployment to AKS.
-   **`docker-compose.yml`**: A file at the root to orchestrate the running of all services locally for integration testing and development.
-   **`.gitlab-ci.yml`**: The configuration file for the GitLab CI/CD pipeline, which will define the stages for building, testing, and deploying the services to the Azure Kubernetes Service (AKS). 