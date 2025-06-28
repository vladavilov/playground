# Project Boilerplate: News Sentiment Component

```
risk-analytics-agent/
│
├── services/
│   │
│   ├── data-ingestion-service/         # Standalone application for the cron job (DIS)
│   │   ├── Dockerfile
│   │   └── ... # Service-specific application and test code
│   │
│   ├── news-processor-service/         # FastAPI service for AI enrichment (NPS)
│   │   ├── Dockerfile
│   │   └── ... # Service-specific application and test code
│   │
│   └── sentiment-score-api/            # Public-facing FastAPI for score calculation (NSSS)
│       ├── Dockerfile
│       └── ... # Service-specific application and test code
│
├── shared/                             # Shared, installable Python libraries
│   └── models/
│       └── pydantic_models.py      # Shared data models (RawNewsArticle, etc.)
│
├── .gitlab-ci.yml                      # GitLab CI/CD pipeline configuration
├── docker-compose.yml                  # For local development and testing
└── README.md
```

### Key Components Explained

-   **`services/`**: This directory acts as a mono-repo for all microservices. Each service is self-contained with its own Dockerfile and dependencies.
-   **`shared/`**: Contains code that is common across multiple services, starting with the Pydantic data models to ensure data consistency.
-   **`docker-compose.yml`**: Orchestrates the running of all services locally for integration testing.
-   **`.gitlab-ci.yml`**: Defines the CI/CD pipeline for building, testing, and deploying the services to Azure Kubernetes Service (AKS). 