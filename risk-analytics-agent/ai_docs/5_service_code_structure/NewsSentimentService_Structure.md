├── services/
│   └── news_sentiment_service/          # Domain-bounded context for News Sentiment
│       ├── common/                      # Shared library for all services
│       │   ├── models/                  # Pydantic domain models
│       │   ├── cosmos_db/               # CosmosDB client
│       │   ├── service_bus/             # Service Bus client
│       │   └── tests/
│       │
│       ├── data_ingestion/              # Components for news ingestion
│       │   ├── orchestrator_service/    # Orchestrates ingestion from multiple adapters (DIS)
│       │   │   ├── src/
│       │   │   ├── tests/
│       │   │   ├── Dockerfile
│       │   │   └── requirements.txt
│       │   │
│       │   └── adapters/                # Provider-specific adapter microservices
│       │       ├── bazinga_adapter/     # Adapter for "Bazinga" news feed
│       │       │   ├── src/
│       │       │   ├── tests/
│       │       │   ├── Dockerfile
│       │       │   └── requirements.txt
│       │       │
│       │       └── _adapter_template/   # Template for creating new adapters
│       │           ├── src/
│       │           ├── tests/
│       │           ├── Dockerfile.template
│       │           └── requirements.txt
│       │
│       ├── news_processor_service/      # AI-enrichment of articles (NPS)
│       │   ├── src/
│       │   ├── tests/
│       │   ├── Dockerfile
│       │   └── requirements.txt
│       │
│       ├── sentiment_api_service/       # Serves aggregated sentiment scores (NSSS)
│       │   ├── src/
│       │   ├── tests/
│       │   ├── Dockerfile
│       │   └── requirements.txt
│       │
│       ├── infrastructure/              # Infrastructure as Code (Bicep)
│       │   ├── environments/
│       │   └── modules/
│       │
│       ├── scripts/                     # Utility and deployment scripts
│       └── README.md                    # Service-level documentation
│
├── docker-compose.yml                   # Defines local multi-service development environment
└── README.md                            # Monorepo root README