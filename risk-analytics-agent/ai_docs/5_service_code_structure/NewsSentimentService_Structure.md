```
services/
└── news_sentiment_service/          # Domain-bounded context for News Sentiment
    ├── common/                      # Reusable utilities shared across sub-services
    │   ├── models/                  # Pydantic domain models (Entities, Events, …)
    │   ├── cosmos_db/               # CosmosDB client abstraction
    │   └── service_bus/             # Service Bus client abstraction
    |   └── tests/                   # Tests for common
    │
    ├── data_ingestion_bazinga/      # Real-Time News Ingestion Service (RTN)
    │   ├── src/
    │   ├── tests/
    │   ├── Dockerfile
    │   └── requirements.txt
    │
    ├── news_processor/              # News Scoring Service (NSS)
    │   ├── src/
    │   ├── tests/
    │   ├── Dockerfile
    │   └── requirements.txt
    │
    ├── sentiment_api/               # Sentiment Score API Service (SSAS)
    │   ├── src/
    │   ├── tests/
    │   ├── Dockerfile
    │   └── requirements.txt
    │
    ├── historical_processor/        # Historical Processing Service (HPS)
    │   ├── src/
    │   ├── tests/
    │   ├── Dockerfile
    │   └── requirements.txt
    │
    ├── README.md                    # Service-level documentation
    └── scripts/

README.md                            # Monorepo overview
```