├── src/
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
│       │       ├── benzinga_adapter/     # Adapter for "Benzinga" news feed
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
│       │   ├── config/                  # Service-specific configuration files
│       │   │   ├── event_weights.yml
│       │   │   └── source_weights.yml
│       │   ├── tests/
│       │   ├── Dockerfile
│       │   └── requirements.txt
│       │
│       ├── infrastructure/              # Infrastructure as Code (Bicep)
│       │   ├── main.bicep             # Entry point for deployment (e.g., `az deployment group create`).
│       │   ├── environments/            # Contains parameter files for each environment.
│       │   │   ├── dev.bicepparam     # Parameters for the 'dev' environment.
│       │   │   └── prod.bicepparam    # Parameters for the 'prod' environment.
│       │   │
│       │   └── modules/                 # Reusable Bicep modules for each Azure resource.
│       │       ├── cosmosDb.bicep       # Provisions Cosmos DB, database, and container.
│       │       ├── serviceBus.bicep     # Provisions Service Bus and the processing queue.
│       │       ├── appConfig.bicep      # Provisions App Configuration store.
│       │       ├── keyVault.bicep       # Provisions Key Vault for secrets.
│       │       ├── appInsights.bicep    # Provisions Application Insights for monitoring.
│       │       ├── openAi.bicep         # Provisions Azure OpenAI service.
│       │       ├── managedIdentity.bicep # Defines User-Assigned Managed Identities.
│       │       └── roleAssignments.bicep# Assigns permissions for identities to access resources.
│       │
│       ├── scripts/                     # Utility and deployment scripts (e.g., seeding, migration)
│       │   └── seed_database.py       # Script for populating initial data
│       └── README.md                    # Service-level documentation
│
├── docker-compose.yml                   # Defines local multi-service development environment
└── README.md                            # Monorepo root README