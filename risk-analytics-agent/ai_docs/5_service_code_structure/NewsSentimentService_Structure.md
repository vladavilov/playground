## Project Structure

```
news_sentiment_service/
├── src/
│   ├── common/                    # Shared utilities and models
        └── service_bus/           # ServiceBus service
        └── cosmos_db/              # CosmosDB
│   ├── data_ingestion_[provider]/ # Example: data_ingestion_bazinga
│   ├── news_processor/            # News Processor Service (NPS)
│   ├── sentiment_api/             # Sentiment Score API Service (SSAS)
│   ├── historical_processor/      # Historical Processing Service (HPS)
|
├── tests/                         # Test suites
├── Readme.md                      # Documentation
├── scripts/                       # Deployment and utility scripts
└── config/                        # Configuration files
```