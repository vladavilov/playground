## Project Structure

```
news_sentiment_service/
├── src/
│   ├── common/                    # Shared utilities and models
│   ├── data_ingestion_[provider]/ # Example: data_ingestion_bloomberg
│   ├── news_processor/           # News Processor Service (NPS)
│   ├── sentiment_api/            # Sentiment Score API Service (SSAS)
│   ├── historical_processor/     # Historical Processing Service (HPS)
│   └── infrastructure/           # Infrastructure utilities
├── tests/                        # Test suites
├── docs/                         # Documentation
├── scripts/                      # Deployment and utility scripts
├── k8s/                         # Kubernetes manifests
├── docker/                      # Docker configurations
└── config/                      # Configuration files
```