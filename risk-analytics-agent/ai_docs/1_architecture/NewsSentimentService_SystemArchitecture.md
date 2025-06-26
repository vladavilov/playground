# News Sentiment Service - System Architecture

---

## Table of Contents
1. [Executive Summary](#1-executive-summary)
2. [High-Level Architecture Overview](#2-high-level-architecture-overview)
3. [C4 Architecture Diagrams](#3-c4-architecture-diagrams)
4. [Detailed Component Design](#4-detailed-component-design)
5. [Data Flow and Processing](#5-data-flow-and-processing)
6. [Scalability and Performance](#6-scalability-and-performance)
7. [Security and Monitoring](#7-security-and-monitoring)
8. [Deployment Architecture](#8-deployment-architecture)
9. [Future Enhancements](#9-future-enhancements)

---

## 1. Executive Summary

The News Sentiment Service is a critical component of the Fixed Income AI Risk Analytics System, responsible for ingesting, processing, and serving aggregated news sentiment scores for fixed-income instruments. This document outlines the complete system architecture, including all sub-components, data flows, technology choices, and design decisions.

The system follows a microservices architecture pattern with clear separation of concerns, enabling high scalability, maintainability, and extensibility. The architecture supports processing 10,000 articles/day with sub-500ms API response times.

### Key Architecture Decisions
- **Microservices Pattern**: 5 independent services with specific responsibilities
- **Event-Driven Processing**: Azure Service Bus for asynchronous message passing
- **AI-Powered Analysis**: GPT-4 integration for intelligent news classification
- **Single Source of Truth**: Unified Cosmos DB collection for all enriched news
- **Performance Optimization**: Redis caching with intelligent TTL management

---

## 2. High-Level Architecture Overview

### 2.1 System Components Overview

```mermaid
graph TB
    subgraph "External Sources"
        Bloomberg["Bloomberg API"]
        TE["TradingEconomics API"]
        Reuters["Reuters API"]
        FNS["Financial News Sources"]
    end

    subgraph "News Sentiment Component"
        subgraph "Abstraction Layer"
            NAL["News Adapter Layer<br/>-----------<br/>- Bloomberg Adapter<br/>- TradingEconomics Adapter<br/>- Reuters Adapter<br/>- Extensible Plugin System"]
        end

        subgraph "Data Ingestion Service"
            DIS["Data Ingestion Service<br/>-----------<br/>- Cron-based Scheduler<br/>- Rate Limiting<br/>- Error Handling<br/>- Batch Processing<br/>- Deduplication"]
        end

        subgraph "News Processing Service"
            NPS["News Processor Service<br/>-----------<br/>- GPT-4 Integration<br/>- Entity Extraction<br/>- Event Classification<br/>- Sentiment Analysis<br/>- Summary Generation"]
        end

        subgraph "Storage Layer"
            CDB["Azure Cosmos DB<br/>-----------<br/>- Enriched News Store<br/>- Indexed by CUSIP/Sector/Date<br/>- Partitioned by Date"]
        end

        subgraph "Sentiment Calculation Service"
            SCS["News Sentiment Score Service<br/>-----------<br/>- Real-Time API<br/>- Historical API<br/>- Aggregation Logic<br/>- Time-Decay Calculation<br/>- Caching Layer"]
        end
    end

    subgraph "Consumers"
        RMA["Risk Management Agent<br/>(Orchestrator Service)"]
        API["Public API Consumers"]
    end

    Bloomberg -->|"Raw Articles"| NAL
    TE -->|"Raw Articles"| NAL
    Reuters -->|"Raw Articles"| NAL
    FNS -->|"Raw Articles"| NAL

    NAL -->|"Normalized Articles"| DIS
    DIS -->|"Batch Articles"| NPS
    NPS -->|"Enriched Events"| CDB
    
    RMA -->|"GET /sentiment/realtime"| SCS
    RMA -->|"GET /sentiment/historical"| SCS
    API -->|"REST API Calls"| SCS
    
    SCS -->|"Query"| CDB

    style Bloomberg fill:#e1f5fe
    style TE fill:#e1f5fe
    style Reuters fill:#e1f5fe
    style FNS fill:#e1f5fe
    style RMA fill:#fff3e0
    style API fill:#fff3e0
    style NAL fill:#c8e6c9
    style DIS fill:#c8e6c9
    style NPS fill:#c8e6c9
    style CDB fill:#bbdefb
    style SCS fill:#c8e6c9
```

### 2.2 Technology Stack

- **Programming Language**: Python 3.11+
- **Web Framework**: FastAPI
- **Message Queue**: Azure Service Bus
- **Caching**: Redis (Azure Cache for Redis)
- **Database**: Azure Cosmos DB
- **Container Orchestration**: Azure Kubernetes Service (AKS)
- **Monitoring**: Azure Monitor, Application Insights
- **API Gateway**: Azure API Management

---

## 3. C4 Architecture Diagrams

### 3.1 Level 1: System Context

```mermaid
graph TB
  linkStyle default fill:#ffffff

  subgraph diagram ["News Sentiment Service - System Context"]
    style diagram fill:#ffffff,stroke:#ffffff

    1["<div style='font-weight: bold'>Bloomberg</div><div style='font-size: 70%; margin-top: 0px'>[Software System]</div><div style='font-size: 80%; margin-top:10px'>Financial news and data<br />provider</div>"]
    style 1 fill:#999999,stroke:#6b6b6b,color:#ffffff
    2["<div style='font-weight: bold'>TradingEconomics</div><div style='font-size: 70%; margin-top: 0px'>[Software System]</div><div style='font-size: 80%; margin-top:10px'>Economic data and news<br />provider</div>"]
    style 2 fill:#999999,stroke:#6b6b6b,color:#ffffff
    3["<div style='font-weight: bold'>Reuters</div><div style='font-size: 70%; margin-top: 0px'>[Software System]</div><div style='font-size: 80%; margin-top:10px'>Global news provider</div>"]
    style 3 fill:#999999,stroke:#6b6b6b,color:#ffffff
    4["<div style='font-weight: bold'>OpenAI GPT-4</div><div style='font-size: 70%; margin-top: 0px'>[Software System]</div><div style='font-size: 80%; margin-top:10px'>AI language model for text<br />analysis</div>"]
    style 4 fill:#999999,stroke:#6b6b6b,color:#ffffff
    6["<div style='font-weight: bold'>Orchestrator Service</div><div style='font-size: 70%; margin-top: 0px'>[Software System]</div><div style='font-size: 80%; margin-top:10px'>Central coordinator of risk<br />analytics workflow</div>"]
    style 6 fill:#1168bd,stroke:#0b4884,color:#ffffff
    7["<div style='font-weight: bold'>News Sentiment Service</div><div style='font-size: 70%; margin-top: 0px'>[Software System]</div><div style='font-size: 80%; margin-top:10px'>Ingests, processes, and<br />serves aggregated news<br />sentiment scores for<br />fixed-income instruments</div>"]
    style 7 fill:#dddddd,stroke:#9a9a9a,color:#000000

    1-. "<div>Provides news articles</div><div style='font-size: 70%'></div>" .->7
    2-. "<div>Provides news articles</div><div style='font-size: 70%'></div>" .->7
    3-. "<div>Provides news articles</div><div style='font-size: 70%'></div>" .->7
    7-. "<div>Sends articles for analysis</div><div style='font-size: 70%'></div>" .->4
    6-. "<div>Requests sentiment scores</div><div style='font-size: 70%'></div>" .->7
    7-. "<div>API calls</div><div style='font-size: 70%'></div>" .->1
    7-. "<div>API calls</div><div style='font-size: 70%'></div>" .->2
    7-. "<div>API calls</div><div style='font-size: 70%'></div>" .->3
  end
```

### 3.2 Level 2: Container Diagram

```mermaid
graph TB
  linkStyle default fill:#ffffff

  subgraph diagram ["News Sentiment Service - Containers"]
    style diagram fill:#ffffff,stroke:#ffffff

    1["<div style='font-weight: bold'>Bloomberg</div><div style='font-size: 70%; margin-top: 0px'>[Software System]</div><div style='font-size: 80%; margin-top:10px'>Financial news and data<br />provider</div>"]
    style 1 fill:#999999,stroke:#6b6b6b,color:#ffffff
    2["<div style='font-weight: bold'>TradingEconomics</div><div style='font-size: 70%; margin-top: 0px'>[Software System]</div><div style='font-size: 80%; margin-top:10px'>Economic data and news<br />provider</div>"]
    style 2 fill:#999999,stroke:#6b6b6b,color:#ffffff
    3["<div style='font-weight: bold'>Reuters</div><div style='font-size: 70%; margin-top: 0px'>[Software System]</div><div style='font-size: 80%; margin-top:10px'>Global news provider</div>"]
    style 3 fill:#999999,stroke:#6b6b6b,color:#ffffff
    4["<div style='font-weight: bold'>OpenAI GPT-4</div><div style='font-size: 70%; margin-top: 0px'>[Software System]</div><div style='font-size: 80%; margin-top:10px'>AI language model for text<br />analysis</div>"]
    style 4 fill:#999999,stroke:#6b6b6b,color:#ffffff
    6["<div style='font-weight: bold'>Orchestrator Service</div><div style='font-size: 70%; margin-top: 0px'>[Software System]</div><div style='font-size: 80%; margin-top:10px'>Central coordinator of risk<br />analytics workflow</div>"]
    style 6 fill:#1168bd,stroke:#0b4884,color:#ffffff

    subgraph 7 ["News Sentiment Service"]
      style 7 fill:#ffffff,stroke:#9a9a9a,color:#9a9a9a

      13["<div style='font-weight: bold'>Data Ingestion Service</div><div style='font-size: 70%; margin-top: 0px'>[Container: Python/AKS CronJob]</div><div style='font-size: 80%; margin-top:10px'>Orchestrates periodic news<br />collection</div>"]
      style 13 fill:#438dd5,stroke:#2e6295,color:#ffffff
      18["<div style='font-weight: bold'>News Processor Service</div><div style='font-size: 70%; margin-top: 0px'>[Container: Python/FastAPI]</div><div style='font-size: 80%; margin-top:10px'>Enriches raw news with AI<br />analysis</div>"]
      style 18 fill:#438dd5,stroke:#2e6295,color:#ffffff
      24[("<div style='font-weight: bold'>Azure Cosmos DB</div><div style='font-size: 70%; margin-top: 0px'>[Container: NoSQL Database]</div><div style='font-size: 80%; margin-top:10px'>Stores enriched news events</div>")]
      style 24 fill:#438dd5,stroke:#2e6295,color:#ffffff
      25[("<div style='font-weight: bold'>Redis Cache</div><div style='font-size: 70%; margin-top: 0px'>[Container: In-Memory Cache]</div><div style='font-size: 80%; margin-top:10px'>Caches sentiment calculations</div>")]
      style 25 fill:#438dd5,stroke:#2e6295,color:#ffffff
      26["<div style='font-weight: bold'>Sentiment Score API</div><div style='font-size: 70%; margin-top: 0px'>[Container: Python/FastAPI]</div><div style='font-size: 80%; margin-top:10px'>Calculates and serves<br />aggregated sentiment scores</div>"]
      style 26 fill:#438dd5,stroke:#2e6295,color:#ffffff
      31["<div style='font-weight: bold'>Azure Service Bus</div><div style='font-size: 70%; margin-top: 0px'>[Container: Message Queue]</div><div style='font-size: 80%; margin-top:10px'>Message queue for async<br />processing</div>"]
      style 31 fill:#438dd5,stroke:#2e6295,color:#ffffff
      8["<div style='font-weight: bold'>News Adapter Layer</div><div style='font-size: 70%; margin-top: 0px'>[Container: Python]</div><div style='font-size: 80%; margin-top:10px'>Provides unified interface to<br />multiple news sources</div>"]
      style 8 fill:#438dd5,stroke:#2e6295,color:#ffffff
    end

    1-. "<div>Provides news articles</div><div style='font-size: 70%'></div>" .->8
    2-. "<div>Provides news articles</div><div style='font-size: 70%'></div>" .->8
    3-. "<div>Provides news articles</div><div style='font-size: 70%'></div>" .->8
    18-. "<div>Sends articles for analysis</div><div style='font-size: 70%'></div>" .->4
    6-. "<div>Requests sentiment scores</div><div style='font-size: 70%'></div>" .->26
    13-. "<div>Fetches articles</div><div style='font-size: 70%'></div>" .->8
    13-. "<div>Queues articles for<br />processing</div><div style='font-size: 70%'></div>" .->31
    31-. "<div>Delivers articles</div><div style='font-size: 70%'></div>" .->18
    18-. "<div>Stores enriched events</div><div style='font-size: 70%'></div>" .->24
    26-. "<div>Queries enriched events</div><div style='font-size: 70%'></div>" .->24
    26-. "<div>Caches results</div><div style='font-size: 70%'></div>" .->25
    8-. "<div>API calls</div><div style='font-size: 70%'></div>" .->1
    8-. "<div>API calls</div><div style='font-size: 70%'></div>" .->2
    8-. "<div>API calls</div><div style='font-size: 70%'></div>" .->3
  end
```

---

## 4. Detailed Component Design

### 4.1 News Adapter Layer (NAL)

The News Adapter Layer provides a unified interface for multiple news sources using the Adapter pattern with Factory.

#### Class Architecture

```mermaid
classDiagram
    class NewsSourceAdapter {
        <<abstract>>
        +fetch_articles(since: datetime) List~RawNewsArticle~
        +authenticate() void
        #rate_limit: int
        #api_key: string
    }
    
    class BloombergAdapter {
        +fetch_articles(since: datetime) List~RawNewsArticle~
        +authenticate() void
        -parse_bloomberg_response(response: dict) List~RawNewsArticle~
    }
    
    class TradingEconomicsAdapter {
        +fetch_articles(since: datetime) List~RawNewsArticle~
        +authenticate() void
        -parse_te_response(response: dict) List~RawNewsArticle~
    }
    
    class ReutersAdapter {
        +fetch_articles(since: datetime) List~RawNewsArticle~
        +authenticate() void
        -parse_reuters_response(response: dict) List~RawNewsArticle~
    }
    
    class NewsAdapterFactory {
        +create_adapter(source_type: str) NewsSourceAdapter
        -adapters: dict
    }
    
    class RawNewsArticle {
        +article_text: str
        +source_name: str
        +publication_time: datetime
        +title: str
        +url: str
    }
    
    NewsSourceAdapter <|-- BloombergAdapter
    NewsSourceAdapter <|-- TradingEconomicsAdapter
    NewsSourceAdapter <|-- ReutersAdapter
    NewsAdapterFactory ..> NewsSourceAdapter : creates
    NewsSourceAdapter ..> RawNewsArticle : produces
```

#### Implementation Example

```python
# Abstract base adapter
class NewsSourceAdapter(ABC):
    @abstractmethod
    async def fetch_articles(self, since: datetime) -> List[RawNewsArticle]:
        pass
    
    @abstractmethod
    def authenticate(self) -> None:
        pass

# Concrete implementations
class BloombergAdapter(NewsSourceAdapter):
    # Bloomberg-specific implementation

class TradingEconomicsAdapter(NewsSourceAdapter):
    # TradingEconomics-specific implementation

# Factory for adapter creation
class NewsAdapterFactory:
    @staticmethod
    def create_adapter(source_type: str) -> NewsSourceAdapter:
        # Returns appropriate adapter based on source type
```

### 4.2 Data Ingestion Service (DIS)

The Data Ingestion Service orchestrates periodic news collection with robust error handling.

#### Configuration

```yaml
news_sources:
  bloomberg:
    enabled: true
    api_key: ${BLOOMBERG_API_KEY}
    rate_limit: 100  # requests per minute
    batch_size: 50
  
  tradingeconomics:
    enabled: true
    api_key: ${TE_API_KEY}
    rate_limit: 60
    batch_size: 100
```

#### Deployment Configuration (AKS CronJob)

```yaml
apiVersion: batch/v1
kind: CronJob
metadata:
  name: news-ingestion-job
spec:
  schedule: "*/15 * * * *"
  jobTemplate:
    spec:
      template:
        spec:
          containers:
          - name: news-ingestion
            image: ${ACR_REGISTRY}/news-ingestion:latest
            env:
            - name: COSMOS_CONNECTION_STRING
              valueFrom:
                secretKeyRef:
                  name: cosmos-secret
                  key: connection-string
          restartPolicy: OnFailure
```

### 4.3 News Processor Service (NPS)

The News Processor Service uses GPT-4 to enrich raw news articles with structured financial analysis.

#### Enhanced GPT-4 Prompt Engineering

```python
SYSTEM_PROMPT = """
You are a specialized financial news analyst for the fixed-income (bond) market. Your role is to analyze news articles and extract structured information that is specifically relevant to bond investors and risk managers.

CONTEXT AND PURPOSE:
- Fixed-income securities (bonds) are debt instruments where investors care about credit risk, interest rate risk, and liquidity risk
- Unlike stocks, bondholders care more about the issuer's ability to pay back debt than growth potential
- Your analysis will be used by professional traders to assess risks in municipal bonds, corporate bonds, and other fixed-income instruments

OUTPUT REQUIREMENTS:
You must output valid JSON with exactly this structure:
{
    "issuer_name": "string or null",
    "sector": "string",
    "cusips": ["array of CUSIP identifiers"],
    "event_type": "string",
    "sentiment": {
        "score": float,
        "magnitude": float
    },
    "summary_excerpt": "string"
}

FIELD SPECIFICATIONS:

1. issuer_name: 
   - Extract the exact legal name of the bond issuer if mentioned
   - For municipalities, use format: "City of X" or "State of Y"
   - Return null if no specific issuer is mentioned

2. sector:
   - For issuer-specific news: Use the issuer's industry sector (e.g., "Technology", "Healthcare", "Utilities")
   - For municipal bonds: Use "Municipal" 
   - For broad market news affecting all bonds: Use "global_market"
   - For unclear/irrelevant news: Use "global_other"

3. cusips:
   - Extract any 9-character CUSIP identifiers mentioned in the article
   - CUSIPs are alphanumeric codes that uniquely identify securities
   - Return empty array if none mentioned

4. event_type (choose the MOST SPECIFIC applicable):
   CRITICAL EVENTS (highest impact on bondholders):
   - "Default": Failure to make required debt payments
   - "Bankruptcy": Legal filing for bankruptcy protection
   - "Credit_Rating_Downgrade": Reduction in credit rating by Moody's, S&P, or Fitch
   
   HIGH IMPACT EVENTS:
   - "Bond_Insurer_Downgrade": Downgrade of bond insurance companies (MBIA, Ambac, etc.)
   - "Central_Bank_Policy": Federal Reserve or other central bank rate decisions
   - "State_Budget_Crisis": Severe state/municipal budget shortfalls
   - "Natural_Disaster_Impact": Natural disasters affecting municipal bond issuers
   
   MEDIUM IMPACT EVENTS:
   - "Credit_Rating_Upgrade": Improvement in credit rating
   - "M&A_Announced": Merger or acquisition announcement
   - "Pension_Funding_Status_Change": Changes in public pension funding levels
   - "Credit_Outlook_Negative": Rating agency negative outlook (not actual downgrade)
   - "Regulatory_Investigation": Government investigations into the issuer
   - "Macro_Economic_Data": CPI, GDP, employment reports
   
   LOWER IMPACT EVENTS:
   - "Guidance_Change": Company guidance updates
   - "Lawsuit_Filed": Major litigation
   - "Credit_Outlook_Positive": Rating agency positive outlook
   - "Earnings_Miss": Earnings below expectations
   - "Earnings_Beat": Earnings above expectations
   - "Executive_Change": CEO/CFO changes
   - "General_News": Use only if no other category fits

5. sentiment:
   - score: Range from -1.0 (extremely negative for bondholders) to 1.0 (extremely positive)
   - magnitude: Range from 0.0 (mixed/neutral sentiment) to 1.0 (strong unanimous sentiment)
   
   IMPORTANT: Consider bondholder perspective specifically:
   - Debt reduction is POSITIVE for bondholders
   - Increased leverage/debt is NEGATIVE for bondholders
   - Strong cash flow is POSITIVE for bondholders
   - M&A funded by debt is typically NEGATIVE for existing bondholders
   - M&A where a stronger company acquires the issuer is typically POSITIVE

6. summary_excerpt:
   - Maximum 200 characters
   - Focus on the key fact most relevant to bond investors
   - Include specific numbers/percentages when available

EXAMPLES:

Example 1 - Credit Rating Downgrade:
Input: "S&P Global Ratings lowered ABC Corp's credit rating from BBB to BBB- citing concerns over declining cash flows..."
Output:
{
    "issuer_name": "ABC Corp",
    "sector": "Technology",
    "cusips": [],
    "event_type": "Credit_Rating_Downgrade",
    "sentiment": {
        "score": -0.8,
        "magnitude": 0.9
    },
    "summary_excerpt": "S&P downgrades ABC Corp from BBB to BBB- due to declining cash flows"
}

Example 2 - Central Bank Policy:
Input: "Federal Reserve raises interest rates by 50 basis points, signaling continued fight against inflation..."
Output:
{
    "issuer_name": null,
    "sector": "global_market",
    "cusips": [],
    "event_type": "Central_Bank_Policy",
    "sentiment": {
        "score": -0.6,
        "magnitude": 0.8
    },
    "summary_excerpt": "Fed raises rates 50bps, negative for bond prices but improves new bond yields"
}

Example 3 - Municipal Natural Disaster:
Input: "Hurricane damage to Miami estimated at $2 billion, city considering emergency bonds..."
Output:
{
    "issuer_name": "City of Miami",
    "sector": "Municipal",
    "cusips": [],
    "event_type": "Natural_Disaster_Impact",
    "sentiment": {
        "score": -0.7,
        "magnitude": 0.8
    },
    "summary_excerpt": "Hurricane causes $2B damage to Miami, city may issue emergency debt"
}
"""

USER_PROMPT_TEMPLATE = """
Analyze the following news article for fixed-income risk assessment:

Source: {source}
Published: {published_at}
Title: {title}
Content: {content}

Extract and return the structured information as specified.
"""
```

### 4.4 Storage Layer Design

The system uses a single, unified Azure Cosmos DB collection for all enriched news events.

#### Database Schema

```mermaid
erDiagram
    ENRICHED_NEWS_EVENT {
        string id PK
        string source
        datetime published_at
        datetime ingested_at
        string event_type
        string issuer_name
        string sector
        string[] cusips
        float sentiment_score
        float sentiment_magnitude
        string source_credibility_tier
        string summary_excerpt
        string raw_article_url
        date _partitionKey
    }
    
    EVENT_TYPE_WEIGHTS {
        string event_type PK
        float weight
        string description
    }
    
    SOURCE_CREDIBILITY_TIERS {
        string tier_name PK
        float weight
        string[] example_sources
    }
    
    ENRICHED_NEWS_EVENT ||--o{ EVENT_TYPE_WEIGHTS : "has type"
    ENRICHED_NEWS_EVENT ||--o{ SOURCE_CREDIBILITY_TIERS : "has credibility"
```

#### Cosmos DB Configuration

```json
// Enriched News Events Container
{
    "id": "unique-guid",
    "source": "Bloomberg",
    "published_at": "2024-01-15T10:30:00Z",
    "ingested_at": "2024-01-15T10:45:00Z",
    "event_type": "Credit_Rating_Downgrade",
    "entities": {
        "issuer_name": "ABC Corporation",
        "sector": "Technology",
        "cusips": ["12345678X"]
    },
    "sentiment": {
        "score": -0.7,
        "magnitude": 0.9
    },
    "source_credibility_tier": "TIER_1_REGULATOR",
    "summary_excerpt": "S&P downgrades ABC Corporation...",
    "raw_article_url": "https://...",
    "_partitionKey": "2024-01-15"  // Date-based partitioning
}
```

#### Indexing Policy

```json
{
    "indexingMode": "consistent",
    "automatic": true,
    "includedPaths": [
        {
            "path": "/*"
        }
    ],
    "compositeIndexes": [
        [
            {"path": "/entities/cusips/*", "order": "ascending"},
            {"path": "/published_at", "order": "descending"}
        ],
        [
            {"path": "/entities/sector", "order": "ascending"},
            {"path": "/published_at", "order": "descending"}
        ],
        [
            {"path": "/entities/issuer_name", "order": "ascending"},
            {"path": "/published_at", "order": "descending"}
        ]
    ]
}
```

### 4.5 News Sentiment Score Service (NSSS)

The API service calculates and serves aggregated sentiment scores.

#### API Design

```python
from fastapi import FastAPI, Query
from datetime import datetime, date
from typing import Optional, List

app = FastAPI(title="News Sentiment Score Service")

@app.get("/sentiment/realtime")
async def get_realtime_sentiment(
    cusip: Optional[str] = Query(None),
    sector: Optional[str] = Query(None),
    issuer_name: Optional[str] = Query(None)
) -> RealtimeSentimentResponse:
    """
    Calculate real-time sentiment score with time decay from current time
    """
    # Implementation

@app.get("/sentiment/historical")
async def get_historical_sentiment(
    as_of_date: date,
    cusip: Optional[str] = Query(None),
    sector: Optional[str] = Query(None),
    issuer_name: Optional[str] = Query(None)
) -> HistoricalSentimentResponse:
    """
    Calculate historical sentiment score with time decay from end of specified date
    """
    # Implementation
```

#### Aggregation Logic Implementation

```python
class SentimentCalculator:
    def calculate_aggregated_sentiment(
        self, 
        events: List[EnrichedNewsEvent], 
        reference_time: datetime,
        mode: CalculationMode
    ) -> float:
        """
        ASS = Σ(Si * Mi * W_eventi * W_timei * W_sourcei) / 
              Σ(Mi * W_eventi * W_timei * W_sourcei)
        """
        numerator = 0.0
        denominator = 0.0
        
        for event in events:
            S = event.sentiment.score
            M = event.sentiment.magnitude
            W_event = EVENT_TYPE_WEIGHTS[event.event_type]
            W_source = SOURCE_CREDIBILITY_WEIGHTS[event.source_credibility_tier]
            W_time = self._calculate_time_decay(
                event.published_at, 
                reference_time, 
                mode
            )
            
            numerator += S * M * W_event * W_time * W_source
            denominator += M * W_event * W_time * W_source
        
        return numerator / denominator if denominator > 0 else 0.0
    
    def _calculate_time_decay(
        self, 
        event_time: datetime, 
        reference_time: datetime,
        mode: CalculationMode
    ) -> float:
        """
        Calculate exponential time decay weight
        """
        if mode == CalculationMode.HISTORICAL:
            # For historical, reference is end of day
            delta_hours = (reference_time - event_time).total_seconds() / 3600
        else:
            # For realtime, reference is current time
            delta_hours = (datetime.utcnow() - event_time).total_seconds() / 3600
        
        # Exponential decay with 72-hour half-life
        return math.exp(-0.693 * delta_hours / 72)
```

---

## 5. Data Flow and Processing

### 5.1 End-to-End Processing Pipeline

```mermaid
flowchart LR
    subgraph "News Processing Pipeline"
        direction TB
        
        subgraph "1. Ingestion Phase"
            IJ[Ingestion Job<br/>Every 15 min]
            NAL[News Adapter Layer]
            DQ[Deduplication]
            IJ --> NAL
            NAL --> DQ
        end
        
        subgraph "2. Processing Phase"
            MQ[Message Queue]
            NPS[News Processor]
            GPT[GPT-4 Analysis]
            DQ --> MQ
            MQ --> NPS
            NPS <--> GPT
        end
        
        subgraph "3. Storage Phase"
            ENS[(Enriched News Store)]
            NPS --> ENS
        end
        
        subgraph "4. Serving Phase"
            API[Sentiment API]
            CALC[Calculator]
            ENS --> API
            API --> CALC
        end
    end
    
    style IJ fill:#f9f,stroke:#333,stroke-width:4px
    style GPT fill:#9f9,stroke:#333,stroke-width:2px
    style ENS fill:#bbf,stroke:#333,stroke-width:2px
```

### 5.2 Request Processing Sequence

```mermaid
sequenceDiagram
    participant RA as Risk Analyst
    participant OS as Orchestrator Service
    participant SSA as Sentiment Score API
    participant CM as Cache Manager
    participant RC as Redis Cache
    participant SC as Sentiment Calculator
    participant CDB as Cosmos DB
    
    RA->>OS: Request risk analysis for CUSIP
    OS->>SSA: GET /sentiment/realtime?cusip=123456789
    
    SSA->>CM: Check cache for key
    CM->>RC: GET sentiment:realtime:123456789
    
    alt Cache Hit
        RC-->>CM: Cached result
        CM-->>SSA: Return cached sentiment
    else Cache Miss
        RC-->>CM: null
        CM->>SC: Calculate sentiment
        SC->>CDB: Query enriched events<br/>(cusip=123456789)
        CDB-->>SC: Return news events
        SC->>SC: Apply aggregation formula<br/>ASS = Σ(S*M*W_event*W_time*W_source)
        SC-->>CM: Calculated sentiment
        CM->>RC: SET with TTL=300s
        CM-->>SSA: Return sentiment
    end
    
    SSA-->>OS: Sentiment response
    OS-->>RA: Complete risk analysis
```

### 5.3 Article State Machine

```mermaid
stateDiagram-v2
    [*] --> RawArticle: News Source Publishes
    
    RawArticle --> Ingested: Data Ingestion Service
    
    Ingested --> Queued: Pass Deduplication
    Ingested --> Discarded: Duplicate Detected
    
    Queued --> Processing: News Processor Picks Up
    
    Processing --> EnrichedEvent: GPT-4 Analysis Success
    Processing --> Failed: Processing Error
    
    EnrichedEvent --> Stored: Save to Cosmos DB
    
    Stored --> Indexed: Create Indexes
    
    Indexed --> Queryable: Available for API
    
    Queryable --> Cached: First Query
    Queryable --> ServedFromCache: Subsequent Queries
    
    Failed --> Retried: Retry Logic
    Retried --> Processing: Retry Attempt
    Retried --> DeadLetter: Max Retries Exceeded
    
    Discarded --> [*]
    ServedFromCache --> [*]
    Cached --> [*]
    DeadLetter --> [*]
```

---

## 6. Scalability and Performance

### 6.1 Horizontal Scaling Strategy

- **Data Ingestion Service**: Can run multiple instances with distributed locking
- **News Processor Service**: Stateless, can scale based on queue depth
- **API Service**: Behind load balancer, auto-scales based on CPU/memory

### 6.2 Caching Strategy

```python
class CacheManager:
    def __init__(self):
        self.redis_client = redis.Redis.from_url(REDIS_URL)
        self.ttl = {
            'realtime': 300,  # 5 minutes
            'historical': 3600  # 1 hour
        }
    
    def get_or_compute(self, key: str, compute_func, ttl_type: str):
        cached = self.redis_client.get(key)
        if cached:
            return json.loads(cached)
        
        result = compute_func()
        self.redis_client.setex(
            key, 
            self.ttl[ttl_type], 
            json.dumps(result)
        )
        return result
```

### 6.3 Performance Requirements

#### SLAs
- API Availability: 99.9%
- Real-time Sentiment Latency: < 500ms p95
- Historical Sentiment Latency: < 2s p95
- News Processing Lag: < 30 minutes

#### Capacity Planning
- Expected volume: 10,000 articles/day
- Peak load: 1,000 articles/hour
- Storage growth: ~500GB/year

---

## 7. Security and Monitoring

### 7.1 Security Architecture

#### API Security
- OAuth 2.0 authentication via Azure AD
- Rate limiting per client
- API key rotation policy

#### Data Security
- Encryption at rest (Cosmos DB)
- Encryption in transit (TLS 1.3)
- Sensitive data masking in logs

#### Secrets Management
- Azure Key Vault for API keys
- Managed identities for Azure resources
- Regular secret rotation

### 7.2 Monitoring and Observability

#### Metrics
- **Ingestion Metrics**: Articles processed/hour, source availability
- **Processing Metrics**: GPT-4 latency, error rates
- **API Metrics**: Request latency, cache hit ratio
- **Business Metrics**: Sentiment score distribution, event type frequency

#### Logging
```python
import structlog

logger = structlog.get_logger()

logger.info(
    "article_processed",
    cusip=enriched_event.entities.cusips,
    event_type=enriched_event.event_type,
    sentiment_score=enriched_event.sentiment.score,
    processing_time_ms=processing_time
)
```

#### Alerting
- Source unavailability > 1 hour
- GPT-4 error rate > 5%
- API p99 latency > 2 seconds
- Cosmos DB RU consumption > 80%

---

## 8. Deployment Architecture

```mermaid
graph TB
    subgraph "Azure Cloud Platform"
        subgraph "Azure Kubernetes Service (AKS)"
            subgraph "News Processing Node Pool"
                DIS1[Data Ingestion Pod 1<br/>CronJob]
                DIS2[Data Ingestion Pod 2<br/>CronJob]
                
                NPS1[News Processor Pod 1]
                NPS2[News Processor Pod 2]
                NPS3[News Processor Pod 3]
                
                API1[Sentiment API Pod 1]
                API2[Sentiment API Pod 2]
                
                LB[Azure Load Balancer]
            end
        end
        
        subgraph "Azure PaaS Services"
            subgraph "Data Layer"
                CDB[(Azure Cosmos DB<br/>-----------<br/>Enriched News: 100 RU/s<br/>Single Collection)]
                REDIS[(Azure Cache for Redis<br/>-----------<br/>Standard C2<br/>6GB Cache)]
            end
            
            subgraph "Messaging"
                ASB[Azure Service Bus<br/>-----------<br/>Standard Tier<br/>news-processing queue]
            end
            
            subgraph "Security"
                KV[Azure Key Vault<br/>-----------<br/>API Keys<br/>Connection Strings]
                AAD[Azure Active Directory<br/>-----------<br/>OAuth 2.0<br/>Service Principals]
            end
            
            subgraph "Monitoring"
                AI[Application Insights<br/>-----------<br/>Logs, Metrics, Traces]
                AM[Azure Monitor<br/>-----------<br/>Alerts, Dashboards]
            end
        end
        
        subgraph "External Services"
            APIM[Azure API Management<br/>-----------<br/>Rate Limiting<br/>API Gateway]
        end
    end
    
    subgraph "External Systems"
        BB[Bloomberg API]
        TE[TradingEconomics API]
        RT[Reuters API]
        OAI[OpenAI GPT-4 API]
    end
    
    %% Connections
    BB -.->|HTTPS| DIS1
    TE -.->|HTTPS| DIS1
    RT -.->|HTTPS| DIS2
    
    DIS1 -->|Write| ASB
    DIS2 -->|Write| ASB
    
    ASB -->|Read| NPS1
    ASB -->|Read| NPS2
    ASB -->|Read| NPS3
    
    NPS1 -.->|HTTPS| OAI
    NPS2 -.->|HTTPS| OAI
    NPS3 -.->|HTTPS| OAI
    
    NPS1 -->|Write| CDB
    NPS2 -->|Write| CDB
    NPS3 -->|Write| CDB
    
    LB -->|Route| API1
    LB -->|Route| API2
    
    API1 -->|Query| CDB
    API2 -->|Query| CDB
    API1 <-->|Cache| REDIS
    API2 <-->|Cache| REDIS
    
    APIM -->|Forward| LB
    
    KV -.->|Secrets| DIS1
    KV -.->|Secrets| NPS1
    KV -.->|Secrets| API1
    
    AAD -.->|Auth| APIM
    
    AI -.->|Collect| DIS1
    AI -.->|Collect| NPS1
    AI -.->|Collect| API1
    
    style BB fill:#e1f5fe
    style TE fill:#e1f5fe
    style RT fill:#e1f5fe
    style OAI fill:#e8f5e9
    style CDB fill:#bbdefb
    style REDIS fill:#ffcdd2
    style ASB fill:#f3e5f5
    style KV fill:#fff9c4
    style AAD fill:#fff9c4
```

### 8.1 Disaster Recovery

#### Backup Strategy
- Cosmos DB continuous backup with 7-day retention
- Configuration backups in Azure DevOps

#### Recovery Procedures
- RPO: 1 hour
- RTO: 4 hours
- Automated failover to secondary region

---

## 9. Future Enhancements

1. **Multi-language Support**: Process non-English news sources
2. **Real-time Streaming**: WebSocket API for live sentiment updates
3. **ML Model Fine-tuning**: Custom sentiment model for finance
4. **Advanced Analytics**: Sentiment trend analysis and anomaly detection
5. **Source Quality Scoring**: Dynamic source credibility based on accuracy 