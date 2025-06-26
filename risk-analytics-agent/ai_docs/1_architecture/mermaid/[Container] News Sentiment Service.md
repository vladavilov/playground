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