graph TB
  linkStyle default fill:#ffffff

  subgraph diagram ["News Sentiment Service - Sentiment Score API - Components"]
    style diagram fill:#ffffff,stroke:#ffffff

    24[("<div style='font-weight: bold'>Azure Cosmos DB</div><div style='font-size: 70%; margin-top: 0px'>[Container: NoSQL Database]</div><div style='font-size: 80%; margin-top:10px'>Stores enriched news events</div>")]
    style 24 fill:#438dd5,stroke:#2e6295,color:#ffffff
    25[("<div style='font-weight: bold'>Redis Cache</div><div style='font-size: 70%; margin-top: 0px'>[Container: In-Memory Cache]</div><div style='font-size: 80%; margin-top:10px'>Caches sentiment calculations</div>")]
    style 25 fill:#438dd5,stroke:#2e6295,color:#ffffff

    subgraph 26 ["Sentiment Score API"]
      style 26 fill:#ffffff,stroke:#2e6295,color:#2e6295

      27["<div style='font-weight: bold'>Realtime Endpoint</div><div style='font-size: 70%; margin-top: 0px'>[Component: REST API]</div><div style='font-size: 80%; margin-top:10px'>Calculates current sentiment</div>"]
      style 27 fill:#85bbf0,stroke:#5d82a8,color:#000000
      28["<div style='font-weight: bold'>Historical Endpoint</div><div style='font-size: 70%; margin-top: 0px'>[Component: REST API]</div><div style='font-size: 80%; margin-top:10px'>Calculates past sentiment</div>"]
      style 28 fill:#85bbf0,stroke:#5d82a8,color:#000000
      29["<div style='font-weight: bold'>Sentiment Calculator</div><div style='font-size: 70%; margin-top: 0px'>[Component: Python Class]</div><div style='font-size: 80%; margin-top:10px'>Core aggregation logic</div>"]
      style 29 fill:#85bbf0,stroke:#5d82a8,color:#000000
      30["<div style='font-weight: bold'>Cache Manager</div><div style='font-size: 70%; margin-top: 0px'>[Component: Python Class]</div><div style='font-size: 80%; margin-top:10px'>Manages Redis caching</div>"]
      style 30 fill:#85bbf0,stroke:#5d82a8,color:#000000
    end

    27-. "<div>Uses</div><div style='font-size: 70%'></div>" .->29
    28-. "<div>Uses</div><div style='font-size: 70%'></div>" .->29
    29-. "<div>Uses</div><div style='font-size: 70%'></div>" .->30
    30-. "<div>Read/Write</div><div style='font-size: 70%'></div>" .->25
    29-. "<div>Queries</div><div style='font-size: 70%'></div>" .->24
  end