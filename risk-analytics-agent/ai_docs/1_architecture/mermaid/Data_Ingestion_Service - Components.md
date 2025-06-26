graph TB
  linkStyle default fill:#ffffff

  subgraph diagram ["News Sentiment Service - Data Ingestion Service - Components"]
    style diagram fill:#ffffff,stroke:#ffffff

    8["<div style='font-weight: bold'>News Adapter Layer</div><div style='font-size: 70%; margin-top: 0px'>[Container: Python]</div><div style='font-size: 80%; margin-top:10px'>Provides unified interface to<br />multiple news sources</div>"]
    style 8 fill:#438dd5,stroke:#2e6295,color:#ffffff

    subgraph 13 ["Data Ingestion Service"]
      style 13 fill:#ffffff,stroke:#2e6295,color:#2e6295

      14["<div style='font-weight: bold'>Scheduler</div><div style='font-size: 70%; margin-top: 0px'>[Component: APScheduler]</div><div style='font-size: 80%; margin-top:10px'>Manages cron-based execution</div>"]
      style 14 fill:#85bbf0,stroke:#5d82a8,color:#000000
      15["<div style='font-weight: bold'>Ingestion Orchestrator</div><div style='font-size: 70%; margin-top: 0px'>[Component: Python Class]</div><div style='font-size: 80%; margin-top:10px'>Coordinates news fetching<br />from all sources</div>"]
      style 15 fill:#85bbf0,stroke:#5d82a8,color:#000000
      16["<div style='font-weight: bold'>Circuit Breaker</div><div style='font-size: 70%; margin-top: 0px'>[Component: Python Class]</div><div style='font-size: 80%; margin-top:10px'>Handles source failures<br />gracefully</div>"]
      style 16 fill:#85bbf0,stroke:#5d82a8,color:#000000
      17["<div style='font-weight: bold'>Deduplicator</div><div style='font-size: 70%; margin-top: 0px'>[Component: Python Class]</div><div style='font-size: 80%; margin-top:10px'>Prevents duplicate article<br />processing</div>"]
      style 17 fill:#85bbf0,stroke:#5d82a8,color:#000000
    end

    15-. "<div>Uses</div><div style='font-size: 70%'></div>" .->8
    15-. "<div>Protected by</div><div style='font-size: 70%'></div>" .->16
    15-. "<div>Filters through</div><div style='font-size: 70%'></div>" .->17
    14-. "<div>Triggers</div><div style='font-size: 70%'></div>" .->15
  end