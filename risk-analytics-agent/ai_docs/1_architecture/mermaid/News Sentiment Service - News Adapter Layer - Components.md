graph TB
  linkStyle default fill:#ffffff

  subgraph diagram ["News Sentiment Service - News Adapter Layer - Components"]
    style diagram fill:#ffffff,stroke:#ffffff

    1["<div style='font-weight: bold'>Bloomberg</div><div style='font-size: 70%; margin-top: 0px'>[Software System]</div><div style='font-size: 80%; margin-top:10px'>Financial news and data<br />provider</div>"]
    style 1 fill:#999999,stroke:#6b6b6b,color:#ffffff
    2["<div style='font-weight: bold'>TradingEconomics</div><div style='font-size: 70%; margin-top: 0px'>[Software System]</div><div style='font-size: 80%; margin-top:10px'>Economic data and news<br />provider</div>"]
    style 2 fill:#999999,stroke:#6b6b6b,color:#ffffff
    3["<div style='font-weight: bold'>Reuters</div><div style='font-size: 70%; margin-top: 0px'>[Software System]</div><div style='font-size: 80%; margin-top:10px'>Global news provider</div>"]
    style 3 fill:#999999,stroke:#6b6b6b,color:#ffffff
    13["<div style='font-weight: bold'>Data Ingestion Service</div><div style='font-size: 70%; margin-top: 0px'>[Container: Python/AKS CronJob]</div><div style='font-size: 80%; margin-top:10px'>Orchestrates periodic news<br />collection</div>"]
    style 13 fill:#438dd5,stroke:#2e6295,color:#ffffff

    subgraph 8 ["News Adapter Layer"]
      style 8 fill:#ffffff,stroke:#2e6295,color:#2e6295

      10["<div style='font-weight: bold'>TradingEconomics Adapter</div><div style='font-size: 70%; margin-top: 0px'>[Component: Python Class]</div><div style='font-size: 80%; margin-top:10px'>Fetches articles from TE API</div>"]
      style 10 fill:#85bbf0,stroke:#5d82a8,color:#000000
      11["<div style='font-weight: bold'>Reuters Adapter</div><div style='font-size: 70%; margin-top: 0px'>[Component: Python Class]</div><div style='font-size: 80%; margin-top:10px'>Fetches articles from Reuters<br />API</div>"]
      style 11 fill:#85bbf0,stroke:#5d82a8,color:#000000
      12["<div style='font-weight: bold'>Adapter Factory</div><div style='font-size: 70%; margin-top: 0px'>[Component: Python Class]</div><div style='font-size: 80%; margin-top:10px'>Creates appropriate adapter<br />based on source type</div>"]
      style 12 fill:#85bbf0,stroke:#5d82a8,color:#000000
      9["<div style='font-weight: bold'>Bloomberg Adapter</div><div style='font-size: 70%; margin-top: 0px'>[Component: Python Class]</div><div style='font-size: 80%; margin-top:10px'>Fetches articles from<br />Bloomberg API</div>"]
      style 9 fill:#85bbf0,stroke:#5d82a8,color:#000000
    end

    9-. "<div>API calls</div><div style='font-size: 70%'></div>" .->1
    10-. "<div>API calls</div><div style='font-size: 70%'></div>" .->2
    11-. "<div>API calls</div><div style='font-size: 70%'></div>" .->3
    12-. "<div>Creates</div><div style='font-size: 70%'></div>" .->9
    12-. "<div>Creates</div><div style='font-size: 70%'></div>" .->10
    12-. "<div>Creates</div><div style='font-size: 70%'></div>" .->11
    13-. "<div>Uses</div><div style='font-size: 70%'></div>" .->12
  end