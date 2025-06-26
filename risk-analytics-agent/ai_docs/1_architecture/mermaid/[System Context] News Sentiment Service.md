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