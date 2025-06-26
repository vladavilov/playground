graph TB
  linkStyle default fill:#ffffff

  subgraph diagram ["News Sentiment Service - News Processor Service - Components"]
    style diagram fill:#ffffff,stroke:#ffffff

    4["<div style='font-weight: bold'>OpenAI GPT-4</div><div style='font-size: 70%; margin-top: 0px'>[Software System]</div><div style='font-size: 80%; margin-top:10px'>AI language model for text<br />analysis</div>"]
    style 4 fill:#999999,stroke:#6b6b6b,color:#ffffff

    subgraph 18 ["News Processor Service"]
      style 18 fill:#ffffff,stroke:#2e6295,color:#2e6295

      19["<div style='font-weight: bold'>GPT Client</div><div style='font-size: 70%; margin-top: 0px'>[Component: OpenAI SDK]</div><div style='font-size: 80%; margin-top:10px'>Interfaces with OpenAI API</div>"]
      style 19 fill:#85bbf0,stroke:#5d82a8,color:#000000
      20["<div style='font-weight: bold'>Prompt Engine</div><div style='font-size: 70%; margin-top: 0px'>[Component: Python Class]</div><div style='font-size: 80%; margin-top:10px'>Manages prompts for GPT-4</div>"]
      style 20 fill:#85bbf0,stroke:#5d82a8,color:#000000
      21["<div style='font-weight: bold'>Entity Extractor</div><div style='font-size: 70%; margin-top: 0px'>[Component: Python Class]</div><div style='font-size: 80%; margin-top:10px'>Extracts issuer, sector,<br />CUSIP</div>"]
      style 21 fill:#85bbf0,stroke:#5d82a8,color:#000000
      22["<div style='font-weight: bold'>Sentiment Analyzer</div><div style='font-size: 70%; margin-top: 0px'>[Component: Python Class]</div><div style='font-size: 80%; margin-top:10px'>Calculates sentiment scores</div>"]
      style 22 fill:#85bbf0,stroke:#5d82a8,color:#000000
      23["<div style='font-weight: bold'>Event Classifier</div><div style='font-size: 70%; margin-top: 0px'>[Component: Python Class]</div><div style='font-size: 80%; margin-top:10px'>Classifies news event types</div>"]
      style 23 fill:#85bbf0,stroke:#5d82a8,color:#000000
    end

    19-. "<div>API calls</div><div style='font-size: 70%'></div>" .->4
    20-. "<div>Sends prompts</div><div style='font-size: 70%'></div>" .->19
    21-. "<div>Uses</div><div style='font-size: 70%'></div>" .->20
    22-. "<div>Uses</div><div style='font-size: 70%'></div>" .->20
    23-. "<div>Uses</div><div style='font-size: 70%'></div>" .->20
  end