### **1. Business Objective & Goals**

The primary business objective is to provide professional traders with a **top-tier decision-support tool** that transforms risk management from a reactive art into a proactive, data-driven discipline. This is not an auto-trader or a "black box" that spits out signals. It is an **outstanding analytical framework** designed for augmentation, acting as a highly intelligent, systematic risk dashboard that aligns with a professional trader's workflow.

It serves as an exceptional **"junior analyst"** for your desk, automating the quantitative heavy lifting and preparing comprehensive briefings on any security. By systematically handling the "what" (the data) and the "why" (the drivers), it leaves traders more time to focus on the "what now?" and execute with higher conviction.

The key goals are to:

*   **Augment Trader Intelligence:** Equip every trader with an 'on-demand analyst' that delivers comprehensive, data-driven briefings on any security, instantly.
*   **Challenge Biases & Uncover Hidden Risk:** Systematically surface non-obvious risks and contradictions (e.g., flagging when a bond is "Illiquid & Overvalued" or when market sentiment contradicts order flow), forcing a deeper look and challenging existing biases.
*   **Boost Efficiency & Coverage:** Enable traders and analysts to screen opportunities and risks across the market far more efficiently, using this as an invaluable "first look" tool to flag instruments that warrant a deeper, qualitative dive.
*   **Build Trust through Transparency:** Deliver fully explainable AI outputs, detailing the key drivers behind every forecast. This transparency is critical for validating model outputs against a trader's own market experience and intuition.
*   **Power Institutional Knowledge:** Automate the creation and enrichment of historical datasets, building a powerful, proprietary data asset that fuels model development and sharpens the entire system's edge over time.

### **2. Main Features**

*   **On-Demand Instrument Analysis:** A synchronous feature that delivers a holistic risk assessment for a specific instrument (identified by CUSIP), consolidating all calculated metrics and forecasts into a single, structured output.
*   **Regime-Aware Risk Forecasting:** The system uses a Temporal Fusion Transformer (TFT) model to predict key risk metrics over multiple horizons (1-day, 5-day, 20-day). These forecasts are contextually aware of the prevailing market regime (e.g., `Bear_Flattener`, `Bull_Steepener`).
*   **Risk Synthesis & Narrative Generation:** The final analytical step, which translates all quantitative data into a concise, qualitative risk narrative, a headline, and a standardized set of risk scores.
*   **Cross-Factor Contradiction Analysis:** A rules-based engine that surfaces non-obvious insights by identifying patterns of confirmation and contradiction between different risk factors (e.g., flagging when a bond is "Illiquid & Overvalued" or when market sentiment contradicts order flow).
*   **Historical Data Preparation:** An asynchronous feature that executes batch jobs to produce the large, enriched datasets needed to train the system's core machine learning models.

### **3. Main Data Flow**


```plantuml
graph TD;
    subgraph "Data Feeds"
        MarketData["Market Data"]
        TradesHistory["Trades History"]
        SecurityMaster["Security Master"]
        StateTax["State Tax"]
        EconomicalCalendar["Economical Calendar"]
        News["News"]
    end

    MarketData --> Calculations["Calculations"]
    TradesHistory --> Calculations
    SecurityMaster --> Calculations
    StateTax --> Calculations
    EconomicalCalendar --> Calculations
    News --> Calculations
    [..] --> Calculations
    
    Calculations --> MarketRegime["Market Regime Model"]
    MarketRegime --> PredictedRisk["Predicted Risk Features"]
    PredictedRisk --> RiskSynthesis["Risk Synthesis"]
```

### **3. Main Workflows**

1.  **On-Demand Analysis Workflow:** This is the primary real-time process.

    *   An API request is made to the `Orchestrator Service` for a specific instrument.
    *   The `Orchestrator` initiates parallel calls to all necessary data services (e.g., `FinCalculations`, `NewsSentiment`, `MarketDataFeed`).
    *   Once initial data is gathered, it's passed to the core ML services (`MarketRegimeService`, `RiskPredictionService`) for classification and forecasting.
    *   All outputs are then consolidated and sent to the `RiskSynthesisService`.
    *   The `RiskSynthesisService` derives standardized risk scores, performs cross-factor analysis, generates the final narrative, and returns the complete, structured analysis.

2.  **ML Training Data Preparation Workflow:** This is the primary asynchronous process.
    *   A job is triggered via the `Orchestrator Service` specifying a job type, date range, and (if applicable) a list of instruments.
    *   The `Orchestrator` fetches and joins historical data from all required source services (`FinCalculations`, `MarketDataFeed`, `NewsSentiment`, `EconomicCalendarService`, etc.).
    *   The final, enriched dataset is saved as a single file to a pre-configured artifact storage location, ready for use by data scientists.

### **4. Main System Components**

The system is designed as a microservices-based architecture. Below are the primary components, with links to their detailed requirements documents.

*   **System Architecture Definition:**
    *   [`RiskAnalyticsSystem_C4.dsl`](./playground/risk-analytics-agent/ai_docs/1_architecture/RiskAnalyticsSystem_C4.dsl): The C4 model defining the system's architecture, components, and relationships.

*   **Application & Orchestration Layer:**
    *   [`Orchestrator Service`](./playground/risk-analytics-agent/ai_docs/4_application_layer/OrchestratorService.md): The central coordinator that manages all workflows.
    *   [`RiskSynthesis Service`](./playground/risk-analytics-agent/ai_docs/4_application_layer/RiskSynthesisService.md): The final service that aggregates all data to produce the user-facing narrative and risk summary.

*   **Core Logic & Machine Learning:**
    *   [`FinancialCalculationService`](./playground/risk-analytics-agent/ai_docs/3_core_logic/FinancialCalculationService.md): A comprehensive engine that computes a wide array of financial metrics for a security.
    *   [`MarketRegime Service`](./playground/risk-analytics-agent/ai_docs/3_core_logic/MarketRegimeService.md): An HMM-based model that classifies the current market state into predefined regimes.
    *   [`RiskPrediction Service`](./playground/risk-analytics-agent/ai_docs/3_core_logic/RiskPredictionService.md): A TFT-based model that provides explainable forecasts for key risk factors.

*   **Data Services:**
    *   [`EconomicCalendar Service`](./playground/risk-analytics-agent/ai_docs/2_data_services/EconomicCalendarService.md): Provides data on scheduled, high-impact economic events.
    *   [`MarketDataFeed`](./playground/risk-analytics-agent/ai_docs/2_data_services/MarketDataFeed.md): Provides broad market indicators (e.g., VIX, credit spreads).
    *   [`NewsSentiment Service`](./playground/risk-analytics-agent/ai_docs/2_data_services/NewsSentimentService.md): Ingests and scores news to provide an aggregated sentiment score.
    *   [`OwnershipData Service`](./playground/risk-analytics-agent/ai_docs/2_data_services/OwnershipDataService.md): Provides data on security ownership concentration.
    *   [`RepoData Service`](./playground/risk-analytics-agent/ai_docs/2_data_services/RepoDataService.md): Provides repo rates used for cost-of-carry calculations.
    *   [`StateFiscalFeed`](./playground/risk-analytics-agent/ai_docs/2_data_services/StateFiscalFeed.md): Provides fiscal health data for U.S. states, used in municipal bond analysis. 