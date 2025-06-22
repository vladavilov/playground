# Orchestrator Service: Requirements Specification

**Version: 1.0**

## 1. Overview & Purpose

The Orchestrator Service acts as the central coordinator for the entire risk analytics workflow. It is responsible for managing the flow of data and requests between the various specialized microservices to fulfill two primary, high-level business functions:
1.  **On-Demand Instrument Analysis:** Processing synchronous requests for a comprehensive risk analysis of a single financial instrument.
2.  **Historical Data Preparation:** Executing asynchronous batch jobs to generate the historical, enriched datasets required for training the `MarketRegime` and `RiskPredictionModel` machine learning models.

This service abstracts the complexity of the underlying microservice architecture, providing a simplified interface for end-user applications and internal data science workflows.

---

## 2. Core Functional Requirements

### 2.1. Feature: On-Demand Instrument Analysis

This feature handles real-time requests to perform a full risk analysis for a specific instrument, identified by its CUSIP.

-   **ORCH-FR-01:** The service **shall** expose a synchronous API endpoint `GET /analyze/instrument/{cusip}`.
-   **ORCH-FR-02:** The endpoint **shall** accept a `cusip` as a URL path parameter and an optional `as_of_date` as a query parameter. If `as_of_date` is not provided, the service **shall** default to the current date and time.
-   **ORCH-FR-03:** The service **shall** execute the "On-Demand Analysis Workflow" (defined in Section 3.1) to gather, process, and synthesize all required data.
-   **ORCH-FR-04:** The service **shall** return the final, structured JSON output from the `RiskSynthesis` service as its response.

### 2.2. Feature: Historical Data Preparation

This feature manages the asynchronous generation of training datasets for the core machine learning models.

-   **ORCH-FR-05:** The service **shall** expose an asynchronous API endpoint `POST /jobs/prepare-training-data` to trigger the data generation process.
-   **ORCH-FR-06:** The request **shall** accept a JSON payload specifying the `job_type` ('market_regime' or 'risk_prediction'), a `start_date`, an `end_date`, and an optional `cusip_list` for instrument-specific data.
    ```json
    {
      "job_type": "string", // 'market_regime' or 'risk_prediction'
      "start_date": "date",
      "end_date": "date",
      "cusip_list": ["string"] // Optional, required for 'risk_prediction'
    }
    ```
-   **ORCH-FR-07:** If `job_type` is 'market_regime', the service **shall** execute the "Market Regime Training Data Workflow" (defined in Section 3.2).
-   **ORCH-FR-08:** If `job_type` is 'risk_prediction', the service **shall** execute the "Risk Prediction Training Data Workflow" (defined in Section 3.3).
-   **ORCH-FR-09:** The service **shall** store the resulting dataset file in a pre-configured storage location (e.g., an S3 bucket or network share) and return a job ID and the planned artifact location, where resulted file appears after processing is complete.

---

## 3. Workflows & Service Integration

### 3.1. On-Demand Analysis Workflow

This workflow defines the sequence of service calls required to fulfill a real-time analysis request. The Orchestrator **shall** manage the dependencies and execute calls in parallel where possible to minimize latency.

1.  **Initial Fan-Out (Parallel Execution):** Upon receiving a request, the service **shall** initiate parallel calls to the following independent data source services, using the input `cusip` and `as_of_date`:
    -   `FinCalculations` Service: To get the core `FinancialDataObject`.
    -   `NewsSentiment` Service: To get the latest `news_sentiment` object.
    -   `RepoDataService`: To get the `cost_of_carry_bps`.
    -   `OwnershipDataService`: To get `ownership_concentration` data.
    -   `MarketDataFeed`: To get the latest general `market_indicators`.
    -   `EconomicCalendarService`: To get the `events_matrix` for the next 20 days.

2.  **First Level Synthesis (Market Regime):**
    -   Once the calls to `FinCalculations` and `MarketDataFeed` have returned, the Orchestrator **shall** invoke the `MarketRegime` Service, providing the required market indicators and financial metrics from the outputs of the previous step.

3.  **Second Level Synthesis (Risk Prediction):**
    -   Once the `FinCalculations`, `NewsSentiment`, `MarketRegime`, and `EconomicCalendarService` calls are complete, the Orchestrator **shall** invoke the `RiskPredictionModel` service with the full set of required inputs.

4.  **Final Assembly (Consolidated Input):**
    -   The Orchestrator **shall** wait for all preceding calls to complete.
    -   It **shall** then assemble the `Consolidated Input Data Model`, as specified in `RiskSynthesis_part.md`, from the outputs of all previously called services.

5.  **Final Synthesis (Narrative Generation):**
    -   The Orchestrator **shall** make a final call to the `RiskSynthesis` service, providing the fully assembled `Consolidated Input Data Model`.

6.  **Response:**
    -   The final JSON output from the `RiskSynthesis` service **shall** be returned as the response to the initial API request.

### 3.2. Market Regime Training Data Workflow

1.  **Input:** `start_date`, `end_date`.
2.  **Process:**
    -   The Orchestrator **shall** request historical data for the specified date range from the `MarketDataFeed`.
    -   The Orchestrator **shall** request historical market context data from the `FinCalculations` service.
    -   It **shall** join these two datasets by date to create a comprehensive feature set based on the input data of MarketRegime_part.md.
3.  **Output:** The final, enriched CSV file **shall** be saved to the designated artifact storage.

### 3.3. Risk Prediction Training Data Workflow

1.  **Input:** `start_date`, `end_date`, `cusip_list`.
2.  **Process:**
    -   `EconomicCalendarService`: To get the `events_matrix` for the next 20 days.
    -   `MarketDataFeed`: To get the historical data for the specified date range.
    -   `FinCalculations`: To get the historical market context data.
    -   `NewsSentiment`: To get the historical news sentiment data.
    -   The Orchestrator **shall** then join the instrument-specific data (`FinancialDataObject`, `NewsSentiment`, `EconomicCalendar`) with the corresponding data from the `MarketRegime` historical dataset on the `date` field.
3.  **Output:** The final, comprehensive training dataset for the `RiskPredictionModel` **shall** be saved as a single file to the designated artifact storage.

---

## 4. Non-Functional Requirements

-   **NFR-ORCH-04 (Performance):** The On-Demand Instrument Analysis workflow **shall** have a target P95 latency of under 3 seconds. The service **shall** aggressively parallelize downstream calls to meet this target. 