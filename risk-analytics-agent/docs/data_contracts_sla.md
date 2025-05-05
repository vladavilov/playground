# Data Contracts and Service Level Agreements (SLAs)

This document outlines the data contracts and SLAs for the integration between the Financial Trading Platform and the GenAI Risk Scoring System, based on requirements section 5.2.2.

## 1. Data Contracts

Data contracts define the expected schema, format, and semantics for data exchanged between the two systems.

### 1.1 Market Data (Kafka Topic: `market-data-fixed-income`)

-   **Format:** JSON
-   **Schema (Example based on Req 5.1.1):**
    ```json
    {
      "type": "object",
      "properties": {
        "dataType": { "type": "string", "enum": ["MARKET_DATA"] },
        "assetClass": { "type": "string", "enum": ["FIXED_INCOME"] },
        "timestamp": { "type": "string", "format": "date-time" }, // ISO 8601
        "dataPoints": {
          "type": "array",
          "items": {
            "type": "object",
            "properties": {
              "metricType": { "type": "string" }, // e.g., YIELD_CURVE, INDEX, FUND_FLOW
              "key": { "type": "string" }, // e.g., US_TREASURY_10Y, MOVE_INDEX
              "value": { "type": "number" },
              "unit": { "type": "string" } // e.g., PERCENT, BPS, USD
            },
            "required": ["metricType", "key", "value", "unit"]
          }
        },
        "metadata": {
          "type": "object",
          "properties": {
            "source": { "type": "string" },
            "confidence": { "type": "number", "minimum": 0, "maximum": 1 }
          },
          "required": ["source"]
        }
      },
      "required": ["dataType", "assetClass", "timestamp", "dataPoints", "metadata"]
    }
    ```
-   **Validation:** The GenAI system will validate incoming messages against this schema. Invalid messages will be logged and potentially moved to a dead-letter queue.

### 1.2 Trade History (REST API & Kafka Topic: `trade-history-fixed-income-updates`)

-   **Format:** JSON
-   **Schema (Example based on Req 5.1.2):**
    ```json
    {
      "type": "object",
      "properties": {
        "dataType": { "type": "string", "enum": ["TRADE_HISTORY"] },
        "tradeId": { "type": "string" }, // UUID format preferred
        "securityId": { "type": "string" }, // CUSIP/ISIN
        "executionDetails": {
          "type": "object",
          "properties": {
            "timestamp": { "type": "string", "format": "date-time" },
            "price": { "type": "number" },
            "direction": { "type": "string", "enum": ["BUY", "SELL"] },
            "quantity": { "type": "number" },
            "counterpartyType": { "type": "string" }, // Enum based on Req 3.1
            "maskedCounterpartyId": { "type": "string" }, // Ensure masking
            "maskedTraderId": { "type": "string" } // Ensure masking
            // Add other fields from Req 3.1 as needed
          },
          "required": ["timestamp", "price", "direction", "quantity"]
        },
        "performanceMetrics": {
          "type": "object",
          "properties": {
             // Define fields based on Req 3.1 Post-Trade Performance
             "t1PriceChange": { "type": ["number", "null"] },
             "t5PriceChange": { "type": ["number", "null"] },
             "realizedPnL": { "type": ["number", "null"] }
             // ... other metrics
          }
        }
      },
      "required": ["dataType", "tradeId", "securityId", "executionDetails"]
    }
    ```
-   **Validation:** Similar validation will be performed for both REST API responses and Kafka messages.

## 2. Service Level Agreements (SLAs)

SLAs define the expected performance and availability of the data services provided by the Financial Trading Platform.

### 2.1 Data Freshness

-   **Market Data (Kafka):** End-to-end latency from source event to message availability in Kafka topic should be **less than 5 seconds** during market hours.
-   **Trade History (Batch REST):** Complete T-1 trade history data must be available via the REST API by **7:00 AM** local market time each trading day.
-   **Trade History (Incremental Kafka):** Incremental updates for new trades or modifications should appear in the Kafka topic with **less than 15 minutes** delay from the event occurrence.

### 2.2 Data Quality

-   **Schema Adherence:** >= 99.9% of messages/records must conform to the defined data contract schemas.
-   **Completeness:** Key required fields (as defined in schemas) should have missing values in **less than 0.1%** of records.
-   **Consistency:** Data provided via incremental Kafka updates must be consistent with the data provided in the subsequent daily batch REST API load. Discrepancies should be minimal and investigated.

### 2.3 Availability

-   **Market Data Service (Kafka):** Guaranteed uptime of **99.99%** during primary market trading hours.
-   **Trade History Service (REST & Kafka):** Guaranteed uptime of **99.9%** overall.
-   **Degraded Performance:** The Financial Trading Platform should notify the GenAI system of any anticipated maintenance or known issues that might affect data delivery or quality.
The GenAI system should implement mechanisms (e.g., circuit breakers, fallback logic) to handle temporary unavailability or degraded performance from the platform.

## 3. Monitoring and Alerting

-   The GenAI system will implement monitoring for:
    -   Kafka consumer lag.
    -   REST API response times and error rates.
    -   Data validation failure rates.
    -   Adherence to freshness SLAs.
-   Alerts will be configured to notify relevant teams upon SLA breaches or significant data quality issues.

print("Placeholder document for Data Contracts and SLAs created.")

