# Economic Calendar Service: Requirements Specification

**Version: 3.0**

---

## Part 1: Overview & Functional Requirements

### 1. Overview & Purpose
The primary purpose of the Economic Calendar Service is to act as a centralized, authoritative source for historical and future scheduled economic events. It ingests data from a specified external provider, filters it for high-impact events, and serves this curated data as a time-series feature matrix via a simple API to internal systems, such as the `RiskPredictionModel`.

### 2. Core Functional Requirements
- **EC-FR-01:** The service **shall** ingest economic event data from the specified external provider.
- **EC-FR-02:** The service **shall** filter events upon ingestion, storing only those that meet the predefined criteria for high market impact.
- **EC-FR-03:** The service **shall** provide a single API endpoint to retrieve a time-series feature matrix for a given date range.
- **EC-FR-04:** The service **shall** maintain a historical record of all *filtered* ingested events.
- **EC-FR-05:** The service **shall** trigger a data ingestion job **once every 24 hours** to fetch new and updated future events from the external provider (`tradingeconomics.com`). This ensures the calendar data is kept current.

---

## Part 2: Data Sourcing, Processing, and Storage

### 3. Data Sourcing
- **EC-DS-01:** The primary external data provider **shall** be **tradingeconomics.com**.
- **EC-DS-02:** The service **shall** use the Trading Economics API's `/calendar` endpoint to fetch raw event data. An API key must be managed as a secure configuration parameter.
- **EC-DS-03:** The raw input for each event from the provider **shall** be parsed according to the following JSON structure:
  ```json
  {
    "Date": "string",       // e.g., "2024-03-12T12:30:00"
    "Country": "string",    // e.g., "United States"
    "Category": "string",   // e.g., "Consumer Price Index (CPI)"
    "Importance": "number"  // e.g., 1 (low), 2 (medium), 3 (high)
  }
  ```
  *Note: Field names (`Category`, `Importance`, `Date`) differ from the internal schema (`event_name`, `importance`, `event_date`). The ingestion process must map these fields correctly.*
- **EC-DS-04:** The Economic Calendar Service shall minimize unnecessary data transfer by using available API filtering parameters provided by Trading Economics. Specifically, it shall request only events for the United States (`country = united states`) and only high-impact events (`importance = 3`), and shall explicitly set `start_date` and `end_date` parameters to limit the time window.
  - Example API call format: `GET /calendar/country/united%20states?c=API_KEY&importance=3&start_date={YYYY-MM-DD}&end_date={YYYY-MM-DD}`
- **EC-DS-05:** The Economic Calendar Service shall use either free or paid data sources, depending on business requirements and available budget.
  - The free version of the Trading Economics API only provides upcoming scheduled economic events, typically covering the next 30–60 days, and does not include any historical event data (past events or historical indicator time series). Free access is currently limited to a few countries (Sweden, Mexico, New Zealand, and Thailand), and does not include U.S. data.
  - In contrast, the paid plans of Trading Economics offer full access to:
      - Historical calendar events (e.g., last 5+ years), including U.S. data.
      - Historical time series for economic indicators (e.g., CPI, GDP, Payrolls).
      - Extended forecast data and additional metadata.
      - Global country coverage (including U.S.).
  - Pricing details are not publicly listed and are available upon request. Historically (as of public references from 2022–2023), the Professional Plan started at approximately ~$90 per month (covering up to 5 years of historical data), while Corporate / Enterprise Plans started at ~$450 per month and above, depending on data scope and number of users. Current actual prices may vary and should be confirmed directly with Trading Economics. More information on available plans can be found at Trading Economics API Pricing.
  - Therefore, if the service uses a paid Trading Economics plan, it can fully cover U.S. historical and future events as initially envisioned. If not, the service shall rely on alternative public API-based data sources (e.g., BLS, BEA, FRED, Fed) and explicitly handle historical data gaps (e.g., by returning all-zero feature vectors for past dates).

### 4. Ingestion & Filtering Logic
- **EC-PR-01:** Upon ingestion, raw events **shall** be immediately filtered *before* being saved to the service's internal storage.
- **EC-PR-02:** The filtering criteria are as follows:
  - `Country` **must** be "United States".
  - `Importance` **must** be `3` (High).
  - `Category` (event name) **must** match one of the canonical categories listed in the table below (case-insensitive search).

| Category Name                          | Description                                 | Update Freq.                | Feature Name             |
|:---|:---|:---|:---|
| CPI                                    | Consumer Price Index (CPI)                 | Monthly                     | cpi                     |
| FOMC Rate Decision                     | Federal Reserve interest rate decision     | ~8 times/year (every ~6 weeks) | fomc_rate_decision     |
| Non-Farm Payrolls                      | US labor market job creation data          | Monthly                     | non_farm_payrolls     |
| Retail Sales                           | US retail sales report                     | Monthly                     | retail_sales          |
| Unemployment Rate                      | Official US unemployment rate              | Monthly                     | unemployment_rate     |
| GDP Growth Rate                        | US GDP quarterly growth                    | Quarterly                   | gdp_growth_rate       |
| Producer Price Index                   | Wholesale price inflation measure          | Monthly                     | producer_price_index |
| ISM Manufacturing PMI                  | Manufacturing sector activity index        | Monthly                     | ism_manufacturing_pmi|
| ISM Services PMI                       | Services sector activity index            | Monthly                     | ism_services_pmi     |
| University of Michigan Consumer Sentiment | Consumer confidence survey            | Monthly (prelim & final)    | university_of_michigan_consumer_sentiment |
| Housing Starts                         | New residential construction starts       | Monthly                     | housing_starts       |
| Fed Balance Sheet                      | Federal Reserve balance sheet update      | Weekly (every Thursday)     | fed_balance_sheet    |

  *Note: The ingestion logic must handle variations in the provider's naming (e.g., "Consumer Price Index (CPI)" should map to "CPI", weekly H.4.1 release should map to "Fed Balance Sheet").*
    
- **EC-PR-03:** The probability of changes to scheduled economic events (e.g., publication date shifts or cancellations) in the United States is very low (estimated at less than 1%), as most events follow a well-defined official calendar published months in advance (e.g., BLS, BEA, FOMC schedules).
To minimize implementation complexity and avoid unnecessary data duplication, the service shall update future events in place, keeping only the most recent and accurate information.
This approach is sufficient for almost all operational and modeling use cases and avoids the overhead of maintaining event version history, which is typically only required in strict audit or regulatory environments.
- **EC-PR-04:** The ingestion logic shall implement explicit canonical mapping rules to convert `Category` values into canonical internal event names, as defined in `EC-PR-02`. This mapping shall be strictly maintained and updated as needed to handle naming variations from the provider.
- **EC-PR-05:** **SHOULD BE DISCUSSED** The ingestion process shall implement explicit filtering based on both `Category` and `Event` fields provided by the Trading Economics API.
First, only events matching the canonical categories defined in `EC-PR-02` shall be considered (using `Category` field, case-insensitive).
Then, for each selected category, additional fine-grained filtering using the `Event` field may be applied to exclude or include specific sub-releases as needed.
The canonical mapping table below defines the expected `Category` values and example `Event` names to guide implementers:

| Canonical Name                          | Example Category (TradingEconomics)       | Example Events inside Category                       |
|:---|:---|:---|
| CPI                                    | Consumer Price Index (CPI)               | CPI YoY, CPI MoM, Core CPI YoY, Core CPI MoM           |
| FOMC Rate Decision                     | FOMC Rate Decision                       | Fed Interest Rate Decision, Federal Funds Rate         |
| Non-Farm Payrolls                      | Non-Farm Payrolls                        | Non-Farm Payrolls, Non-Farm Payrolls Private, Non-Farm Payrolls Manufacturing |
| Retail Sales                           | Retail Sales                             | Retail Sales MoM, Retail Sales Ex Autos MoM            |
| Unemployment Rate                      | Unemployment Rate                        | Unemployment Rate, Participation Rate, U-6 Rate       |
| GDP Growth Rate                        | GDP Growth Rate                          | GDP Growth Rate QoQ Adv, GDP Growth Rate QoQ Final    |
| Producer Price Index                   | Producer Price Index                     | PPI YoY, PPI MoM, Core PPI YoY, Core PPI MoM          |
| ISM Manufacturing PMI                  | ISM Manufacturing PMI                    | Manufacturing PMI, New Orders, Employment Index       |
| ISM Services PMI                       | ISM Non-Manufacturing PMI                | Services PMI, Business Activity Index, New Orders    |
| University of Michigan Consumer Sentiment | Michigan Consumer Sentiment          | Sentiment Preliminary, Sentiment Final                 |
| Housing Starts                         | Housing Starts                           | Housing Starts, Building Permits, Housing Completions |
| Fed Balance Sheet                      | Fed Balance Sheet                        | Total Assets, Securities Held Outright, Reserve Balances |

Note: This table provides example `Event` names. Final implementation shall define precise matching logic (e.g., strict string match or allowed sub-strings) to avoid ambiguity and ensure consistent mapping.


### 5. Data Storage
- **EC-ST-01:** Filtered events **shall** be stored internally in a database table or document collection with the following schema:
  ```json
  {
    "event_date": "YYYY-MM-DD",
    "event_name": "string", // The canonical name from EC-PR-02
  }
  ```

---

## Part 3: API Specification

### 6. Get Events Matrix Endpoint
- **Endpoint:** `GET /events/matrix`
- **Purpose:** To retrieve a time-series feature matrix of one-hot encoded economic events.
- **Query Parameters:**
  - `start_date`: `YYYY-MM-DD` (inclusive) - Required.
  - `end_date`: `YYYY-MM-DD` (inclusive) - Required.
- **Processing Logic:**
  1.  Define the `feature_list` from the canonical event names in `EC-PR-02`.
  2.  Fetch all filtered events from internal storage for the requested date range.
  3.  Initialize an output JSON object, `events_matrix`.
  4.  Iterate through each day `d` from `start_date` to `end_date`. For each day, create a key `d` in `events_matrix` with a value being an object where each feature from `feature_list` is set to `0`.
  5.  Iterate through the fetched events. For each `event` on a given `event_date`, set the corresponding feature in `events_matrix[event_date]` to `1`.
 
**EC-API-01:** The API response shall always include all canonical event names defined in `EC-PR-02` as keys in the returned feature object for every date, even if no events occur on that date. In such cases, the corresponding value shall be `0`. This ensures the feature matrix has a consistent fixed schema across different date ranges.

**Success Response (200 OK):**
  ```json
  {
    "2024-03-11": {"fomc_rate_decision": 0, "cpi": 0},
    "2024-03-12": {"fomc_rate_decision": 0, "cpi": 1},
    "2024-03-13": {"fomc_rate_decision": 0, "cpi": 0}
  }
  ```

--- 
