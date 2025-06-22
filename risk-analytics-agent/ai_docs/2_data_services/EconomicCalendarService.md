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

### 4. Ingestion & Filtering Logic
- **EC-PR-01:** Upon ingestion, raw events **shall** be immediately filtered *before* being saved to the service's internal storage.
- **EC-PR-02:** The filtering criteria are as follows:
  - `Country` **must** be "United States".
  - `Importance` **must** be `3` (High).
  - `Category` (event name) **must** contain one of the following canonical strings (case-insensitive search):
    - "FOMC Rate Decision"
    - "CPI"
    - "Non-Farm Payrolls"
    - "Retail Sales"
    - "Unemployment Rate"
    - "GDP Growth Rate"
    - "Producer Price Index"
    - "ISM Manufacturing PMI"
    - "ISM Services PMI"
    - "University of Michigan Consumer Sentiment"
    - "Housing Starts"
    - "Fed Balance Sheet"
    *Note: The ingestion logic must handle variations in the provider's naming (e.g., "Consumer Price Index (CPI)" should map to "CPI", weekly H.4.1 release should map to "Fed Balance Sheet").*

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
- **Success Response (200 OK):**
  ```json
  {
    "2024-03-11": {"fomc_rate_decision": 0, "cpi": 0},
    "2024-03-12": {"fomc_rate_decision": 0, "cpi": 1},
    "2024-03-13": {"fomc_rate_decision": 0, "cpi": 0}
  }
  ```

--- 