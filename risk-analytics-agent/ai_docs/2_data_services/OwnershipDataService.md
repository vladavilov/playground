# Ownership Data Service: Requirements Specification


## 1. Overview & Purpose
The Ownership Data Service is a specialized microservice responsible for sourcing raw holder data for a given security from dedicated financial data vendors. Its sole purpose is to abstract the complexity of vendor API integration and provide a clean, standardized list of holders and their positions.

This service acts as a simple proxy for vendor data, performing no calculations or aggregations.

## 2. System Requirements

### 2.1. Functional Requirements
- **ODS-R-01:** The service **shall** accept an instrument's CUSIP and an `as_of_date` as input.
- **ODS-R-02:** The service **shall** query a pre-configured financial data vendor's API to retrieve the ownership data for the given CUSIP.
- **ODS-R-03:** The service **shall** parse the vendor's API response into the standardized output model defined below.
- **ODS-R-04:** The service **shall** implement a caching mechanism to store and retrieve ownership data, reducing redundant API calls, as this data is updated infrequently. The cache key should be based on the CUSIP and the relevant reporting period (e.g., quarter).

## 3. Data Models

### 3.1. Input Model
```json
{
  "cusip": "string",
  "as_of_date": "date"
}
```

### 3.2. Output Model
```json
{
  "cusip": "string",
  "as_of_date": "date",
  "holders": [
    {
      "holder_name": "string",
      "ownership_pct": "float"
    }
  ]
}
```

## 4. Methodology: Data Sourcing
- **ODS-R-05:** The **Primary Data Source** **shall** be a dedicated financial data vendor that aggregates ownership data from global regulatory filings (e.g., SEC 13F). Examples include **Bloomberg (HDS), Refinitiv Eikon, FactSet**.
- **ODS-R-06:** The specific vendor API endpoint and authentication credentials **shall** be externally configurable.

## 5. Non-Functional Requirements
- **NFR-ODS-01 (Configurability):** All data source endpoints and API keys **shall** be externally configurable.
- **NFR-ODS-02 (Caching):** The caching layer **shall** have a configurable time-to-live (TTL) appropriate for quarterly-updated data (e.g., 30 days). 