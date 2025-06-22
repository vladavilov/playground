# Ownership Data Service: Requirements Specification

**Version: 1.0**

## 1. Overview & Purpose
The Ownership Data Service is a specialized microservice responsible for sourcing and aggregating security ownership information from dedicated financial data vendors. Its purpose is to provide a clean, standardized view of an instrument's ownership structure, which is a critical input for calculating concentration risk.

This service abstracts the complexity of integrating with vendor APIs and managing data caching for information that is updated infrequently (e.g., quarterly).

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
  "ownership": {
    "is_concentrated_flag": "boolean",
    "top_3_holders_pct": "float"
  }
}
```

## 4. Methodology: Data Sourcing
- **ODS-R-05:** The **Primary Data Source** **shall** be a dedicated financial data vendor that aggregates ownership data from global regulatory filings (e.g., SEC 13F). Examples include **Bloomberg (HDS), Refinitiv Eikon, FactSet**.
- **ODS-R-06:** The specific vendor API endpoint and authentication credentials **shall** be externally configurable.

### 4.1. Ownership Concentration
- **Purpose (Trader View):** To identify hidden liquidity risk. If a small number of entities hold a large percentage of the issue, a decision by a single holder to sell can disproportionately impact the price.
- **Input Model:** `OwnershipData`
- **Logic:**
    1. Sort the `holders` array by `ownership_pct` in descending order.
    2. Sum the `ownership_pct` of the top 3 holders to get `top_3_holders_pct`.
    3. Apply **Business Rule BR-21** to set the `is_concentrated_flag`.
- **BR-21:** The `ownership.is_concentrated_flag` **shall** be set to `true` if `top_3_holders_pct` is greater than or equal to 50.0. Otherwise, it **shall** be `false`.

## 5. Non-Functional Requirements
- **NFR-ODS-01 (Configurability):** All data source endpoints and API keys **shall** be externally configurable.
- **NFR-ODS-02 (Caching):** The caching layer **shall** have a configurable time-to-live (TTL) appropriate for quarterly-updated data (e.g., 30 days). 