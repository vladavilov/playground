# State Fiscal Data Feed Requirements

**Version: 1.0**

---

## 1. Introduction

### 1.1. Purpose
This document specifies the requirements for the **State Fiscal Data Feed**, which is the sole source of input data for the **Financial Calculation Engine** regarding state-level economic health. The primary purpose of this feed is to provide a consistent and reliable stream of fiscal indicators for analyzing municipal securities.

### 1.2. Scope
These requirements exclusively cover the sourcing, processing, and delivery of the state fiscal indicators listed in this document. The internal logic of any consuming system is out of scope.

---

## 2. Data Dictionary & Schema

### 2.1. Data Requirements
- **SFF-DR-01:** The State Fiscal Data Feed **shall** provide the following indicators as a single data object for a given U.S. State and timestamp.
- **SFF-DR-02:** All indicators **shall** be delivered as `float` data types.
- **SFF-DR-03:** The feed **shall** guarantee that no values are null or missing for a requested state and date. The data ingestion service is responsible for handling any upstream data gaps, potentially by carrying forward the last known value if a new one is not yet published.

| Field Name                                   | Data Type | Description                                                                                               |
|:---------------------------------------------|:----------|:----------------------------------------------------------------------------------------------------------|
| `state_tax_receipts_yoy_growth`              | `float`   | Year-over-year growth in total tax receipts for the relevant state.                                       |
| `state_budget_surplus_deficit_as_pct_of_gsp` | `float`   | The state's budget surplus or deficit as a percentage of its Gross State Product (GSP).                     |

---

## 3. Data Sourcing & System Architecture

### 3.1. Architectural Mandate
- **SFF-AR-01:** A dedicated **State Fiscal Data Ingestion Service** **shall** be implemented and used to source all externally-provided indicators.
- **SFF-AR-02:** The Financial Calculation Engine **shall not** connect directly to any external data vendor or public source. It **shall** consume data exclusively from the State Fiscal Data Ingestion Service.
- **SFF-AR-03:** The State Fiscal Data Ingestion Service **shall** periodically poll and persist all external fiscal indicators into an internal time-series data store, keyed by state.

### 3.2. Sourcing Requirements
- **SFF-SR-01:** The State Fiscal Data Ingestion Service **shall** source each indicator from the system specified in the table below.
- **SFF-SR-02:** The service **shall** be responsible for all vendor API integration, authentication, and data normalization.

| Feature Name                                   | Source System           | Primary Source / Endpoint                                                                                               |
|:-----------------------------------------------|:------------------------|:------------------------------------------------------------------------------------------------------------------------|
| `state_tax_receipts_yoy_growth`                | **Trading Economics API** | **Federal Reserve (FRED) Endpoint**. This endpoint provides access to state-level data, including tax revenue series.   |
| `state_budget_surplus_deficit_as_pct_of_gsp`   | **Trading Economics API** | **Federal Reserve (FRED) Endpoint**. This provides access to Gross State Product (GSP) and state government budget data. |

*Note: The Trading Economics API acts as a data aggregator for this information. The ultimate underlying data sources are public entities like the U.S. Census Bureau and the Bureau of Economic Analysis (BEA), which are made available via the FRED database.*

---

## 4. Data Delivery Contract

### 4.1. API Delivery
- **SFF-DC-01:** The service **shall** expose a synchronous API endpoint that accepts a target `state` (as a two-letter postal code, e.g., `CA`) and a target `date` to retrieve the relevant data.
- **SFF-DC-02:** Upon receiving a request, the service **shall** query its internal data store and return the latest available set of fiscal indicators recorded on or before the specified `date` for the given `state`.
- **SFF-DC-03:** The API response **shall** conform to the following JSON schema, where `data_timestamp` reflects the actual "as of" date of the sourced data (e.g., quarter-end).

```json
{
  "data_timestamp": "date",
  "state": "string",
  "state_fiscal_indicators": {
    "state_tax_receipts_yoy_growth": "float",
    "state_budget_surplus_deficit_as_pct_of_gsp": "float"
  }
}
```

### 4.2. Historical Delivery
- **SFF-DC-04:** For historical back-testing or bulk data needs, the service **shall** provide a mechanism to retrieve a file (e.g., CSV) containing a `timestamp` column, a `state` column, and a column for each of the fiscal indicators.
- **SFF-DC-05:** The column headers in the historical file **shall** exactly match the field names specified in the data dictionary (Section 2.1).

---

## 5. Non-Functional Requirements

### 5.1. Timeliness & Freshness
- **SFF-NFR-01:** Given the low frequency of source data publication (quarterly/annually), the data in the internal store **should** be updated within 5 business days of its publication by the source entity.