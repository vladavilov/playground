# Market Data Feed Requirements

**Version: 1.0**

---

## 1. Introduction

### 1.1. Purpose
This document specifies the requirements for the **Market Data Feed**, which is the sole source of input data for the **Market Regime Classification Model**. The primary purpose of this feed is to provide a consistent, reliable, and timely stream of financial and economic indicators necessary for the model to perform its real-time and historical classification tasks.

### 1.2. Scope
These requirements exclusively cover the sourcing, processing, and delivery of the input indicators listed in this document. The internal logic of the consuming system (the Market Regime Classification Model) is out of scope.

---

## 2. Data Dictionary & Schema

### 2.1. Data Requirements
- **MDF-DR-01:** The Market Data Feed **shall** provide the following indicators as a single data object for each requested timestamp.
- **MDF-DR-02:** All indicators **shall** be delivered as `float` data types.
- **MDF-DR-03:** The feed **shall** guarantee that no values are null or missing. The data ingestion service is responsible for handling any upstream data gaps.

| Field Name                       | Data Type | Description                                                                 |
|:---------------------------------|:----------|:----------------------------------------------------------------------------|
| `vix_index`                      | `float`   | CBOE Volatility Index.                                                      |
| `move_index`                     | `float`   | Merrill Lynch Option Volatility Estimate Index.                             |
| `investment_grade_credit_spread` | `float`   | The spread of the Bloomberg U.S. Corporate Investment Grade Index.          |
| `high_yield_credit_spread`       | `float`   | The spread of the Bloomberg U.S. Corporate High Yield Index.                |
| `tips_breakeven_5y`              | `float`   | The 5-Year TIPS Breakeven Rate.                                             |
| `swap_spread_10y`                | `float`   | The 10-year U.S. Dollar interest rate swap vs. Treasury spread.             |
| `muni_fund_flows_net`            | `float`   | Net flows into municipal bond funds.                                        |
| `us_cpi_yoy`                     | `float`   | Year-over-year change in the Consumer Price Index.                          |

---

## 3. Data Sourcing & System Architecture

### 3.1. Architectural Mandate
- **MDF-AR-01:** As per requirement **MR-NFR-01**, a dedicated **Market Data Ingestion Service** **shall** be implemented and used to source all externally-provided indicators.
- **MDF-AR-02:** The Market Regime Classification Model **shall not** connect directly to any external data vendor. It **shall** consume data exclusively from the Market Data Ingestion Service.
- **MDF-AR-03:** The Market Data Ingestion Service **shall** continuously poll and persist all external market indicators into an internal time-series data store.

### 3.2. Sourcing Requirements
- **MDF-SR-01:** The Market Data Ingestion Service **shall** source each indicator from the systems and vendors specified in the table below.
- **MDF-SR-02:** The service **shall** be responsible for all vendor API integration, authentication, and data normalization.

| Feature Name                     | Source System         | Primary Source / Ticker                                       |
|:---------------------------------|:----------------------|:--------------------------------------------------------------|
| `vix_index`                      | External Data Vendor  | **CBOE** (e.g., `VIX Index`)                                  |
| `move_index`                     | External Data Vendor  | **ICE/BofA** (e.g., `MOVE Index`)                             |
| `investment_grade_credit_spread` | External Data Vendor  | **Bloomberg** (e.g., LQD ETF spread vs. Treasury)             |
| `high_yield_credit_spread`       | External Data Vendor  | **Bloomberg** (e.g., HYG ETF spread vs. Treasury)             |
| `tips_breakeven_5y`              | External Data Vendor  | **U.S. Treasury / FRED** (Ticker: `T5YIE`)                    |
| `swap_spread_10y`                | External Data Vendor  | Market Data Aggregator (e.g., **Bloomberg**)                  |
| `muni_fund_flows_net`            | External Data Vendor  | **Refinitiv Lipper** or **ICI**                               |
| `us_cpi_yoy`                     | External Data Vendor  | **U.S. Bureau of Labor Statistics (BLS)**                     |
! `treasury_curve`                 | External Data Vendor  | FRED  For code in DGS1-DGS30                                  |
| `swaption_vol`                   | External Data Vendor  | CME,BBG,FinPricing,ICE                                            | 
  

---

## 4. Data Delivery Contract

### 4.1. Real-Time Delivery
- **MDF-DC-01:** The service **shall** expose a synchronous API endpoint that accepts a target `date` to retrieve market data.
- **MDF-DC-02:** Upon receiving a request, the service **shall** query its internal data store and return the latest available set of market indicators recorded on or before the specified `date`.
- **MDF-DC-03:** The API response **shall** conform to the following JSON schema, where `data_timestamp` reflects the actual timestamp of the retrieved data from the internal store.

```json
{
  "data_timestamp": "datetime",
  "market_indicators": {
    "vix_index": "float",
    "move_index": "float",
    "investment_grade_credit_spread": "float",
    "high_yield_credit_spread": "float",
    "tips_breakeven_5y": "float",
    "swap_spread_10y": "float",
    "muni_fund_flows_net": "float",
    "us_cpi_yoy": "float"
  }
}
```

### 4.2. Historical Delivery
- **MDF-DC-04:** For historical data requests, the service **shall** provide a file (e.g., CSV) containing a `timestamp` column and a column for each of the eight indicators.
- **MDF-DC-05:** The column headers in the historical file **shall** exactly match the field names specified in the data dictionary (Section 2.1).

---

## 5. Non-Functional Requirements

### 5.1. Timeliness & Freshness
- **MDF-NFR-01:** For the **Real-Time Mode**, the `data_timestamp` for any provided data point **should** be no more than 60 minutes older than the request time.
- **MDF-NFR-02:** The end-to-end latency for the real-time API call (from request to response) **should** be under 100ms.