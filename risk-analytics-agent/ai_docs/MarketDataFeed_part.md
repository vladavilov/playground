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

| Field Name                       | Data Type | Description                                                            | Update Frequency     |
|:---------------------------------|:----------|:-----------------------------------------------------------------------|----------------------|
| `vix_index`                      | `float`   | CBOE Volatility Index.                                                 | Daily                |
| `move_index`                     | `float`   | Merrill Lynch Option Volatility Estimate Index.                        | Daily                |
| `investment_grade_credit_spread` | `float`   | The spread of the Bloomberg U.S. Corporate Investment Grade Index.     | Daily                |
| `high_yield_credit_spread`       | `float`   | The spread of the Bloomberg U.S. Corporate High Yield Index.           | Daily                |
| `tips_breakeven_5y`              | `float`   | The 5-Year TIPS Breakeven Rate.                                        | Daily                |
| `swap_spread_10y`                | `float`   | The 10-year U.S. Dollar interest rate swap vs. Treasury spread.        | Daily                |
| `muni_fund_flows_net`            | `float`   | Net flows into municipal bond funds.                                   | Weekly               |
| `us_cpi_yoy`                     | `float`   | Year-over-year change in the Consumer Price Index.                     | Monthly, mid-month   |

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

---

## 4. Data Providers

### 4.1. API Calls for Financial Indicators

| Index                          | Source                             | Sample API call / Request Script                                                                                                                                         |
|--------------------------------|------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| VIX                            | TradingEconomics                   | `curl -X GET 'https://api.tradingeconomics.com/markets/symbol/VIX:IND?c=YOUR_API_KEY'`                                                                                   |
| MOVE                           | TradingEconomics                   | `curl -X GET "https://api.tradingeconomics.com/markets/symbol/MOVE:IND?c=YOUR_API_KEY"`                                                                                   |
| MOVE                           | ICE                                | `curl -X GET "https://api.theice.com/marketdata/v1/timeseries/MOVE_INDEX/latest" -H "Authorization: Bearer YOUR_ICE_API_KEY"`                                              |
| MOVE                           | Bloomberg                          | Bloomberg BLAPI                    |
| LQD ETF                        | Bloomberg                          | Bloomberg BLAPI                    |
| HYG ETF                        | Bloomberg                          | Bloomberg BLAPI                    |
| T5YIE                          | Federal Reserve (FRED)             | `curl "https://api.stlouisfed.org/fred/series/observations?series_id=T5YIE&api_key=YOUR_FRED_API_KEY&file_type=json"`                                                     |
| 10-year Swap Spread            | Federal Reserve (FRED)             | see ### 4.2 below  |
| 10-year Swap Spread            | ICE                                | `curl -X GET "https://api.theice.com/marketdata/v1/timeseries/USD_SWAP_SPREAD_10Y/latest" -H "Authorization: Bearer YOUR_ICE_API_KEY"`                                        |
| 10-year Swap Spread            | Bloomberg                          | Bloomberg BLAPI                    |
| Net Flows into Muni Bond Funds | Investment Company Institute (ICI) | No official API  |
| Net Flows into Muni Bond Funds | Federal Reserve (FRED)             | `curl "https://api.stlouisfed.org/fred/series/observations?series_id=MUTFUNDSMUNIBONDS&api_key=YOUR_FRED_API_KEY&file_type=json"`                                          |
| Net Flows into Muni Bond Funds | Bloomberg                          | Bloomberg BLAPI         |
| US CPI YoY                     | TradingEconomics                   | `curl -X GET "https://api.tradingeconomics.com/historical/country/united%20states/indicator/inflation%20rate?c=guest:guest&format=json"`                                     |
| US CPI YoY                     | Federal Reserve (FRED)             | `https://api.stlouisfed.org/fred/series/observations?series_id=CPIAUCSL&units=pc1&api_key=YOUR_API_KEY`                      |
| US CPI YoY                     | Bloomberg                          | Bloomberg BLAPI          |

### 4.2. 10-year Swap Spread from Federal Reserve (FRED)
#### Step 1: Retrieve 10-Year Swap Rate (ICERATES1100USD10Y)
curl "https://api.stlouisfed.org/fred/series/observations?series_id=ICERATES1100USD10Y&api_key=YOUR_FRED_API_KEY&file_type=json"
#### Step 1: Retrieve 10-Year Treasury Yield (DGS10)
curl "https://api.stlouisfed.org/fred/series/observations?series_id=DGS10&api_key=YOUR_FRED_API_KEY&file_type=json"
#### Step 3: Calculate the 10-Year Swap Spread
10-Year Swap Spread (bps) = (Swap Rate - Treasury Yield) * 100

## 5. Data Delivery Contract

### 5.1. Real-Time Delivery
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

### 5.2. Historical Delivery
- **MDF-DC-04:** For historical data requests, the service **shall** provide a file (e.g., CSV) containing a `timestamp` column and a column for each of the eight indicators.
- **MDF-DC-05:** The column headers in the historical file **shall** exactly match the field names specified in the data dictionary (Section 2.1).

---

## 6. Non-Functional Requirements

### 6.1. Timeliness & Freshness
- **MDF-NFR-02:** The end-to-end latency for the real-time API call (from request to response) **should** be under 100ms.
