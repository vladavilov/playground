# Market Data Feed Requirements

**Version: 1.1**

---

## 1. Introduction

### 1.1. Purpose
This document specifies the requirements for the **Market Data Feed**, which is a centralized source of financial and economic indicators. The primary purpose of this feed is to provide a consistent, reliable, and timely stream of data to all downstream consumer systems, including but not limited to the **Market Regime Classification Model** and the **Financial Calculation Service**.

### 1.2. Scope
These requirements cover the sourcing, processing, and delivery of all general (i.e., not instrument-specific) market indicators required for analytics.

---

## 2. Data Specification

### 2.1. General Requirements
- **MDF-GR-01:** The Market Data Feed **shall** provide all indicators defined in the Data Dictionary (Section 2.2) as a single data object for each requested timestamp.
- **MDF-GR-02:** All indicators **shall** be delivered as `float` data types.
- **MDF-GR-03:** The feed **shall** guarantee that no values are null or missing. The data ingestion service is responsible for handling any upstream data gaps.

### 2.2. Data Dictionary and Sourcing
The Market Data Ingestion Service **shall** source each indicator from the systems and vendors specified in the table below. The service is responsible for all vendor API integration, authentication, and data normalization.

| Field Name | Description | Update Freq. | Primary Source | API / Calculation Details | Sample Response File |
|:---|:---|:---|:---|:---|:---|
| `vix_index` | CBOE Volatility Index. | Daily | TradingEconomics | `curl -X GET 'https://api.tradingeconomics.com/markets/symbol/VIX:IND?c=YOUR_API_KEY'` | [`TradingEconomics - VIX - Response.json`](./market_data_feed_responses/TradingEconomics%20-%20VIX%20-%20Response.json) |
| `move_index` | Merrill Lynch Option Volatility Estimate Index. | Daily | TradingEconomics | `curl -X GET "https://api.tradingeconomics.com/markets/symbol/MOVE:IND?c=YOUR_API_KEY"` <br/> *Alternative Sources: ICE, Bloomberg* | **Primary:** [`TradingEconomics - MOVE - Response.json`](./market_data_feed_responses/TradingEconomics%20-%20MOVE%20-%20Response.json) <br/> **Alt (ICE):** [`ICE - MOVE - Response.json`](./market_data_feed_responses/ICE%20-%20MOVE%20-%20Response.json) <br/> **Alt (Bloomberg):** [`Bloomberg BLAPI - MOVE - Response.json`](./market_data_feed_responses/Bloomberg%20BLAPI%20-%20MOVE%20-%20Response.json) |
| `investment_grade_credit_spread` | The spread of the Bloomberg U.S. Corporate Investment Grade Index. | Daily | Bloomberg | Requires Bloomberg Terminal access (BLAPI). If unavailable, use the LQD ETF proxy calculation defined in **Section 2.3.1**. | [`Bloomberg BLAPI - LQD - Response.json`](./market_data_feed_responses/Bloomberg%20BLAPI%20-%20LQD%20-%20Response.json) |
| `high_yield_credit_spread` | The spread of the Bloomberg U.S. Corporate High Yield Index. | Daily | Bloomberg | Requires Bloomberg Terminal access (BLAPI). If unavailable, use the HYG ETF proxy calculation defined in **Section 2.3.2**. | [`Bloomberg BLAPI - HYG - Response.json`](./market_data_feed_responses/Bloomberg%20BLAPI%20-%20HYG%20-%20Response.json) |
| `tips_breakeven_5y` | The 5-Year TIPS Breakeven Rate. | Daily | Federal Reserve (FRED) | `curl "https://api.stlouisfed.org/fred/series/observations?series_id=T5YIE&api_key=YOUR_FRED_API_KEY&file_type=json"` | [`FRED - T5YIE - Response.json`](./market_data_feed_responses/FRED%20-%20T5YIE%20-%20Response.json) |
| `swap_spread_10y` | The 10-year U.S. Dollar interest rate swap vs. Treasury spread. | Daily | Federal Reserve (FRED) | Calculated by subtracting the 10-Year Treasury Yield from the 10-Year Swap Rate. See details in **Section 2.2.1**. <br/> *Alternative Sources: ICE, Bloomberg* | **Primary:** [`FRED - 10-year Swap Spread - Response.json`](./market_data_feed_responses/FRED%20-%2010-year%20Swap%20Spread%20-%20Response.json) <br/> **Alt (ICE):** [`ICE - 10-year Swap Spread - Response.json`](./market_data_feed_responses/ICE%20-%2010-year%20Swap%20Spread%20-%20Response.json) |
| `muni_fund_flows_net` | Net flows into municipal bond funds. | Weekly | Federal Reserve (FRED) | `curl "https://api.stlouisfed.org/fred/series/observations?series_id=MUTFUNDSMUNIBONDS&api_key=YOUR_FRED_API_KEY&file_type=json"` <br/> *Alternative Sources: Investment Company Institute (ICI), Bloomberg. **Note: The ICI source requires manual data ingestion from their website.**` | **Alt (ICI):** [`ICI - Net Flows into Muni - MUNIFLOW - Response.json`](./market_data_feed_responses/ICI%20-%20Net%20Flows%20into%20Muni%20-%20MUNIFLOW%20-%20Response.json) <br/> **Alt (Bloomberg):** [`Bloomberg BLAPI - MUNIFLOW - Response.json`](./market_data_feed_responses/Bloomberg%20BLAPI%20-%20MUNIFLOW%20-%20Response.json) |
| `us_cpi_yoy` | Year-over-year change in the Consumer Price Index. | Monthly | Federal Reserve (FRED) | `curl "https://api.stlouisfed.org/fred/series/observations?series_id=CPIAUCSL&units=pc1&api_key=YOUR_API_KEY"` <br/> *Alternative Sources: TradingEconomics, Bloomberg* | **Primary:** [`FRED - USCPIYOY - Response.json`](./market_data_feed_responses/FRED%20-%20USCPIYOY%20-%20Response.json) <br/> **Alt (TradingEconomics):** [`TradingEconomics - USCPIYOY - Response.json`](./market_data_feed_responses/TradingEconomics%20-%20USCPIYOY%20-%20Response.json) <br/> **Alt (Bloomberg):** [`Bloomberg BLAPI - USCPIYOY - Response.json`](./market_data_feed_responses/Bloomberg%20BLAPI%20-%20USCPIYOY%20-%20Response.json) |
| `ust_benchmark_curve` | U.S. Treasury yields, keyed by tenor (e.g., '1M', '3M', '1Y', '10Y'). | Daily | Federal Reserve (FRED) | Requires multiple API calls for each tenor (e.g., DGS1MO, DGS3MO, DGS1, etc.). | - |
| `mmd_benchmark_curve` | Municipal Market Data (MMD) AAA benchmark curve, keyed by tenor. | Daily | ICE / Bloomberg | Requires specialized data license (e.g., ICE Data Services, Bloomberg BVAL). | - |
| `sector_credit_spread_curve` | A curve of credit spreads for various sectors/industries, keyed by tenor. | Daily | ICE / Bloomberg | Requires specialized data license (e.g., Bloomberg BVAL). | - |
| `interest_rate_volatility_surface` | A matrix of implied volatilities for different tenors and strikes. | Daily | ICE / Bloomberg | Requires specialized data license. | - |

#### 2.2.1. Calculation for 10-year Swap Spread (FRED)
The spread is calculated in basis points as `(Swap Rate - Treasury Yield) * 100`.
- **Step 1: Retrieve 10-Year Swap Rate (ICERATES1100USD10Y)**<br/>`curl "https://api.stlouisfed.org/fred/series/observations?series_id=ICERATES1100USD10Y&api_key=YOUR_FRED_API_KEY&file_type=json"`
- **Step 2: Retrieve 10-Year Treasury Yield (DGS10)**<br/>`curl "https://api.stlouisfed.org/fred/series/observations?series_id=DGS10&api_key=YOUR_FRED_API_KEY&file_type=json"`

### 2.3. Proxy Calculations for Credit Spreads
This section defines the requirements for calculating credit spreads using ETF data when primary Bloomberg index data is not available.

#### 2.3.1. Investment Grade Credit Spread (LQD ETF Proxy)
- **MDF-PR-01:** The service **shall** retrieve the current yield for the LQD ETF from a designated financial data provider.
  - *Example (Financial Modeling Prep API):* `curl "https://financialmodelingprep.com/api/v3/quote/LQD?apikey=YOUR_FMP_API_KEY"`
- **MDF-PR-02:** The service **shall** retrieve the 10-Year Treasury Constant Maturity Rate (DGS10) from FRED as the risk-free benchmark.
  - *API Call:* `curl "https://api.stlouisfed.org/fred/series/observations?series_id=DGS10&api_key=YOUR_FRED_API_KEY&file_type=json"`
- **MDF-PR-03:** The Investment Grade Credit Spread **shall** be calculated in basis points as: `(LQD Yield - DGS10) * 100`.

#### 2.3.2. High Yield Credit Spread (HYG ETF Proxy)
- **MDF-PR-04:** The service **shall** retrieve the current yield for the HYG ETF from a designated financial data provider.
  - *Example (Financial Modeling Prep API):* `curl "https://financialmodelingprep.com/api/v3/quote/HYG?apikey=YOUR_FMP_API_KEY"`
- **MDF-PR-05:** The service **shall** retrieve the 5-Year Treasury Constant Maturity Rate (DGS5) from FRED as the risk-free benchmark.
  - *API Call:* `curl "https://api.stlouisfed.org/fred/series/observations?series_id=DGS5&api_key=YOUR_FRED_API_KEY&file_type=json"`
- **MDF-PR-06:** The High Yield Credit Spread **shall** be calculated in basis points as: `(HYG Yield - DGS5) * 100`.

---

## 3. System Architecture and Integration

### 3.1. Data Ingestion
- **MDF-AR-01:** A dedicated **Market Data Ingestion Service** **shall** be implemented to source all indicators.
- **MDF-AR-02:** Consuming systems (e.g., Market Regime Classification Model, Financial Calculation Service) **shall** consume data exclusively from the Market Data Ingestion Service and **shall not** connect directly to any external data vendor.
- **MDF-AR-03:** The Market Data Ingestion Service **shall** continuously poll and persist all external market indicators into an internal time-series data store.

### 3.2. Data Delivery
#### 3.2.1. Real-Time Delivery
- **MDF-DD-01:** The service **shall** expose a synchronous API endpoint that accepts a target `date` to retrieve market data.
- **MDF-DD-02:** Upon request, the service **shall** query its internal data store and return the latest available set of market indicators recorded on or before the specified `date`.
- **MDF-DD-03:** The API response **shall** conform to the following JSON schema, where `data_timestamp` reflects the actual timestamp of the retrieved data.

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
    "us_cpi_yoy": "float",
    "ust_benchmark_curve": "object",
    "mmd_benchmark_curve": "object",
    "sector_credit_spread_curve": "object",
    "interest_rate_volatility_surface": "object"
  }
}
```

#### 3.2.2. Historical Delivery
- **MDF-DD-04:** For historical data requests, the flow should be the same as the real-time delivery, but the data should be retrieved from the internal store first. If the data is not available in the internal store, the service should retrieve it from the external provider for a specified date.

---

## 4. Non-Functional Requirements

### 4.1. Timeliness & Freshness

- **MDF-NFR-01:** The end-to-end latency for the real-time API call (from request to response) **should** be under 100ms.