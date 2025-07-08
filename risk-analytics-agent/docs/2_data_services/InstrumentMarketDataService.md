# Instrument Market Data Service: Requirements Specification

## 1. Overview

### 1.1. Purpose
This document defines the requirements for the `InstrumentMarketDataService` that provides real-time and historical instrument-specific market data for fixed-income securities. This service is a critical input to the `FinancialCalculationService` and directly populates the `InstrumentMarketData` model, which drives core pricing, liquidity analysis, and risk calculations.

### 1.2. Service Dependencies
The `InstrumentMarketDataService` feeds directly into the `FinancialCalculationService` where its output is used for:
- Primary price determination (FR-04)
- Bid/ask spread calculations (FR-15) 
- Liquidity composite scoring (40% weight)
- Market depth analysis
- Duration-dependent risk calculations (OAS, CS01, benchmark interpolation)

## 2. Functional Requirements

### 2.1. Operating Modes
- **FR-IMD-01:** The service **shall** support **Current Mode**, providing the most recent real-time market data for a specified CUSIP.
- **FR-IMD-02:** The service **shall** support **Historical Mode**, providing market data as-of a specified historical date for a given CUSIP.
- **FR-IMD-03:** Historical data retrieval **shall** be point-in-time accurate, reflecting the exact market state that existed at the specified timestamp.

### 2.2. Data Output Schema
The service **shall** produce an output conforming to the `InstrumentMarketData` model:

```json
{
  "cusip": "string",
  "data_timestamp": "datetime",
  "calculation_mode": "string", // "current" or "historical"
  "as_of_date": "date", // For historical mode
  "market_data": {
    "last_trade_price": "float",
    "bid_price": "float", 
    "ask_price": "float",
    "bid_size": "float",
    "ask_size": "float",
    "duration": "float"
  },
  "data_quality_flags": { // optional for phase 1
    "price_source": "string",
    "duration_source": "string", 
    "bid_ask_staleness_minutes": "integer",
    "last_trade_age_hours": "integer"
  }
}
```

### 2.3. Data Sources & Provider Integration

#### 2.3.1. Primary Market Data Providers
- **FR-IMD-04:** The service **shall** integrate with the following market data providers in order of priority:
  1. **Bloomberg Terminal API (BLAPI)** - Primary source for real-time quotes and trade data
  2. **Thomson Reuters Eikon** - Secondary source for quote validation and gap-filling
  3. **MarketAxess** - Electronic trading platform data for institutional quotes
  4. **Tradeweb** - Alternative electronic trading platform
  5. **MSRB EMMA** - Municipal securities transaction data
  6. **FINRA TRACE** - Corporate bond transaction reporting

#### 2.3.2. Duration Data Sources  
- **FR-IMD-05:** The `duration` field **shall** be sourced from analytical data providers in the following priority order:
  1. **Bloomberg BVAL (Bloomberg Valuation)** - Primary duration calculations
  2. **ICE Data Services** - Secondary duration analytics
  3. **Refinitiv (formerly Thomson Reuters)** - Tertiary duration source
  4. **Internal Duration Calculation Engine** - Fallback calculation using modified/effective duration methodology from Section 4.13 of FinancialCalculationService.md

### 2.4. Data Population Algorithms

#### 2.4.1. Quote Aggregation Methodology
- **FR-IMD-06:** The service **shall** implement a **Best Bid/Offer (BBO) aggregation algorithm** that:
  1. Collects real-time quotes from all connected market data providers
  2. Identifies the highest bid price across all sources as `bid_price`
  3. Identifies the lowest ask/offer price across all sources as `ask_price`
  4. Aggregates the total par amount available at the best bid as `bid_size`
  5. Aggregates the total par amount available at the best ask as `ask_size`
  6. Timestamps each quote and applies data quality filters

#### 2.4.2. Last Trade Price Determination
- **FR-IMD-07:** The `last_trade_price` **shall** be determined using the following priority algorithm:
  1. **Real-time Trade Feeds:** Most recent executed trade from Bloomberg, Reuters, or electronic platforms within the last 4 hours
  2. **TRACE/EMMA Data:** Most recent reported transaction within the last trading day
  3. **Pricing Service Estimates:** Bloomberg BVAL or equivalent evaluated price if no recent trades
  4. **Previous Close:** Last validated closing price if no current day activity
  5. **Null Handling:** Field shall be `null` if no price discovery mechanism yields a valid result

#### 2.4.3. Duration Calculation Priority Logic
- **FR-IMD-08:** The `duration` field population **shall** follow this waterfall logic:
  1. **Bloomberg BVAL Duration:** Direct API call to Bloomberg's analytical duration calculation
  2. **ICE Analytics Duration:** Retrieved from ICE Data Services analytics feed  
  3. **Refinitiv Duration:** Retrieved from Refinitiv analytics platform
  4. **Internal Calculation:** Fallback to internal modified duration or effective duration calculation based on bond's callable status
  5. **Estimated Duration:** If all sources fail, use duration estimate based on time-to-maturity and coupon rate using the approximation: `Duration ≈ (1 - (1 + y)^(-n)) / y` where `y` = yield/frequency and `n` = periods to maturity

### 2.5. Data Quality & Validation Rules

#### 2.5.1. Quote Validation Rules
- **FR-IMD-09:** All incoming quotes **shall** pass the following validation filters:
  1. **Bid ≤ Ask Constraint:** `bid_price` must be less than or equal to `ask_price`
  2. **Price Range Validation:** Bid and ask prices must be within ±20% of the previous day's closing price
  3. **Size Validation:** `bid_size` and `ask_size` must be positive values and within reasonable market size limits (≤ $50MM for individual quotes)
  4. **Staleness Filter:** Quotes older than 15 minutes shall be flagged but not discarded
  5. **Cross-Venue Arbitrage Check:** If bid > ask across venues, flag for manual review

#### 2.5.2. Data Completeness Scoring
- **FR-IMD-10:** The service **shall** calculate and report a **Data Completeness Score** for each instrument:
  - **Complete (Score: 100):** All 6 fields populated from primary sources within staleness thresholds
  - **Good (Score: 80-99):** 5-6 fields populated, with some secondary source usage
  - **Fair (Score: 60-79):** 4-5 fields populated, significant secondary/fallback source usage  
  - **Poor (Score: 40-59):** 3-4 fields populated, extensive fallback usage
  - **Inadequate (Score: <40):** Fewer than 3 fields reliably populated

### 2.6. Update Frequency & Latency Requirements

#### 2.6.1. Real-Time Update Frequency
- **FR-IMD-11:** In **Current Mode**, the service **shall** update market data with the following frequencies:
  - **Quote Updates:** Maximum 250 millisecond latency from source to service output
  - **Trade Updates:** Maximum 500 millisecond latency for last trade price updates
  - **Duration Updates:** Maximum 5-minute latency for duration recalculations
  - **Batch Processing:** All market data fields shall be synchronized and timestamped consistently

#### 2.6.2. Historical Data Performance
- **FR-IMD-12:** In **Historical Mode**, the service **shall**:
  - Return complete historical market data within 2 seconds for any date within the last 5 years
  - Support bulk historical requests for up to 250 instruments simultaneously
  - Maintain point-in-time accuracy with proper as-of-date handling for all data sources

### 2.7. Failover & Redundancy

#### 2.7.1. Provider Failover Logic
- **FR-IMD-13:** The service **shall** implement automated failover mechanisms:
  1. **Primary Provider Health Monitoring:** Continuous monitoring of Bloomberg/Reuters connection status
  2. **Automatic Failover:** Switch to secondary providers within 30 seconds of primary provider failure
  3. **Data Quality Degradation Alerts:** Notify downstream systems when operating on secondary/tertiary data sources
  4. **Graceful Degradation:** Continue operating with reduced data quality rather than complete service failure

#### 2.7.2. Cache & Persistence Strategy  
- **FR-IMD-14:** The service **shall** maintain local data persistence:
  - **Real-time Cache:** 15-minute rolling cache of all market data for immediate failover
  - **Daily Snapshots:** End-of-day persistent storage for historical mode support
  - **Quote History:** 48-hour rolling history of all quote updates for audit and reconciliation
  - **Backup Recovery:** Ability to rebuild service state from persistent storage within 5 minutes

## 3. Data Quality Monitoring & Alerting

### 3.1. Automated Quality Checks
- **FR-IMD-15:** The service **shall** implement continuous quality monitoring:
  1. **Quote Reasonableness:** Alert if bid/ask spreads exceed 500 basis points for investment-grade bonds
  2. **Price Jump Detection:** Alert if last trade price moves >5% without corresponding market news
  3. **Stale Data Detection:** Alert if any instrument lacks fresh quotes for >30 minutes during market hours
  4. **Cross-Venue Validation:** Alert if same instrument shows >100 basis points spread differential across venues

### 3.2. Service Level Objectives (SLOs)
- **FR-IMD-16:** The service **shall** maintain the following performance targets:
  - **Data Availability:** 99.9% uptime during market hours (6:00 AM - 8:00 PM ET)
  - **Data Freshness:** 95% of quotes updated within 5 minutes during active market hours
  - **Data Accuracy:** <0.1% of quotes requiring post-trade correction or adjustment
  - **Latency Target:** 95th percentile response time <500ms for current mode requests

## 4. API Specification

### 4.1. REST API Endpoints

#### 4.1.1. Current Market Data Request
```
GET /api/v1/market-data/current/{cusip}
```
**Response:**
- Returns current `InstrumentMarketData` object
- HTTP 200: Success with data payload
- HTTP 404: CUSIP not found or not supported
- HTTP 503: Service temporarily unavailable

#### 4.1.2. Historical Market Data Request  
```
GET /api/v1/market-data/historical/{cusip}?as_of_date={YYYY-MM-DD}&time={HH:MM:SS}
```
**Response:**
- Returns historical `InstrumentMarketData` object for specified date/time
- HTTP 200: Success with historical data
- HTTP 404: CUSIP or date not found
- HTTP 400: Invalid date format

#### 4.1.3. Bulk Data Request
```
POST /api/v1/market-data/bulk
Content-Type: application/json

{
  "cusips": ["string"],
  "mode": "current|historical", 
  "as_of_date": "date", // Required if mode=historical
  "include_quality_flags": "boolean"
}
```

### 4.2. Real-Time Data Streaming
- **FR-IMD-17:** The service **shall** support WebSocket streaming for real-time updates:
  - **Subscription Model:** Clients subscribe to specific CUSIPs for live updates
  - **Throttling:** Maximum 1 update per second per subscribed instrument
  - **Heartbeat:** 30-second heartbeat messages to maintain connection health
  - **Reconnection Logic:** Automatic reconnection with state recovery for client applications

## 5. Error Handling & Fallback Procedures

### 5.1. Graceful Degradation Matrix

| Scenario | Fallback Action | Data Quality Impact | Alert Level |
|----------|----------------|-------------------|-------------|
| Bloomberg API Down | Switch to Reuters/ICE | Minimal | Warning |
| All Real-time Providers Down | Use last cached quotes + staleness flags | Moderate | Critical |
| Duration Source Unavailable | Internal calculation fallback | Low | Info |
| CUSIP Not Found | Return structured error response | N/A | Info |
| Historical Date Out of Range | Return oldest/newest available date | Low | Warning |

### 5.2. Circuit Breaker Implementation
- **FR-IMD-18:** The service **shall** implement circuit breaker patterns:
  - **Failure Threshold:** Open circuit after 10 consecutive failures from a data provider
  - **Recovery Testing:** Test provider recovery every 60 seconds when circuit is open  
  - **Partial Recovery:** Allow gradual traffic increase during provider recovery
  - **Downstream Protection:** Prevent cascade failures to `FinancialCalculationService`

## 6. Security & Compliance Requirements

### 6.1. Data Access Controls
- **FR-IMD-19:** The service **shall** implement role-based access controls:
  - **Read-Only Access:** Standard users can retrieve market data
  - **Administrative Access:** System administrators can modify provider configurations
  - **Audit Logging:** All data access requests logged with user identification and timestamp

### 6.2. Market Data Licensing Compliance
- **FR-IMD-20:** The service **shall** enforce data licensing restrictions:
  - **Provider Attribution:** Maintain source attribution for all market data
  - **Usage Tracking:** Monitor and report data usage to comply with vendor agreements
  - **Redistribution Controls:** Prevent unauthorized redistribution of licensed market data
  - **Delayed Data Handling:** Support delayed data feeds for non-licensed users when required

## 7. Performance & Scalability

### 7.1. Throughput Requirements
- **FR-IMD-21:** The service **shall** support minimum throughput of:
  - **10,000 current mode requests per minute** during peak usage
  - **1,000 historical mode requests per minute** with average 2-second response time
  - **500 concurrent WebSocket connections** for real-time streaming
  - **50 bulk requests per minute** with up to 250 instruments per request

### 7.2. Resource Optimization
- **FR-IMD-22:** The service **shall** implement efficient resource utilization:
  - **Connection Pooling:** Maintain persistent connections to market data providers
  - **Data Compression:** Use compression for data transmission and storage
  - **Intelligent Caching:** Cache frequently requested instruments with dynamic TTL
  - **Load Balancing:** Distribute requests across multiple service instances

## 8. Testing & Validation

### 8.1. Automated Testing Requirements
- **FR-IMD-23:** The service **shall** include comprehensive test coverage:
  - **Unit Tests:** 95% code coverage for all core algorithms
  - **Integration Tests:** End-to-end testing with all market data providers
  - **Performance Tests:** Load testing at 150% of expected peak throughput
  - **Failover Tests:** Automated testing of all provider failover scenarios

### 8.2. Data Validation Testing
- **FR-IMD-24:** The service **shall** implement continuous validation:
  - **Cross-Provider Consistency Checks:** Daily reconciliation of quotes across providers
  - **Historical Data Integrity:** Monthly validation of historical data completeness
  - **Calculation Accuracy Testing:** Weekly validation of duration calculations against benchmark sources
  - **End-to-End Testing:** Daily validation of complete data flow to `FinancialCalculationService` 