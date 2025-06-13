# Financial Data Calculation and Modeling

## 1. Purpose
This document defines the data models, sources, and calculation methodologies required to produce a comprehensive financial data object for a single fixed-income instrument. This process can be executed for the **current, real-time state** or for any **historical date**, providing a consistent output for trend analysis and back-testing. The output object serves as the standardized input for all downstream risk analysis, forecasting, and narrative generation systems.

## 2. Operating Modes
The calculation service must support two distinct operating modes:
- **Current Mode:** All input data reflects the most recent, real-time information available.
- **Historical Mode:** The service accepts a specific date (e.g., "T-10") as a parameter. All input data (`SecurityMaster`, `MarketData`, `TradeHistory`) must be sourced *as of* that specified historical date. The calculation logic remains identical, but the inputs are historical, producing a snapshot of the instrument's characteristics on that day.

## 3. Input Data Models
This section defines the raw data structures required from upstream sources. For **Historical Mode**, all data must be retrieved as it was on the specified date.

### 3.1. Sources
Data must be consolidated from the following market-wide data providers: **msrb, trace, bloomberg, ice**.

### 3.2. SecurityMaster
Static reference data for a specific instrument.
- `cusip`: `string` - Unique identifier for the security.
- `instrument_type`: `string` - The primary instrument classification. Must be one of: 'MUNI', 'TFI_CORPORATE', 'TFI_TREASURY', 'TFI_AGENCY'.
- `issuer_name`: `string`
- `coupon_rate`: `float` - Annual coupon rate.
- `maturity_date`: `date`
- `payment_frequency`: `integer` - Number of coupon payments per year.
- `face_value`: `float`
- `sector`: `string` - For MUNIs, must be one of: 'GENERAL_OBLIGATION', 'REVENUE_TRANSPORTATION', 'REVENUE_HEALTHCARE', 'REVENUE_UTILITIES', 'REVENUE_HOUSING', 'REVENUE_EDUCATION'. For TFI, must align with a standard industry classification (e.g., GICS).
- `rating`: `string` - Agency rating (e.g., Moody's, S&P).
- `state`: `string` - (for `instrument_type` = 'MUNI' only) The issuing U.S. state.
- `tax_status`: `string` - (for `instrument_type` = 'MUNI' only) Must be one of: 'TAX_EXEMPT_FEDERAL', 'TAXABLE', 'AMT', 'TAX_EXEMPT_FEDERAL_AND_STATE'.
- `call_schedule`: `array` - An array of all call options, where each object contains:
    - `call_date`: `date`
    - `call_price`: `float`
    - `call_type`: `string` - The exercise type. Must be one of: 'AMERICAN', 'EUROPEAN', 'BERMUDAN', 'NO_CALL'.

### 3.3. MarketData
Real-time market data for a specific instrument and for broader market context.
- `last_trade_price`: `float`
- `bid_price`: `float`
- `ask_price`: `float`
- `ust_benchmark_curve`: `object` - U.S. Treasury yields, keyed by tenor. Used as the primary benchmark for instrument types 'TFI_CORPORATE', 'TFI_AGENCY', and for 'MUNI' where tax_status is 'TAXABLE'.
- `mmd_benchmark_curve`: `object` - MMD yields for munis, keyed by tenor. Used as the primary benchmark for 'MUNI' where tax_status is any value other than 'TAXABLE'.
- `sector_credit_spread_curve`: `object` - A curve of credit spreads for various sectors/industries, used for more specific relative value in TFI.
- `interest_rate_volatility_surface`: `object` - A matrix of implied volatilities for different tenors and strikes, used for options modeling.
- `state_level_fiscal_indicators`: `object` - (for `instrument_type` = 'MUNI' only) Populated only for municipal securities.
    - `state_tax_receipts_yoy_growth`: `float` - Year-over-year growth in tax receipts for the relevant state. Sourced from state financial reports or rating agency data.
    - `state_budget_surplus_deficit_as_pct_of_gsp`: `float` - The state's budget surplus or deficit as a percentage of Gross State Product. Sourced from state financial reports or rating agency data.

### 3.4. TradeHistory
A list of individual trade records for a specific instrument over a 20-day look-back period preceding the calculation date. This longer period allows for summaries across multiple time horizons (1-day, 5-day, 20-day).
- `trades`: `array` - An array of trade objects, where each object contains:
    - `trade_datetime`: `datetime`
    - `price`: `float`
    - `par_volume`: `float`
    - `dealer_id`: `string` - An identifier for the dealer reporting the trade.
    - `counterparty_type`: `string` - The counterparty classification. Must be one of: 'CUSTOMER_BUY', 'CUSTOMER_SELL', 'INTER_DEALER'.
    - `trade_size_category`: `string` - The trade size classification. Must be one of: 'BLOCK' (>=$1MM), 'ROUND_LOT' ($100k-$999k), 'ODD_LOT' (<$100k).

## 4. Risk Feature Calculations
This section defines the logic for transforming input data into the calculated risk features required by the output model. The choice of benchmark is critical and determined by the instrument's type and tax status:
- **UST Benchmark:** Used for `instrument_type` in ('TFI_CORPORATE', 'TFI_AGENCY') and for `instrument_type` = 'MUNI' with `tax_status` = 'TAXABLE'.
- **MMD Benchmark:** Used for `instrument_type` = 'MUNI' with `tax_status` in ('TAX_EXEMPT_FEDERAL', 'AMT', 'TAX_EXEMPT_FEDERAL_AND_STATE').
- **No Benchmark:** `instrument_type` = 'TFI_TREASURY' instruments are the benchmark themselves; spread-based calculations (OAS, Relative Value) are not applicable and their results shall be 0 or null.

| Feature | Calculation Method | Key Data Inputs |
|---|---|---|
| Price | Mid-point of Bid/Ask, or last traded price if unavailable. | MarketData: Bid, Ask, LastTradePrice |
| YTM | IRR solving for yield given current price and all cash flows to maturity. | SecurityMaster: Coupon, Maturity, Face; MarketData: Price |
| YTC | IRR solving for yield to each specific call date and call price. | SecurityMaster: CallSchedule; MarketData: Price |
| YTW | The minimum of the calculated YTM and all calculated YTCs. | Calculated YTM, YTCs |
| DV01 | Price change of the bond given a one basis point (0.01%) decrease in yield. | SecurityMaster: Cash flows; MarketData: Price |
| CS01 | Price change of the bond given a one basis point (0.01%) increase in credit spread. | SecurityMaster: Cash flows; MarketData: Price, BenchmarkCurve |
| OAS | The spread that equates the theoretical price (with options) to the market price, calculated against the appropriate benchmark as defined above. | See Section 4.3 for full detail. |
| Relative Value (MMD) | (For MUNI instruments using the MMD benchmark) YTW of the bond minus the yield of the MMD benchmark curve at the same duration. | Calculated YTW; MarketData: MMD Curve |
| Relative Value (UST) | (For all instruments using the UST benchmark) YTW of the bond minus the yield of the UST benchmark curve at the same duration (I-spread). | Calculated YTW; MarketData: UST Curve |
| Relative Value (Peer) | The bond's OAS minus the average OAS of its identified peer group. | Calculated OAS; Peer Group OAS |
| Liquidity Score | Composite score based on bid/ask spread, trade volume, and dealer count. | MarketData, Output of Trade History Aggregation |
| Trade History Summary | Aggregated metrics derived from raw trade data. | See Section 4.4 for full detail. |

### 4.1. Peer Group Identification
- **Purpose:** To identify a set of comparable bonds for relative value analysis. The peer universe is determined based on the specified calculation date (current or historical).
- **Logic:**
    1.  **Filter Criteria:**
        -   **`instrument_type`:** Must be an exact match.
        -   **State:** (for `instrument_type` = 'MUNI' only) Exact match.
        -   **Sector/Industry:** Exact match.
        -   **Maturity:** Within ±2 years of the target bond.
    2.  **Validation:** The group must contain at least 3 comparables. If not, relax filters in this order: 1) Rating (allow ±1 notch), 2) Maturity (widen to ±3 years).
- **Output:** A list of CUSIPs for the identified peers.

### 4.2. Liquidity Score Calculation
- **Purpose:** To generate a single, normalized score representing an instrument's market liquidity.
- **Formula:** `Score = 0.4 * (Bid/Ask Spread Score) + 0.4 * (Composite Volume Score) + 0.2 * (Composite Dealer Count Score)`
- **Component Logic:**
    - **Bid/Ask Spread Score:** The instrument's current bid/ask spread (in bps) is normalized against its sector average (z-score). This is a point-in-time metric.
    - **Composite Trade Volume Score:** This is a weighted average of normalized scores from multiple time horizons.
        - `Score_1d` = z-score of `t1d.total_par_volume` vs. sector average 1-day volume.
        - `Score_5d` = z-score of `t5d.total_par_volume` vs. sector average 5-day volume.
        - `Score_20d` = z-score of `t20d.total_par_volume` vs. sector average 20-day volume.
        - **Final Score = (0.2 * Score_1d) + (0.5 * Score_5d) + (0.3 * Score_20d)**
    - **Composite Dealer Count Score:** This is a weighted average of normalized scores from multiple time horizons.
        - `Score_1d` = z-score of `t1d.unique_dealer_count` vs. sector average 1-day dealer count.
        - `Score_5d` = z-score of `t5d.unique_dealer_count` vs. sector average 5-day dealer count.
        - `Score_20d` = z-score of `t20d.unique_dealer_count` vs. sector average 20-day dealer count.
        - **Final Score = (0.2 * Score_1d) + (0.5 * Score_5d) + (0.3 * Score_20d)**
- **Output:** A single float value representing the composite liquidity score.

### 4.3. Option-Adjusted Spread (OAS) Calculation
- **Purpose:** To calculate the spread over a benchmark yield curve that equates a bond's theoretical price (accounting for embedded call options) to its observed market price.
- **Input Model:**
    - `market_price`: `float`
    - `cash_flows`: `array` (derived from SecurityMaster)
    - `call_schedule`: `array` (from SecurityMaster, including `call_type`)
    - `benchmark_yield_curve`: `object` (from MarketData - the appropriate UST or MMD curve as per rules in Section 4)
    - `interest_rate_volatility`: `float` (derived from the volatility surface in MarketData)
- **Calculation Method:**
    1.  This calculation is not performed for `instrument_type` = 'TFI_TREASURY'.
    2.  A binomial interest rate lattice is constructed based on the instrument's appropriate benchmark yield curve and interest rate volatility. This lattice models the potential paths of future interest rates.
    3.  The bond's cash flows are valued backwards through the lattice, from maturity to the present.
    4.  At each node representing a call date, the model checks if the issuer's optimal action is to call the bond based on its `call_type` (e.g., if the bond's price is higher than its call price for an American call). The cash flow is adjusted accordingly.
    5.  A numerical root-finding solver is used to find the `OAS` value. The `OAS` is the constant spread that, when added to all interest rates in the lattice, makes the calculated present value of the bond equal to its current `market_price`.
- **Output:** A single float value, `oas_in_bps`.

### 4.4. Trade History Aggregation
- **Purpose:** To process the raw trade list from the `TradeHistory` input into structured summaries for multiple time horizons (1-day, 5-day, 20-day).
- **Input:** A list of trade objects from `TradeHistory` for the last 20 trading days.
- **Output:** The `trade_history_summary` object as defined in the Output Data Model (Section 5), containing `t1d`, `t5d`, and `t20d` sub-objects.
- **Calculation Logic:**
    - The following aggregations are performed independently for three filtered lists of trades: those within the last 1 day, 5 days, and 20 days.
        - `total_par_volume`: Sum of `par_volume` for all trades in the filtered list.
        - `trade_count`: Total count of trade objects in the filtered list.
        - `unique_dealer_count`: Count of unique `dealer_id` values from all trades in the filtered list.
        - `block_trade_par_volume`: Sum of `par_volume` for trades where `trade_size_category` is 'BLOCK'.
        - `odd_lot_par_volume`: Sum of `par_volume` for trades where `trade_size_category` is 'ODD_LOT'.
        - `customer_buy_par_volume`: Sum of `par_volume` for trades where `counterparty_type` is 'CUSTOMER_BUY'.
        - `customer_sell_par_volume`: Sum of `par_volume` for trades where `counterparty_type` is 'CUSTOMER_SELL'.
        - `high_trade_price`: The maximum `price` from all trades in the filtered list.
        - `low_trade_price`: The minimum `price` from all trades in the filtered list.
        - `trade_price_volatility`: Calculated as `(high_trade_price - low_trade_price) / low_trade_price`.

## 5. Output Data Model
This section defines the final, consolidated JSON object produced by the data processing pipeline. This object is the standard input for all downstream consumers and serves as the definitive schema for generating training, validation, and testing datasets for all downstream GenAI models.

```json
{
  "calculation_context": {
    "mode": "string", // "current" or "historical"
    "as_of_date": "date" // The date used for historical calculations
  },
  "cusip": "string",
  "data_timestamp": "datetime",
  "security_details": {
    "issuer_name": "string",
    "coupon_rate": "float",
    "maturity_date": "date",
    "sector": "string",
    "rating": "string",
    "call_schedule": "array"
  },
  "market_data": {
    "price": "float",
    "bid_price": "float",
    "ask_price": "float",
    "bid_ask_spread_bps": "float"
  },
  "calculated_risk_metrics": {
    "yield_to_maturity": "float",
    "yield_to_worst": "float",
    "dv01": "float",
    "cs01": "float",
    "option_adjusted_spread_bps": "float"
  },
  "liquidity": {
    "composite_score": "float",
    "is_illiquid_flag": "boolean"
  },
  "trade_history_summary": {
    "t1d": {
      "total_par_volume": "float",
      "trade_count": "integer",
      "unique_dealer_count": "integer",
      "block_trade_par_volume": "float",
      "odd_lot_par_volume": "float",
      "customer_buy_par_volume": "float",
      "customer_sell_par_volume": "float",
      "high_trade_price": "float",
      "low_trade_price": "float",
      "trade_price_volatility": "float"
    },
    "t5d": {
      "total_par_volume": "float",
      "trade_count": "integer",
      "unique_dealer_count": "integer",
      "block_trade_par_volume": "float",
      "odd_lot_par_volume": "float",
      "customer_buy_par_volume": "float",
      "customer_sell_par_volume": "float",
      "high_trade_price": "float",
      "low_trade_price": "float",
      "trade_price_volatility": "float"
    },
    "t20d": {
      "total_par_volume": "float",
      "trade_count": "integer",
      "unique_dealer_count": "integer",
      "block_trade_par_volume": "float",
      "odd_lot_par_volume": "float",
      "customer_buy_par_volume": "float",
      "customer_sell_par_volume": "float",
      "high_trade_price": "float",
      "low_trade_price": "float",
      "trade_price_volatility": "float"
    }
  },
  "relative_value": {
    "vs_mmd_bps": "float",
    "vs_ust_bps": "float",
    "vs_peers_bps": "float",
    "peer_group_size": "integer",
    "peer_group_cusips": "array"
  },
  "state_fiscal_health": {
      "tax_receipts_yoy_growth": "float",
      "budget_surplus_deficit_pct_gsp": "float"
  }
}
```