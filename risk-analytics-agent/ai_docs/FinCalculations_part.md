# Financial Calculation Engine: Requirements Specification

**Version: 1.2**

## 1. Overview

### 1.1. Purpose
This document defines the data models, sources, and calculation methodologies required to produce a comprehensive financial data object for a single fixed-income instrument. This process can be executed for the **current, real-time state** or for any **historical date**, providing a consistent output for trend analysis and back-testing. The output object serves as the standardized input for all downstream risk analysis, forecasting, and narrative generation systems.

## 2. System-Level Requirements

### 2.1. Operating Modes
The calculation service must support two distinct operating modes:
- **FR-01:** The system **shall** operate in **Current Mode**, using the most recent, real-time information available for all inputs.
- **FR-02:** The system **shall** operate in **Historical Mode**, accepting a specific date as a parameter and sourcing all input data (`SecurityMaster`, `MarketData`, `TradeHistory`) *as of* that specified historical date.

### 2.2. Data Sourcing
- **FR-03:** The system **shall** consolidate input data from the following market-wide data providers: **msrb, trace, bloomberg, ice**.

## 3. Data Models & Schemas

### 3.1. Input Models

#### 3.1.1. SecurityMaster Model
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

#### 3.1.2. MarketData Model
Real-time market data for a specific instrument and for broader market context.
- `last_trade_price`: `float`
- `bid_price`: `float`
- `ask_price`: `float`
- `duration`: `float` - The instrument's effective duration, provided as an input.
- `ust_benchmark_curve`: `object` - U.S. Treasury yields, keyed by tenor. Used as the primary benchmark for instrument types 'TFI_CORPORATE', 'TFI_AGENCY', and for 'MUNI' where `tax_status` is 'TAXABLE'.
- `mmd_benchmark_curve`: `object` - MMD yields for munis, keyed by tenor. Used as the primary benchmark for 'MUNI' where `tax_status` is any value other than 'TAXABLE'.
- `sector_credit_spread_curve`: `object` - A curve of credit spreads for various sectors/industries, used for more specific relative value in TFI.
- `interest_rate_volatility_surface`: `object` - A matrix of implied volatilities for different tenors and strikes, used for options modeling.

#### 3.1.3. StateFiscalFeed Model
- `state_level_fiscal_indicators`: `object` - (for `instrument_type` = 'MUNI' only) Populated only for municipal securities. This object **shall** be retrieved from the `StateFiscalFeed` service for the relevant U.S. state.
    - `state_tax_receipts_yoy_growth`: `float` - Year-over-year growth in tax receipts for the relevant state.
    - `state_budget_surplus_deficit_as_pct_of_gsp`: `float` - The state's budget surplus or deficit as a percentage of Gross State Product.

#### 3.1.4. TradeHistory Model
A list of individual trade records for a specific instrument over a 25-calendar-day look-back period preceding the calculation date to ensure sufficient data for 20-trading-day calculations.
- `trades`: `array` - An array of trade objects, where each object contains:
    - `trade_datetime`: `datetime`
    - `price`: `float`
    - `par_volume`: `float`
    - `dealer_id`: `string` - An identifier for the dealer reporting the trade.
    - `counterparty_type`: `string` - The counterparty classification. Must be one of: 'CUSTOMER_BUY', 'CUSTOMER_SELL', 'INTER_DEALER'.
    - `trade_size_category`: `string` - The trade size classification. Must be one of: 'BLOCK' (>=$1MM), 'ROUND_LOT' ($100k-$999k), 'ODD_LOT' (<$100k).

### 3.2. Output Model: FinancialDataObject
This section defines the final, consolidated JSON object produced by the data processing pipeline. This object is the standard input for all downstream consumers.

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
    "option_adjusted_spread_bps": "float",
    "downside_price_volatility_5d": {
      "metric_type": "string",
      "value": "float"
    },
    "downside_price_volatility_20d": {
      "metric_type": "string",
      "value": "float"
    }
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
  "market_context": {
    "yield_curve_slope_10y2y": "float",
    "mmd_ust_ratio_10y": "float"
  },
  "state_fiscal_health": {
      "tax_receipts_yoy_growth": "float",
      "budget_surplus_deficit_pct_gsp": "float"
  }
}
```

## 4. Core Logic & Business Rules
This section defines the key business logic, formulas, and methodologies for transforming input data into the calculated features.

### 4.1. Business Rule: Benchmark Selection
The choice of benchmark is critical and determined by the instrument's type and tax status:
- **BR-01:** The **UST Benchmark** **shall** be used for `instrument_type` in ('TFI_CORPORATE', 'TFI_AGENCY') and for `instrument_type` = 'MUNI' with `tax_status` = 'TAXABLE'.
- **BR-02:** The **MMD Benchmark** **shall** be used for `instrument_type` = 'MUNI' with `tax_status` in ('TAX_EXEMPT_FEDERAL', 'AMT', 'TAX_EXEMPT_FEDERAL_AND_STATE').
- **BR-03:** **No Benchmark** is applicable for `instrument_type` = 'TFI_TREASURY'. Spread-based calculations (OAS, Relative Value) are not applicable and their results shall be 0 or null.

### 4.2. Business Rule: Peer Group Identification
- **BR-04:** A peer group of comparable bonds **shall** be identified for relative value analysis. The peer universe is determined based on the specified calculation date (current or historical).
- **Logic:**
    1.  **Filter Criteria:**
        -   **`instrument_type`:** Must be an exact match.
        -   **State:** (for `instrument_type` = 'MUNI' only) Exact match.
        -   **Sector/Industry:** Exact match.
        -   **Maturity:** Within ±2 years of the target bond.
    2.  **Validation:** The group must contain at least 3 comparables. If not, relax filters in this order: 1) Rating (allow ±1 notch), 2) Maturity (widen to ±3 years).
- **Rating Notch Definition:** To programmatically handle "±1 notch", the following ordinal rating scale **shall** be used. A difference of 1 on this scale is one notch.

| Rating (S&P/Fitch) | Rating (Moody's) | Scale Value |
|:---|:---|:---|
| AAA | Aaa | 1 |
| AA+ | Aa1 | 2 |
| AA | Aa2 | 3 |
| AA- | Aa3 | 4 |
| A+ | A1 | 5 |
| A | A2 | 6 |
| A- | A3 | 7 |
| BBB+ | Baa1 | 8 |
| BBB | Baa2 | 9 |
| BBB- | Baa3 | 10 |

*Lower ratings follow a similar pattern*

- **Output:** A list of CUSIPs for the identified peers.

### 4.3. Formula: Liquidity Score Calculation
- **Purpose:** To generate a single, normalized score representing an instrument's market liquidity.
- **Formula:** `Score = 0.4 * (Bid/Ask Spread Score) + 0.4 * (Composite Volume Score) + 0.2 * (Composite Dealer Count Score)`
- **Component Logic:**
    - All "sector average" metrics (for spread, volume, dealer count) **shall** be calculated by taking the average of that metric across all bonds in the peer group identified in Section 4.2.
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

#### 4.3.1. Business Rule: Illiquidity Flag
- **BR-05:** The `is_illiquid_flag` **shall** be set to `true` if the calculated `liquidity.composite_score` is less than -1.5. This threshold indicates that the instrument's liquidity is significantly below the average of its peer group. Otherwise, the flag **shall** be `false`.

### 4.4. Methodology: Option-Adjusted Spread (OAS) Calculation
- **Purpose:** To calculate the spread over a benchmark yield curve that equates a bond's theoretical price (accounting for embedded call options) to its observed market price.
- **Input Model:**
    - `market_price`: `float`
    - `cash_flows`: `array` (derived from SecurityMaster)
    - `call_schedule`: `array` (from SecurityMaster, including `call_type`)
    - `benchmark_yield_curve`: `object` (from MarketData - the appropriate UST or MMD curve as per rule BR-01/BR-02)
    - `interest_rate_volatility_surface`: `object` (from MarketData)
- **Calculation Method:**
    1.  This calculation is not performed for `instrument_type` = 'TFI_TREASURY'.
    2.  An `interest_rate_volatility` float **shall** be derived from the input `interest_rate_volatility_surface`. The logic is as follows:
        -   The goal is to find the implied volatility that corresponds to the bond's duration at an at-the-money strike.
        -   From the volatility surface, which is a matrix of tenor and strike, select the volatilities for the 'at-the-money' strike (e.g., strike = 100).
        -   Using this list of volatilities keyed by tenor, perform a **linear interpolation** to find the volatility at the specific duration of the bond. This is identical to the benchmark yield interpolation logic in Section 4.6.
    3.  A binomial interest rate lattice is constructed based on the instrument's appropriate benchmark yield curve and the interpolated `interest_rate_volatility` derived in the previous step. This lattice models the potential paths of future interest rates.
    4.  The bond's cash flows are valued backwards through the lattice, from maturity to the present.
    5.  At each node representing a call date, the model checks if the issuer's optimal action is to call the bond based on its `call_type` (e.g., if the bond's price is higher than its call price for an American call). The cash flow is adjusted accordingly.
    6.  A numerical root-finding solver is used to find the `OAS` value. The `OAS` is the constant spread that, when added to all interest rates in the lattice, makes the calculated present value of the bond equal to its current `market_price`.
- **Output:** A single float value, `oas_in_bps`.

### 4.5. Methodology: Trade History Aggregation
- **Purpose:** To process the raw trade list from the `TradeHistory` input into structured summaries for multiple time horizons (1-day, 5-day, 20-day).
- **Input:** A list of trade objects from `TradeHistory` for the last 20 trading days.
- **Output:** The `trade_history_summary` object as defined in the Output Model (Section 3.2).
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

### 4.6. Methodology: Benchmark Yield Interpolation
- **Purpose:** To determine the precise benchmark yield for a bond's specific duration when that duration falls between the standard tenors of the benchmark curve.
- **Method:** The system **shall** use **linear interpolation** to calculate the yield.
- **Logic:**
    1. Identify the bond's duration.
    2. From the appropriate benchmark curve (`UST` or `MMD`), find the two consecutive tenors (`T_lower`, `T_upper`) that bracket the bond's duration. Let their corresponding yields be `Y_lower` and `Y_upper`.
    3. Calculate the interpolated yield using the following formula:
       `Interpolated_Yield = Y_lower + ((Bond_Duration - T_lower) * (Y_upper - Y_lower)) / (T_upper - T_lower)`
- **Edge Case Handling:**
    - If the bond's duration is less than the shortest tenor on the benchmark curve, the system **shall** use the yield of the shortest tenor.
    - If the bond's duration is greater than the longest tenor on the benchmark curve, the system **shall** use the yield of the longest tenor.
    - Extrapolation **shall not** be used.

### 4.7. Methodology: Market Context Calculation
- **Purpose:** To calculate broad market indicators using the benchmark curves available to the engine.
- **Calculations:**
    - **Yield Curve Slope (10Y-2Y):**
        - **Input:** `ust_benchmark_curve`
        - **Logic:** Retrieve the 10-year yield (`Yield_10Y`) and the 2-year yield (`Yield_2Y`) from the curve.
        - **Formula:** `yield_curve_slope_10y2y = Yield_10Y - Yield_2Y`
    - **MMD/UST Ratio (10Y):**
        - **Inputs:** `ust_benchmark_curve`, `mmd_benchmark_curve`
        - **Logic:** Retrieve the 10-year MMD yield (`MMD_Yield_10Y`) and the 10-year Treasury yield (`UST_Yield_10Y`).
        - **Formula:** `mmd_ust_ratio_10y = MMD_Yield_10Y / UST_Yield_10Y`

### 4.8. Methodology: Downside Price Volatility Calculation
- **Purpose:** To calculate the realized downside volatility (or semi-deviation) of an instrument's returns over a trailing window. This metric quantifies downside risk by focusing only on negative price movements.
- **Inputs:**
  - `TradeHistory`: `array[object]` - The raw trade history for the lookback window.
  - `calculation_date` (`T`): `date` - The date for which the volatility is being calculated.
  - `lookback_window`: `integer` - The number of trading days to include in the calculation (e.g., 5, 20).

**Business Rule: Deriving Daily Closing Prices**
- **BR-19:** A daily closing price **shall** be derived from the `TradeHistory` data. For each trading day in the lookback period, the closing price is defined as the `price` of the last trade recorded on that calendar day. If no trades occurred on a given trading day, the closing price from the most recent previous day with a trade **shall** be carried forward.

- **Algorithm:**
  1.  **Derive Daily Price Series:** Following rule **BR-19**, process the `TradeHistory` input to generate a time-series of daily closing prices for the `lookback_window + 1` trading days ending on `calculation_date`. Let this series be `P`.
  2.  **Calculate Log Returns:** Create a new series, `R`, of `lookback_window` daily logarithmic returns:
      `R_t = ln(P_t / P_{t-1})`.
  3.  **Calculate Downside Volatility (Semi-Deviation):** The semi-deviation is calculated as the square root of the semi-variance. The semi-variance is the average of the squared deviations of returns that are below a target (in this case, zero).
      \[ \sigma_{downside} = \sqrt{\frac{1}{N-1} \sum_{r_i \in R, r_i < 0}^{} (r_i)^2} \]
      Where:
      - `N` is the `lookback_window` size (the total number of returns in the period).
      - `r_i` are the individual log returns in the series `R`. The summation is only over returns less than zero.
      - We use `N-1` in the denominator for the sample-based calculation, which is standard practice to get an unbiased estimator of the variance.
- **Output:**
  - `downside_price_volatility`: `float` - The calculated trailing daily downside volatility.

**Business Rule: Populating `metric_type`**
- **Purpose:** To provide a human-readable description of the calculation methodology for the downside price volatility metric, selected as trailing 5D or 20D for day-to-day risk monitoring.
- **BR-20:** The `metric_type` field for each `downside_price_volatility` object **shall** be populated with a string that clearly describes the calculation methodology.
  - For the 5-day calculation, the value **shall** be `"Trailing 5D Downside Volatility (Log-Returns)"`.
  - For the 20-day calculation, the value **shall** be `"Trailing 20D Downside Volatility (Log-Returns)"`.

## 5. Functional Requirements: Feature Calculation
- **FR-04:** The system **shall** calculate the instrument's **Price** as the mid-point of Bid/Ask, or the last traded price if bid/ask is unavailable.
  - **Formula:** `Price = (Bid_Price + Ask_Price) / 2`. If `Bid_Price` or `Ask_Price` is unavailable, `Price = Last_Trade_Price`.

- **FR-05:** The system **shall** calculate **Yield to Maturity (YTM)** as the internal rate of return (IRR) solving for the yield given the current price and all cash flows to maturity.
  - **Formula:** Solve for `y` in the equation: `Market_Price = Σ [C_t / (1 + y/f)^(t)] + [Face_Value / (1 + y/f)^(N)]`
    - `C_t`: Coupon payment at time `t`
    - `y`: Yield to Maturity (annualized)
    - `f`: Payment frequency per year
    - `N`: Total number of periods to maturity
    - `t`: Period when cash flow is received

- **FR-06:** The system **shall** calculate **Yield to Call (YTC)** as the IRR solving for yield to each specific call date and call price.
  - **Formula:** For each call date in the `call_schedule`, solve for `y_c` in the equation: `Market_Price = Σ [C_t / (1 + y_c/f)^(t)] + [Call_Price / (1 + y_c/f)^(N_c)]`
    - `y_c`: Yield to Call (annualized)
    - `N_c`: Total number of periods to the call date
    - `Call_Price`: The price at which the bond can be called

- **FR-07:** The system **shall** calculate **Yield to Worst (YTW)** as the minimum of the calculated YTM and all calculated YTCs.
  - **Formula:** `YTW = min(YTM, YTC_1, YTC_2, ..., YTC_n)`

- **FR-08:** The system **shall** calculate **DV01** as the price change of the bond given a one basis point (0.01%) decrease in yield.
  - **Formula:** `DV01 = |Price(y - 0.0001) - Price(y)|`, where `y` is the bond's current **Yield to Worst (YTW)**.

- **FR-09:** The system **shall** calculate **CS01** as the price change of the bond given a one basis point (0.01%) increase in its credit spread.
  - **Formula Logic:** CS01 measures price sensitivity to credit spread changes. Its calculation requires re-pricing the bond after widening the spread.
    1.  **Determine the Bond's Discount Rate:** A bond's price is determined by its cash flows and a discount rate. This discount rate is a sum of the benchmark rate and the bond's credit spread.
    2.  **Calculate the Interpolated Benchmark Yield:** Using the methodology in Section 4.6, calculate the `Interpolated_Benchmark_Yield` for the bond's specific duration from the appropriate benchmark curve. This is a float value (e.g., 0.035).
    3.  **Calculate the Initial Credit Spread:** The bond's implied credit spread is `Initial_Spread = YTW - Interpolated_Benchmark_Yield`.
    4.  **Calculate Price with Widened Spread:** Calculate a new theoretical price for the bond (`Price_New`) by discounting all of its cash flows using a new, widened discount rate. The formula for the discount rate at each cash flow is: `New_Discount_Rate = Interpolated_Benchmark_Yield + Initial_Spread + 0.0001`.
    5.  **Calculate CS01:** The CS01 value is the absolute difference between the bond's current market price and the new theoretical price. `CS01 = |Market_Price - Price_New|`.

- **FR-10:** The system **shall** calculate **Option-Adjusted Spread (OAS)** as per the methodology in Section 4.4.

- **FR-11:** The system **shall** calculate **Relative Value** against the appropriate benchmark (MMD or UST) by subtracting the benchmark yield at the same duration from the bond's YTW.
  - **Formula:** `Relative_Value_bps = (YTW - Interpolated_Benchmark_Yield) * 10000`. The benchmark yield is interpolated to match the bond's duration as per the methodology in Section 4.6.

- **FR-12:** The system **shall** calculate **Relative Value** against the identified peer group by subtracting the average OAS of the peer group from the bond's OAS.
  - **Formula:** `Relative_Value_vs_Peers_bps = option_adjusted_spread_bps - AVG(peer_OAS_1, ..., peer_OAS_n)`

- **FR-13:** The system **shall** calculate a composite **Liquidity Score** based on the formula in Section 4.3.

- **FR-14:** The system **shall** produce a **Trade History Summary** by aggregating raw trade data as per the methodology in Section 4.5.

- **FR-15:** The system **shall** calculate the **Bid/Ask Spread in BPS**.
  - **Formula:** `bid_ask_spread_bps = (ask_price - bid_price) / ((ask_price + bid_price) / 2) * 10000`

- **FR-16:** The system **shall** calculate the **10Y/2Y U.S. Treasury Yield Curve Slope** as per the methodology in Section 4.7.

- **FR-17:** The system **shall** calculate the **10Y MMD/UST Ratio** as per the methodology in Section 4.7.

- **FR-18:** The system **shall** calculate trailing **Downside Price Volatility** for 5-day and 20-day lookback windows, populating `downside_price_volatility_5d` and `downside_price_volatility_20d` respectively, as per the methodology in Section 4.8.

- **FR-19:** The system **shall** populate the `relative_value.peer_group_cusips` field with the list of CUSIPs identified in the peer group process (Section 4.2) and the `relative_value.peer_group_size` field with the count of that list.

## 6. Non-Functional Requirements
### 6.1. Data Consistency
- **NFR-DC-01:** Calculations for a given historical date **must** be immutable and reproducible, using the exact data available as of that date's cutoff.
### 6.2. Accuracy
- **NFR-AC-01:** All financial calculations (e.g., YTM, OAS, DV01) **must** adhere to industry-standard formulas and achieve a precision of at least 4 decimal places.