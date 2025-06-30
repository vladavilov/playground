# Valuation Intelligence Module: Functional Requirements

## 1. Overview

### 1.1. Purpose
The Valuation Intelligence module provides traders with an actionable, strategy-based fair value price for a given fixed-income instrument. It moves beyond displaying raw data by synthesizing multiple forecast models into a single, justifiable price level. The core function is to allow traders to align the valuation logic with their specific trading goals and holding periods, by adjusting the `Forecast Horizon` and `Valuation Strategy`.

## 2. Core Components & Inputs

The module's calculations are driven by three primary spread forecast inputs. These represent different views on the instrument's near-term value.

-   `1D Trend Forecast Spread`: A spread forecast based on very short-term (1-day) price momentum. This input reflects the instrument's current intraday trading direction.
-   `5D Trend Forecast Spread`: A spread forecast based on medium-term (5-day) price momentum. This input captures the trend over the past week.
-   `Peer RV Target Spread`: A spread forecast based on fundamental relative value analysis. This input represents the spread at which the instrument would be considered "fairly valued" against its closest comparable peers, ignoring short-term market noise.

## 3. User Controls & Interaction

The user can influence the final price calculation through two primary controls:

### 3.1. Forecast Horizon Selector
-   **Purpose**: Allows the trader to specify the intended holding period or time frame for the valuation.
-   **Function**: The selected horizon acts as a primary driver for the weighting logic applied to the input spreads. Shorter horizons prioritize momentum, while longer horizons prioritize fundamental value.
    -   **Short Horizon (e.g., 1 Day)**: Signals a tactical, short-term trading goal. The model will heavily weight momentum indicators (`1D Trend Forecast`).
    -   **Medium Horizon (e.g., 5 Days)**: Signals a goal to hold for several days. The model will balance momentum and fundamental value (`5D Trend` and `Peer RV`).
    -   **Long Horizon (e.g., "Fair Value")**: Signals a strategic, investment-oriented goal. The model will heavily weight the fundamental `Peer RV Target`.

### 3.2. Valuation Strategy Selector
-   **Purpose**: Allows the trader to select a pre-defined weighting model that aligns with a specific trading style.
-   **Function**: Each strategy defines a specific mix of the input spreads.
    -   **Trend-Based / Momentum**: This strategy gives high importance to the `1D` and `5D Trend Forecasts`.
    -   **Fair Value / Mean Reversion**: This strategy gives high importance to the `Peer RV Target Spread`.
    -   **Cautious / Defensive**: This strategy would likely blend inputs but select the most conservative (highest spread) outcome to create a margin of safety.

## 4. Calculation Logic & Price Derivation

The final suggested price is derived through a sequential, transparent process:

### Step 1: Determine the Target Spread via Weighting

The system calculates the `Final Target Spread` by applying a weighted average to the three input spreads. The weights are determined by the user's selected `Forecast Horizon` and `Valuation Strategy`.

**Conceptual Formula:**
`Final Target Spread = (W_1d * Spread_1d) + (W_5d * Spread_5d) + (W_rv * Spread_rv)`
Where `W` represents the weight for each input, and `W_1d + W_5d + W_rv = 1`.

#### Example of Horizon-Based Weighting:

Given the following inputs:
-   `1D Trend Forecast Spread`: +70 bps
-   `5D Trend Forecast Spread`: +60 bps
-   `Peer RV Target Spread`: +45 bps

The `Final Target Spread` changes based on the selected horizon:

-   **If Horizon = "1-Day" (Momentum-focused):**
    -   Weights: `W_1d=0.7`, `W_5d=0.2`, `W_rv=0.1`
    -   `Final Target Spread` = `(0.7 * 70) + (0.2 * 60) + (0.1 * 45)` = **65.5 bps**

-   **If Horizon = "5-Day" (Balanced):**
    -   Weights: `W_1d=0.1`, `W_5d=0.5`, `W_rv=0.4`
    -   `Final Target Spread` = `(0.1 * 70) + (0.5 * 60) + (0.4 * 45)` = **55.0 bps**

-   **If Horizon = "Fair Value" (Value-focused):**
    -   Weights: `W_1d=0.0`, `W_5d=0.1`, `W_rv=0.9`
    -   `Final Target Spread` = `(0.0 * 70) + (0.1 * 60) + (0.9 * 45)` = **46.5 bps**

### Step 2: Calculate the Target Yield

The `Final Target Spread` is added to the instrument's base benchmark yield to determine the final target yield.

**Formula:**
`Target Yield = Base Benchmark Yield + Final Target Spread`

-   The `Base Benchmark Yield` is the interpolated yield from the appropriate benchmark curve (UST or MMD) for the bond's specific duration.

### Step 3: Calculate the Final Suggested Price

The system solves for the price of the bond that corresponds to the calculated `Target Yield`. This is a standard price/yield calculation using the bond's cash flows (coupon, maturity, face value).

**Result**: The final `suggested-price-input` displayed in the UI is this calculated price. The price and yield fields are interlinked; a change in one will recalculate the other.

## 5. Business Rationale

The primary business value of this feature is its ability to model a trader's thought process. It acknowledges that a bond's "correct" price is not a single number, but is dependent on the trading strategy and intended holding period.

-   **For Tactical Trading**: By selecting a short horizon, traders get a price that respects current market momentum, protecting them from bidding against a strong trend.
-   **For Strategic Investing**: By selecting a longer horizon, portfolio managers can see through short-term volatility and identify fundamentally mispriced assets, basing their decision on value rather than noise.

This flexibility allows the tool to serve both fast-paced trading desks and long-term investment managers within the same interface. 