# Business Requirements Document: MUNI Bid-Wanted Trading Support UI

**Version: 1.0**

---

## 1. Introduction & Purpose

### 1.1. Overview
This document outlines the business requirements for a User Interface (UI) designed to support a Municipal (MUNI) bond trader's daily workflow, specifically in responding to "bid-wanted" lists. The UI will consume data from the backend Risk Analytics System to provide rapid, actionable insights.

### 1.2. Business Objective
The primary objective is to enable traders to make faster, more informed, and risk-aware decisions when bidding on MUNI bonds. This is achieved by:
-   **Accelerating Triage:** Allowing traders to quickly filter large bid-wanted lists to a smaller, more relevant set of bonds.
-   **Deepening Insight:** Providing a comprehensive, forward-looking risk and value assessment for individual securities.
-   **Quantifying Hidden Risks:** Surfacing idiosyncratic risks that are not apparent from standard valuation metrics alone.

---

## 2. Core Functional Requirements (User Stories)

### 2.1. UC-01: Rapid Triage of Bid-Wanted Lists
As a MUNI trader, I need to view a real-time grid of all active bid-wanted items, each populated with key risk indicators, so that I can rapidly triage the entire list by sorting and filtering to find actionable opportunities and critical risks.

### 2.2. UC-02: Detailed Instrument Analysis
As a MUNI trader, after selecting a CUSIP from the triage view, I need to see a detailed analysis page for that bond that includes forward-looking spread forecasts, so that I can confidently determine a fair and competitive bid price based on its expected trajectory.

### 2.3. UC-03: Explainable Forecasts
As a MUNI trader, when viewing a forecast (e.g., for spread widening), I need to understand the primary factors driving that prediction, so that I can validate the model's output against my own market intuition and build trust in the system.

### 2.4. UC-04: Intelligent Filtering and Review
As a MUNI trader, I need a "quick filter" to hide high-risk items from my main Triage Grid, but also a way to review those hidden items later, so I can maintain focus on viable opportunities without completely losing sight of the broader market activity.

### 2.5. UC-05: Proactive Opportunity Alerting
As a MUNI trader, I want to be proactively alerted to high-conviction opportunities as they arrive on the bid-wanted list, so that I can immediately focus my attention on bonds with favorable risk/reward characteristics and their underlying drivers.

### 2.6. UC-06: Executing a Bid or Offer
As a MUNI trader, when analyzing a bond in the detailed view, I need to see clear, actionable buttons to place my bid or offer directly, so that I can execute on my decision quickly and efficiently.

### 2.7. UC-07: Interactive Yield-Based Valuation
As a trader, when analyzing a bond, I need an interactive valuation tool that provides a system-suggested price derived from a blended **yield** forecast, so I have a quantitative, model-driven starting point for my valuation.

### 2.8. UC-08: Rapid Scenario Modeling
As a trader, using the valuation tool, I need to instantly see how changes in **yield** affect the price and vice-versa, and apply pre-configured scenarios to the blended yield, so I can rapidly model different market views and test my own assumptions before bidding.

---

## 3. UI & Feature Requirements

### 3.1. FR-01: Bid-Wanted Triage View
-   **UI-01.1:** The UI **shall** display a "Triage Grid" as the primary view, showing active bid-wanted items, presumably populated from a real-time data feed.
-   **UI-01.2:** Each row in the grid **shall** represent a single CUSIP from a bid-wanted list.
-   **UI-01.3:** The Triage Grid **shall** be sortable and filterable by any column to facilitate rapid triage.
-   **UI-01.4:** The Triage Grid **shall** contain, at a minimum, the columns defined in **Section 4.1**.
-   **UI-01.5:** Rows in the Triage Grid **shall** be clickable to navigate to the "Detailed Instrument View" for that CUSIP.
-   **UI-01.6:** Cells containing critical risk indicators (e.g., `is_illiquid_flag`, `probability_negative_news_pct`) **shall** use conditional formatting (e.g., color-coding) to draw immediate attention to potential risks.
-   **UI-01.7:** New rows added to the grid in real-time **shall** appear at the top of the grid and be temporarily highlighted (e.g., with a green background for 5 seconds) to signify their arrival.
-   **UI-01.8:** The UI **shall** include a prominent toggle button labeled "Hide Risky".
-   **UI-01.9:** When "Hide Risky" is active, the Triage Grid **shall** hide bonds that meet any of the following "High Risk" criteria, which must be configurable:
    -   `liquidity.is_illiquid_flag` is `true`.
    -   `forecasted_values.1-day.probability_negative_news_pct` exceeds a predefined threshold (e.g., 15%).
    -   `ownership.is_concentrated_flag` is `true`.
    -   The forecasted 1-day yield is predicted to rise significantly (e.g., `forecasted_ytw_change_1d_bps` > 5 bps).
-   **UI-01.10:** When "Hide Risky" is active, a new tab or view labeled "Screened Items" **shall** become available, containing all the items hidden by the filter. This tab should have a note, stating what are the filter conditions applied.

### 3.2. FR-02: Detailed Instrument View
-   **UI-02.1:** This view **shall** present a comprehensive overview of a single CUSIP.
-   **UI-02.2:** The view **shall** be organized into logical sections or "widgets" for clarity. Required sections are:
    -   **Header:** Key identifiers (Issuer Name, CUSIP, Rating).
    -   **Key Security Terms:** Displays core bond attributes like coupon and maturity.
    -   **Yield Assessment:** Displays current and forecasted yield metrics.
    -   **Peer Relative Value:** Displays how the bond's value compares to similar bonds.
    -   **Liquidity Assessment:** Displays the custom liquidity score and forecasted transaction costs.
    -   **Idiosyncratic Risk:** Highlights the "Market Stress Detector" score.
    -   **Downside Risk:** Shows forecasted downside price volatility.
    -   **Market Regime Context:** Displays the current global and MUNI-specific market regimes.
    -   **Tax & Fiscal Profile:** Displays tax characteristics and state fiscal health indicators.
    -   **Trade History:** Summarizes key trading statistics.
-   **UI-02.3:** Each section **shall** display the relevant data points as defined in **Section 4.2**.
-   **UI-02.4:** Forecasted values **shall** clearly indicate the time horizon (e.g., 1-Day, 5-Day).
-   **UI-02.5:** The "Valuation Intelligence" widget, as defined in **FR-05**, **shall** be displayed prominently within the Detailed Instrument View.
-   **UI-02.6:** The Header section of the Detailed Instrument View **shall** contain actionable "Bid" and "Offer" buttons to allow for immediate trade execution.
-   **UI-02.7:** The buttons **shall** be dynamically labeled with the price and size from the incoming request (e.g., "Bid 50k @ 101.50").
-   **UI-02.8:** The button corresponding to the side of the incoming request (e.g., the "Bid" button for a "Bid Wanted" item) **shall** be styled as a primary, high-emphasis action. The opposing action button **shall** be styled as a secondary, lower-emphasis action.
-   **UI-02.9:** In the Peer Relative Value section, the UI **shall** display a clear, qualitative label of either "Cheap" or "Rich" based on the `relative_value.vs_peers_yield_bps` metric. A positive value indicates "Cheap" (higher yield), and a negative value indicates "Rich" (lower yield).
-   **UI-02.10:** The Peer Relative Value section **shall** also display the number of bonds in the peer group and provide an affordance (e.g., a tooltip or clickable link) to view the CUSIPs of the peer instruments and the logic used for their selection.
-   **UI-02.11:** The "Market Regime Context" widget **shall** display both the `global_macro_regime.label` and `contextual_regime.label`. For each regime, it **shall** list the key indicators and their values (`market_regime_context.*.evidence`) that serve as evidence for the classification.

### 3.3. FR-03: Forecast Explainability
-   **UI-03.1:** In the "Detailed Instrument View," next to each key forecast (e.g., Yield, `probability_negative_news_pct`), there **shall** be an affordance (e.g., an "info" icon) that, upon user interaction, reveals the top 3-5 drivers for that forecast.
-   **UI-03.2:** The explainability view **shall** display the feature name and its attribution (contribution) value, indicating the magnitude and direction of its influence.

### 3.4. FR-04: Proactive Opportunity Alert
-   **UI-04.1:** The system **shall** trigger a non-modal, dismissible pop-up alert for any new bid-wanted item that meets the "High-Conviction Opportunity" criteria.
-   **UI-04.2:** The "High-Conviction Opportunity" criteria are defined as meeting **all** of the following configurable conditions:
    -   The model forecasts significant 1-day yield decline (spread tightening), e.g., `forecasted_ytw_change_1d_bps` < -5 bps.
    -   `liquidity.is_illiquid_flag` is `false`.
    -   `forecasted_values.1-day.probability_negative_news_pct` is below a low-risk threshold (e.g., < 5%).
-   **UI-04.3:** The alert pop-up **shall** display:
    -   Key identifiers (CUSIP, Issuer Name).
    -   The specific metric that triggered the alert (e.g., "Forecasted Yield Change: -7 bps").
    -   The top 2-3 **positive** drivers for the favorable forecast (from `forecast_explainability.feature_attributions`).
-   **UI-04.4:** The alert **shall** provide a button to navigate directly to the "Detailed Instrument View" for that CUSIP.

### 3.5. FR-05: Valuation Intelligence

This section defines the requirements for the interactive price/yield valuation tool.

-   **UI-05.1:** The UI **shall** feature a "Valuation Intelligence" widget that serves as an interactive calculator for price and yield.
-   **UI-05.2:** The widget **shall** display a "System Suggested Price," which is calculated based on a blended yield derived from valuation scenarios (see Section 5.4).
-   **UI-05.3:** The widget **shall** contain two primary input fields: **Price** and **Yield** (displaying Yield to Worst). These two fields **shall** be interlinked.
    -   When the user manually enters a **Price**, the **Yield** field **shall** automatically update to reflect the calculated yield for that price.
    -   When the user manually enters a **Yield**, the **Price** field **shall** automatically update to reflect the calculated price for that yield.
-   **UI-05.4:** The UI **shall** provide a mechanism to perform the price/yield calculations. This may involve a client-side library or an API call to a backend service that has access to the instrument's full cash flow schedule.
-   **UI-05.5:** The widget **shall** allow the user to select from pre-defined "Valuation Scenarios" (as defined in Section 5.4) via radio buttons or a dropdown.
-   **UI-05.6:** The widget **shall** also display sliders allowing the user to manually override the weights (`w_ytw_...`) for the blended yield calculation. As sliders are adjusted, the "System Suggested Price" and corresponding yield **shall** update in real-time.

---

## 4. Data Requirements for UI

The UI requires the backend to provide a JSON object for each CUSIP. The fields listed below are the minimum required for the UI to fulfill the specified requirements.

### 4.1. Data for Triage Grid (per CUSIP)

| Field Path                                        | Data Type | Description                             |
| :------------------------------------------------ | :-------- | :-------------------------------------- |
| `cusip`                                           | `string`  | The instrument's CUSIP.                 |
| `security_details.issuer_name`                    | `string`  | Issuer's name.                          |
| `security_details.coupon_rate`                    | `float`   | The bond's annual coupon rate.          |
| `security_details.maturity_date`                  | `date`    | The bond's maturity date.               |
| `security_details.call_features.next_call_date`   | `date`    | The next date the bond can be called.   |
| `security_details.rating`                         | `string`  | Current credit rating.                  |
| `liquidity.is_illiquid_flag`                      | `boolean` | **Critical:** Flag for low liquidity.   |
| `forecasted_values.1-day.bid_ask_spread_pct`      | `float`   | Forecasted 1-day transaction cost.      |
| `forecasted_values.1-day.probability_negative_news_pct` | `float` | **Critical:** Market Stress indicator.  |
| `relative_value.vs_peers_yield_bps`               | `float`   | Yield vs. comparable bonds.             |
| `forecasted_ytw_change_1d_bps`                    | `float`   | **Critical:** Forecasted 1-day change in Yield-to-Worst (YTW). Negative values indicate yield declining (opportunity), positive values indicate yield rising (risk). |

_**Note on Data Derivation:** The field `forecasted_ytw_change_1d_bps` is a derived metric, calculated by the backend orchestration layer before data is sent to the UI. The formula is: `forecasted_values.1-day.yield_to_worst` (derived from RiskPredictionService's spread forecast) - `calculated_risk_metrics.yield_to_worst` (from FinancialCalculationService)._

### 4.2. Data for Detailed Instrument View

This view requires all data from **Section 4.1**, plus the following:

#### 4.2.1. Key Security Terms
| Field Path                                      | Data Type | Description                               |
| :---------------------------------------------- | :-------- | :---------------------------------------- |
| `security_details.coupon_rate`                  | `float`   | The bond's annual coupon rate.            |
| `security_details.maturity_date`                | `date`    | The bond's maturity date.                 |
| `security_details.call_features.is_callable`    | `boolean` | Indicates if the bond has call options.   |
| `security_details.call_features.next_call_date` | `date`    | The next date the bond can be called.     |
| `security_details.call_features.next_call_price`| `float`   | The price at which the bond can be called on the next call date. |

#### 4.2.2. Yield Assessment
This section displays the bond's current yield alongside the model's forward-looking forecasts to show its expected trajectory.

| Field Path                                      | Data Type | Description                                                     |
| :---------------------------------------------- | :-------- | :-------------------------------------------------------------- |
| `calculated_risk_metrics.yield_to_worst`        | `float`   | Current Yield-to-Worst (YTW).                                   |
| `forecasted_values.1-day.yield_to_worst`        | `float`   | Forecasted YTW for the next trading day.                        |
| `forecasted_values.5-day.yield_to_worst`        | `float`   | Forecasted YTW for the next 5 trading days.                     |

_**Note on Yield Forecast Derivation:** The underlying `RiskPredictionService` forecasts the instrument's spread-to-benchmark, not the absolute yield. The `forecasted_values.*.yield_to_worst` metrics displayed in the UI are derived by the backend orchestration layer. This derivation makes a key simplifying assumption common in practice: the benchmark yield curve is held constant over the forecast horizon. The formula is: **Forecasted YTW = Current Benchmark Yield + Forecasted Spread-to-Benchmark.** This provides a clear, actionable yield forecast based on the model's alpha signal (the spread change) while isolating it from general market rate movements._

#### 4.2.3. Peer and Benchmark Relative Value
This section provides a focused view on the instrument's valuation compared to its direct comparables and standard market benchmarks, all on a yield basis.

| Field Path                          | Data Type | Description                                                                                                                              |
| :---------------------------------- | :-------- | :--------------------------------------------------------------------------------------------------------------------------------------- |
| `relative_value.vs_peers_yield_bps` | `float`   | Current YTW vs. the average YTW of the peer group. A positive value indicates the bond is "cheaper" (higher yield); negative is "richer."   |
| `relative_value.vs_mmd_yield_bps`   | `float`   | Current YTW vs. MMD benchmark yield.                                                                                                     |
| `relative_value.vs_ust_yield_bps`   | `float`   | Current YTW vs. UST benchmark yield.                                                                                                     |
| `relative_value.peer_group_size`    | `integer` | The number of comparable bonds identified in the peer group.                                                                             |
| `relative_value.peer_group_cusips`  | `array`   | An array of CUSIPs for the bonds in the peer group.                                                                                      |
| `peer_selection_logic`              | `string`  | A textual description of the criteria used to select the peer group (e.g., "Same State, Same Sector, Maturity +/- 2 Years"). This is backend-generated. |

#### 4.2.4. Liquidity Assessment
| Field Path                                    | Data Type |
| :-------------------------------------------- | :-------- |
| `liquidity.composite_score`                   | `float`   |
| `forecasted_values.5-day.bid_ask_spread_pct`  | `float`   |
| `ownership.is_concentrated_flag`              | `boolean` |
| `trade_history_summary.t1d.total_par_volume`  | `float`   |
| `trade_history_summary.t5d.total_par_volume`  | `float`   |
| `trade_history_summary.t1d.unique_dealer_count` | `integer` |
| `trade_history_summary.t5d.unique_dealer_count` | `integer` |

#### 4.2.5. Idiosyncratic & Downside Risk
| Field Path                                            | Data Type |
| :---------------------------------------------------- | :-------- |
| `forecasted_values.5-day.probability_negative_news_pct` | `float`   |
| `forecasted_values.1-day.downside_price_volatility.value` | `float`   |
| `forecasted_values.5-day.downside_price_volatility.value` | `float`   |

#### 4.2.6. Market Regime Context
This section provides high-level context on the state of the broader market and the specific asset class, which is crucial for interpreting the instrument's risk profile.

| Field Path                                 | Data Type | Description                                                                                                                                  |
| :----------------------------------------- | :-------- | :------------------------------------------------------------------------------------------------------------------------------------------- |
| `market_regime_context.global_macro_regime`  | `object`  | An object containing the global regime classification. See `5.2.6.1`.                                                                        |
| `market_regime_context.contextual_regime`    | `object`  | An object containing the MUNI-specific regime classification. See `5.2.6.1`.                                                                 |

##### 4.2.6.1. Regime Object Structure

| Key        | Data Type | Description                                                                 |
| :--------- | :-------- | :-------------------------------------------------------------------------- |
| `label`    | `string`  | The classified regime label (e.g., "Bull_Steepener", "Muni_Cheap_Market").    |
| `evidence` | `array`   | An array of objects, where each object represents a key indicator providing evidence for the classification. See structure in `5.2.6.2`. |

##### 4.2.6.2. Evidence Object Structure

| Key     | Data Type | Description                                                                 |
| :------ | :-------- | :-------------------------------------------------------------------------- |
| `name`  | `string`  | The display name of the indicator (e.g., "10Y-2Y Slope", "VIX", "MMD/UST Ratio"). |
| `value` | `string`  | The formatted value of the indicator to be displayed in the UI (e.g., "150 bps", "18.5", "85%"). |

#### 4.2.7. Tax & Fiscal Profile
This section provides details on the bond's tax implications and the fiscal health of the issuing state, which is a key driver for MUNI credit quality.

| Field Path                                                  | Data Type | Description                                                 |
| :---------------------------------------------------------- | :-------- | :---------------------------------------------------------- |
| `security_details.tax_profile.tax_status`                   | `string`  | The bond's tax status (e.g., 'TAX_EXEMPT_FEDERAL').         |
| `security_details.tax_profile.is_amt`                       | `boolean` | Flag indicating if the bond is subject to Alternative Minimum Tax. |
| `security_details.tax_profile.in_state_tax_exempt`          | `boolean` | Flag indicating if income is also exempt from state tax for in-state residents. |
| `state_fiscal_health.tax_receipts_yoy_growth`               | `float`   | Year-over-year growth in tax receipts for the issuing state. |
| `state_fiscal_health.budget_surplus_deficit_pct_gsp`        | `float`   | The state's budget surplus/deficit as a percentage of its Gross State Product. |

#### 4.2.8. Forecast Explainability
| Field Path                                                     | Data Type | Description                                                                                                                              |
| :------------------------------------------------------------- | :-------- | :--------------------------------------------------------------------------------------------------------------------------------------- |
| `forecast_explainability.feature_attributions.*`               | `array`   | An array of objects, where each object represents a single feature attribution for a given forecast. See structure in `4.2.8.1`. |

#### 4.2.8.1. Feature Attribution Object Structure
The `feature_attributions` array contains objects with the following key-value pairs. This structure is required to isolate positive drivers for the Opportunity Alert feature.

| Key           | Data Type | Description                                             |
| :------------ | :-------- | :------------------------------------------------------ |
| `feature`     | `string`  | The name of the feature driver (e.g., "Repo Specialness"). |
| `attribution` | `float`   | The feature's contribution score (can be positive or negative). |

#### 4.2.9. Bid Wanted Details
This object contains the specific details for the actionable bid/offer request.

| Field Path                  | Data Type | Description                                                                 |
| :-------------------------- | :-------- | :-------------------------------------------------------------------------- |
| `bid_wanted_details.side`   | `string`