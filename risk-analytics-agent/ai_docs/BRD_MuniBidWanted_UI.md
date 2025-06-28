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

### 2.7. UC-07: Quantitative Valuation Assessment
As a trader, when analyzing a bond, I need to see a system-suggested price based on aggregated forecasts and key risk metrics, so I have a quantitative, model-driven starting point for my own valuation.

### 2.8. UC-08: Scenario-Based Valuation Modeling
As a trader, I need to apply pre-configured valuation scenarios (e.g., 'Defensive', 'Base Case') to the suggested price calculation, so I can quickly see how different market views or strategies impact the bond's valuation without adjusting weights manually each time.

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
    -   The forecasted 1-day spread is predicted to widen significantly (e.g., `forecasted_oas_change_1d_bps` > 5 bps).
-   **UI-01.10:** When "Hide Risky" is active, a new tab or view labeled "Screened Items" **shall** become available, containing all the items hidden by the filter. This tab should have a note, stating what are the filter conditions applied.

### 3.2. FR-02: Detailed Instrument View
-   **UI-02.1:** This view **shall** present a comprehensive overview of a single CUSIP.
-   **UI-02.2:** The view **shall** be organized into logical sections or "widgets" for clarity. Required sections are:
    -   **Header:** Key identifiers (Issuer Name, CUSIP, Rating).
    -   **Value Assessment:** Displays relative value metrics and forward-looking spread forecasts.
    -   **Liquidity Assessment:** Displays the custom liquidity score and forecasted transaction costs.
    -   **Idiosyncratic Risk:** Highlights the "Market Stress Detector" score.
    -   **Downside Risk:** Shows forecasted downside price volatility.
    -   **Trade History:** Summarizes key trading statistics.
-   **UI-02.3:** Each section **shall** display the relevant data points as defined in **Section 4.2**.
-   **UI-02.4:** Forecasted values **shall** clearly indicate the time horizon (e.g., 1-Day, 5-Day).
-   **UI-02.5:** The Header section of the Detailed Instrument View **shall** contain actionable "Bid" and "Offer" buttons to allow for immediate trade execution.
-   **UI-02.6:** The buttons **shall** be dynamically labeled with the price and size from the incoming request (e.g., "Bid 50k @ 101.50").
-   **UI-02.7:** The button corresponding to the side of the incoming request (e.g., the "Bid" button for a "Bid Wanted" item) **shall** be styled as a primary, high-emphasis action. The opposing action button **shall** be styled as a secondary, lower-emphasis action.

### 3.3. FR-03: Forecast Explainability
-   **UI-03.1:** In the "Detailed Instrument View," next to each key forecast, there **shall** be an affordance (e.g., an "info" icon) that, upon user interaction (e.g., hover or click), reveals the top 3-5 drivers for that forecast.
-   **UI-03.2:** The explainability view **shall** display the feature name and its attribution (contribution) value, indicating the magnitude and direction of its influence.

### 3.4. FR-04: Proactive Opportunity Alert
-   **UI-04.1:** The system **shall** trigger a non-modal, dismissible pop-up alert for any new bid-wanted item that meets the "High-Conviction Opportunity" criteria.
-   **UI-04.2:** The "High-Conviction Opportunity" criteria are defined as meeting **all** of the following configurable conditions:
    -   The model forecasts significant 1-day spread tightening (e.g., `forecasted_oas_change_1d_bps` < -5 bps).
    -   `liquidity.is_illiquid_flag` is `false`.
    -   `forecasted_values.1-day.probability_negative_news_pct` is below a low-risk threshold (e.g., < 5%).
-   **UI-04.3:** The alert pop-up **shall** display:
    -   Key identifiers (CUSIP, Issuer Name).
    -   The specific metric that triggered the alert (e.g., "Forecasted Tightening: -7 bps").
    -   The top 2-3 **positive** drivers for the favorable forecast (from `forecast_explainability.feature_attributions`).
-   **UI-04.4:** The alert **shall** provide a button to navigate directly to the "Detailed Instrument View" for that CUSIP.

---

## 4. Data Requirements for UI

The UI requires the backend to provide a JSON object for each CUSIP. The fields listed below are the minimum required for the UI to fulfill the specified requirements.

### 4.1. Data for Triage Grid (per CUSIP)

| Field Path                                        | Data Type | Description                             |
| :------------------------------------------------ | :-------- | :-------------------------------------- |
| `cusip`                                           | `string`  | The instrument's CUSIP.                 |
| `security_details.issuer_name`                    | `string`  | Issuer's name.                          |
| `security_details.rating`                         | `string`  | Current credit rating.                  |
| `liquidity.is_illiquid_flag`                      | `boolean` | **Critical:** Flag for low liquidity.   |
| `forecasted_values.1-day.bid_ask_spread_pct`      | `float`   | Forecasted 1-day transaction cost.      |
| `forecasted_values.1-day.probability_negative_news_pct` | `float` | **Critical:** Market Stress indicator.  |
| `relative_value.vs_peers_bps`                     | `float`   | Spread vs. comparable bonds.            |
| `forecasted_oas_change_1d_bps`                  | `float`   | **Critical:** Forecasted 1-day change in OAS. Negative values indicate tightening (opportunity), positive values indicate widening (risk). |

_**Note on Data Derivation:** The field `forecasted_oas_change_1d_bps` is a derived metric, calculated by the backend orchestration layer before data is sent to the UI. The formula is: `forecasted_values.1-day.credit_spread_oas_bps` (from RiskPredictionService) - `calculated_risk_metrics.option_adjusted_spread_bps` (from FinancialCalculationService)._

### 4.2. Data for Detailed Instrument View

This view requires all data from **Section 4.1**, plus the following:

#### 4.2.1. Value & Forecast Assessment
This section displays the bond's current value relative to benchmarks and peers, alongside the model's forward-looking forecasts to show its expected trajectory.

| Field Path                                            | Data Type | Description                                                     |
| :---------------------------------------------------- | :-------- | :-------------------------------------------------------------- |
| `calculated_risk_metrics.option_adjusted_spread_bps`  | `float`   | Current Option-Adjusted Spread (OAS).                           |
| `relative_value.vs_peers_bps`                         | `float`   | Current spread vs. comparable bonds.                            |
| `forecasted_values.1-day.credit_spread_oas_bps`       | `float`   | Forecasted OAS for the next trading day.                        |
| `forecasted_values.5-day.credit_spread_oas_bps`       | `float`   | Forecasted OAS for the next 5 trading days.                     |
| `relative_value.vs_mmd_bps`                           | `float`   | Current spread vs. MMD benchmark.                               |
| `relative_value.vs_ust_bps`                           | `float`   | Current spread vs. UST benchmark.                               |

#### 4.2.2. Liquidity Assessment
| Field Path                                    | Data Type |
| :-------------------------------------------- | :-------- |
| `liquidity.composite_score`                   | `float`   |
| `forecasted_values.5-day.bid_ask_spread_pct`  | `float`   |
| `ownership.is_concentrated_flag`              | `boolean` |
| `trade_history_summary.t1d.total_par_volume`  | `float`   |
| `trade_history_summary.t5d.total_par_volume`  | `float`   |
| `trade_history_summary.t1d.unique_dealer_count` | `integer` |
| `trade_history_summary.t5d.unique_dealer_count` | `integer` |

#### 4.2.3. Idiosyncratic & Downside Risk
| Field Path                                            | Data Type |
| :---------------------------------------------------- | :-------- |
| `forecasted_values.5-day.probability_negative_news_pct` | `float`   |
| `forecasted_values.1-day.downside_price_volatility.value` | `float`   |
| `forecasted_values.5-day.downside_price_volatility.value` | `float`   |

#### 4.2.4. Forecast Explainability
| Field Path                                                     | Data Type | Description                                                                                                                              |
| :------------------------------------------------------------- | :-------- | :--------------------------------------------------------------------------------------------------------------------------------------- |
| `forecast_explainability.feature_attributions.*`               | `array`   | An array of objects, where each object represents a single feature attribution for a given forecast. See structure in `4.2.4.1`. |

#### 4.2.4.1. Feature Attribution Object Structure
The `feature_attributions` array contains objects with the following key-value pairs. This structure is required to isolate positive drivers for the Opportunity Alert feature.

| Key           | Data Type | Description                                             |
| :------------ | :-------- | :------------------------------------------------------ |
| `feature`     | `string`  | The name of the feature driver (e.g., "Repo Specialness"). |
| `attribution` | `float`   | The feature's contribution score (can be positive or negative). |

#### 4.2.5. Bid Wanted Details
This object contains the specific details for the actionable bid/offer request.

| Field Path                  | Data Type | Description                                                                 |
| :-------------------------- | :-------- | :-------------------------------------------------------------------------- |
| `bid_wanted_details.side`   | `string`  | The side of the request, e.g., "BWIC" (Bid Wanted) or "OWIC" (Offer Wanted). |
| `bid_wanted_details.price`  | `float`   | The price associated with the request.                                      |
| `bid_wanted_details.size`   | `integer` | The par value size of the request.                                          |

### 4.3. Data for Valuation Intelligence

| Field Path                                            | Data Type | Description                                                                 |
| :---------------------------------------------------- | :-------- | :-------------------------------------------------------------------------- |
| `calculated_risk_metrics.modified_duration`           | `float`   | The bond's modified duration, used for price sensitivity calculations.      |
| `security_details.price`                              | `float`   | The current clean market price of the bond.                                 |
| `forecasted_values.20-day.credit_spread_oas_bps`      | `float`   | Forecasted OAS for the next 20 trading days.                                |
| `market_context.regime`                               | `string`  | The current market regime (e.g., "Risk-On", "Risk-Off", "Neutral").          |

### 4.4. Valuation Scenarios

The "Valuation Intelligence" widget **shall** provide several pre-configured scenarios that programmatically set the forecast weights. The user **shall** retain the ability to manually override these weights using the sliders.

| Scenario Name       | Trigger / Condition                                            | `w_current` | `w_1d` | `w_5d` | `w_20d` | Explanation                                                                               |
| :------------------ | :------------------------------------------------------------- | :---------- | :----- | :----- | :------ | :---------------------------------------------------------------------------------------- |
| **Base Case**       | Default view.                                                  | 40          | 30     | 20     | 10      | A balanced view considering short-term and long-term forecasts.                           |
| **Defensive Price** | `probability_negative_news_pct` > 15% OR `market_regime` = "Risk-Off" | 80          | 10     | 5      | 5       | Heavily weights the current OAS for a conservative price due to negative factors.         |
| **Aggressive Alpha**  | User-selected.                                               | 10          | 50     | 30     | 10      | Weights near-term forecasts to find opportunities in short-term spread tightening.        |
| **Strategic Value** | User-selected.                                               | 10          | 10     | 30     | 50      | Weights longer-term forecasts, looking past short-term volatility for strategic entry points. |