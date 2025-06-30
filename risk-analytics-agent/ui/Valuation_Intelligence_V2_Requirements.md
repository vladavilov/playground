**Title:** Business Requirements: Valuation Intelligence Enhancements (v2)

**Author:** FinReq Architect

**Version:** 1.0

### 1. Introduction

This document outlines the required enhancements for the "Valuation Intelligence" component of the MUNI Bid-Wanted Trading Support UI. The requirements are based on feedback from trading professionals to improve the accuracy, transparency, and decision-making utility of the tool. The core objective is to evolve the valuation section from a data aggregator into a truly integrated decision-support engine.

---

### 2. General Requirements

| ID | Title | Description |
| :-- | :--- | :--- |
| **GR-01** | **User Override Capability for Forecast Inputs** | Users must be able to manually override the key forecast input values used in the strategy calculations. This allows traders to inject their own market view or experience into the model, fostering trust and providing flexibility. |
| **GR-02** | **Provide Explanatory Tooltips for All Key Values** | To enhance transparency and build user trust, all key calculated, forecasted, or model-driven values displayed in the "Valuation Intelligence" and other detail panels must include an information icon (`i`). Hovering over this icon will reveal a tooltip explaining how the value was derived or what its primary components/drivers are. |

**Acceptance Criteria for GR-01:**
- The following displayed values in the "Forecast Components" section should be converted to editable input fields:
    - `1D Trend Forecast`
    - `5D Trend Forecast`
    - `Peer RV Target` (see CF-02 for rename)
- When a user modifies a value in one of these fields, the "Strategy-Based Fair Value" (both Price and Yield) must recalculate and update in real-time.
- A visual indicator (e.g., a small icon, italics, or a colored border) must appear next to any value that has been manually overridden to distinguish it from the model-generated value.

**Acceptance Criteria for GR-02:**
- An info icon with a tooltip is present for:
    - Each "Forecast Component" value (`1D Trend`, `5D Trend`, `Peer RV Target`). The tooltip should explain the source of the forecast (e.g., "1D trend model output").
    - The "Strategy-Based Fair Value" Price and Yield. The tooltip should summarize the strategy, weights, and adjustments applied (e.g., "Calculated using Fair Value strategy with a 2.5 bps risk premium adjustment").
- The implementation should leverage the existing `createForecastLine` function or a similar reusable component to ensure consistency.

---

### 3. Calculation and Formula Accuracy

| ID | Title | Description |
| :-- | :--- | :--- |
| **CF-01** | **Use Effective Duration for Price/Yield Calculations** | The current model uses Modified Duration to estimate price changes from yield changes. This is inaccurate for the large portion of MUNI bonds that are callable. The calculation must be updated to use Effective Duration, which correctly accounts for the impact of changing yields on the bond's cash flows due to embedded options. |
| **CF-02** | **Rename "Peer RV Forecast" to "Peer RV Target"** | The input labeled "Peer RV Forecast" is sourced from the *current* spread vs. peers, not a future predicted value. The label is misleading and must be changed to "Peer RV Target" or a similar, more accurate term to reflect that it represents a mean-reversion target. |

**Details for CF-01: Formula & Data Dependencies**

*   **Current Formula (Incorrect for Callables):** 
    `ΔP ≈ -D_mod × P × Δy`
*   **Required Formula (Correct):**
    `ΔP ≈ -D_eff × P × Δy`
    Where:
    *   `ΔP` = Change in Price
    *   `D_eff` = **Effective Duration**
    *   `P` = Initial Price
    *   `Δy` = Change in Yield
*   **Data Dependency:**
    *   The backend data payload for each CUSIP must be updated to provide `effective_duration` instead of `modified_duration`. The `uiController.js` must be updated to consume this new field.

**Acceptance Criteria:**
- **CF-01:** The `updatePriceFromYield` and `updateYieldFromPrice` functions in `uiController.js` must use the `effective_duration` field from the data object for all calculations.
- **CF-02:** All instances of the label "Peer RV Forecast" in `index.html` and `uiController.js` must be changed to "Peer RV Target".

---

### 4. Strategy & Model Logic Enhancements

| ID | Title | Description |
| :-- | :--- | :--- |
| **SL-01** | **Integrate Risk Factors into Valuation Model** | The "Valuation Intelligence" section currently operates in isolation from the other risk panels. The core `blendedSpread` calculation must be enhanced to incorporate key risk factors as direct inputs, making the final valuation context-aware. |
| **SL-02** | **Refine "Cautious" Strategy Logic** | The "Cautious" strategy, which currently defaults to 100% Peer RV, is too simplistic. It should be refined to represent a truly risk-averse view by demanding a wider spread, not just ignoring trend data. |
| **SL-03** | **Make Horizon Adjustment Dynamic** | The current horizon adjustment adds a static, arbitrary number of basis points (e.g., 2bps for 5D). This "magic number" lacks justification and should be replaced with a dynamic premium linked to a transparent market factor. |
| **SL-04** | **Reconcile UI Horizon Options with Available Forecast Data** | The UI provides a "10D" forecast horizon selector, but the underlying data model provides forecasts for 1, 5, and 20-day periods. This discrepancy must be resolved. The 10-day forecast needs a clearly defined calculation method, or the UI should be updated to reflect the available data points. |

**Details for SL-01: Risk Factor Integration**

The `blendedSpread` calculation in `updateValuation` (`uiController.js`) must be modified. The new formula should be conceptually:
`Final Spread = (Weighted Sum of Forecasts) + Risk Premium Adjustment`

The **Risk Premium Adjustment** must be a function of:
1.  **Market Regime:** The `global_macro_regime` and `contextual_regime` should influence the adjustment. (e.g., `Bear_Steepener` regime adds a positive basis point adjustment).
2.  **Liquidity Score:** The `liquidity.composite_score` must be a direct input. Lower scores must result in a higher (wider) spread adjustment.
3.  **Idiosyncratic Risk:** The `forecasted_values['5-day'].probability_negative_news_pct` must be a direct input. Higher probability must result in a higher spread adjustment.

**Details for SL-02: "Cautious" Strategy**

*   The weights for the "Cautious" strategy should be redefined. Instead of `100% PEER_RV`, it should be `100% PEER_RV + a static risk premium (e.g., 5 bps)` or be linked to the `Risk Premium Adjustment` from SL-01.

**Details for SL-03: Dynamic Horizon Premium**

*   The hardcoded horizon adjustment logic (`HORIZONS.indexOf(state.activeHorizon) * 2`) must be replaced.
*   The new premium should be a function of a market volatility indicator.
*   **Data Dependency:** The backend must provide a relevant volatility measure (e.g., **MOVE Index value**) in the main data payload.
*   **Example Logic:** `Horizon Premium = (Horizon Step) × (MOVE Index / Constant)`

**Details for SL-04: 10-Day Forecast Calculation**
*   If the "10D" horizon is kept, a calculation method must be implemented. A suggested approach is linear interpolation between the 5-day and 20-day forecast values.
*   **Formula:** `Value_10D = Value_5D + (Value_20D - Value_5D) * ((10 - 5) / (20 - 5))`
*   This interpolation should apply to all relevant forecasted metrics used in the valuation.
*   Alternatively, the UI could be changed to offer `1D`, `5D`, and `20D` horizons to align directly with the backend data. The product owner should be consulted on this choice.

**Acceptance Criteria:**
- **SL-01:** The `updateValuation` function is refactored to include a `Risk Premium Adjustment` based on Market Regime, Liquidity Score, and Negative News Probability. The logic for this adjustment is clearly commented.
- **SL-02:** The "Cautious" strategy calculation in `uiController.js` is updated to add a risk premium to the resulting spread.
- **SL-03:** The static horizon adjustment logic is replaced with a dynamic calculation that depends on a market volatility indicator provided by the backend.
- **SL-04:** A clear decision is made and implemented: either the "10D" option is replaced with "20D" in `uiController.js`, or a 10-day value is calculated via interpolation. If interpolation is used, the logic is clearly commented in the code.

---

### 5. High-Level Work Breakdown

A logical implementation sequence is proposed below to manage dependencies.

**Phase 1: Foundational Fixes (Low Complexity)**
1.  **Task 1.1:** (Backend) Update data payload to replace `modified_duration` with `effective_duration`.
2.  **Task 1.2:** (Frontend) Update `uiController.js` to use `effective_duration` (Requirement CF-01).
3.  **Task 1.3:** (Frontend) Rename "Peer RV Forecast" label in all relevant files to "Peer RV Target" (Requirement CF-02).
4.  **Task 1.4:** (Frontend) Implement user override capability for forecast inputs (Requirement GR-01).

**Phase 2: Strategy & Model Enhancements (Medium to High Complexity)**
1.  **Task 2.1:** (Backend) Add `MOVE Index` value (or similar volatility metric) to the data payload.
2.  **Task 2.2:** (Frontend) Refactor the horizon adjustment to be dynamic based on the new volatility metric (Requirement SL-03).
3.  **Task 2.3:** (Frontend) Refine the "Cautious" strategy logic (Requirement SL-02).
4.  **Task 2.4:** (Frontend) Design and implement the `Risk Premium Adjustment` logic, integrating data from the other UI panels into the `blendedSpread` calculation. This is the most complex task and may require further prototyping of the adjustment formula (Requirement SL-01).
5.  **Task 2.5:** (Frontend) Implement reconciliation for the 10D forecast horizon, either by changing the UI or implementing interpolation logic (Requirement SL-04).
6.  **Task 2.6:** (Frontend) Add explanatory tooltips to all key calculated and forecasted values in the detail view to improve transparency (Requirement GR-02). 