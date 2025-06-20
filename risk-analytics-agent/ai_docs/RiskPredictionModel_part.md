# Risk Prediction Model: Requirements Specification

**Version: 1.0**

---

## Part 1: Business & Functional Requirements

This part outlines the high-level business goals and the core functional capabilities of the Risk Prediction Model.

### 1. Overview & Purpose
The primary purpose of this model is to forecast key risk factors over short-term horizons for a given instrument in the Fixed Income Risk Agent. This enables the generation of regime/news-sentiment-aware, explainable, and actionable risk insights.

The model is not intended to replace trader discretion but to act as a powerful **decision-support tool**. Its primary value is to systematically provide quantitative, forward-looking insights that enhance a trader's ability to manage risk and identify opportunities. The model enables:
- Systematic scanning for risks and opportunities across a portfolio.
- Quantification of risks that may otherwise be purely intuitive.
- Proactive risk management based on anticipated market moves rather than reactions.
- The ability to challenge and validate existing market assumptions with quantitative data.

The practical application and business value of the model's outputs in a trading context are detailed below:

#### 1.1. Use Cases by Forecasted Metric
The model's outputs are designed to provide a holistic risk view, combining forecasts for standard market risk (OAS, Bid-Ask Spread) with indicators for idiosyncratic event risk (`probability_negative_news_pct`). These two types of metrics are complementary and intended to be used together to form a comprehensive risk assessment, as they manage different types of risk.

##### Forecasted Credit Spread / OAS
- **Business Value:** Provides a forward-looking view on relative value, enabling a shift from reactive to proactive portfolio management.
- **Use Case 1 (Risk Mitigation):** A forecast of significant spread widening for an instrument **shall** serve as an early-warning indicator, prompting a formal position review to mitigate risk or apply a hedge.
- **Use Case 2 (Opportunity Identification):** A forecast of spread tightening **shall** serve as a potential buy signal, triggering further due diligence.

##### Forecasted Bid-Ask Spread
- **Business Value:** Directly forecasts transaction costs and market liquidity, which is especially critical in the municipal bond market.
- **Use Case (Execution Strategy):** A forecast of widening bid-ask spreads **shall** be used to inform the timing and strategy of executing large trades, potentially compelling an accelerated sale to avoid deteriorating liquidity.

##### Probability of Negative News (Market Stress Detection)
- **Business Value:** Acts as a quantitative indicator for instrument-specific, idiosyncratic event risk (or "jump risk"). The metric is not interpreted as a literal prediction of a news event, but rather as a **Market Stress Detector**. It identifies when an instrument is exhibiting trading patterns that have historically preceded negative credit events, serving as a critical early warning system.
- **Use Case 1 (Risk Override):** This metric **shall** act as a critical override and sanity check for pure quantitative value models. For example, if a bond is flagged as "cheap" based on a forecasted OAS, a high `probability_negative_news_pct` would override a potential "buy" decision and instead trigger a deeper investigation into the source of the market stress.
- **Use Case 2 (Complementary Risk Management):** This metric **shall** be used alongside quantitative risk metrics (like OAS) to manage two distinct types of risk. While OAS manages market-driven relative value risk, this metric manages issuer-specific event risk, providing a more complete risk profile.

##### Downside Price Volatility (VaR / Expected Shortfall)
- **Business Value:** Provides a forward-looking, quantitative measure of potential short-term loss, superior to historical-only metrics.
- **Use Case (Risk & Sizing):** This metric **shall** be a primary input for managing portfolio risk limits and making decisions on position sizing. A forecasted spike in volatility would support a decision to reduce exposure.

#### 1.2. Criticality of Model Explainability
- **Requirement:** The `forecast_explainability` output, detailing feature attributions, is a critical, non-negotiable feature for business adoption.
- **Business Value:** The feature attributions (the "why") are essential for validating the model's outputs against a trader's market experience and intuition. This transparency is what elevates the model from an untrusted "black box" to a professional-grade tool, as it allows users to understand the reasoning behind a forecast and make a more informed final decision.

### 2. Core Functional Requirements
- **RP-FR-01:** The system **shall** employ a regime- and news-sentiment-aware time-series forecasting model to predict future values of key quantitative risk factors.
- **RP-FR-02:** The model **shall** support configurable forecasting horizons (e.g., 1-day, 5-day, 20-day).
- **RP-FR-03:** The model **shall** be explainable, providing the key drivers and their contributions for each forecast generated.
- **RP-FR-04:** The forecasted outputs **shall** be used as "Underlying Risk" inputs into the downstream risk synthesis and narrative generation process.
- **RP-FR-05:** The system **shall** use the forecasted risk values to calculate other forward-looking metrics, such as future Yield to Worst (YTW) and DV01, to support scenario analysis (Optional).
- **RP-FR-06:** The model **shall** ingest historical and real-time news sentiment data as a time-varying input to inform its forecasts.

---

## Part 2: System Interfaces & Data Models

This part defines the data contracts for the system, including all inputs and outputs.

### 3. Input Data Model
The model requires a combination of static, time-varying, and known future inputs for each instrument.

#### 3.1. Input Data Sources
The model training and inference processes **shall** ingest data from the following sources for each instrument:
- **`FinancialDataObject`**: The full data object as produced by the `FinCalculations` engine. This provides the primary source of historical time-series data and calculated metrics.
- **`NewsSentiment`**: Historical and current sentiment scores for the instrument.
- **`MarketRegimeClassification`**: The full classification object, as defined in `MarketRegime_part.md`.
- **`EconomicCalendarService`**: Provides a feature matrix of scheduled high-impact U.S. economic events (e.g., "FOMC Rate Decision", "CPI"). This service, defined in `EconomicCalendarService_part.md`, is the source for time-varying known future inputs.

#### 3.2. Detailed Input-to-Model Feature Mapping
To prepare the data for the Temporal Fusion Transformer (TFT) model, the raw inputs from the sources above **shall** be mapped to the following feature categories.

##### 3.2.1. Static Features (Features that do not change over time for a given instrument)
These features describe the instrument's immutable characteristics.
- **Source:** `FinancialDataObject.security_details` & underlying `SecurityMaster` data.
  - `instrument_type`: `string` - To be one-hot encoded (e.g., 'MUNI', 'TFI_CORPORATE').
  - `sector`: `string` - To be one-hot encoded.
  - `rating`: `string` - To be ordinally encoded based on the scale defined in `FinCalculations_part.md`.
  - `coupon_rate`: `float` - To be used as a continuous feature.
  - `maturity_date`: `date` - To be used to derive a `time_to_maturity` feature, calculated relative to the forecast date.
  - `state`: `string` - (For `instrument_type` = 'MUNI' only) To be one-hot encoded.
  - `tax_status`: `string` - (For `instrument_type` = 'MUNI' only) To be one-hot encoded.

##### 3.2.2. Time-Varying Known Inputs (Features whose future values are known in advance)
- **Source:** `FinancialDataObject.security_details`
  - `call_schedule`: `array` - To be engineered into features such as `days_to_next_call` and `next_call_price`. These values are known for all future dates.
- **Source:** `EconomicCalendarService`
  - `scheduled_economic_events`: `array` - A list of filtered, high-importance economic events. These are retrieved from the `EconomicCalendarService`. The detailed algorithm for transforming these events into model-ready features is specified in **Part 7, Section 11**.

##### 3.2.3. Time-Varying Observed Inputs (Historical time-series features whose future values are unknown)
These features form the historical context for the model's predictions. For daily forecasting, these values are taken from the `t1d` summary of the `FinancialDataObject`.
- **Source:** `FinancialDataObject.market_data`
  - `price`: `float`
  - `bid_ask_spread_bps`: `float`
- **Source:** `FinancialDataObject.calculated_risk_metrics`
  - `yield_to_maturity`, `yield_to_worst`, `dv01`, `cs01`, `option_adjusted_spread_bps`, `downside_price_volatility_5d`, `downside_price_volatility_20d`: all `float` to be used as continuous features.
- **Source:** `FinancialDataObject.liquidity`
  - `composite_score`: `float`
  - `is_illiquid_flag`: `boolean` (encoded as `integer` 0 or 1)
- **Source:** `FinancialDataObject.trade_history_summary.t1d`
  - All fields (e.g., `total_par_volume`, `trade_count`, `unique_dealer_count`, `customer_buy_par_volume`, `trade_price_volatility`) to be used as continuous features.
- **Source:** `FinancialDataObject.relative_value`
  - `vs_mmd_bps`, `vs_ust_bps`, `vs_peers_bps` to be used as continuous features.
- **Source:** `FinancialDataObject.market_context` & `state_fiscal_health`
  - All fields (e.g., `yield_curve_slope_10y2y`, `mmd_ust_ratio_10y`, `tax_receipts_yoy_growth`) to be used as continuous features. Null values for non-applicable instruments (e.g., state data for corporates) must be imputed or handled by the model.
- **Source:** `NewsSentiment`
  - `sentiment_score`: `float`
- **Source:** `MarketRegimeClassification`
  - `regime_label`: `string` - To be one-hot encoded.
  - `regime_probabilities`: `object` - The vector of probabilities (e.g., `Bull_Steepener`, `Bear_Flattener`) to be used as a set of continuous features.

##### 3.2.4. Prediction Targets
These are the outputs the model will be trained to forecast. Their historical values are also used as time-varying observed inputs where applicable.
- **`credit_spread_oas_bps`**: Sourced directly from `FinancialDataObject.calculated_risk_metrics.option_adjusted_spread_bps`.
- **`bid_ask_spread_pct`**: Sourced from `FinancialDataObject.market_data.bid_ask_spread_bps`. A conversion from bps to percent (`value / 100`) is required to match the specified output unit.
- **`probability_negative_news_pct`**: The model will be trained to predict the probability of the `NewsSentiment.negative_news_flag` being `true` (1). The flag itself is the historical target label.
- **`downside_price_volatility`**: Sourced from `FinancialDataObject.calculated_risk_metrics` (e.g., `downside_price_volatility_5d` for the 5-day forecast). This metric is pre-calculated by the `FinCalculations` service. Its historical values serve as both a time-varying observed input and the target for forecasting.

### 4. Output Data Model

#### 4.1. Output Schema
The standard output for a single forecast run **shall** conform to the following JSON schema:
```json
{
  "cusip": "string",
  "forecast_context": {
    "forecast_date": "datetime",
    "model_version": "string"
  },
  "forecasted_values": [
    {
      "horizon": "1-day",
      "credit_spread_oas_bps": "float",
      "bid_ask_spread_pct": "float",
      "probability_negative_news_pct": "float",
      "downside_price_volatility": {
        "metric_type": "1-day VaR",
        "value": "float"
      }
    },
    {
      "horizon": "5-day",
      "credit_spread_oas_bps": "float",
      "bid_ask_spread_pct": "float",
      "probability_negative_news_pct": "float",
      "downside_price_volatility": {
        "metric_type": "5-day VaR",
        "value": "float"
      }
    },
    {
      "horizon": "20-day",
      "credit_spread_oas_bps": "float",
      "bid_ask_spread_pct": "float",
      "probability_negative_news_pct": "float",
      "downside_price_volatility": {
        "metric_type": "20-day VaR",
        "value": "float"
      }
    }
  ],
  "forecast_explainability": {
    "feature_attributions": {
      "credit_spread_oas_bps": [
        {"feature": "market_regime.Bull_Steepener", "attribution": "float"},
        {"feature": "trade_history.t5d.total_par_volume", "attribution": "float"},
        {"feature": "historical_oas.lag_1d", "attribution": "float"}
      ]
    }
  }
}
```

#### 4.2. Output Interpretation
- **RP-AI-01:** The `forecasted_values` provide the point estimates for each risk factor at different time horizons.
- **RP-AI-02:** The `forecast_explainability` object **shall** be used to identify the top positive and negative contributors to a given forecast, which is a critical input for the narrative generation engine.

---

## Part 3: Core Logic & Methodology

This part describes the business logic and technical foundations for the prediction model.

### 5. Forecasting Methodology
- **RP-BR-01:** The system **shall** use a **Temporal Fusion Transformer (TFT)** model architecture. This choice is driven by the model's ability to handle diverse data types (static, time-varying, known future inputs) and its built-in explainability features.
- **RP-BR-02:** The model **shall** be trained to simultaneously predict all required output targets (multi-headed output).
- **RP-BR-03:** The `Market Regime Classification` inputs **shall** be treated as static features for the duration of a single forecast. The `regime_label` will be encoded as a categorical variable, and the `regime_probabilities` will be used as a vector of continuous inputs.

### 6. Validation Rules
- **RP-VR-01:** Model predictions **must** be explainable. The feature attributions for each forecast are a mandatory output.
- **RP-VR-02:** Forecasts **must** be regime-aware. The `regime_label` must be a feature used by the model.
- **RP-VR-03:** Forecasted values **must** be available for all risk factors and horizons specified in the `Core Functional Requirements`.
- **RP-VR-04:** Back-testing accuracy **shall** be measured using standard metrics such as Mean Absolute Error (MAE) and Root Mean Squared Error (RMSE) for continuous variables, and Brier Score for probabilistic forecasts.

---

## Part 4: Technical Implementation Plan

This part provides a guide for developers for training and running the prediction model.

### 7. Model Training
- **RP-IMPL-01:** The training data **shall** be structured as time-series data for each CUSIP, containing all input features specified in Section 3.1, aligned by date.
- **RP-IMPL-02:** The model **shall** be trained on a historical dataset covering multiple market cycles and regimes to ensure robustness.

### 8. Inference
- **RP-IMPL-03:** The system **shall** support real-time inference for a single instrument, using the most up-to-date input features.
- **RP-IMPL-04:** The system **shall** support batch inference for generating forecasts for a universe of instruments.

---

## Part 5: Non-Functional & Operational Requirements

This part covers system-wide constraints related to performance, operation, and maintenance.

### 9. System Requirements
- **RP-NFR-01:** The model **must** be performant enough to support real-time inference requests with acceptable latency (e.g., < 1 second per instrument).
- **RP-NFR-02:** The model architecture **must** support explainability.
- **RP-NFR-03:** The model **must** be periodically retrained on new data to prevent drift and maintain accuracy. Retraining frequency should be evaluated on a quarterly basis.
- **RP-NFR-04:** The model's predictions **shall** be versioned and logged to allow for reproducibility and auditability.

### 10. Integration Points
- **RP-INT-01:** The model's outputs **shall** be consumed by the **Risk Synthesis & Narrative Generation** engine as "Underlying Risk" evidence.
- **RP-INT-02:** The model's outputs **shall** be used by the **Scenario Analysis** engine to calculate future-dated risk metrics.
- **RP-INT-03:** The model **shall** integrate with data flows for both model training (historical data) and real-time inference (live data feeds).

---

## Part 7: Feature Engineering Algorithms

This part provides explicit, programmable algorithms for creating specific model features from raw data sources.

### 11. Scheduled Economic Events Feature Generation

#### 11.1. Overview
The `RiskPredictionModel` **shall not** perform any feature engineering for economic events. It is a direct consumer of the `EconomicCalendarService`.

#### 11.2. Data Consumption Algorithm
- **Purpose:** To retrieve a pre-packaged matrix of time-varying known features for a given forecast date `T` and a forecast horizon of 20 days.
- **Algorithm:**
  1.  Define the `forecast_date` (`T`).
  2.  Make a request to the `EconomicCalendarService` to retrieve the feature matrix for the date range `[T, T + 20 days]`.
      - **Request:** `GET /events/matrix?start_date={T}&end_date={T+20d}`
  3.  Receive the `events_matrix` JSON object from the service. This object is the final `known_features` input required by the model.
  4.  This entire process is repeated for every `(cusip, date)` pair in the training dataset, where `T` is the date for that specific row.