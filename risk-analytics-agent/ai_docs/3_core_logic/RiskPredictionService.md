# Risk Prediction Model: Requirements Specification

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
The model's outputs are designed to provide a holistic risk view, combining forecasts for standard market risk (Spread-to-Benchmark, Bid-Ask Spread) with indicators for idiosyncratic event risk (`probability_negative_news_pct`). These two types of metrics are complementary and intended to be used together to form a comprehensive risk assessment, as they manage different types of risk.

##### Forecasted Spread-to-Benchmark (OAS or MMD Spread)
- **Business Value:** Provides a forward-looking view on relative value against the instrument's appropriate benchmark (MMD for tax-exempt MUNIs, UST for taxable bonds), enabling a shift from reactive to proactive portfolio management.
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
- **`MarketRegimeService`**: The full classification object, as defined in `MarketRegime_part.md`.
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
These features form the historical context for the model's predictions. This section describes the superset of available features; the specific features used by each specialized model are detailed in the training dataset specifications in Section 8.1.
- **Source:** `FinancialDataObject.market_data`
  - `price`: `float`
  - `bid_ask_spread_bps`: `float`
- **Source:** `FinancialDataObject.calculated_risk_metrics`
  - `yield_to_maturity`, `yield_to_worst`, `dv01`, `cs01`, `option_adjusted_spread_bps`, `downside_price_volatility_5d`, `downside_price_volatility_20d`: all `float` to be used as continuous features.
- **Source:** `FinancialDataObject.liquidity`
  - `composite_score`: `float`
  - `market_depth`: `object` (contains `bid_size_par` and `ask_size_par`)
- **Source:** `FinancialDataObject.trade_history_summary`
  - All fields from the `t1d`, `t5d`, and `t20d` summaries (e.g., `total_par_volume`, `trade_count`) to be used as continuous features.
- **Source:** `FinancialDataObject.relative_value`
  - `vs_mmd_bps`, `vs_ust_bps`, `vs_peers_bps` to be used as continuous features. These serve as the primary training targets for spread forecasting.
- **Source:** `FinancialDataObject.market_context` & `state_fiscal_health`
  - All fields (e.g., `yield_curve_slope_10y2y`, `mmd_ust_ratio_10y`, `tax_receipts_yoy_growth`) to be used as continuous features. Null values for non-applicable instruments (e.g., state data for corporates) must be imputed or handled by the model.
- **Source:** `NewsSentiment`
  - `sentiment_score`: `float`
- **Source:** `MarketRegimeService`
  - `global_macro_regime_label`, `contextual_regime_label`: `string` - To be one-hot encoded.
  - `regime_probabilities`: `object` - The vector of probabilities (e.g., `Bull_Steepener`, `Bear_Flattener`) to be used as a set of continuous features.

##### 3.2.4. Prediction Targets
These are the outputs the model will be trained to forecast. Their historical values are also used as time-varying observed inputs where applicable.
- **`spread_to_benchmark_bps`**: This is a conceptual target. The actual training target is derived from the instrument's appropriate benchmark-relative spread, sourced from the `FinancialDataObject.relative_value` object. For 'MUNI' instruments, this is `vs_mmd_bps`. For 'TFI_CORPORATE', this is `vs_ust_bps`.
- **`bid_ask_spread_pct`**: Sourced from `FinancialDataObject.market_data.bid_ask_spread_bps`. A conversion from bps to percent (`value / 10000`) is required to match the specified output unit.
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
      "benchmark_type": "string",
      "spread_to_benchmark_bps": "float",
      "bid_ask_spread_pct": "float",
      "probability_negative_news_pct": "float",
      "downside_price_volatility": {
        "metric_type": "1-day VaR",
        "value": "float"
      }
    },
    {
      "horizon": "5-day",
      "benchmark_type": "string",
      "spread_to_benchmark_bps": "float",
      "bid_ask_spread_pct": "float",
      "probability_negative_news_pct": "float",
      "downside_price_volatility": {
        "metric_type": "5-day VaR",
        "value": "float"
      }
    },
    {
      "horizon": "20-day",
      "benchmark_type": "string",
      "spread_to_benchmark_bps": "float",
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
      "spread_to_benchmark_bps": [
        {"feature": "string", "attribution": "float"}
      ],
      "bid_ask_spread_pct": [
        {"feature": "string", "attribution": "float"}
      ],
      "probability_negative_news_pct": [
        {"feature": "string", "attribution": "float"}
      ],
      "downside_price_volatility": [
        {"feature": "string", "attribution": "float"}
      ]
    }
  }
}
```

#### 4.2. Output Interpretation
- **RP-AI-01:** The `forecasted_values` provide the point estimates for each risk factor at different time horizons. The `benchmark_type` field is set to 'MMD' for the MUNI model and 'UST' for the TFI model, reflecting the benchmark used for the `spread_to_benchmark_bps` forecast.
- **RP-AI-02:** The `forecast_explainability` object **shall** be used to identify the top positive and negative contributors to a given forecast, which is a critical input for the narrative generation engine.

---

## Part 3: Core Logic & Methodology

This part describes the business logic and technical foundations for the prediction model.

### 5. Forecasting Methodology
- **RP-BR-01:** The system **shall** use a router-based architecture that directs prediction requests to one of two specialized **Temporal Fusion Transformer (TFT)** models based on the instrument's type. This approach avoids negative transfer by training models on homogenous datasets.
- **RP-BR-01a (MUNI Model):** A model trained exclusively on `instrument_type` = 'MUNI' data.
    - **Prediction Target:** `relative_value.vs_mmd_bps`.
    - **Key Features:** This model will heavily leverage MUNI-specific features such as `state_fiscal_health`, `muni_fund_flows_net`, `mmd_ust_ratio_10y`, and `tax_status`.
- **RP-BR-01b (TFI Model):** A model trained exclusively on taxable fixed income instruments (e.g., `instrument_type` = 'TFI_CORPORATE').
    - **Prediction Target:** `relative_value.vs_ust_bps`.
    - **Key Features:** This model will leverage features relevant to taxable markets, such as `investment_grade_credit_spread`, `high_yield_credit_spread`, and `sector_credit_spread_curve`.
- **RP-BR-02:** Each specialized model **shall** be trained to simultaneously predict all required output targets for its asset class (multi-headed output).
- **RP-BR-03:** The `Market Regime Service` inputs **shall** be treated as static features for the duration of a single forecast. The `regime_label` will be encoded as a categorical variable, and the `regime_probabilities` will be used as a vector of continuous inputs.

### 6. Validation Rules
- **RP-VR-01:** Model predictions **must** be explainable. The feature attributions for each forecast are a mandatory output.
- **RP-VR-02:** Forecasts **must** be regime-aware. The `regime_label` must be a feature used by the model.
- **RP-VR-03:** Forecasted values **must** be available for all risk factors and horizons specified in the `Core Functional Requirements`.
- **RP-VR-04:** Back-testing accuracy **shall** be measured using standard metrics such as Mean Absolute Error (MAE) and Root Mean Squared Error (RMSE) for continuous variables, and Brier Score for probabilistic forecasts.

### 7. Model Training
- **RP-IMPL-01:** The training data **shall** be segregated into two distinct datasets based on `instrument_type`: one for MUNI instruments and one for TFI instruments.
- **RP-IMPL-02:** Two separate TFT models **shall** be trained on their respective datasets, allowing each to learn the unique patterns of its asset class. Both models shall be trained on historical data covering multiple market cycles and regimes to ensure robustness.

### 8. Inference
- **RP-IMPL-03:** The system **shall** support real-time inference for a single instrument. Upon receiving a request, the service will check the `instrument_type` and route the request to the appropriate specialized model (MUNI or TFI).
- **RP-IMPL-04:** The system **shall** support batch inference. The logic will iterate through the instrument universe, routing each instrument to the correct model for forecasting.

### 8.1. Dataset Specifications
To ensure clarity and precision, the specific training datasets for the two specialized models are defined below. Each dataset is a time-series for its respective instrument universe, where each row represents a `(cusip, date)` pair.

#### 8.1.1. MUNI Forecasting Model Dataset
- **Universe:** Instruments where `instrument_type` = 'MUNI'.
- **Prediction Targets:**
    - `spread_to_benchmark_bps`: Primary target, sourced from `relative_value.vs_mmd_bps`.
    - `bid_ask_spread_pct`
    - `probability_negative_news_pct`
    - `downside_price_volatility`
- **Input Feature Set:**

| Feature Category | Feature Name / Derivation | Source Data Field |
| :--- | :--- | :--- |
| **Static Identifiers** | `sector`, `rating`, `coupon_rate`, `time_to_maturity`, `state`, `tax_status` | `security_details.*` |
| **MUNI Market Context**| `mmd_ust_ratio_10y` | `FinancialDataObject.market_context.mmd_ust_ratio_10y` |
| **MUNI Fund Flows** | `muni_fund_flows_net` | `FinancialDataObject.market_context.muni_fund_flows_net` |
| **Issuer Health** | `tax_receipts_yoy_growth`, `budget_surplus_deficit_pct_gsp`| `FinancialDataObject.state_fiscal_health.*` |
| **Regime Context** | `global_macro_regime_label`, `contextual_regime_label` | `MarketRegimeService` |
| **Historical Price/Yield**| Lagged values of `yield_to_worst`, `price`, `vs_mmd_bps` | `FinancialDataObject.calculated_risk_metrics.*`, `FinancialDataObject.market_data.price`, `FinancialDataObject.relative_value.vs_mmd_bps` |
| **Liquidity**| `composite_score`, `market_depth` (bid/ask sizes) | `FinancialDataObject.liquidity.*` |
| **Trade History**| All fields from `t1d`, `t5d`, and `t20d` summaries. | `FinancialDataObject.trade_history_summary.*` |
| **Ownership**| `is_concentrated_flag`, `top_3_holders_pct` | `FinancialDataObject.ownership.*` |
| **Financing**| `cost_of_carry_bps` | `FinancialDataObject.financing.cost_of_carry_bps` |
| **Idiosyncratic Risk**| `sentiment_score` | `NewsSentiment.sentiment_score` |

#### 8.1.2. TFI Forecasting Model Dataset
- **Universe:** Instruments where `instrument_type` IN ('TFI_CORPORATE', 'TFI_AGENCY').
- **Prediction Targets:**
    - `spread_to_benchmark_bps`: Primary target, sourced from `relative_value.vs_ust_bps`.
    - `bid_ask_spread_pct`
    - `probability_negative_news_pct`
    - `downside_price_volatility`
- **Input Feature Set:**

| Feature Category | Feature Name / Derivation | Source Data Field |
| :--- | :--- | :--- |
| **Static Identifiers** | `sector`, `rating`, `coupon_rate`, `time_to_maturity` | `security_details.*` |
| **Macro Rate Context** | `yield_curve_slope_10y2y` | `FinancialDataObject.market_context.yield_curve_slope_10y2y` |
| **Credit Market Health**| `investment_grade_credit_spread`, `high_yield_credit_spread` | `MarketDataFeed.*` |
| **Economic Events** | `is_fomc_week`, `is_cpi_week`, etc. | `EconomicCalendarService` |
| **Regime Context** | `global_macro_regime_label` | `MarketRegimeService` |
| **Historical Price/Yield**| Lagged values of `option_adjusted_spread_bps`, `price`, `vs_ust_bps` | `FinancialDataObject.calculated_risk_metrics.*`, `FinancialDataObject.market_data.price`, `FinancialDataObject.relative_value.vs_ust_bps`|
| **Liquidity** | `composite_score`, `market_depth` (bid/ask sizes) | `FinancialDataObject.liquidity.*` |
| **Trade History** | All fields from `t1d`, `t5d`, and `t20d` summaries. | `FinancialDataObject.trade_history_summary.*`|
| **Ownership**| `is_concentrated_flag`, `top_3_holders_pct` | `FinancialDataObject.ownership.*` |
| **Financing**| `cost_of_carry_bps` | `FinancialDataObject.financing.cost_of_carry_bps` |
| **Cross-Asset Risk** | `correlation_60d` | `FinancialDataObject.cross_asset_correlation.*` |
| **Idiosyncratic Risk**| `sentiment_score` | `NewsSentiment.sentiment_score` |

---

## Part 5: Non-Functional & Operational Requirements

This part covers system-wide constraints related to performance, operation, and maintenance.

### 9. System Requirements
- **RP-NFR-01:** The model **must** be performant enough to support real-time inference requests with acceptable latency (e.g., < 1 second per instrument).
- **RP-NFR-02:** The model architecture **must** support explainability.
- **RP-NFR-03:** The models **must** be periodically retrained on new data to prevent drift and maintain accuracy. Retraining frequency should be evaluated on a quarterly basis for each model independently.
- **RP-NFR-04:** The model's predictions **shall** be versioned and logged to allow for reproducibility and auditability. The log shall include which specialized model (MUNI or TFI) was used for the forecast.

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