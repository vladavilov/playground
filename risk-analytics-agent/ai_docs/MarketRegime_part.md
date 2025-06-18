# Market Regime Feature Requirements

**Version: 2.0**

---

## Part 1: Business & Functional Requirements

This part outlines the high-level business goals and the core functional capabilities of the Market Regime feature.

### 1. Overview & Purpose
The primary purpose of this feature is to classify the current market state into a set of discrete, predefined regimes. This classification serves as a critical input for regime-aware risk analytics, predictive forecasting, and narrative generation within the Fixed Income Risk Agent.

### 2. Core Functional Requirements
- **MR-FR-01:** The system **shall** calculate and provide the current market regime classification for use in real-time risk analysis.
- **MR-FR-02:** The calculated `regime_label` **shall** be used as a primary static feature input for downstream predictive forecasting models.
- **MR-FR-03:** The calculated `regime_label` **shall** be explicitly included in the final narrative synthesis provided to the end-user.
- **MR-FR-04:** The system **shall** provide the full `Market Regime Classification` object (specified in Section 4.1) for all primary use cases.
- **MR-FR-05:** To support the training of other models, the system **shall** support a **Historical Mode** (defined in **MR-SR-02**) to perform batch classification on a historical dataset, enriching it with `regime_label` assignments for each observation.

---

## Part 2: System Interfaces & Data Models

This part defines the data contracts for the system, including all inputs and outputs.

### 3. Input Data: Market Indicator Feed

#### 3.1. Input Schema
The regime classification model requires the following real-time market indicators.
- `vix_index`: `float` - CBOE Volatility Index.
- `move_index`: `float` - Merrill Lynch Option Volatility Estimate Index.
- `yield_curve_slope_10y2y`: `float` - The spread between the 10-year and 2-year U.S. Treasury yields.
- `investment_grade_credit_spread`: `float` - The spread of the Bloomberg U.S. Corporate Investment Grade Index over the benchmark Treasury curve.
- `high_yield_credit_spread`: `float` - The spread of the Bloomberg U.S. Corporate High Yield Index over the benchmark Treasury curve.
- `tips_breakeven_5y`: `float` - The 5-Year TIPS Breakeven Rate.
- `swap_spread_10y`: `float` - The 10-year U.S. Dollar interest rate swap vs. Treasury spread.
- `mmd_ust_ratio_10y`: `float` - The 10-Year AAA MMD to U.S. Treasury yield ratio.
- `muni_fund_flows_net`: `float` - Net flows into municipal bond funds.
- `us_cpi_yoy`: `float` - Year-over-year change in the Consumer Price Index.

#### 3.2. Data Sourcing
- **MR-DS-01:** The input features **shall** be sourced as specified in the table below. The `FinancialDataObject` features are provided by the internal `FinCalculations` Engine. All other features are considered external and **shall** be provided by the **Market Data Ingestion Service**, whose requirements are detailed in the `MarketDataFeed_part.md` document.

| Feature Name                     | Source System                    | Primary Source / Ticker                                       |
|:---------------------------------|:---------------------------------|:--------------------------------------------------------------|
| `yield_curve_slope_10y2y`        | `FinancialDataObject`            | `FinCalculations` Engine                                      |
| `mmd_ust_ratio_10y`              | `FinancialDataObject`            | `FinCalculations` Engine                                      |
| `vix_index`                      | `Market Data Ingestion Service`  | **CBOE** (e.g., `VIX Index`)                       |
| `move_index`                     | `Market Data Ingestion Service`  | **ICE/BofA** (e.g., `MOVE Index`)                  |
| `investment_grade_credit_spread` | `Market Data Ingestion Service`  | **Bloomberg** (e.g., LQD ETF spread vs. Treasury)             |
| `high_yield_credit_spread`       | `Market Data Ingestion Service`  | **Bloomberg** (e.g., HYG ETF spread vs. Treasury)             |
| `tips_breakeven_5y`              | `Market Data Ingestion Service`  | **U.S. Treasury / FRED** (Ticker: `T5YIE`)                    |
| `swap_spread_10y`                | `Market Data Ingestion Service`  | Market Data Aggregator (e.g., **Bloomberg**)       |
| `muni_fund_flows_net`            | `Market Data Ingestion Service`  | **Refinitiv Lipper** or **ICI**|
| `us_cpi_yoy`                     | `Market Data Ingestion Service`  | **U.S. Bureau of Labor Statistics (BLS)**                     |

### 4. Output Data: Market Regime Classification

#### 4.1. Real-Time Output Schema
- The standard output object for real-time classification **shall** conform to the following JSON schema:
```json
{
  "data_timestamp": "datetime",
  "regime_classification": {
    "regime_label": "string",
    "confidence_score": "float",
    "regime_probabilities": {
      "Bull_Steepener": "float",
      "Bear_Flattener": "float",
      "Bear_Steepener": "float",
      "Bull_Flattener": "float",
      "Recession_Easing": "float",
      "Idiosyncratic_Distress": "float"
    }
  }
}
```

#### 4.2. Historical Mode Output Schema
- **MR-DS-02:** For historical batch classification, the output **shall** be a structured data file (e.g., CSV) containing all the columns from the input dataset, with the following two columns appended:
    - `regime_label`: `string` - The assigned regime label for the historical observation.
    - `confidence_score`: `float` - The model's confidence in the assignment.

#### 4.3. Output Interpretation
- **MR-AI-03:** The `regime_label` **shall** be used as the system's best estimate of the market state.
- **MR-AI-04:** The `confidence_score` **shall** be used as a measure of model certainty. A low score (e.g., below 0.5) indicates an ambiguous market state.
- **MR-AI-05:** The `regime_probabilities` object (available in real-time mode) **shall** be used for nuanced analysis, such as identifying potential transitions between regimes.

---

## Part 3: Core Logic & Methodology

This part describes the business logic and mathematical foundations for the classification.

### 5. Regime Classification Methodology
- **MR-BR-02:** The system **shall** use a **Hidden Markov Model (HMM)** to perform the classification on the input features defined in Section 3.1.
- **MR-BR-01:** The system **shall** classify the market into one of the six discrete regimes whose quantitative signatures are defined in the table below.

| Regime Label               | Indicator                           | Target Z-Score ($T_{R,I}$) | Economic Signature Interpretation                                           |
|:---------------------------|:------------------------------------|:---------------------------:|:----------------------------------------------------------------------------|
| `Bull_Steepener`           | `yield_curve_slope_10y2y`           |            +1.0             | High / Steepening                                                           |
|                            | `investment_grade_credit_spread`    |            -1.0             | Low / Tightening                                                            |
|                            | `high_yield_credit_spread`          |            -1.0             | Low / Tightening                                                            |
|                            | `vix_index`                         |            -1.0             | Low Volatility                                                              |
|                            | `move_index`                        |            -1.0             | Low Volatility                                                              |
| `Bear_Flattener`           | `yield_curve_slope_10y2y`           |            -1.0             | Low / Flattening                                                            |
|                            | `move_index`                        |            +1.0             | High / Rising Volatility                                                    |
| `Bear_Steepener`           | `yield_curve_slope_10y2y`           |            +1.0             | High / Steepening                                                           |
|                            | `tips_breakeven_5y`                 |            +1.0             | High Inflation Expectations                                                 |
|                            | `move_index`                        |            +1.0             | High Volatility                                                             |
|                            | `vix_index`                         |            +1.0             | High Volatility                                                             |
| `Bull_Flattener`           | `yield_curve_slope_10y2y`           |            -1.0             | Low / Flattening                                                            |
|                            | `investment_grade_credit_spread`    |            +1.0             | High / Widening (Flight to Quality)                                         |
|                            | `high_yield_credit_spread`          |            +1.0             | High / Widening (Flight to Quality)                                         |
|                            | `vix_index`                         |            +1.0             | High Volatility                                                             |
| `Recession_Easing`         | `yield_curve_slope_10y2y`           |            -1.5             | Very Low / Inverted                                                         |
|                            | `investment_grade_credit_spread`    |            +1.5             | Very High / Wide                                                            |
|                            | `high_yield_credit_spread`          |            +1.5             | Very High / Wide                                                            |
|                            | `move_index`                        |            +1.5             | Very High Volatility                                                        |
| `Idiosyncratic_Distress`   | `mmd_ust_ratio_10y`                 |            +1.5             | Very High (Muni Market Stress)                                              |
|                            | `muni_fund_flows_net`               |            -1.5             | Very Low (Large Muni Outflows)                                              |
|                            | `high_yield_credit_spread`          |             N/A             | Special condition: See description                                          |
|                            | `investment_grade_credit_spread`    |             N/A             | Special condition: This regime is identified by relative credit spread moves. |

### 6. Mathematical Formulas & Validation
- **MR-MF-01:** Key input features **shall** be calculated as:
  - Yield Curve Slope: $ \text{yield\_curve\_slope\_10y2y} = \text{Yield}_{10Y} - \text{Yield}_{2Y} $
- **MR-MF-03:** The final output values **shall** be determined from the HMM's raw probability vector $ P $:
  - $ \text{regime\_label} = \arg\max(P) $
  - $ \text{confidence\_score} = \max(P) $
- **MR-VR-01:** The sum of all probabilities in `regime_probabilities` **shall** sum to 1.0 (with a tolerance of $ \pm 0.01 $).
- **MR-VR-02:** The `confidence_score` **shall** be a value between 0 and 1.

---

## Part 4: Technical Implementation Plan

This part provides a prescriptive guide for developers for training and running the HMM.

### 7. Model Training

#### 7.1. Training Dataset Format
- **MR-IMPL-01:** The training data **shall** be a chronologically sorted CSV or DataFrame with a `timestamp` column and columns for each indicator specified in Section 3.1, with no missing values.

#### 7.2. HMM Configuration for Training
- **MR-IMPL-02:** The system **shall** use a Gaussian HMM with the following training configuration:
    - `n_components`: **6**
    - `covariance_type`: **"full"**
    - `n_iter`: **1000**
    - `tol`: **1e-4**
    - `random_state`: A fixed integer (e.g., `42`) for reproducibility.

### 8. Post-Training: State-to-Label Mapping
- **MR-BR-06:** After the HMM is trained, its anonymous hidden states **must** be mapped to the semantic regime labels (defined in Section 5) using the objective, quantitative z-score methodology detailed here. This ensures reproducibility.

#### 8.1. Mapping Algorithm
- **MR-IMPL-03:** The one-time mapping **shall** be executed via the following algorithm to produce a final `state_mapping` dictionary (e.g., `{0: 'Recession_Easing', 1: 'Bull_Steepener', ...}`).

1.  **Calculate Global Statistics:** From the training data, calculate the global mean and standard deviation for each indicator.
2.  **Extract State Means:** From the trained HMM, extract the mean value of each indicator for each of the 6 hidden states.
3.  **Compute State Z-Scores:** For each state, calculate the z-score of its indicator means relative to the global statistics from Step 1.

$$
Z_{S,I} = \frac{\mu_{S,I} - \mu_{I}}{\sigma_{I}}
$$

    Where:
    - $Z_{S,I}$ is the final **Z-score** for Indicator $I$ within a specific hidden State $S$.
    - $\mu_{S,I}$ is the **State Mean**: The average value of Indicator $I$ learned by the HMM for that State.
    - $\mu_{I}$ is the **Global Mean**: The average value of Indicator $I$ across the entire historical dataset.
    - $\sigma_{I}$ is the **Global Standard Deviation**: The historical volatility of Indicator $I$ across the entire dataset.
4.  **Match to Target Profiles:** For each HMM state, find the regime label from the table in Section 5 that has the minimum Euclidean distance between its target z-score profile and the state's calculated z-score profile.

$$
\text{Score}(S, R) = \sqrt{\sum_{I \in R} (Z_{S,I} - T_{R,I})^2}
$$

5.  **Assign via Optimal Matching:** Use the Hungarian method (or a similar optimal assignment algorithm) on the matrix of all state-to-regime distances to find the lowest-cost global assignment. This produces the final, definitive mapping.

### 9. Inference Modes

#### 9.1. Real-Time Inference
- **MR-IMPL-04:** The input for real-time classification **shall** be a single observation with feature columns ordered identically to the training data.
- **MR-IMPL-05:** The real-time inference process **shall** execute as follows:
    1. Pass the input to the trained HMM to get a raw probability array for the 6 hidden states.
    2. Identify the index of the highest probability.
    3. Use the `state_mapping` dictionary (from Section 8.1) to look up the `regime_label` for that index.
    4. Assign the highest probability value to `confidence_score`.
    5. Map the raw probabilities to their corresponding labels to construct the `regime_probabilities` object for the final JSON output (defined in Section 4.1).

#### 9.2. Historical (Batch) Inference
- **MR-IMPL-06:** The input for historical batch classification **shall** be a file (e.g., CSV) conforming to the training dataset format defined in **MR-IMPL-01**.
- **MR-IMPL-07:** The processing logic **shall** iterate through each row of the input dataset, apply the core inference logic (steps 1-4 from **MR-IMPL-05**) to each row, and generate the final output file.
- **MR-IMPL-08:** The output **shall** be a new file containing the data from the input file plus the appended `regime_label` and `confidence_score` columns, as specified in **MR-DS-02**.

---

## Part 5: Non-Functional & Operational Requirements

This part covers system-wide constraints related to performance, operation, and maintenance.

### 10. Operating Modes & Performance

#### 10.1. Operating Modes
- **MR-SR-01:** The system **shall** support a **Real-Time Mode**, using the most recent market data available to provide an immediate classification.
- **MR-SR-02:** The system **shall** support a **Historical Mode** for the asynchronous batch processing of historical datasets.

### 11. Model Governance & Maintenance
- **MR-NFR-01:** The Agent **shall** rely on a dedicated **Market Data Ingestion Service** to source all externally-provided indicators. The requirements for this service are defined in the `MarketDataFeed_part.md` document.
- **MR-NFR-03:** The HMM **must** be periodically retrained and validated.
- **MR-AI-06:** The model **shall** be retrained on a **semi-annual basis** (every 6 months).
- **MR-AI-08:** Following each retraining, the State-to-Label Mapping procedure (detailed in Section 8) **must** be re-executed to ensure the mapping remains valid.
- **MR-BR-07:** The final mapping of HMM states to `Regime Label`s **shall** be validated and signed-off by a designated subject matter expert (SME) after initial training and after every subsequent retraining.