# Market Regime Feature Requirements

**Version: 3.1**

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
- **MR-FR-05:** To support the training of other models, the system **shall** support a **Historical Mode** to perform batch classification on a historical dataset, enriching it with `regime_label` assignments for each observation.
- **MR-FR-06:** The system **shall** classify the market's volatility into a discrete regime ("Low", "Medium", "High") and provide this as part of its standard output.

---

## Part 2: System Interfaces & Data Models

This part defines the data contracts for the system, including all inputs and outputs.

### 3. Input Data: Market Indicator Feed

#### 3.1. Data Sourcing
- **MR-DS-01:** The input features **shall** be sourced as specified in the table below. The `FinancialDataObject` features are provided by the internal **Fin Calculations Service** Engine. All other features are considered external and **shall** be provided by the **Market Data Ingestion Service**.

| Feature Name                     | Source System                    | DataModel Source                                      |
|:---------------------------------|:---------------------------------|:--------------------------------------------------------------|
| `yield_curve_slope_10y2y`        | `FinCalculations`            | `FinancialDataObject`                                      |
| `mmd_ust_ratio_10y`              | `FinCalculations`            | `FinancialDataObject`                                   |
| `vix_index`                      | `Market Data Feed`  | `MarketDataObject` |
| `move_index`                     | `Market Data Feed`  | `MarketDataObject`                  |
| `investment_grade_credit_spread` | `Market Data Feed`  | `MarketDataObject`             |
| `high_yield_credit_spread`       | `Market Data Feed`  | `MarketDataObject`             |
| `tips_breakeven_5y`              | `Market Data Feed`  | `MarketDataObject`                    |
| `swap_spread_10y`                | `Market Data Feed`  | `MarketDataObject`       |
| `muni_fund_flows_net`            | `Market Data Feed`  | `MarketDataObject`|
| `us_cpi_yoy`                     | `Market Data Feed`  | `MarketDataObject`                     |

### 4. Output Data: Market Regime Classification

#### 4.1. Real-Time Output Schema
- **MR-DS-02:** The standard output object for real-time classification **shall** conform to the following JSON schema:
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
  },
  "volatility_classification": {
    "volatility_regime": "string",
    "volatility_index_name": "string",
    "volatility_index_value": "float"
  }
}
```

#### 4.2. Historical Mode Output Schema
- **MR-DS-03:** For historical batch classification, the output **shall** be a structured data file (e.g., CSV) containing all the columns from the input dataset, with the following **four** columns appended:
    - `regime_label`: `string` - The assigned regime label for the historical observation.
    - `confidence_score`: `float` - The model's confidence in the assignment.
    - `volatility_regime`: `string` - The assigned volatility regime for the observation.
    - `volatility_index_value`: `float` - The value of the VIX index for the observation.

#### 4.3. Output Interpretation
- **MR-AI-01:** The `regime_label` **shall** be used as the system's best estimate of the market state.
- **MR-AI-02:** The `confidence_score` **shall** be used as a measure of model certainty. A low score (e.g., below 0.5) indicates an ambiguous market state.
- **MR-AI-03:** The `regime_probabilities` object **shall** be used for nuanced analysis, such as identifying potential transitions between regimes.

---

## Part 3: Core Logic & Methodology

This part describes the business logic and mathematical foundations for the classification.

### 5. Regime Classification Methodology
- **MR-BR-01:** The system **shall** classify the market into one of the six discrete regimes whose quantitative signatures are defined in the table below.
- **MR-BR-02:** The system **shall** use a **Hidden Markov Model (HMM)** to perform the classification on the input features defined in Section 3.1.
- **MR-BR-03:** The `Idiosyncratic_Distress` regime requires a special validation condition based on relative market stress. A state **shall** be mapped to this regime only if it meets the primary municipal stress triggers (`mmd_ust_ratio_10y` Z-score ≥ +1.5 and `muni_fund_flows_net` Z-score ≤ -1.5) **while also** exhibiting significant divergence from the corporate credit market. This divergence **shall** be defined as the Z-score for `mmd_ust_ratio_10y` being at least `1.5` points greater than the Z-score for `high_yield_credit_spread`.

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
|                            | `high_yield_credit_spread`          |             N/A             | Relative divergence condition defined in **MR-BR-03**.                      |

### 6. Volatility Regime Calculation
- **MR-BR-06:** The system **shall** calculate the `volatility_regime` based on the value of the `vix_index` input. The logic **shall** be as follows:
    - IF `vix_index` < 20, `volatility_regime` = "Low"
    - IF 20 <= `vix_index` < 30, `volatility_regime` = "Medium"
    - IF `vix_index` >= 30, `volatility_regime` = "High"
- **MR-BR-07:** The `volatility_classification` output object **shall** be populated as follows:
    - `volatility_regime`: The value calculated in **MR-BR-06**.
    - `volatility_index_name`: The static string "VIX".
    - `volatility_index_value`: The raw `vix_index` value from the input feed.

### 7. Mathematical Formulas & Validation
- **MR-MF-01:** The final output values **shall** be determined from the HMM's raw probability vector $ P $:
  - $ \text{regime\_label} = \arg\max(P) $
  - $ \text{confidence\_score} = \max(P) $
- **MR-VR-01:** The sum of all probabilities in `regime_probabilities` **shall** sum to 1.0 (with a tolerance of $ \pm 0.01 $).
- **MR-VR-02:** The `confidence_score` **shall** be a value between 0 and 1.

---

## Part 4: Technical Implementation Plan

This part provides a prescriptive guide for developers for training and running the HMM.

### 8. Model Training

#### 8.1. Training Dataset Format
- **MR-IMPL-01:** The training data **shall** be a chronologically sorted CSV or DataFrame with a `timestamp` column and columns for each indicator specified in Section 3.1, with no missing values.

#### 8.2. HMM Configuration for Training
- **MR-IMPL-02:** The system **shall** use a Gaussian HMM with the following training configuration:
    - `n_components`: **6**
    - `covariance_type`: **"full"**
    - `n_iter`: **1000**
    - `tol`: **1e-4**
    - `random_state`: A fixed integer (e.g., `42`) for reproducibility.

### 9. State-to-Label Mapping
- **MR-BR-04:** After the HMM is trained, its anonymous hidden states **must** be mapped to the semantic regime labels (defined in Section 5) using the objective, quantitative z-score methodology detailed here. This ensures reproducibility.
- **MR-IMPL-03:** The one-time mapping **shall** be executed via the following algorithm to produce a final `state_mapping` dictionary (e.g., `{0: 'Recession_Easing', 1: 'Bull_Steepener', ...}`).

1.  **Calculate Global Statistics:** From the training data, calculate the global mean and standard deviation for each indicator.
2.  **Extract State Means:** From the trained HMM, extract the mean value of each indicator for each of the 6 hidden states.
3.  **Compute State Z-Scores:** For each state, calculate the z-score of its indicator means relative to the global statistics from Step 1.

$$
Z_{S,I} = \frac{\mu_{S,I} - \mu_{I}}{\sigma_{I}}
$$

Where:
- $ Z_{S,I} $ is the final **Z-score** for Indicator $I$ within a specific hidden State $S$.
- $\mu_{S,I}$ is the **State Mean**: The average value of Indicator $I$ learned by the HMM for that State.
- $\mu_{I}$ is the **Global Mean**: The average value of Indicator $I$ across the entire historical dataset.
- $\sigma_{I}$ is the **Global Standard Deviation**: The historical volatility of Indicator $I$ across the entire dataset.
4.  **Match to Target Profiles:** For each HMM state, find the regime label from the table in Section 5 that has the minimum Euclidean distance between its target z-score profile and the state's calculated z-score profile.

$$
\text{Score}(S, R) = \sqrt{\sum_{I \in R} (Z_{S,I} - T_{R,I})^2}
$$

5.  **Assign via Optimal Matching:** Use the Hungarian method (or a similar optimal assignment algorithm) on the matrix of all state-to-regime distances to find the lowest-cost global assignment. This produces the final, definitive mapping.

### 10. Inference Modes

#### 10.1. Real-Time Inference
- **MR-IMPL-04:** The input for real-time classification **shall** be a single observation with feature columns ordered identically to the training data.
- **MR-IMPL-05:** The real-time inference process **shall** execute as follows:
    1. Pass the input to the trained HMM to get a raw probability array for the 6 hidden states.
    2. Identify the index of the highest probability.
    3. Use the `state_mapping` dictionary (from Section 9) to look up the `regime_label` for that index.
    4. Assign the highest probability value to `confidence_score`.
    5. Map the raw probabilities to their corresponding labels to construct the `regime_probabilities` object for the final JSON output (defined in Section 4.1).
    6. Apply the logic from Section 6 to calculate and populate the `volatility_classification` object.

#### 10.2. Historical (Batch) Inference
- **MR-IMPL-06:** The input for historical batch classification **shall** be a file (e.g., CSV) conforming to the training dataset format defined in **MR-IMPL-01**.
- **MR-IMPL-07:** The processing logic **shall** iterate through each row of the input dataset, apply the core inference logic (steps 1-4 from **MR-IMPL-05**) and the volatility classification logic (from Section 6) to each row, and generate the final output file.
- **MR-IMPL-08:** The output **shall** be a new file containing the data from the input file plus the appended `regime_label`, `confidence_score`, `volatility_regime`, and `volatility_index_value` columns, as specified in **MR-DS-03**.

---

## Part 5: Non-Functional & Operational Requirements

This part covers system-wide constraints related to performance, operation, and maintenance.

### 11. Model Governance & Maintenance
- **MR-NFR-01:** The HMM **must** be periodically retrained and validated.
- **MR-AI-04:** The model **shall** be retrained on a **semi-annual basis** (every 6 months).
- **MR-AI-05:** Following each retraining, the State-to-Label Mapping procedure (detailed in Section 9) **must** be re-executed to ensure the mapping remains valid.
- **MR-BR-05:** The final mapping of HMM states to `Regime Label`s **shall** be validated and signed-off by a designated subject matter expert (SME) after initial training and after every subsequent retraining.