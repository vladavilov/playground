# Market Regime Feature Requirements

---

## Part 1: Business & Functional Requirements

This part outlines the high-level business goals and the core functional capabilities of the Market Regime feature.

### 1. Overview & Purpose
The primary purpose of this feature is to classify the market state using a **hierarchical approach**, providing both a **Global Macro Regime** classification and an asset-class-specific **Contextual Regime** where applicable (e.g., for Municipal Bonds). This classification serves as a critical input for regime-aware risk analytics, predictive forecasting, and narrative generation within the Fixed Income Risk Agent.

### 2. Core Functional Requirements
- **MR-FR-01:** The system **shall** calculate and provide the current market regime classification using a hierarchical model for use in real-time risk analysis.
- **MR-FR-02:** The calculated regime labels (`global_macro_regime` and `contextual_regime`) **shall** be used as primary static feature inputs for downstream predictive forecasting models.
- **MR-FR-03:** The calculated regime labels **shall** be explicitly included in the final narrative synthesis provided to the end-user.
- **MR-FR-04:** The system **shall** provide the full `Market Regime Classification` object (specified in Section 4.1), containing both global and contextual regimes, for all primary use cases.
- **MR-FR-05:** To support the training of other models, the system **shall** support a **Historical Mode** to perform batch classification on a historical dataset, enriching it with all applicable regime label assignments for each observation.
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
  "global_macro_regime": {
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
  "contextual_regime": {
    "regime_type": "string", // for now hardcoded to "MUNI_SPECIFIC"
    "regime_label": "string",
    "confidence_score": "float",
    "regime_probabilities": {
        "Muni_Rich_Market": "float",
        "Muni_Cheap_Market": "float",
        "Muni_Neutral": "float"
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
- **MR-DS-03:** For historical batch classification, the output **shall** be a structured data file (e.g., CSV) containing all the columns from the input dataset, with the following **six** columns appended:
    - `global_macro_regime_label`: `string` - The assigned global macro regime label.
    - `global_macro_confidence_score`: `float` - The model's confidence in the global assignment.
    - `global_macro_regime_probabilities`: `object` - A JSON object containing the probability distribution across all six global macro regimes:
        ```json
        {
          "Bull_Steepener": "float",
          "Bear_Flattener": "float", 
          "Bear_Steepener": "float",
          "Bull_Flattener": "float",
          "Recession_Easing": "float",
          "Idiosyncratic_Distress": "float"
        }
        ```
    - `contextual_regime_label`: `string` - (Populated for MUNI instruments) The assigned contextual regime label.
    - `contextual_regime_confidence_score`: `float` - (Populated for MUNI instruments) The model's confidence in the contextual assignment.
    - `contextual_regime_probabilities`: `object` - A JSON object containing the probability distribution across all three contextual regimes:
        ```json
        {
          "Muni_Rich_Market": "float",
          "Muni_Cheap_Market": "float",
          "Muni_Neutral": "float"
        }
        ```
    - `volatility_regime`: `string` - The assigned volatility regime for the observation.
    - `volatility_index_value`: `float` - The value of the VIX index for the observation.

#### 4.3. Output Interpretation
- **MR-AI-01:** The `global_macro_regime.regime_label` **shall** be used as the system's best estimate of the broad market state. The `contextual_regime.regime_label` **shall** be used for asset-class specific context.
- **MR-AI-02:** The `confidence_score` for each regime type **shall** be used as a measure of model certainty. A low score (e.g., below 0.5) indicates an ambiguous market state.
- **MR-AI-03:** The `regime_probabilities` object for each regime type **shall** be used for nuanced analysis, such as identifying potential transitions between regimes.

---

## Part 3: Core Logic & Methodology

This part describes the business logic and mathematical foundations for the classification.

### 5. Global Macro Regime Classification
- **MR-BR-01:** The system **shall** classify the broad market into one of the six discrete global macro regimes whose quantitative signatures are defined in the table below. This model is trained on a wide array of cross-asset indicators.
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

### 6. Contextual Regime Classification (MUNI-Specific)
- **MR-BR-04:** For instruments of type 'MUNI', a second, specialized classification model **shall** be used to determine the state of the municipal bond market. This provides context that can differ from the global macro view.
- **MR-BR-05:** The MUNI-specific model **shall** be a **Hidden Markov Model (HMM)** trained exclusively on MUNI-relevant indicators: `mmd_ust_ratio_10y` and `muni_fund_flows_net`.
- **MR-BR-06:** The MUNI model **shall** classify the municipal market into one of the three discrete regimes defined below.

| Regime Label          | Indicator             | Target Z-Score ($T_{R,I}$) | Economic Signature Interpretation                                           |
|:----------------------|:----------------------|:---------------------------:|:----------------------------------------------------------------------------|
| `Muni_Rich_Market`    | `mmd_ust_ratio_10y`   |            -1.0             | Low MMD/UST ratio; MUNIs are outperforming UST.                             |
|                       | `muni_fund_flows_net` |            +1.0             | Positive fund flows; retail demand is strong.                               |
| `Muni_Cheap_Market`   | `mmd_ust_ratio_10y`   |            +1.0             | High MMD/UST ratio; MUNIs are underperforming UST.                          |
|                       | `muni_fund_flows_net` |            -1.0             | Negative fund flows; retail is selling.                                     |
| `Muni_Neutral`        | `mmd_ust_ratio_10y`   |             0.0             | Ratio is near its historical average.                                       |
|                       | `muni_fund_flows_net` |             0.0             | Fund flows are not a significant driver.                                    |

### 7. Volatility Regime Calculation
- **MR-BR-07:** The system **shall** calculate the `volatility_regime` based on the value of the `vix_index` input. The logic **shall** be as follows:
    - IF `vix_index` < 20, `volatility_regime` = "Low"
    - IF 20 <= `vix_index` < 30, `volatility_regime` = "Medium"
    - IF `vix_index` >= 30, `volatility_regime` = "High"
- **MR-BR-08:** The `volatility_classification` output object **shall** be populated as follows:
    - `volatility_regime`: The value calculated in **MR-BR-07**.
    - `volatility_index_name`: The static string "VIX".
    - `volatility_index_value`: The raw `vix_index` value from the input feed.

### 8. Mathematical Formulas & Validation
- **MR-MF-01:** For each model (Global Macro and Contextual), the final output values **shall** be determined from its raw probability vector $ P $:
  - $ \text{regime\_label} = \arg\max(P) $
  - $ \text{confidence\_score} = \max(P) $
- **MR-VR-01:** The sum of all probabilities in each `regime_probabilities` object **shall** sum to 1.0 (with a tolerance of $ \pm 0.01 $).
- **MR-VR-02:** The `confidence_score` **shall** be a value between 0 and 1.

---

## Part 4: Technical Implementation Plan

This part provides a prescriptive guide for developers for training and running the HMM.

### 9. Model Training

#### 9.1. Training Dataset Format
- **MR-IMPL-01:** The training data for each model **shall** be a chronologically sorted CSV or DataFrame with a `timestamp` column and columns for its specific indicators.

#### 9.2. HMM Configuration for Training
- **MR-IMPL-02:** The system **shall** use a Gaussian HMM for each model (Global Macro and Contextual), with configurations appropriate for their complexity.
    - **Global Macro HMM:** `n_components`: **6**
    - **Contextual MUNI HMM:** `n_components`: **3**
    - **Shared Parameters:** `covariance_type`: **"full"**, `n_iter`: **1000**, `tol`: **1e-4**, `random_state`: **42**.

### 10. State-to-Label Mapping
- **MR-BR-09:** After each HMM is trained, its anonymous hidden states **must** be independently mapped to their respective semantic regime labels (defined in Section 5 and 6) using the objective, quantitative z-score methodology.
- **MR-IMPL-03:** The one-time mapping **shall** be executed for each model to produce two final `state_mapping` dictionaries.

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

5.  **Assign via Optimal Matching:** Use the Hungarian method (or a similar optimal assignment algorithm) on the matrix of all state-to-regime distances to find the lowest-cost global assignment. This produces the final, definitive mapping for one model. The process is repeated for the second model.

### 11. Inference Modes

#### 11.1. Real-Time Inference
- **MR-IMPL-04:** The input for real-time classification **shall** be a single observation containing all features required for both models.
- **MR-IMPL-05:** The real-time inference process **shall** execute as follows:
    1. Pass the relevant input features to the trained Global Macro HMM to get its probability array.
    2. Populate the `global_macro_regime` object in the output JSON.
    3. Check the `instrument_type` of the request. If it is 'MUNI', proceed to step 4. Otherwise, the process is complete.
    4. Pass the relevant MUNI-specific features to the trained Contextual MUNI HMM to get its probability array.
    5. Populate the `contextual_regime` object in the output JSON.
    6. Apply the logic from Section 7 to calculate and populate the `volatility_classification` object.

#### 11.2. Historical (Batch) Inference
- **MR-IMPL-06:** The input for historical batch classification **shall** be a file (e.g., CSV) conforming to the training dataset format and containing all features for all models.
- **MR-IMPL-07:** The processing logic **shall** iterate through each row of the input dataset, applying the full real-time inference logic from **MR-IMPL-05** to each row.
- **MR-IMPL-08:** The output **shall** be a new file containing the data from the input file plus the appended columns as specified in **MR-DS-03**.

---

## Part 5: Non-Functional & Operational Requirements

This part covers system-wide constraints related to performance, operation, and maintenance.

### 12. Model Governance & Maintenance
- **MR-NFR-01:** Both HMMs **must** be periodically retrained and validated independently.
- **MR-AI-04:** The models **shall** be retrained on a **semi-annual basis** (every 6 months).
- **MR-AI-05:** Following each retraining, the State-to-Label Mapping procedure (detailed in Section 10) **must** be re-executed for each model to ensure mappings remain valid.
- **MR-BR-10:** The final mapping of HMM states to `Regime Label`s for both models **shall** be validated and signed-off by a designated subject matter expert (SME) after initial training and after every subsequent retraining.