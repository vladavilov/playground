# Risk Synthesis & Narrative Generation Service

**Version: 4.1**

---

## 1. Overview & Purpose
The Risk Synthesis service aggregates and interprets all computed risk factors for a given fixed-income instrument to generate a single, actionable, and explainable narrative for a trader. This service acts as the final analytical step, translating quantitative data into a concise, qualitative risk summary. Its primary goal is to highlight the most salient and non-obvious risks by interpreting a consolidated data object, flag conflicts between market behavior and derived fundamental risks with enhanced nuance, provide transparency into underlying data, and suggest concrete next steps to support trading decisions.

## 2. Feature Specifications
The service's functionality is broken down into the following distinct features.

### 2.1. Feature: Consolidated Data Ingestion
The process begins with a single, comprehensive input object containing the complete analytical picture from all upstream services.

- **SYS-R-01:** The system **shall** ingest a single data object containing the full outputs from the `FinCalculations`, `MarketRegime`, `NewsSentiment`, and `RiskPredictionModel` services.
- **SYS-R-01A:** The system **shall** also ingest data from specialized, supplemental data services and populate the `supplemental_data` field of the input model. The `supplemental_data` field **shall** be constructed from the outputs of:
    - `RepoDataService`
    - `OwnershipDataService`
    - `CovenantExtractionService`
- **SYS-R-02:** The input object **must** conform to the following `Consolidated Input Data Model`.

#### Consolidated Input Data Model
```json
{
  "financial_data_object": {
    "cusip": "string",
    "security_details": { "instrument_type": "string", "state": "string" },
    "market_data": { "price": "float", "bid_ask_spread_bps": "float" },
    "calculated_risk_metrics": {
        "yield_to_maturity": "float",
        "yield_to_worst": "float",
        "dv01": "float",
        "cs01": "float",
        "option_adjusted_spread_bps": "float",
        "downside_price_volatility_5d": "float",
        "downside_price_volatility_20d": "float"
    },
    "liquidity": { "composite_score": "float", "is_illiquid_flag": "boolean" },
    "trade_history_summary": {
        "t1d": {
            "trade_price_volatility": "float",
            "customer_buy_par_volume": "float",
            "customer_sell_par_volume": "float"
        },
        "t5d": {
            "trade_price_volatility": "float",
            "customer_buy_par_volume": "float",
            "customer_sell_par_volume": "float"
        },
        "t20d": {
            "trade_price_volatility": "float",
            "customer_buy_par_volume": "float",
            "customer_sell_par_volume": "float"
        }
    },
    "relative_value": {
        "vs_peers_bps": "float",
        "vs_mmd_bps": "float",
        "vs_ust_bps": "float",
        "peer_group_cusips": ["string"]
    },
    "state_fiscal_health": {
        "tax_receipts_yoy_growth": "float",
        "budget_surplus_deficit_pct_gsp": "float"
    }
  },
  "market_regime": {
    // The full output from the MarketRegime service
    "regime_classification": {
      "regime_label": "string",
      "regime_probabilities": { "Bear_Steepener": "float", "...": "..." }
    }
  },
  "news_sentiment": {
    "aggregated_sentiment_score": "float",
    "top_articles": ["string"] // list of summaries from the top_articles
  },
  "risk_forecasts": {
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
  },
  "supplemental_data": {
    "cost_of_carry_bps": "float",
    "ownership_concentration": {
      "is_concentrated_flag": "boolean",
      "top_3_holders_pct": "float"
    },
    "cross_asset_correlation": {
      "benchmark_ticker": "string",
      "correlation_60d": "float"
    },
    "key_covenants": ["string"]
  }
}
```

### 2.2. Feature: Risk Factor Derivation & Standardization
The system standardizes the consolidated input into a uniform structure for analysis.

- **SYS-R-03:** The system **shall** transform the `Consolidated Input Data Model` into a standardized `risk_factors` array. Each element in the array represents a distinct risk, normalized to a common 0-1 scale where higher scores indicate higher risk.
- **SYS-R-03A:** The system **shall** utilize a `risk_normalization_scales` object to define the high-risk thresholds for `dv01` and `cs01` based on instrument class and maturity. This object **shall** be externally configurable.

#### Risk Normalization Scales Data Model
This object would typically be loaded from a configuration file at startup.
```json
{
  "risk_normalization_scales": {
    "by_instrument_class": {
      "MUNI_GO": {
        "maturity_buckets_years": [3, 7, 15],
        "dv01_high_risk_thresholds": [0.03, 0.065, 0.13, 0.20],
        "cs01_high_risk_thresholds": [0.025, 0.06, 0.12, 0.18]
      },
      "MUNI_REVENUE": {
        "maturity_buckets_years": [3, 7, 15],
        "dv01_high_risk_thresholds": [0.03, 0.065, 0.14, 0.22],
        "cs01_high_risk_thresholds": [0.035, 0.07, 0.14, 0.21]
      },
      "MUNI_PREREFUNDED": {
        "maturity_buckets_years": [2, 5, 10],
        "dv01_high_risk_thresholds": [0.02, 0.045, 0.09, 0.15],
        "cs01_high_risk_thresholds": [0.005, 0.01, 0.015, 0.02]
      },
      "CORP_IG": {
        "maturity_buckets_years": [3, 5, 10],
        "dv01_high_risk_thresholds": [0.028, 0.048, 0.09, 0.15],
        "cs01_high_risk_thresholds": [0.025, 0.045, 0.08, 0.13]
      },
      "CORP_HY": {
        "maturity_buckets_years": [3, 5, 7],
        "dv01_high_risk_thresholds": [0.03, 0.05, 0.07, 0.10],
        "cs01_high_risk_thresholds": [0.10, 0.18, 0.25, 0.35]
      },
      "US_TREASURY": {
        "maturity_buckets_years": [2, 5, 10, 20],
        "dv01_high_risk_thresholds": [0.02, 0.045, 0.09, 0.16, 0.25],
        "cs01_high_risk_thresholds": [0, 0, 0, 0, 0]
      },
      "DEFAULT": {
        "maturity_buckets_years": [10],
        "dv01_high_risk_thresholds": [0.10, 0.25],
        "cs01_high_risk_thresholds": [0.08, 0.20]
      }
    }
  }
}
```

#### Rationale for Thresholds
The thresholds provided in the `risk_normalization_scales` are based on standard market conventions and principles of fixed-income duration.

*   **General Principles:**
    *   **Increasing with Maturity:** Both `dv01` (interest rate sensitivity) and `cs01` (credit spread sensitivity) increase with a bond's maturity. A longer time to maturity means more coupon payments are exposed to rate and spread changes, increasing overall price volatility. Therefore, thresholds are set progressively higher for longer-maturity buckets across all instrument types.
    *   **Credit Quality:** `cs01` is a direct measure of credit risk sensitivity. Instruments with higher perceived credit risk (e.g., High Yield Corporates, Revenue Bonds) have a larger portion of their yield attributed to credit spread and are thus more sensitive to changes in that spread. They are assigned higher `cs01` thresholds than safer instruments (e.g., GO Munis, IG Corporates).

*   **Specific Instrument Rationale:**
    *   **MUNI_GO vs. MUNI_REVENUE:** Revenue bonds are backed by specific, sometimes volatile, revenue streams (e.g., tolls, usage fees) and thus carry more project-specific risk than General Obligation bonds, which are backed by the general taxing power of the issuer. This justifies the slightly higher `cs01` thresholds for Revenue bonds. Their `dv01` profiles are very similar.
    *   **MUNI_PREREFUNDED:** These bonds are "defeased" and secured by an escrow account containing US Treasury securities sufficient to pay all future obligations. Their credit risk is effectively replaced by the credit risk of the US government. Consequently, their `cs01` thresholds are near zero, reflecting minimal sensitivity to credit spread changes.
    *   **CORP_IG vs. CORP_HY:** This represents the primary divide in credit quality. High-yield bonds have a much higher probability of default and their prices are far more sensitive to changes in credit spreads and market sentiment. This is reflected in the significantly higher `cs01` thresholds for `CORP_HY` compared to the more stable `CORP_IG` class. HY bonds also tend to be of shorter duration, as investors are less willing to take on long-term credit risk, which is reflected in the chosen maturity buckets.
    *   **US_TREASURY:** As the benchmark risk-free asset, US Treasuries have no credit spread component in their yield. Therefore, their `cs01` is always zero. They are, however, a pure reflection of interest rate risk, and their `dv01` thresholds serve as a baseline for all other asset classes.

*Logic for applying scales:*
1.  Identify the instrument's class (e.g., `MUNI_GO` from `security_details.instrument_type`) and maturity (from `security_details`). If the class is not found, use `DEFAULT`.
2.  Find the correct bucket from `maturity_buckets_years`. For example, with buckets `[5, 15]` and a maturity of 8 years, the instrument falls into the second bucket (5-15 years).
3.  Select the corresponding threshold from `dv01_high_risk_thresholds` or `cs01_high_risk_thresholds`.

- **SYS-R-04:** The following logic **shall** be used to derive and normalize each risk factor into a standardized 0-1 score:
    - **Valuation Risk:** This risk quantifies if an instrument is overvalued ("trading rich") relative to its peers and the appropriate market benchmark. A high score indicates significant overvaluation, suggesting a higher risk of price correction. The score is a weighted average of two components: peer relative value and benchmark relative value.
        - **Component 1: Peer Valuation (`vs_peers_bps`)**
            - *Logic:* Compares the instrument's yield to a curated group of similar securities. A positive value indicates the instrument is trading at a lower yield (richer) than its peers.
            - *Threshold for High Risk:* `vs_peers_bps > 15 bps`. A value above 15 bps is considered a strong indicator of overvaluation.
        - **Component 2: Benchmark Valuation (`vs_mmd_bps` or `vs_ust_bps`)**
            - *Logic:* The benchmark is selected based on the instrument type: Municipal Market Data (MMD) for `MUNI` instruments and US Treasuries (UST) for `TFI` instruments. A positive value means the instrument is trading rich to its benchmark curve.
            - *Threshold for High Risk:* A spread vs. the benchmark of `> 25 bps` is considered a strong indicator.
            - **Score Calculation:** The individual components are normalized and combined into a final score.
            ```math
            \begin{aligned}
            \text{BenchmarkSpread} &= \begin{cases} \text{vs\_mmd\_bps} & \text{if instrument\_type is MUNI} \\ \text{vs\_ust\_bps} & \text{if instrument\_type is TFI} \end{cases} \\
            \text{PeerScore} &= \text{Normalize}(\text{vs\_peers\_bps}, 15) \\
            \text{BenchmarkScore} &= \text{Normalize}(\text{BenchmarkSpread}, 25) \\
            \text{ValuationRiskScore} &= (0.6 \cdot \text{PeerScore}) + (0.4 \cdot \text{BenchmarkScore})
            \end{aligned}
            ```
            - *Where the `Normalize(value, threshold)` function scales the input `value` to a 0-1 score, such as `min(max(value, 0) / threshold, 1)`. Negative values (trading cheap) result in a score of 0.*
    - **News Risk:** This risk measures the impact of negative news sentiment surrounding an instrument. The score is derived from `news_sentiment.aggregated_sentiment_score`, which is a sophisticated metric weighted by event type, source credibility, and timeliness. A highly negative score indicates significant risk.
        - *Threshold for High Risk:* An `aggregated_sentiment_score < -0.5` is considered a strong indicator of high risk.
        - *Score Calculation:* The sentiment score is normalized to the standard 0-1 risk scale. Since lower (more negative) sentiment scores represent higher risk, the sign is inverted before normalization.
        ```math
        \text{NewsRiskScore} = \text{Normalize}(-\text{aggregated\_sentiment\_score}, 0.5)
        ```
    - **Illiquidity Risk:** This risk measures how difficult it might be to trade the instrument at a fair price. It is assessed by categorizing the `liquidity.composite_score` (a z-score of liquidity vs. peers) into distinct levels. The `is_illiquid_flag` (triggered when score is `< -1.5`) is a key input, corresponding to the 'Medium' and 'High' risk tiers.
        - **Risk Level Definitions:** The `liquidity.composite_score` is mapped to a qualitative risk level as follows:
            - **High:** `composite_score < -2.0`. Represents extreme illiquidity (more than 2 standard deviations below the peer average).
            - **Medium:** `-2.0 <= composite_score < -1.0`. Represents significant illiquidity. The `is_illiquid_flag` is typically `true` in this range.
            - **Low:** `composite_score >= -1.0`. Represents normal liquidity relative to peers.
        - *Note: This factor provides a qualitative level (High/Medium/Low) instead of a normalized 0-1 score to directly support narrative generation.*
    - **Volatility Trend Risk:** This risk measures the acceleration of recent price volatility by synthesizing two different standpoints: downside-specific volatility (a forward-looking risk measure) and realized trade price volatility (a backward-looking market measure). A high score indicates that short-term volatility is increasing rapidly, confirmed by multiple sources.
        - *Logic:* The final score is a weighted average of two independently calculated acceleration scores. Each score is derived by comparing the 5-day volatility against its 20-day baseline. A ratio greater than 1 signifies accelerating risk.
        - *Threshold for High Risk:* For each component, a 5-day volatility that is **100% higher** than its 20-day baseline is considered high risk, mapping to a component score of 1.0.
        - *Score Calculation:*
        ```math
        \begin{aligned}
        \text{vol\_down\_5d} &= \text{calculated\_risk\_metrics.downside\_price\_volatility\_5d} \\
        \text{vol\_down\_20d} &= \text{calculated\_risk\_metrics.downside\_price\_volatility\_20d} \\
        \text{RawAccel\_Downside} &= \begin{cases} \left( \frac{\text{vol\_down\_5d}}{\text{vol\_down\_20d}} - 1 \right) & \text{if vol\_down\_20d > 0} \\ 1.0 & \text{if vol\_down\_20d = 0 and vol\_down\_5d > 0} \\ 0 & \text{if vol\_down\_20d = 0 and vol\_down\_5d = 0} \end{cases} \\
        \text{DownsideScore} &= \text{Normalize}(\text{RawAccel\_Downside}, 1.0) \\
        \\
        \text{vol\_trade\_5d} &= \text{trade\_history\_summary.t5d.trade\_price\_volatility} \\
        \text{vol\_trade\_20d} &= \text{trade\_history\_summary.t20d.trade\_price\_volatility} \\
        \text{RawAccel\_Trade} &= \begin{cases} \left( \frac{\text{vol\_trade\_5d}}{\text{vol\_trade\_20d}} - 1 \right) & \text{if vol\_trade\_20d > 0} \\ 1.0 & \text{if vol\_trade\_20d = 0 and vol\_trade\_5d > 0} \\ 0 & \text{if vol\_trade\_20d = 0 and vol\_trade\_5d = 0} \end{cases} \\
        \text{TradeScore} &= \text{Normalize}(\text{RawAccel\_Trade}, 1.0) \\
        \\
        \text{VolatilityTrendRiskScore} &= (0.5 \cdot \text{DownsideScore}) + (0.5 \cdot \text{TradeScore})
        \end{aligned}
        ```
        - *Where the `Normalize(value, threshold)` function is `min(max(value, 0) / threshold, 1)`. The threshold of `1.0` ensures a 100% increase in volatility maps to the maximum component score.*
        - *A sudden appearance of volatility (where the 20-day baseline is zero) signifies a regime change and is treated as a high-risk event by assigning a component score of 1.0.*
    - **Order Flow Pressure:** This factor measures the direction and magnitude of sustained trading pressure by analyzing net customer order flow. The score ranges from -1 (intense, sustained buying pressure) to +1 (intense, sustained selling pressure), with values near 0 indicating balanced flow. A positive score indicates risk from selling pressure, while a negative score can highlight opportunities or squeeze dynamics.
        - **Step 1: Calculate Net Flows & Average Daily Volume (ADV)**
            - Net Flow (`NF`) is the difference between customer sell and buy volumes for a given period. The `ADV_20d` provides the baseline for typical volume.
            ```math
            \begin{aligned}
            NF_{period} &= \text{customer\_sell\_par\_volume}_{period} - \text{customer\_buy\_par\_volume}_{period} \\
            \text{ADV}_{20d} &= \frac{\text{customer\_buy\_par\_volume}_{20d} + \text{customer\_sell\_par\_volume}_{20d}}{20}
            \end{aligned}
            ```
        - **Step 2: Normalize Net Flows**
            - Each period's net flow is normalized against the total expected volume for that period, producing a score from -1 to +1.
            ```math
            \begin{aligned}
            \text{NormFlow}_{1d} &= \text{NormalizeSigned}(NF_{1d}, \text{ADV}_{20d}) \\
            \text{NormFlow}_{5d} &= \text{NormalizeSigned}(NF_{5d}, 5 \cdot \text{ADV}_{20d}) \\
            \text{NormFlow}_{20d} &= \text{NormalizeSigned}(NF_{20d}, 20 \cdot \text{ADV}_{20d})
            \end{aligned}
            ```
            - *Where `NormalizeSigned(value, threshold)` is `max(min(value / threshold, 1), -1)`.*
        - **Step 3: Calculate Final Weighted Score**
            - The final score weights longer-term trends more heavily to capture persistent pressure.
            ```math
            \text{OrderFlowPressureScore} = (0.5 \cdot \text{NormFlow}_{20d}) + (0.3 \cdot \text{NormFlow}_{5d}) + (0.2 \cdot \text{NormFlow}_{1d})
            ```
    - **State Credit Risk:** (For MUNIs only) This risk is derived from the `state_fiscal_health` indicators from the `financial_data_object`. A high score indicates deteriorating fiscal health. The calculation uses a weighted average of two sub-scores: one for revenue growth and one for the state's budget balance.
        - **Component 1: Tax Receipts Growth Risk (`GrowthScore`)**
            - *Logic:* This score assesses the risk from changes in year-over-year state tax receipts. Negative growth is a significant red flag.
            - *Score Calculation:* The `state_tax_receipts_yoy_growth` is mapped to a 0-10 risk score.
            ```math
            \text{GrowthScore} = \begin{cases} 1 & \text{if growth > 2\%} \\ 3 & \text{if 0\% < growth} \le \text{2\%} \\ 7 & \text{if -2\% < growth} \le \text{0\%} \\ 10 & \text{if growth} \le \text{-2\%} \end{cases}
            ```
        - **Component 2: Budget Balance Risk (`BudgetScore`)**
            - *Logic:* This score assesses the risk from the state's budget surplus or deficit, normalized as a percentage of its Gross State Product (GSP). A deficit is a direct indicator of fiscal imbalance.
            - *Score Calculation:* The `state_budget_surplus_deficit_as_pct_of_gsp` is mapped to a 0-10 risk score.
            ```math
            \text{BudgetScore} = \begin{cases} 0 & \text{if surplus/deficit > 0.5\%} \\ 2 & \text{if 0\% < surplus/deficit} \le \text{0.5\%} \\ 6 & \text{if -1.5\% < surplus/deficit} \le \text{0\%} \\ 9 & \text{if surplus/deficit} \le \text{-1.5\%} \end{cases}
            ```
        - **Final Score Calculation & Normalization:** The final score is a weighted average of the two components, giving more weight to the budget balance as it represents the ultimate fiscal outcome. The resulting score is normalized to a standard 0-1 scale (where 1 is highest risk) for consistency with other risk factors.
        ```math
        \text{StateCreditRiskScore} = \frac{(0.4 \cdot \text{GrowthScore}) + (0.6 \cdot \text{BudgetScore})}{10}
        ```
        - **Risk Thresholds:** To make the score actionable, the following thresholds are defined for triggering risk alerts:
            - **High Risk:** `StateCreditRiskScore >= 0.6`
                - *Rationale:* This level indicates a significant fiscal distress, such as a combination of negative revenue growth and a budget deficit. This should trigger a primary risk alert.
            - **Medium Risk:** `0.35 <= StateCreditRiskScore < 0.6`
                - *Rationale:* This level indicates that at least one key fiscal indicator is showing weakness. It serves as a warning sign that requires monitoring.
            - **Low Risk:** `StateCreditRiskScore < 0.35`
                - *Rationale:* The state's fiscal indicators are stable or positive. No risk is triggered.
    - **Interest Rate Risk:** This risk measures the instrument's sensitivity to a 1 basis point change in interest rates. A high absolute `dv01` relative to the instrument's class and maturity indicates higher risk. The score is normalized using a configurable threshold.
        - *Logic:* The system retrieves the appropriate `dv01_high_risk_threshold` from the `risk_normalization_scales` configuration based on the instrument's type and maturity. This threshold represents the `dv01` value that corresponds to a maximum risk score of 1.0.
        - *Score Calculation:*
        ```math
        \begin{aligned}
        \text{dv01\_threshold} &= \text{LookupThreshold}(\text{instrument\_class}, \text{maturity}, \text{"dv01"}) \\
        \text{InterestRateRiskScore} &= \text{Normalize}(\text{abs}(\text{calculated\_risk\_metrics.dv01}), \text{dv01\_threshold})
        \end{aligned}
        ```
        - *Where `Normalize(value, threshold)` is `min(value / threshold, 1)`.*
    - **Spread Duration Risk:** This risk measures the instrument's sensitivity to a 1 basis point change in its credit spread. A high absolute `cs01` relative to its peers indicates higher risk. The score is normalized using a configurable threshold.
        - *Logic:* The system retrieves the appropriate `cs01_high_risk_threshold` from the `risk_normalization_scales` configuration based on the instrument's type and maturity. This threshold represents the `cs01` value that corresponds to a maximum risk score of 1.0.
        - *Score Calculation:*
        ```math
        \begin{aligned}
        \text{cs01\_threshold} &= \text{LookupThreshold}(\text{instrument\_class}, \text{maturity}, \text{"cs01"}) \\
        \text{SpreadDurationRiskScore} &= \text{Normalize}(\text{abs}(\text{calculated\_risk\_metrics.cs01}), \text{cs01\_threshold})
        \end{aligned}
        ```
        - *Where `Normalize(value, threshold)` is `min(value / threshold, 1)`.*
    - **Predicted Event Risk:** This risk measures the likelihood of a negative news event over multiple time horizons. It is derived by calculating a weighted average of the forecasted probabilities for the 1-day, 5-day, and 20-day periods, giving more weight to the near-term forecast.
        - *Logic:* The score is a weighted average of the `probability_negative_news_pct` from each forecast horizon. A composite score greater than 0.65 can be considered to indicate high risk.
        - *Score Calculation:*
        ```math
        \begin{aligned}
        \text{Prob}_{1d} &= \text{risk\_forecasts.forecasted\_values[0].probability\_negative\_news\_pct} / 100 \\
        \text{Prob}_{5d} &= \text{risk\_forecasts.forecasted\_values[1].probability\_negative\_news\_pct} / 100 \\
        \text{Prob}_{20d} &= \text{risk\_forecasts.forecasted\_values[2].probability\_negative\_news\_pct} / 100 \\
        \\
        \text{PredictedEventRiskScore} &= (0.5 \cdot \text{Prob}_{1d}) + (0.3 \cdot \text{Prob}_{5d}) + (0.2 \cdot \text{Prob}_{20d})
        \end{aligned}
        ```
    - **Predicted Spread Widening Risk:** This risk measures the potential for the instrument's credit spread to increase across multiple time horizons. A high score indicates a significant risk of underperformance due to spread widening.
        - **SYS-R-04A:** The system **shall** utilize a `predicted_spread_widening_thresholds` object to define the high-risk thresholds for forecasted spread widening based on instrument class. This object **shall** be externally configurable.

        #### Predicted Spread Widening Thresholds Data Model
        This object defines the basis point (bps) change in credit spread over a given forecast horizon that is considered high risk (mapping to a normalized score of 1.0).
        ```json
        {
          "predicted_spread_widening_thresholds": {
            "by_instrument_class": {
              "MUNI_GO":              { "threshold_1d_bps": 3,  "threshold_5d_bps": 6,  "threshold_20d_bps": 12 },
              "MUNI_REVENUE":         { "threshold_1d_bps": 4,  "threshold_5d_bps": 8,  "threshold_20d_bps": 15 },
              "MUNI_PREREFUNDED":     { "threshold_1d_bps": 1,  "threshold_5d_bps": 2,  "threshold_20d_bps": 4  },
              "CORP_IG":              { "threshold_1d_bps": 5,  "threshold_5d_bps": 10, "threshold_20d_bps": 20 },
              "CORP_HY":              { "threshold_1d_bps": 15, "threshold_5d_bps": 30, "threshold_20d_bps": 50 },
              "US_TREASURY":          { "threshold_1d_bps": 0,  "threshold_5d_bps": 0,  "threshold_20d_bps": 0  },
              "DEFAULT":              { "threshold_1d_bps": 8,  "threshold_5d_bps": 15, "threshold_20d_bps": 25 }
            }
          }
        }
        ```

        *Logic for applying thresholds:*
        1.  Identify the instrument's class (e.g., `CORP_HY` from `security_details.instrument_type`).
        2.  If the class is not found in the configuration, use the `DEFAULT` thresholds.
        3.  Retrieve the `threshold_1d_bps`, `threshold_5d_bps`, and `threshold_20d_bps` for the given class. These values will be used in the `Predicted Spread Widening Risk` calculation.

        #### Rationale for Thresholds
        // ... existing code ...
        - *Logic:* The score is a weighted average of normalized spread changes for each forecast horizon (1d, 5d, 20d). Because a small widening in the short term can be more significant than a larger widening over the long term, each horizon is normalized against a different, configurable threshold that is looked up based on the instrument's class.
        - *Score Calculation:*
        ```math
        \begin{aligned}
        \text{Thresholds} &= \text{LookupThresholds}(\text{instrument\_class}) \\
        \\
        \text{Spread}_{1d} &= \text{forecasts.values[0].credit\_spread\_oas\_bps} \\
        \text{Spread}_{5d} &= \text{forecasts.values[1].credit\_spread\_oas\_bps} \\
        \text{Spread}_{20d} &= \text{forecasts.values[2].credit\_spread\_oas\_bps} \\
        \\
        \text{Score}_{1d} &= \text{Normalize}(\text{Spread}_{1d}, \text{Thresholds.threshold\_1d\_bps}) \\
        \text{Score}_{5d} &= \text{Normalize}(\text{Spread}_{5d}, \text{Thresholds.threshold\_5d\_bps}) \\
        \text{Score}_{20d} &= \text{Normalize}(\text{Spread}_{20d}, \text{Thresholds.threshold\_20d\_bps}) \\
        \\
        \text{PredictedSpreadWideningRiskScore} &= (0.5 \cdot \text{Score}_{1d}) + (0.3 \cdot \text{Score}_{5d}) + (0.2 \cdot \text{Score}_{20d})
        \end{aligned}
        ```
        - *Where `Normalize(value, threshold)` is `min(max(value, 0) / threshold, 1)`.*
    - **Predicted Volatility Risk:** This risk measures the forecasted downside price volatility, adjusted for time. VaR estimates from different horizons are not directly comparable, so they are first scaled to a "daily-equivalent" volatility before being scored and combined.
        - *Logic:* The VaR value for each horizon is divided by the square root of the number of days in its period. These daily-equivalent volatilities are then normalized against a single threshold and combined in a weighted average.
        - *Score Calculation:*
        ```math
        \begin{aligned}
        \text{DailyEquivVol}_{1d} &= \frac{\text{forecasts.values[0].downside\_price\_volatility.value}}{\sqrt{1}} \\
        \text{DailyEquivVol}_{5d} &= \frac{\text{forecasts.values[1].downside\_price\_volatility.value}}{\sqrt{5}} \\
        \text{DailyEquivVol}_{20d} &= \frac{\text{forecasts.values[2].downside\_price\_volatility.value}}{\sqrt{20}} \\
        \\
        \text{Score}_{1d} &= \text{Normalize}(\text{DailyEquivVol}_{1d}, \text{Threshold: 0.5\%}) \\
        \text{Score}_{5d} &= \text{Normalize}(\text{DailyEquivVol}_{5d}, \text{Threshold: 0.5\%}) \\
        \text{Score}_{20d} &= \text{Normalize}(\text{DailyEquivVol}_{20d}, \text{Threshold: 0.5\%}) \\
        \\
        \text{PredictedVolatilityRiskScore} &= (0.5 \cdot \text{Score}_{1d}) + (0.3 \cdot \text{Score}_{5d}) + (0.2 \cdot \text{Score}_{20d})
        \end{aligned}
        ```

### 2.2.1. Feature: Supplemental Data Surfacing
- **SYS-R-05:** The system **shall** surface key supplemental data points in the final output. The following mappings from the `supplemental_data` input object to the `quantitative_risk_factors` output object **shall** be applied, with sample formatting:
    - `cost_of_carry_bps` -> `cost_of_carry`: e.g., "15 bps".
    - `ownership_concentration` -> `ownership`: e.g., "Concentrated (Top 3 holders own 75%)".
    - `cross_asset_correlation` -> `correlation`: e.g., "60d Corr. to HYG: 0.6".
    - `key_covenants` -> `key_covenants`: Formatted as a comma-separated string.

### 2.3. Feature: Evidence Extraction & Conflict Detection
The system extracts key data points to identify and score conflicts between market signals and the derived fundamental risk factors.

- **SYS-R-06:** The system **shall** calculate a `conflict_score` for the instrument based on the generalized `Conflict Scoring Algorithm`, using the derived `risk_factors` from feature 2.2.
- **SYS-R-07:** Any detected conflicts, along with their scores and primary contributing risk factor, **must** be explicitly identified and reported in the `conflicts` field of the final output. (See **Feature 2.6**)
- **SYS-R-08:** To provide qualitative evidence for the `News Risk` score, the system **shall** list of summaries from `news_sentiment.top_articles`. This excerpt **shall** be made available for inclusion in the `key_evidence` section of the output.


### 2.4. Feature: Cross-Factor Confirmation & Contradiction Analysis
This feature provides deeper, non-obvious insights by analyzing the relationships *between* different risk factors, moving beyond a single conflict score.

- **SYS-R-09:** The system **shall** implement a rule-based engine to detect pre-defined patterns of confirmation and contradiction among the derived `risk_factors`.
- **SYS-R-10:** The engine **shall** evaluate the following patterns:
    - **Confirmation (Fundamental + Forecast):** Detects when a current fundamental weakness is predicted to worsen.
        - *Logic:* `Valuation Risk Score > 0.7` AND `Predicted Spread Widening Risk Score > 0.7`.
        - *Output Insight:* "Instrument is trading rich and models forecast further spread widening, confirming valuation concerns."
    - **Confirmation (Volatility Cluster):** Detects when recent and predicted volatility are both high.
        - *Logic:* `Volatility Trend Risk Score > 0.7` AND `Predicted Volatility Risk Score > 0.7`.
        - *Output Insight:* "Accelerating realized volatility is confirmed by forecasts, suggesting a sustained high-risk volatility regime."
    - **Contradiction (Sentiment vs. Flow):** Detects when positive news is met with heavy selling.
        - *Logic:* `News Risk Score < 0.3` (i.e., positive sentiment) AND `Order Flow Imbalance Risk Score > 0.7`.
        - *Output Insight:* "Despite positive news sentiment, order flows show significant net selling, indicating market participants may be disbelieving the news or using it as a liquidity event."
    - **Contradiction (Credit vs. Forecast):** Detects when strong state-level fundamentals are contradicted by negative model forecasts.
        - *Logic:* `State Credit Risk Score < 0.3` AND `Predicted Event Risk Score > 0.7`.
        - *Output Insight:* "While state fiscal health appears strong, predictive models are flagging a high probability of a negative event, suggesting a potential disconnect or forward-looking risk not yet in fundamental data."
    - **Confirmation (Falling Knife):** Detects when accelerating price instability is met with heavy selling pressure.
        - *Logic:* `Volatility Trend Risk Score > 0.7` AND `Order Flow Imbalance Risk Score > 0.7`.
        - *Output Insight:* "Price instability is accelerating amidst heavy, persistent selling pressure, suggesting sellers are becoming more aggressive and are willing to accept lower prices."
    - **Confirmation (Smart Money):** Detects when institutional selling pressure is validated by predictive models.
        - *Logic:* `Order Flow Imbalance Risk Score > 0.7` AND `Predicted Spread Widening Risk Score > 0.7`.
        - *Output Insight:* "Persistent selling by market participants is confirmed by models forecasting significant spread widening, suggesting the negative sentiment is well-founded."
    - **Contradiction (Deceptive Calm):** Detects when a quiet market masks a high probability of future volatility.
        - *Logic:* `Volatility Trend Risk Score < 0.3` AND `Predicted Volatility Risk Score > 0.7`.
        - *Output Insight:* "The market is currently quiet, but models are forecasting a sharp increase in volatility. This indicates a potential pending event or a build-up of risk not yet reflected in price."
    - **Contradiction (Illiquid & Overvalued):** Detects when a high valuation may be the result of unreliable pricing in an illiquid market.
        - *Logic:* `Illiquidity Risk is 'High'` AND `Valuation Risk Score > 0.7`.
        - *Output Insight:* "The bond is marked as both highly illiquid and significantly overvalued. Its 'richness' may be an artifact of stale, unreliable pricing rather than true market value."
    - **Contradiction (Rich & Squeezing Higher):** Detects when an already overvalued instrument is subject to extreme buying pressure.
        - *Logic:* `Valuation Risk Score > 0.7` AND `OrderFlowPressureScore < -0.7`.
        - *Output Insight:* "Instrument is already trading rich to its peers, but persistent, strong buying pressure continues. This could indicate a short squeeze, asset scarcity, or a large, non-economic buyer forcing the price higher."
- **SYS-R-11:** Detected patterns **shall** be stored in a `pattern_analysis` array within the final output object, including the pattern type, a description of the finding, and the contributing factors.

### 2.6. Feature: Final Output Assembly
The system populates the final user-facing output by combining raw quantitative data with the LLM-generated narrative.

- **SYS-R-15:** The final output **must** conform to the structured `Output Data Model`.
- **SYS-R-16:** The Agent **must** provide explainable, structured, and actionable output to ensure trader trust and usability, as rendered in the `Output Display Template`.

#### Output Data Model
The feature's output **shall** conform to the following structured JSON schema.
```json
{
  "headline": "string",
  "synthesized_narrative": "string",
  "key_evidence": {
    "market_fact": { "value": "string", "timestamp": "ISO8601" },
    "underlying_risk": { "value": "string", "timestamp": "ISO8601" },
    "valuation": {
      "value": "string",
      "peer_group_id": "string",
      "peer_group_definition": "string"
    },
    "liquidity": "string"
  },
  "quantitative_risk_factors": {
    "forecast": {
      "display_text": "string",
      "drivers": [
        { "feature_name": "string", "contribution": "float", "description": "string" }
      ]
    },
    "yield_to_worst_ytw": "string",
    "yield_to_maturity_ytm": "string",
    "dv01": "string",
    "cs01": "string",
    "option_adjusted_spread_oas": "string",
    "cost_of_carry": "string",
    "ownership": "string",
    "correlation": "string",
    "key_covenants": "string"
  },
  "pattern_analysis": [
    {
      "pattern_type": "string",
      "insight_summary": "string",
      "contributing_factors": ["string"]
    }
  ],
  "conflicts": [
    {
      "conflict_score": "float",
      "contributing_factors": ["string"]
    }
  ],
  "suggested_actions": [
    {
      "action_type": "string", // e.g., Investigate, Review
      "description": "string",
      "related_cusips": ["string"]
    }
  ]
}
```

#### Output Display Template
This template defines how the final structured data **shall** be rendered for the end-user.
```
------------------------------------------------------------------
HEADLINE: [e.g., CONTRADICTION (Score: 0.85)] IN [e.g., RISK-OFF] MARKET

[Synthesized Narrative - e.g., "Despite positive news headlines, order flows show heavy institutional selling over the last 20 days, suggesting a potential distribution."]

Key Patterns:
 • [Confirmation]: [e.g., High realized volatility is confirmed by model forecasts, suggesting sustained risk.]
 • [Contradiction]: [e.g., Positive news sentiment is contradicted by heavy net selling.]

Key Evidence:
 • Market Fact: [e.g., Realized downside volatility is low at 0.2%]
 • Primary Conflicting Risk ([Risk Type]): [e.g., Aggregated news sentiment is highly negative]
 • Valuation: [e.g., Trading 20bps rich vs. peers (Show Peers)]
 • Liquidity: [e.g., Bid/Ask is 2 points wide. Marked as ILLIQUID]
 • Order Flow: [e.g., Net customer selling of $5M in last day]

Quantitative Risk Factors:
 • Forecast (5d): [e.g., 85% prob. of negative news; Volatility predicted at 0.8% (Show Drivers)]
 • YTW / YTM: {financial_data_object.calculated_risk_metrics.yield_to_worst} / {financial_data_object.calculated_risk_metrics.yield_to_maturity}
 • DV01 / CS01: {financial_data_object.calculated_risk_metrics.dv01} / {financial_data_object.calculated_risk_metrics.cs01}
 • OAS: {financial_data_object.calculated_risk_metrics.option_adjusted_spread_bps} bps
 • Cost of Carry: [e.g., 15 bps]
 • Ownership: [e.g., Concentrated (Top 3 holders own 75%)]
 • Correlation: [e.g., 60d Corr. to HYG: 0.6]
 • Key Covenants: [e.g., Sinking fund begins 2025]

Suggested Actions:
 • [Investigate] price/fundamental divergence.
 • [Review] cheaper alternatives in peer group.
 • [Hedge] interest rate risk given the high DV01.
 • [Monitor] for continuation of 20-day selling pressure before adding risk.
------------------------------------------------------------------
```

### 2.7. Feature: Data Transparency
This feature ensures that valuation metrics are transparent and trustworthy.

- **SYS-R-17:** For any valuation metric derived from a peer comparison, the system **must** make the `peer_group_definition` from the `financial_data_object` available.
- **SYS-R-18:** The UI **shall** provide a mechanism to display the full `financial_data_object` on user request to allow for complete data drill-down.

### 2.8. Feature: Forecast Explainability
This feature ensures that predictive models are not "black boxes."

- **SYS-R-19:** Any predictive forecast displayed in the output **must** be accompanied by its primary drivers, sourced from `risk_forecasts.forecast_explainability`.
- **SYS-R-20:** Each driver **must** include its name, its contribution to the outcome, and a human-readable description.

### 2.9. Feature: Actionable Insights Generation
This feature translates the risk synthesis into concrete, actionable suggestions for the trader.

- **SYS-R-21:** The system **shall** generate a list of `suggested_actions` based on the synthesized risk profile and detected patterns using a rule-based engine.
- **SYS-R-22:** The rule engine **shall** implement, at a minimum, the following logic:
    - **IF** a `CONFLICT` is detected with a score > 0.7, **THEN** generate an `[Investigate]` action to analyze the price/fundamental divergence.
    - **IF** the `Valuation Risk` score is > 0.7, **THEN** generate a `[Review]` action to find cheaper alternatives in the peer group, attaching `relative_value.peer_group_cusips`.
    - **IF** the `Interest Rate Risk` score is > 0.8, **THEN** generate a `[Hedge]` action to mitigate interest rate exposure.
    - **IF** a `Contradiction (Sentiment vs. Flow)` pattern is detected, **THEN** generate an `[Investigate]` action to understand the source of the selling pressure.
    - **IF** an `Order Flow Imbalance Risk` score is > 0.7, **THEN** generate a `[Monitor]` action to track the persistent selling/buying pressure and its impact on price.
    - **IF** an `Illiquidity Risk` score is > 0.8, **THEN** generate a `[Review]` action to consider the impact of wide bid/ask spreads on execution cost.
- **SYS-R-23:** Actions **shall** be categorized (e.g., Investigate, Review, Execute, Hedge) and provide context, such as referencing peer CUSIPs for a swap.

## 3. Integration Points
The output of the Risk Synthesis service is consumed by:
- The final UI component presenting information to the trader.
- Idea generation workflows, using `suggested_actions` as input.