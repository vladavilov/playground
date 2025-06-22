# Risk Synthesis & Narrative Generation Service

---

## 1. Overview & Purpose
The Risk Synthesis service aggregates and interprets all computed risk factors for a given fixed-income instrument to generate a single, actionable, and explainable narrative for a trader. This service acts as the final analytical step, translating quantitative data into a concise, qualitative risk summary. Its primary goal is to highlight the most salient and non-obvious risks by interpreting a consolidated data object, flag conflicts between market behavior and derived fundamental risks with enhanced nuance, provide transparency into underlying data.

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
    "security_details": {
        "instrument_type": "string",
        "state": "string",
        "maturity": "date",
        "tax_profile": {
            "is_amt": "boolean",
            "in_state_tax_exempt": "boolean",
            "de_minimis_issue": "boolean",
            "bank_qualified": "boolean"
        },
        "issuer_details": {
            "debt_service_coverage_ratio": "float",
            "is_dsr_covenant_breached": "boolean"
        },
        "call_features": {
            "is_callable": "boolean",
            "next_call_date": "date",
            "next_call_price": "float"
        }
    },
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
    "liquidity": {
        "composite_score": "float",
        "is_illiquid_flag": "boolean",
        "market_depth": {
            "bid_size_par": "float",
            "ask_size_par": "float"
        }
    },
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
    },
    "cross_asset_correlation": {
      "benchmark_ticker": "string",
      "correlation_60d": "float"
    }
  },
  "market_regime": {
    "data_timestamp": "datetime",
    "regime_classification": {
      "regime_label": "string",
      "confidence_score": "float",
      "regime_probabilities": { "Bear_Steepener": "float", "...": "..." }
    },
    "volatility_classification": {
      "volatility_regime": "string",
      "volatility_index_name": "string",
      "volatility_index_value": "float"
    }
  },
  "news_sentiment": {
    "aggregated_sentiment_score": "float",
    "top_articles": ["string"] // list of summaries from the top_articles
  },
  "risk_forecasts": {
    "model_performance": {
        "negative_news_forecast_accuracy": { "precision": "float", "recall": "float" },
        "spread_widening_forecast_accuracy": { "precision": "float", "recall": "float" },
        "volatility_forecast_accuracy": { "precision": "float", "recall": "float" }
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
    }
  }
}
```

### 2.2. Feature: Risk Factor Derivation & Standardization
The system standardizes the consolidated input into a uniform structure for analysis.

- **SYS-R-04:** The system **shall** transform the `Consolidated Input Data Model` into a standardized `risk_factors` array. Each element in the array represents a distinct risk, normalized to a common 0-1 scale where higher scores indicate higher risk.
- **SYS-R-05:** The system **shall** utilize a `risk_normalization_scales` object to define the high-risk thresholds for `dv01` and `cs01` based on instrument class and maturity. This object **shall** be externally configurable.

#### Risk Factors Data Model
The `risk_factors` array **shall** contain a series of objects, where each object represents a single, standardized risk factor. The structure for each object **must** conform to the following JSON schema:

```json
{
  "risk_type": "string",
  "description": "string",
  "score": "float",
  "evidence": [
    {
      "name": "string",
      "value": "string"
    }
  ]
}
```

- **`risk_type`**: The standardized name of the risk factor (e.g., "Valuation Risk", "News Risk").
- **`description`**: A brief, one-sentence explanation of what this risk represents and why it is important.
- **`score`**: The normalized 0-1 score for the risk, where 1 indicates the highest level of risk.
- **`evidence`**: An array of key data points that were used to calculate the score. This provides transparency and allows for drill-down analysis.
    - **`name`**: The human-readable name of the data point (e.g., "Peer Spread vs. Avg (bps)").
    - **`value`**: The value of the data point, formatted as a string for display (e.g., "18.5", "High", "Concentrated").

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

- **SYS-R-06:** The following logic **shall** be used to derive and normalize each risk factor into a standardized 0-1 score:
    - **Valuation Risk:** This risk quantifies if an instrument is overvalued ("trading rich") relative to its peers and the appropriate market benchmark. A high score indicates significant overvaluation, suggesting a higher risk of price correction. The score is a weighted average of two components: peer relative value and benchmark relative value, with thresholds that adjust based on market volatility.
        - **SYS-R-06A:** The system **shall** utilize a `valuation_risk_thresholds` object to define the high-risk thresholds for valuation based on the market's volatility regime. This object **shall** be externally configurable.

        #### Valuation Risk Thresholds Data Model
        This object defines the basis point (bps) spread considered to be "rich" (high risk) under different market volatility regimes.
        ```json
        {
          "valuation_risk_thresholds": {
            "by_volatility_regime": {
              "Low":    { "peer_bps": 10, "benchmark_bps": 20 },
              "Medium": { "peer_bps": 15, "benchmark_bps": 25 },
              "High":   { "peer_bps": 25, "benchmark_bps": 40 }
            }
          }
        }
        ```
        - ***Rationale:*** In low-volatility environments, spreads are typically tighter, so a smaller deviation (`10 bps` vs. peers) is significant. In high-volatility environments, spreads widen across the board, so a much larger deviation (`25 bps` vs. peers) is required to confidently flag a bond as overvalued. The system uses the `market_regime.volatility_classification.volatility_regime` input to select the appropriate set of thresholds.

        - **Score Calculation:** The individual components are normalized using the dynamically selected thresholds and combined into a final score.
            ```math
            \begin{aligned}
            \text{VolatilityRegime} &= \text{market\_regime.volatility\_classification.volatility\_regime} \\
            \text{Thresholds} &= \text{LookupThresholds}(\text{VolatilityRegime}, \text{"valuation\_risk\_thresholds"}) \\
            \\
            \text{BenchmarkSpread} &= \begin{cases} \text{vs\_mmd\_bps} & \text{if instrument\_type is MUNI} \\ \text{vs\_ust\_bps} & \text{if instrument\_type is TFI} \end{cases} \\
            \text{PeerScore} &= \text{Normalize}(\text{vs\_peers\_bps}, \text{Thresholds.peer\_bps}) \\
            \text{BenchmarkScore} &= \text{Normalize}(\text{BenchmarkSpread}, \text{Thresholds.benchmark\_bps}) \\
            \text{ValuationRiskScore} &= (0.6 \cdot \text{PeerScore}) + (0.4 \cdot \text{BenchmarkScore})
            \end{aligned}
            ```
            - *Where the `Normalize(value, threshold)` function scales the input `value` to a 0-1 score, such as `min(max(value, 0) / threshold, 1)`. Negative values (trading cheap) result in a score of 0.*
        - **Output Definition:**
            - **`risk_type`**: "Valuation"
            - **`description`**: "Measures if the instrument is overvalued ('rich') relative to its peers and benchmark, adjusted for market volatility."
            - **`evidence`**:
                - `name`: "Spread vs. Peers (bps)", `value`: `financial_data_object.relative_value.vs_peers_bps`
                - `name`: "Peer Valuation Threshold (bps)", `value`: The looked-up `Thresholds.peer_bps` value.
                - `name`: "Spread vs. Benchmark (bps)", `value`: The value of `vs_mmd_bps` or `vs_ust_bps` used in the calculation. Name should differ based on what benchmark is used.
                - `name`: "Benchmark Valuation Threshold (bps)", `value`: The looked-up `Thresholds.benchmark_bps` value.
                - `name`: `market_regime.volatility_classification.volatility_index_name`, `value`: `market_regime.volatility_classification.volatility_index_name`
                - `name`: "Volatility Regime", `value`: `market_regime.volatility_classification.volatility_regime`
    - **News Risk:** This risk measures the impact of negative news sentiment surrounding an instrument. The score is derived from `news_sentiment.aggregated_sentiment_score`, which is a sophisticated metric weighted by event type, source credibility, and timeliness. A highly negative score indicates significant risk.
        - *Threshold for High Risk:* An `aggregated_sentiment_score < -0.5` is considered a strong indicator of high risk.
        - *Score Calculation:* The sentiment score is normalized to the standard 0-1 risk scale. Since lower (more negative) sentiment scores represent higher risk, the sign is inverted before normalization.
        ```math
        \text{NewsRiskScore} = \text{Normalize}(-\text{aggregated\_sentiment\_score}, 0.5)
        ```
        - **Output Definition:**
            - **`risk_type`**: "News Sentiment"
            - **`description`**: "Measures the risk from negative news sentiment surrounding the instrument, weighted by source credibility and timeliness."
            - **`evidence`**:
                - `name`: "Aggregated Sentiment Score", `value`: `news_sentiment.aggregated_sentiment_score`
                - `name`: "Aggregated Relevant Articles", `value`: `news_sentiment.top_articles`
    - **Illiquidity Risk:** This risk measures how difficult it might be to trade the instrument at a fair price. The score is a weighted average of two components: its liquidity relative to peers and the absolute, observable market depth.
        - **Component 1: Relative Liquidity Score (60% weight)**
            - *Logic:* The `liquidity.composite_score` (a z-score of liquidity vs. peers) is mapped to a qualitative risk level.
            - *Risk Level Mapping:*
                - **High:** `composite_score < -2.0`. Score = 0.9
                - **Medium:** `-2.0 <= composite_score < -1.0`. Score = 0.6
                - **Low:** `composite_score >= -1.0`. Score = 0.2
        - **Component 2: Market Depth Score (40% weight)**
            - *Logic:* The score is derived from the total par value available on the bid and ask sides.
            - *Risk Level Mapping:*
                - **High:** `TotalDepth < $250,000`. Score = 0.9
                - **Medium:** `$250,000 <= TotalDepth < $1,000,000`. Score = 0.5
                - **Low:** `TotalDepth >= $1,000,000`. Score = 0.1
        - **Final Score Calculation:**
        ```math
        \begin{aligned}
        \text{RelativeScore} &= \text{MapToScore}(\text{liquidity.composite\_score}) \\
        \text{TotalDepth} &= \text{liquidity.market\_depth.bid\_size\_par} + \text{liquidity.market\_depth.ask\_size\_par} \\
        \text{DepthScore} &= \text{MapToScore}(\text{TotalDepth}) \\
        \\
        \text{IlliquidityRiskScore} &= (0.6 \cdot \text{RelativeScore}) + (0.4 \cdot \text{DepthScore})
        \end{aligned}
        ```
        - **Output Definition:**
            - **`risk_type`**: "Illiquidity"
            - **`description`**: "Measures the difficulty of trading at a fair price, based on a blend of liquidity relative to peers and absolute market depth."
            - **`score`**: The final `IlliquidityRiskScore`.
            - **`evidence`**:
                - `name`: "Liquidity Score vs. Peers (z-score)", `value`: `financial_data_object.liquidity.composite_score`
                - `name`: "Bid Size (Par)", `value`: `financial_data_object.liquidity.market_depth.bid_size_par`
                - `name`: "Ask Size (Par)", `value`: `financial_data_object.liquidity.market_depth.ask_size_par`
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
        - **Output Definition:**
            - **`risk_type`**: "Volatility Trend"
            - **`description`**: "Measures the acceleration of recent price volatility by comparing short-term (5d) to long-term (20d) volatility."
            - **`evidence`**:
                - `name`: "5d Downside Volatility", `value`: `calculated_risk_metrics.downside_price_volatility_5d`
                - `name`: "20d Downside Volatility", `value`: `calculated_risk_metrics.downside_price_volatility_20d`
                - `name`: "5d Trade Volatility", `value`: `trade_history_summary.t5d.trade_price_volatility`
                - `name`: "20d Trade Volatility", `value`: `trade_history_summary.t20d.trade_price_volatility`
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
        - **Output Definition:**
            - **`risk_type`**: "Order Flow Pressure"
            - **`description`**: "Measures the direction and magnitude of sustained trading pressure by analyzing net customer order flow. The score ranges from -1 (intense buying pressure) to +1 (intense selling pressure)."
            - **`score`**: The raw `OrderFlowPressureScore` is used directly, as its full range from -1 to +1 is required for downstream analysis. A positive score indicates risk from selling pressure, while a negative score indicates buying pressure, which is used to detect contradictions (e.g., 'Rich & Squeezing Higher').
            - **`evidence`**:
                - `name`: "1d Customer Buy Volume", `value`: `trade_history_summary.t1d.customer_buy_par_volume`
                - `name`: "1d Customer Sell Volume", `value`: `trade_history_summary.t1d.customer_sell_par_volume`
                - `name`: "5d Customer Buy Volume", `value`: `trade_history_summary.t5d.customer_buy_par_volume`
                - `name`: "5d Customer Sell Volume", `value`: `trade_history_summary.t5d.customer_sell_par_volume`
                - `name`: "20d Customer Buy Volume", `value`: `trade_history_summary.t20d.customer_buy_par_volume`
                - `name`: "20d Customer Sell Volume", `value`: `trade_history_summary.t20d.customer_sell_par_volume`
    - **State Credit Risk:** (For MUNIs only) This risk is derived from the `state_fiscal_health` indicators from the `financial_data_object`. A high score indicates deteriorating fiscal health. The calculation uses a weighted average of two sub-scores: one for revenue growth and one for the state's budget balance.
        - **Component 1: Tax Receipts Growth Risk (`GrowthScore`)**
            - *Logic:* This score assesses the risk from changes in year-over-year state tax receipts. Negative growth is a significant red flag. (Note: While these buckets provide a clear, simple implementation, a future enhancement could use a continuous function, such as a sigmoid function, for normalization to provide a more granular risk score.)
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
        - **Output Definition:**
            - **`risk_type`**: "State Credit"
            - **`description`**: "Measures the risk of deteriorating fiscal health for the issuer's state (for municipal bonds only)."
            - **`evidence`**:
                - `name`: "Tax Receipts YoY Growth (%)", `value`: `financial_data_object.state_fiscal_health.tax_receipts_yoy_growth`
                - `name`: "Budget Surplus/Deficit (% of GSP)", `value`: `financial_data_object.state_fiscal_health.budget_surplus_deficit_pct_gsp`
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
        - **Output Definition:**
            - **`risk_type`**: "Interest Rate Sensitivity"
            - **`description`**: "Measures the instrument's price sensitivity to a 1 basis point change in interest rates (DV01)."
            - **`evidence`**:
                - `name`: "DV01", `value`: `financial_data_object.calculated_risk_metrics.dv01`
                - `name`: "DV01 High Risk Threshold", `value`: The looked-up `dv01_threshold`.
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
        - **Output Definition:**
            - **`risk_type`**: "Credit Spread Sensitivity"
            - **`description`**: "Measures the instrument's price sensitivity to a 1 basis point change in its credit spread (CS01)."
            - **`evidence`**:
                - `name`: "CS01", `value`: `financial_data_object.calculated_risk_metrics.cs01`
                - `name`: "CS01 High Risk Threshold", `value`: The looked-up `cs01_threshold`.
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
        - **Output Definition:**
            - **`risk_type`**: "Predicted Negative Event"
            - **`description`**: "Measures the model-forecasted probability of a negative news event over multiple time horizons."
            - **`evidence`**:
                - `name`: "1d Prob. Negative News (%)", `value`: `risk_forecasts.forecasted_values[0].probability_negative_news_pct`
                - `name`: "5d Prob. Negative News (%)", `value`: `risk_forecasts.forecasted_values[1].probability_negative_news_pct`
                - `name`: "20d Prob. Negative News (%)", `value`: `risk_forecasts.forecasted_values[2].probability_negative_news_pct`
                - *The system shall iterate through the `risk_forecasts.forecast_explainability.feature_attributions.probability_negative_news_pct` array and create an evidence item for each feature, using the format:*
                - `name`: "Driver: {feature_name}", `value`: "{attribution}"
    - **Predicted Spread Widening Risk:** This risk measures the potential for the instrument's credit spread to increase across multiple time horizons. A high score indicates a significant risk of underperformance due to spread widening.
        - **SYS-R-07:** The system **shall** utilize a `predicted_spread_widening_thresholds` object to define the high-risk thresholds for forecasted spread widening based on instrument class. This object **shall** be externally configurable.

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
        - **Output Definition:**
            - **`risk_type`**: "Predicted Spread Widening"
            - **`description`**: "Measures the risk of underperformance due to a model-forecasted increase in the instrument's credit spread."
            - **`evidence`**:
                - `name`: "1d Forecast Spread Widening (bps)", `value`: The `Spread_1d` value.
                - `name`: "1d Spread Widening Threshold (bps)", `value`: The `Thresholds.threshold_1d_bps` value.
                - `name`: "5d Forecast Spread Widening (bps)", `value`: The `Spread_5d` value.
                - `name`: "5d Spread Widening Threshold (bps)", `value`: The `Thresholds.threshold_5d_bps` value.
                - `name`: "20d Forecast Spread Widening (bps)", `value`: The `Spread_20d` value.
                - `name`: "20d Spread Widening Threshold (bps)", `value`: The `Thresholds.threshold_20d_bps` value.
                - *The system shall iterate through the `risk_forecasts.forecast_explainability.feature_attributions.credit_spread_oas_bps` array and create an evidence item for each feature, using the format:*
                - `name`: "Driver: {feature_name}", `value`: "{attribution}"
                - `name`: "Model Precision", `value`: `risk_forecasts.model_performance.spread_widening_forecast_accuracy.precision`
                - `name`: "Model Recall", `value`: `risk_forecasts.model_performance.spread_widening_forecast_accuracy.recall`
    - **Predicted Volatility Risk:** This risk measures the forecasted downside price volatility, adjusted for time. VaR estimates from different horizons are not directly comparable, so they are first scaled to a "daily-equivalent" volatility before being scored and combined.
        - **SYS-R-07A:** The system **shall** utilize a `predicted_volatility_thresholds` object to define the high-risk thresholds for daily-equivalent forecasted volatility based on instrument class. This object **shall** be externally configurable.

        #### Predicted Volatility Thresholds Data Model
        This object defines the daily-equivalent VaR (%) that is considered high risk (mapping to a normalized score of 1.0). A `0.5%` daily VaR might be trivial for a High-Yield bond but would be catastrophic for a pre-refunded MUNI.
        ```json
        {
          "predicted_volatility_thresholds": {
            "by_instrument_class": {
              "MUNI_GO":              { "threshold_daily_equiv_var_pct": 0.3 },
              "MUNI_REVENUE":         { "threshold_daily_equiv_var_pct": 0.4 },
              "MUNI_PREREFUNDED":     { "threshold_daily_equiv_var_pct": 0.1 },
              "CORP_IG":              { "threshold_daily_equiv_var_pct": 0.5 },
              "CORP_HY":              { "threshold_daily_equiv_var_pct": 1.0 },
              "US_TREASURY":          { "threshold_daily_equiv_var_pct": 0.2 },
              "DEFAULT":              { "threshold_daily_equiv_var_pct": 0.6 }
            }
          }
        }
        ```

        - *Logic:* The VaR value for each horizon is divided by the square root of the number of days in its period. These daily-equivalent volatilities are then normalized against a single, class-specific threshold and combined in a weighted average.
        - *Score Calculation:*
        ```math
        \begin{aligned}
        \text{VolatilityThreshold} &= \text{LookupThreshold}(\text{instrument\_class}, \text{"volatility"}) \\
        \\
        \text{DailyEquivVol}_{1d} &= \frac{\text{forecasts.values[0].downside\_price\_volatility.value}}{\sqrt{1}} \\
        \text{DailyEquivVol}_{5d} &= \frac{\text{forecasts.values[1].downside\_price\_volatility.value}}{\sqrt{5}} \\
        \text{DailyEquivVol}_{20d} &= \frac{\text{forecasts.values[2].downside\_price\_volatility.value}}{\sqrt{20}} \\
        \\
        \text{Score}_{1d} &= \text{Normalize}(\text{DailyEquivVol}_{1d}, \text{VolatilityThreshold}) \\
        \text{Score}_{5d} &= \text{Normalize}(\text{DailyEquivVol}_{5d}, \text{VolatilityThreshold}) \\
        \text{Score}_{20d} &= \text{Normalize}(\text{DailyEquivVol}_{20d}, \text{VolatilityThreshold}) \\
        \\
        \text{PredictedVolatilityRiskScore} &= (0.5 \cdot \text{Score}_{1d}) + (0.3 \cdot \text{Score}_{5d}) + (0.2 \cdot \text{Score}_{20d})
        \end{aligned}
        ```
        - **Output Definition:**
            - **`risk_type`**: "Predicted Volatility"
            - **`description`**: "Measures the model-forecasted downside price volatility, adjusted for time."
            - **`evidence`**:
                - `name`: "1d Forecasted VaR", `value`: `risk_forecasts.forecasted_values[0].downside_price_volatility.value`
                - `name`: "5d Forecasted VaR", `value`: `risk_forecasts.forecasted_values[1].downside_price_volatility.value`
                - `name`: "20d Forecasted VaR", `value`: `risk_forecasts.forecasted_values[2].downside_price_volatility.value`
                - `name`: "Volatility Normalization Threshold (Daily-Eq.)", `value`: The looked-up `VolatilityThreshold`.
                - *The system shall iterate through the `risk_forecasts.forecast_explainability.feature_attributions.downside_price_volatility` array and create an evidence item for each feature, using the format:*
                - `name`: "Driver: {feature_name}", `value`: "{attribution}"
                - `name`: "Model Precision", `value`: `risk_forecasts.model_performance.volatility_forecast_accuracy.precision`
                - `name`: "Model Recall", `value`: `risk_forecasts.model_performance.volatility_forecast_accuracy.recall`
    - **Predicted Liquidity Degradation:** This risk measures the potential for transaction costs to increase due to a forecasted widening of the bid-ask spread.
        - *Logic:* The score is a weighted average of the forecasted percentage increase in the bid-ask spread over multiple horizons, compared to the current spread. A significant forecasted widening indicates higher risk.
        - *Score Calculation:*
        ```math
        \begin{aligned}
        \text{Spread}_{\text{current}} &= \frac{\text{market\_data.bid\_ask\_spread\_bps}}{10000} \\
        \text{Spread}_{\text{1d}} &= \text{risk\_forecasts.forecasted\_values[0].bid\_ask\_spread\_pct} / 100 \\
        \text{Spread}_{\text{5d}} &= \text{risk\_forecasts.forecasted\_values[1].bid\_ask\_spread\_pct} / 100 \\
        \text{Spread}_{\text{20d}} &= \text{risk\_forecasts.forecasted\_values[2].bid\_ask\_spread\_pct} / 100 \\
        \\
        \text{Widening}_{\text{1d}} &= \text{Spread}_{\text{1d}} - \text{Spread}_{\text{current}} \\
        \text{Widening}_{\text{5d}} &= \text{Spread}_{\text{5d}} - \text{Spread}_{\text{current}} \\
        \text{Widening}_{\text{20d}} &= \text{Spread}_{\text{20d}} - \text{Spread}_{\text{current}} \\
        \\
        \text{Threshold} &= 0.5 \cdot \text{Spread}_{\text{current}} \\
        \text{Score}_{1d} &= \text{Normalize}(\text{Widening}_{\text{1d}}, \text{Threshold}) \\
        \text{Score}_{5d} &= \text{Normalize}(\text{Widening}_{\text{5d}}, \text{Threshold}) \\
        \text{Score}_{20d} &= \text{Normalize}(\text{Widening}_{\text{20d}}, \text{Threshold}) \\
        \\
        \text{PredictedLiquidityRiskScore} &= (0.5 \cdot \text{Score}_{1d}) + (0.3 \cdot \text{Score}_{5d}) + (0.2 \cdot \text{Score}_{20d})
        \end{aligned}
        ```
        - *Where `Normalize(value, threshold)` is `min(max(value, 0) / threshold, 1)`. A 50% increase in spread maps to a component score of 1.0.*
        - **Output Definition:**
            - **`risk_type`**: "Predicted Liquidity Degradation"
            - **`description`**: "Measures the risk of increasing transaction costs due to a forecasted widening of the bid-ask spread."
            - **`evidence`**:
                - `name`: "Current Bid-Ask Spread (bps)", `value`: `financial_data_object.market_data.bid_ask_spread_bps`
                - `name`: "1d Forecast Bid-Ask Spread (%)", `value`: `risk_forecasts.forecasted_values[0].bid_ask_spread_pct`
                - `name`: "5d Forecast Bid-Ask Spread (%)", `value`: `risk_forecasts.forecasted_values[1].bid_ask_spread_pct`
                - `name`: "20d Forecast Bid-Ask Spread (%)", `value`: `risk_forecasts.forecasted_values[2].bid_ask_spread_pct`
    - **Negative Carry:** This risk indicates if the bond's yield is less than the financing cost, resulting in a daily loss if the price does not appreciate.
        - *Logic:* This is a binary risk factor. Any negative carry is flagged as high risk.
        - *Score Calculation:* The score is 1.0 if `cost_of_carry_bps` is negative, and 0 otherwise.
        ```math
        \text{NegativeCarryScore} = \begin{cases} 1.0 & \text{if supplemental\_data.cost\_of\_carry\_bps < 0} \\ 0.0 & \text{otherwise} \end{cases}
        ```
        - **Output Definition:**
            - **`risk_type`**: "Negative Carry"
            - **`description`**: "Indicates if the bond's yield is less than the financing cost, resulting in a daily loss if the price does not appreciate."
            - **`evidence`**:
                - `name`: "Cost of Carry (bps)", `value`: `supplemental_data.cost_of_carry_bps`
    - **Ownership Concentration:** This risk measures price fragility due to a small number of entities holding a large percentage of the bond's outstanding issue.
        - *Logic:* This is a binary risk factor based on the pre-calculated `is_concentrated_flag`.
        - *Score Calculation:* The score is 1.0 if `is_concentrated_flag` is true, and 0 otherwise.
        ```math
        \text{OwnershipConcentrationScore} = \begin{cases} 1.0 & \text{if supplemental\_data.ownership\_concentration.is\_concentrated\_flag} \\ 0.0 & \text{otherwise} \end{cases}
        ```
        - **Output Definition:**
            - **`risk_type`**: "Ownership Concentration"
            - **`description`**: "Measures the risk of price fragility due to a small number of entities holding a large percentage of the bond's outstanding issue."
            - **`evidence`**:
                - `name`: "Top 3 Holders Ownership (%)", `value`: `supplemental_data.ownership_concentration.top_3_holders_pct`
    - **Market Contagion:** This risk measures the likelihood that the bond's price will be negatively impacted by broader market movements due to high correlation with a major market benchmark.
        - *Logic:* The risk score is derived from the 60-day correlation to a benchmark asset. A correlation above 0.7 is considered high risk.
        - *Score Calculation:*
        ```math
        \text{MarketContagionScore} = \text{Normalize}(\text{supplemental\_data.cross\_asset\_correlation.correlation\_60d}, 0.7)
        ```
        - *Where `Normalize(value, threshold)` is `min(max(value, 0) / threshold, 1)`.*
        - **Output Definition:**
            - **`risk_type`**: "Market Contagion"
            - **`description`**: "Measures the risk that the bond's price will be negatively impacted by broader market movements due to high correlation with a major market benchmark."
            - **`evidence`**:
                - `name`: "Benchmark Ticker", `value`: `supplemental_data.cross_asset_correlation.benchmark_ticker`
                - `name`: "60-day Correlation", `value`: `supplemental_data.cross_asset_correlation.correlation_60d`
    - **Tax Profile Risk:** (For MUNIs only) This risk quantifies the negative impact of an instrument's tax features on its value to the broadest base of investors. A high score indicates features that limit its appeal (e.g., subject to AMT), potentially justifying a lower price or indicating a narrower, specialized buyer base.
        - *Logic:* This risk is scored based on a penalty system. Each negative tax feature adds points to a total score, which is then normalized. A bond's attractiveness is highly dependent on its tax status for its natural buyer base. The absence of key favorable features (like in-state exemption or Bank-Qualified status) is a significant risk factor.
        - *Score Calculation:*
            ```math
            \begin{aligned}
            \text{TaxPenaltyScore} &= 0 \\
            \text{if is\_amt is true:} & \quad \text{TaxPenaltyScore} += 5 \\
            \text{if in\_state\_tax\_exempt is false:} & \quad \text{TaxPenaltyScore} += 7 \\
            \text{if de\_minimis\_issue is true:} & \quad \text{TaxPenaltyScore} += 3 \\
            \text{if bank\_qualified is false:} & \quad \text{TaxPenaltyScore} += 2 \\
            \\
            \text{TaxProfileRiskScore} &= \frac{\text{TaxPenaltyScore}}{17}
            \end{aligned}
            ```
        - **Output Definition:**
            - **`risk_type`**: "Tax Profile"
            - **`description`**: "Measures the risk that specific tax features (e.g., AMT, De Minimis, In-State Taxability) could limit the instrument's investor base and negatively impact its value."
            - **`evidence`**:
                - `name`: "Subject to AMT", `value`: `financial_data_object.security_details.tax_profile.is_amt`
                - `name`: "In-State Tax Exempt", `value`: `financial_data_object.security_details.tax_profile.in_state_tax_exempt`
                - `name`: "De Minimis Issue", `value`: `financial_data_object.security_details.tax_profile.de_minimis_issue`
                - `name`: "Bank Qualified", `value`: `financial_data_object.security_details.tax_profile.bank_qualified`
    - **Issuer & Covenant Risk:** (For MUNI Revenue and Corporate bonds) This risk measures the creditworthiness of the specific issuer based on key financial ratios and adherence to bond covenants, rather than relying on broad, state-level metrics.
        - *Logic:* Risk is assessed based on the Debt Service Coverage Ratio (DSCR). A ratio close to or below 1.0x indicates severe risk, as the issuer may not be generating enough cash to cover its debt payments.
        - *Threshold for High Risk:* A `debt_service_coverage_ratio < 1.2` is a significant warning sign. A breach of the covenant is a critical risk event.
        - *Score Calculation:*
        ```math
        \text{IssuerCovenantRiskScore} = \begin{cases} 1.0 & \text{if is\_dsr\_covenant\_breached is true} \\
        \text{NormalizeInverted}((\text{dscr} - 1.0), 0.5) & \text{otherwise} \end{cases}
        ```
        - *Where `NormalizeInverted(value, range)` is `1 - min(max(value, 0) / range, 1)`. As DSCR falls from 1.5 towards 1.0, the score rises from 0 towards 1.0.*
        - **Output Definition:**
            - **`risk_type`**: "Issuer & Covenant"
            - **`description`**: "Measures issuer-specific credit risk based on financial health (DSCR) and adherence to debt covenants."
            - **`evidence`**:
                - `name`: "Debt Service Coverage Ratio", `value`: `financial_data_object.security_details.issuer_details.debt_service_coverage_ratio`
                - `name`: "DSR Covenant Breached", `value`: `financial_data_object.security_details.issuer_details.is_dsr_covenant_breached`
    - **Call Risk:** This risk measures the likelihood that the issuer will redeem the bond before its maturity date, which is especially pertinent in a falling interest rate environment. This can negatively impact a holder's total return by forcing reinvestment at lower rates.
        - *Logic:* The risk is highest when a callable bond's market price is at or above its next call price, and the next call date is approaching.
        - *Score Calculation:* The score is a combination of a price factor and a time factor.
        ```math
        \begin{aligned}
        \text{price\_ratio} &= \frac{\text{market\_data.price}}{\text{call\_features.next\_call\_price}} \\
        \text{PriceScore} &= \text{Normalize}(\text{price\_ratio} - 1.0, 0.03) \\
        \text{TimeScore} &= \max(1 - \frac{\text{DaysUntil}(\text{next\_call\_date})}{365}, 0) \\
        \\
        \text{CallRiskScore} &= \begin{cases} \sqrt{\text{PriceScore} \cdot \text{TimeScore}} & \text{if is\_callable is true} \\ 0 & \text{otherwise} \end{cases}
        \end{aligned}
        ```
        - *Where `Normalize(value, threshold)` is `min(max(value, 0) / threshold, 1)`. A price 3% above the call price gets a max score. `TimeScore` increases as the call date gets closer.*
        - **Output Definition:**
            - **`risk_type`**: "Call Risk"
            - **`description`**: "Measures the risk of the bond being called by the issuer, potentially leading to lower-than-expected returns."
            - **`evidence`**:
                - `name`: "Is Callable", `value`: `financial_data_object.security_details.call_features.is_callable`
                - `name`: "Market Price", `value`: `financial_data_object.market_data.price`
                - `name`: "Next Call Date", `value`: `financial_data_object.security_details.call_features.next_call_date`
                - `name`: "Next Call Price", `value`: `financial_data_object.security_details.call_features.next_call_price`

### 2.2.1. Feature: Supplemental Data Surfacing
- **SYS-R-05:** The system **shall** surface key supplemental data points in the final output. The following mappings from the `supplemental_data` input object to the `quantitative_risk_factors` output object **shall** be applied, with sample formatting:
    - `cost_of_carry_bps` -> `cost_of_carry`: e.g., "15 bps".
    - `ownership_concentration` -> `ownership`: e.g., "Concentrated (Top 3 holders own 75%)".
    - `cross_asset_correlation` -> `correlation`: e.g., "60d Corr. to HYG: 0.6".

### 2.2.2 Feature: Market Regime Contextualization
This feature uses the overall market regime, as determined by the `MarketRegime` service, to provide context and dynamically adjust risk scores and narrative focus.

- **SYS-R-08**: The system **shall** use the `market_regime.regime_label` as a contextual multiplier to adjust specific, calculated `risk_factors` scores. The regime acts as an amplifier or dampener for risks that are particularly sensitive to the prevailing market environment.
- **SYS-R-09**: The system **shall** apply the following adjustments based on the detected market regime:

    #### Bearish Rate Regimes
    - **IF** `regime_label` is `Bear_Steepener` or `Bear_Flattener`:
        - `InterestRateRiskScore` **shall** be amplified by a factor of **1.25**.
        - `CallRiskScore` **shall** be dampened by a factor of **0.8**.
        - *Rationale:* In a rising rate environment, sensitivity to interest rate changes (DV01) is the most critical risk. Conversely, the probability of a bond being called decreases, dampening call risk.

    #### Bullish Rate Regimes
    - **IF** `regime_label` is `Bull_Steepener`, `Bull_Flattener`, or `Recession_Easing`:
        - `InterestRateRiskScore` **shall** be dampened by a factor of **0.8**.
        - `CallRiskScore` **shall** be amplified by a factor of **1.3**.
        - *Rationale:* In a falling rate environment, high duration becomes a positive attribute, reducing the immediate risk. Conversely, the probability of a bond being called increases significantly, amplifying the risk.

    #### Risk-Off Credit Regimes
    - **IF** `regime_label` is `Bear_Steepener`, `Bear_Flattener`, or `Bull_Flattener`:
        - `SpreadDurationRiskScore` and `PredictedSpreadWideningRiskScore` **shall** be amplified by a factor of **1.3**.
        - `MarketContagionRiskScore` **shall** be amplified by a factor of **1.5**.
        - `CallRiskScore` **shall** be amplified by a factor of **1.3**.
        - *Rationale:* In a flight-to-quality or general risk-off environment, credit risk is severely punished and correlations to broader markets spike.

    #### Risk-On Credit Regimes
    - **IF** `regime_label` is `Bull_Steepener` or `Recession_Easing`:
        - `SpreadDurationRiskScore` and `PredictedSpreadWideningRiskScore` **shall** be dampened by a factor of **0.85**.
        - `IlliquidityRisk` score **shall** be dampened by a factor of **0.8**.
        - `CallRiskScore` **shall** be amplified by a factor of **1.3**.
        - *Rationale:* In a risk-seeking environment, credit spreads tend to tighten and liquidity improves, making these risks less acute.

    #### Neutral Regimes
    - **IF** `regime_label` is `Idiosyncratic_Distress`:
        - No risk score adjustments **shall** be applied.
        - *Rationale:* This regime implies that risk is specific to the asset or its sector, and broad macro-based adjustments are not applicable.

### 2.4. Feature: Cross-Factor Confirmation & Contradiction Analysis
This feature provides deeper, non-obvious insights by analyzing the relationships *between* different risk factors, moving beyond a single conflict score.

- **SYS-R-13:** The system **shall** implement a rule-based engine to detect pre-defined patterns of confirmation and contradiction among the derived `risk_factors`.
- **SYS-R-14:** The engine **shall** evaluate the following patterns:
    - **Confirmation (Fundamental + Forecast):** Detects when a current fundamental weakness is predicted to worsen.
        - *Logic:* `Valuation Risk Score > 0.7` AND `Predicted Spread Widening Risk Score > 0.7`.
        - *Output Insight:* "Instrument is trading rich and models forecast further spread widening, confirming valuation concerns."
    - **Confirmation (Covenant Pressure):** Detects when deteriorating issuer fundamentals are confirmed by predictive models.
        - *Logic:* `Issuer & Covenant Risk Score > 0.7` AND `Predicted Spread Widening Risk Score > 0.7`.
        - *Output Insight:* "Deteriorating issuer-level metrics, like a declining debt coverage ratio, are confirmed by models forecasting significant spread widening."
    - **Confirmation (Volatility Cluster):** Detects when recent and predicted volatility are both high.
        - *Logic:* `Volatility Trend Risk Score > 0.7` AND `Predicted Volatility Risk Score > 0.7`.
        - *Output Insight:* "Accelerating realized volatility is confirmed by forecasts, suggesting a sustained high-risk volatility regime."
    - **Contradiction (Sentiment vs. Flow):** Detects when positive news is met with heavy selling.
        - *Logic:* `News Risk Score < 0.3` (i.e., positive sentiment) AND `Order Flow Pressure Score > 0.7`.
        - *Output Insight:* "Despite positive news sentiment, order flows show significant net selling, indicating market participants may be disbelieving the news or using it as a liquidity event."
    - **Contradiction (Credit vs. Forecast):** Detects when strong state-level fundamentals are contradicted by negative model forecasts.
        - *Logic:* `State Credit Risk Score < 0.3` AND `Predicted Event Risk Score > 0.7`.
        - *Output Insight:* "While state fiscal health appears strong, predictive models are flagging a high probability of a negative event, suggesting a potential disconnect or forward-looking risk not yet in fundamental data."
    - **Confirmation (Falling Knife):** Detects when accelerating price instability is met with heavy selling pressure.
        - *Logic:* `Volatility Trend Risk Score > 0.7` AND `Order Flow Pressure Score > 0.7`.
        - *Output Insight:* "Price instability is accelerating amidst heavy, persistent selling pressure, suggesting sellers are becoming more aggressive and are willing to accept lower prices."
    - **Confirmation (Smart Money):** Detects when institutional selling pressure is validated by predictive models.
        - *Logic:* `Order Flow Pressure Score > 0.7` AND `Predicted Spread Widening Risk Score > 0.7`.
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
    - **Contradiction (Tax-Driven Value):** Detects when a bond appears overvalued but has a highly favorable tax status.
        - *Logic:* `Valuation Risk Score > 0.7` AND `Tax Profile Risk Score < 0.2`.
        - *Output Insight:* "The 'rich' valuation is likely justified by its highly favorable tax status (e.g., non-AMT, in-state exempt), which attracts a specific and less price-sensitive buyer base."
    - **Confirmation (Bottom Fishing / Contrarian Buying):** Detects when market participants are buying into weakness.
        - *Logic:* `(Issuer & Covenant Risk Score > 0.7 OR State Credit Risk Score > 0.7)` AND `Order Flow Pressure Score < -0.7`.
        - *Output Insight:* "Despite deteriorating fundamentals (e.g., low DSCR or poor state finances), order flow shows persistent net buying. This may indicate some market participants believe the risks are fully priced in and are buying on weakness, potentially seeing value where others see risk."
    - **Contradiction (Value Trap / Negative Carry):** Detects when a 'cheap' bond's low price is offset by its negative carry.
        - *Logic:* `Valuation Risk Score < 0.2` AND `Negative Carry Score == 1.0`.
        - *Output Insight:* "The bond appears cheap relative to its peers, but its negative cost of carry will erode total return unless its price appreciates. This could be a value trap if spreads fail to tighten."
    - **Contradiction (Technicals vs. Fundamentals Divergence):** Detects when market appetite contradicts model-based forecasts.
        - *Logic:* `(Predicted Spread Widening Risk Score > 0.7 OR Predicted Event Risk Score > 0.7)` AND `OrderFlowPressureScore < -0.7`.
        - *Output Insight:* "Predictive models are forecasting significant spread widening or a negative event, yet order flow shows strong, persistent buying pressure. This highlights a sharp divergence between model-based forecasts and current market appetite."
- **SYS-R-15:** Detected patterns **shall** be stored in a `pattern_analysis` array within the final output object, including the pattern type, a description of the finding, and the contributing factors.

### 2.5. Feature: Final Output Assembly
The system populates the final user-facing output by combining raw quantitative data with the LLM-generated narrative.

- **SYS-R-16:** The final output **must** conform to the structured `Output Data Model`.
- **SYS-R-17:** The Agent **must** provide explainable, structured, and actionable output to ensure trader trust and usability, as rendered in the `Output Display Template`.

#### Output Data Model
The feature's output **shall** conform to the following structured JSON schema.
```json
{
  "headline": "string",
  "synthesized_narrative": "string",
  "risk_factors": [
    {
      "risk_type": "string",
      "description": "string",
      "score": "float",
      "evidence": ["string"]
    }
  ],
  "pattern_analysis": [
    {
      "pattern_type": "string",
      "insight_summary": "string",
      "contributing_factors": ["string"]
    }
  ]
}
```

#### Output Display Template
This template defines how the final structured data **shall** be rendered for the end-user.
```
---------------------------------------------------------------------------------------------------
**[headline]**

**Market-Adjusted Risk Narrative:**
[synthesized_narrative]

**Context: [market_regime.regime_label] ([market_regime.volatility_classification.volatility_regime] Volatility)**
*Note: Risk scores for factors like Interest Rate Sensitivity, Spread Duration, and Call Risk have been adjusted based on the current market regime.*

**Key Insights (Cross-Factor Analysis):**
 • **[pattern_analysis[0].pattern_type]:** [pattern_analysis[0].insight_summary]
   *Contributing Factors: [pattern_analysis[0].contributing_factors.join(', ')]*
 • **[pattern_analysis[1].pattern_type]:** [pattern_analysis[1].insight_summary]
   *Contributing Factors: [pattern_analysis[1].contributing_factors.join(', ')]*

**Top Risk Factors (Normalized 0-1 Score):**
 • **[risk_factors[0].risk_type]: [risk_factors[0].score]** - *[risk_factors[0].description]*
   *Evidence: [risk_factors[0].evidence[0].name]: [risk_factors[0].evidence[0].value], [risk_factors[0].evidence[1].name]: [risk_factors[0].evidence[1].value]*
 • **[risk_factors[1].risk_type]: [risk_factors[1].score]** - *[risk_factors[1].description]*
   *Evidence: [risk_factors[1].evidence[0].name]: [risk_factors[1].evidence[0].value], [risk_factors[1].evidence[1].name]: [risk_factors[1].evidence[1].value]*
 • **[risk_factors[2].risk_type]: [risk_factors[2].score]** - *[risk_factors[2].description]*
   *Evidence: [risk_factors[2].evidence[0].name]: [risk_factors[2].evidence[0].value], [risk_factors[2].evidence[1].name]: [risk_factors[2].evidence[1].value]*

**Quantitative Metrics Summary:**

| Category       | Metric                                 | Value                                                                                                     |
|----------------|----------------------------------------|-----------------------------------------------------------------------------------------------------------|
| **Valuation**  | YTW / YTM                              | {financial_data_object.calculated_risk_metrics.yield_to_worst} / {financial_data_object.calculated_risk_metrics.yield_to_maturity} |
|                | OAS (bps)                              | {financial_data_object.calculated_risk_metrics.option_adjusted_spread_bps}                                    |
|                | Relative Value vs Peers (bps)          | {financial_data_object.relative_value.vs_peers_bps}                                                       |
|                | Relative Value vs Benchmark (bps)      | {financial_data_object.relative_value.vs_mmd_bps} or {financial_data_object.relative_value.vs_ust_bps}          |
| **Sensitivity**| DV01 / CS01                            | {financial_data_object.calculated_risk_metrics.dv01} / {financial_data_object.calculated_risk_metrics.cs01} |
| **Liquidity**  | Composite Score (z-score)              | {financial_data_object.liquidity.composite_score}                                                         |
|                | Bid / Ask Depth ($)                    | {financial_data_object.liquidity.market_depth.bid_size_par} / {financial_data_object.liquidity.market_depth.ask_size_par} |
| **Fundamentals** | Issuer DSCR                            | {financial_data_object.security_details.issuer_details.debt_service_coverage_ratio}                       |
|                | State Fiscal Health                    | Tax Growth: {state_fiscal_health.tax_receipts_yoy_growth}%, Budget: {state_fiscal_health.budget_surplus_deficit_pct_gsp}% |
| **Supplemental** | Ownership                              | [e.g., Concentrated (Top 3 own 75%)]                                                                      |
|                | Cost of Carry (bps)                    | {supplemental_data.cost_of_carry_bps}                                                                     |
|                | Correlation (60d)                      | {supplemental_data.cross_asset_correlation.benchmark_ticker}: {supplemental_data.cross_asset_correlation.correlation_60d} |
|                | Tax Profile                            | [e.g., AMT / In-State Taxable]                                                                            |
|                | Call Risk                              | [e.g., High - Trading to 2y call]                                                                         |
| **Forecasts (5d)** | Prob. Negative News                    | {risk_forecasts.forecasted_values[1].probability_negative_news_pct}% (Acc: {model_performance.negative_news_forecast_accuracy.precision}) |
|                | Spread Widening (bps)                  | {risk_forecasts.forecasted_values[1].credit_spread_oas_bps} (Acc: {model_performance.spread_widening_forecast_accuracy.precision}) |
|                | Volatility (VaR)                       | {risk_forecasts.forecasted_values[1].downside_price_volatility.value} (Acc: {model_performance.volatility_forecast_accuracy.precision}) |

---------------------------------------------------------------------------------------------------
```

## 3. Integration Points
The output of the Risk Synthesis service is consumed by:
- The final UI component presenting information to the trader.