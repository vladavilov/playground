# GenAI Model Research Summary

This document summarizes the findings from researching the GenAI models specified in the requirements document for the Pre-Trade Risk Scoring system.

## Model 1: Market Regime Predictor - Temporal Fusion Transformer (TFT)

- **Purpose:** Designed for multi-horizon time series forecasting, leveraging self-attention to capture complex temporal patterns across multiple time steps.
- **Key Features:**
    - Transformer-based architecture.
    - Incorporates static covariates, known future inputs, and other exogenous variables.
    - Includes gating mechanisms to skip unused components of the architecture.
    - Variable selection networks for identifying relevant input variables.
    - Interpretable: Provides insights into feature importance and temporal patterns.
- **Relevance:** Suitable for identifying market regimes by analyzing various market indicators and technical factors over time. Its interpretability aligns with the requirement to identify key characterizing factors for regimes. Pytorch Forecasting library provides an implementation.
- **Sources:**
    - https://aihorizonforecast.substack.com/p/temporal-fusion-transformer-time
    - https://medium.com/ai-simplified-in-plain-english/an-in-depth-exploration-of-temporal-fusion-transformers-for-time-series-forecasting-91e74040a079
    - https://www.mdpi.com/1424-8220/25/3/976
    - https://thesai.org/Downloads/Volume15No7/Paper_13-Temporal_Fusion_Transformers_for_Enhanced_Multivariate_Time_Series.pdf
    - https://www.sciencedirect.com/science/article/abs/pii/S0925231223006239
    - https://pytorch-forecasting.readthedocs.io/en/stable/tutorials/stallion.html

## Model 2: Adaptive Risk Scoring - N-BEATS / DeepAR

### N-BEATS (Neural Basis Expansion Analysis for Interpretable Time Series Forecasting)

- **Purpose:** A deep learning model specifically for univariate time series forecasting, known for its performance often exceeding statistical models.
- **Key Features:**
    - Purely deep learning-based, no time-series-specific components.
    - Architecture based on backward and forward residual links and a stack of fully connected layers.
    - Decomposes the time series into trend and seasonality components, offering interpretability.
    - Can be extended (NBEATSx) to incorporate exogenous variables.
- **Relevance:** Suitable for forecasting tasks within specific market regimes. Its potential for interpretability is valuable. Can be used for predicting price movements or other relevant metrics contributing to the risk score. Pytorch Forecasting library provides an implementation.
- **Sources:**
    - https://arxiv.org/html/2409.00480v2
    - https://www.sciencedirect.com/science/article/pii/S0957417423033043
    - https://medium.com/towards-data-science/n-beats-beating-statistical-models-with-neural-nets-28a4ba4a4de8
    - https://amlgolabs.com/documents/amlgolabs_com_enhance_financial_forecasting_with_n_beats_ai.pdf
    - https://www.researchgate.net/publication/333418084_N-BEATS_Neural_basis_expansion_analysis_for_interpretable_time_series_forecasting

### DeepAR (Deep Autoregressive Recurrent Network)

- **Purpose:** A probabilistic forecasting model using recurrent neural networks (RNNs) for scalar time series.
- **Key Features:**
    - Learns a distribution over future values, providing probabilistic forecasts (prediction intervals) rather than just point forecasts.
    - Uses LSTMs or other RNNs to model the temporal dynamics.
    - Can incorporate covariates (related time series and features).
    - Developed by Amazon and available in tools like AWS SageMaker.
- **Relevance:** Excellent for risk assessment due to its probabilistic nature, allowing for the generation of confidence intervals as required. Suitable for modeling scenarios with uncertainty, like credit-driven or rate-driven selloffs, and providing calibrated uncertainty estimates.
- **Sources:**
    - https://medium.com/the-modern-scientist/revolutionizing-forecasting-harnessing-the-power-of-deepar-for-enhanced-predictive-accuracy-and-a3e1d1102703
    - https://docs.aws.amazon.com/sagemaker/latest/dg/deepar.html
    - https://link.springer.com/article/10.1007/s42521-022-00050-0
    - https://www.researchgate.net/publication/359932146_DeepVaR_a_framework_for_portfolio_risk_assessment_leveraging_probabilistic_deep_neural_networks

## Conclusion

The research confirms the suitability of TFT for market regime prediction due to its ability to handle complex temporal dependencies and provide interpretability. For adaptive risk scoring, both N-BEATS (potentially NBEATSx for covariates) and DeepAR are strong candidates. N-BEATS offers interpretability through decomposition, while DeepAR excels at probabilistic forecasting, crucial for risk assessment and confidence intervals. The choice between N-BEATS and DeepAR for specific regime sub-models can be based on the characteristics of the regime (e.g., DeepAR for high uncertainty regimes, N-BEATS for calmer ones) as suggested in the requirements.
