# Data Preprocessing Pipeline
import pandas as pd
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from .feature_generator import apply_feature_engineering
from .risk_metrics import apply_risk_metric_calculations
from .regime_labeler import label_market_regimes
import joblib
import os

def preprocess_data_for_model(raw_data_df, model_type, is_training=True, scaler_path=None, encoder_path=None, save_transformers=False, transformers_dir=None):
    """Applies feature engineering, risk metrics, labeling (if training),
    and final preprocessing (scaling, encoding) for a specific model.
    Supports sklearn-style transformers and serialization for inference consistency.
    """
    print(f"[Preprocessing] Starting preprocessing for {model_type}...")
    df = apply_feature_engineering(raw_data_df.copy())
    df = apply_risk_metric_calculations(df)
    if model_type == "model1" and is_training:
        df = label_market_regimes(df)
    # TODO: Define columns to scale/encode based on model requirements and data types
    continuous_cols = [] # TODO: Populate based on schema
    categorical_cols = [] # TODO: Populate based on schema
    scaler, encoder = None, None
    if continuous_cols:
        if scaler_path and os.path.exists(scaler_path):
            scaler = joblib.load(scaler_path)
            df[continuous_cols] = scaler.transform(df[continuous_cols])
        else:
            scaler = StandardScaler()
            df[continuous_cols] = scaler.fit_transform(df[continuous_cols])
            if save_transformers and transformers_dir:
                joblib.dump(scaler, os.path.join(transformers_dir, f"{model_type}_scaler.joblib"))
        print("[Preprocessing] Continuous features scaled.")
    if categorical_cols:
        if encoder_path and os.path.exists(encoder_path):
            encoder = joblib.load(encoder_path)
            encoded_data = encoder.transform(df[categorical_cols])
        else:
            encoder = OneHotEncoder(sparse_output=False, handle_unknown='ignore')
            encoded_data = encoder.fit_transform(df[categorical_cols])
            if save_transformers and transformers_dir:
                joblib.dump(encoder, os.path.join(transformers_dir, f"{model_type}_encoder.joblib"))
        encoded_df = pd.DataFrame(encoded_data, columns=encoder.get_feature_names_out(categorical_cols), index=df.index)
        df = pd.concat([df.drop(columns=categorical_cols), encoded_df], axis=1)
        print("[Preprocessing] Categorical features encoded.")
    # TODO: Ensure all necessary columns for TimeSeriesDataSet are present and correctly formatted
    print(f"[Preprocessing] Preprocessing for {model_type} completed.")
    return df, scaler, encoder 