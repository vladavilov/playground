# Dataset Creator
import pandas as pd
from pytorch_forecasting import TimeSeriesDataSet
from pytorch_forecasting.data.encoders import GroupNormalizer
from .data_preprocessing import preprocess_data_for_model

def create_timeseries_dataset(raw_df, model_type, is_training=True, **preprocessing_kwargs):
    """Creates the TimeSeriesDataSet object required by pytorch-forecasting models."""
    # 1. Preprocess the data for the specific model
    df, scaler, encoder = preprocess_data_for_model(raw_df, model_type=model_type, is_training=is_training, **preprocessing_kwargs)
    # TODO: Define all dataset parameters based on model_type and schema
    # Placeholder: Example parameters for model1 and model2
    if model_type == "model1":
        target = "target_regime"
        group_ids = ["group_id"]
        static_categoricals = ["group_id"]
        static_reals = []
        time_varying_known_categoricals = ["seasonal_indicator"]
        time_varying_known_reals = ["time_idx"]
        time_varying_unknown_categoricals = []
        time_varying_unknown_reals = ["treasury_10y", "move_index"]
        target_normalizer = None
        max_encoder_length = 60
        max_prediction_length = 1
    elif model_type == "model2":
        target = "target_risk_metric"
        group_ids = ["trade_request_id", "cusip"]
        static_categoricals = ["cusip", "credit_rating"]
        static_reals = ["modified_duration"]
        time_varying_known_categoricals = ["trade_direction", "predicted_regime"]
        time_varying_known_reals = ["trade_par_amount", "time_idx"]
        time_varying_unknown_categoricals = []
        time_varying_unknown_reals = ["liquidity_score", "avg_dealer_spread", "move_index", "target_risk_metric"]
        target_normalizer = GroupNormalizer(groups=["cusip"], transformation="softplus")
        max_encoder_length = 30
        max_prediction_length = 1
    else:
        raise ValueError(f"Unknown model_type: {model_type}")
    # 2. Determine cutoff for training/validation split
    if is_training:
        training_cutoff = df["time_idx"].max() - max_prediction_length
        dataset_df = df[lambda x: x.time_idx <= training_cutoff]
        print(f"[Dataset Creation] Creating training dataset for {model_type}. Cutoff: {training_cutoff}")
    else:
        dataset_df = df
        print(f"[Dataset Creation] Creating validation/inference dataset for {model_type}.")
    if dataset_df.empty:
        print("[Dataset Creation] Warning: DataFrame is empty after applying cutoff.")
        return None
    # 3. Create TimeSeriesDataSet object
    try:
        timeseries_dataset = TimeSeriesDataSet(
            dataset_df,
            time_idx="time_idx",
            target=target,
            group_ids=group_ids,
            max_encoder_length=max_encoder_length,
            max_prediction_length=max_prediction_length,
            static_categoricals=static_categoricals,
            static_reals=static_reals,
            time_varying_known_categoricals=time_varying_known_categoricals,
            time_varying_known_reals=time_varying_known_reals,
            time_varying_unknown_categoricals=time_varying_unknown_categoricals,
            time_varying_unknown_reals=time_varying_unknown_reals,
            target_normalizer=target_normalizer,
            add_relative_time_idx=True,
            add_target_scales=True if target_normalizer is not None else False,
            add_encoder_length=True,
            allow_missing_timesteps=True
        )
        print(f"[Dataset Creation] TimeSeriesDataSet for {model_type} created successfully.")
        # TODO: Save the dataset object if needed
        return timeseries_dataset
    except Exception as e:
        print(f"[Dataset Creation] Error creating TimeSeriesDataSet for {model_type}: {e}")
        print("DataFrame info:")
        dataset_df.info()
        print("DataFrame head:")
        print(dataset_df.head())
        return None 