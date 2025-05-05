# Pipeline Connector for Data Preprocessing Pipeline
"""
This module orchestrates the data flow between preprocessing components:
- Feature engineering
- Risk metrics
- Regime labeling
- Dataset creation
It defines input/output interfaces and documents the transformation sequence.
"""
from .data_preprocessing import preprocess_data_for_model
from .dataset_creator import create_timeseries_dataset
# TODO: Add monitoring hooks (e.g., Prometheus, logging)
# TODO: Add error handling between stages
# TODO: Create visual diagram of the complete data flow

def run_preprocessing_pipeline(raw_df, model_type, is_training=True, **kwargs):
    """Executes the full preprocessing pipeline and returns the TimeSeriesDataSet object."""
    print(f"[Pipeline] Running preprocessing pipeline for {model_type} (is_training={is_training})...")
    preprocessed_df, scaler, encoder = preprocess_data_for_model(raw_df, model_type, is_training, **kwargs)
    # TODO: Add monitoring hook here (e.g., log shape, missing values)
    dataset = create_timeseries_dataset(preprocessed_df, model_type, is_training)
    # TODO: Add monitoring hook here (e.g., log dataset stats)
    print(f"[Pipeline] Pipeline completed for {model_type}.")
    return dataset

# TODO: Document transformation sequence and data schema at each stage
# TODO: Implement pipeline execution controller with error handling 