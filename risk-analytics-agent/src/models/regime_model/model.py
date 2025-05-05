"""
Temporal Fusion Transformer model for market regime prediction.
Reference: https://pytorch-forecasting.readthedocs.io/en/v1.2.0/api/pytorch_forecasting.models.temporal_fusion_transformer.TemporalFusionTransformer.html
"""

import os
import yaml
import numpy as np
import pandas as pd
import torch
import pytorch_lightning as pl
from pytorch_forecasting.models import TemporalFusionTransformer
from pytorch_forecasting.metrics import MultiLoss, QuantileLoss
from typing import Dict, List, Optional, Tuple, Union
import logging
from pytorch_forecasting.data import TimeSeriesDataSet
from pytorch_lightning import Trainer
from pytorch_lightning.callbacks import EarlyStopping, LearningRateMonitor

logger = logging.getLogger(__name__)

class MarketRegimePredictor:
    """
    Market Regime Predictor using Temporal Fusion Transformer architecture.
    
    This model identifies different market regimes (e.g., calm, volatile, crisis) 
    based on market indicators and time series data.
    """
    
    def __init__(self, config_path: str):
        """
        Initialize the Market Regime Predictor.
        
        Args:
            config_path: Path to the configuration YAML file
        """
        with open(config_path, 'r') as file:
            self.config = yaml.safe_load(file)
            
        self.name = self.config['name']
        self.version = self.config['version']
        self.model = None
        self.trainer = None
        self.training_data = None
        self.validation_data = None
        self.test_data = None
        self.regime_labels = {
            0: "calm_normal",
            1: "volatility_driven",
            2: "liquidity_crisis",
            3: "credit_driven",
            4: "rate_driven",
            5: "strong_inflow",
            6: "technical_dislocation"
        }
    
    def _prepare_data(self, data: pd.DataFrame) -> TimeSeriesDataSet:
        """
        Prepare data for the Temporal Fusion Transformer.
        
        Args:
            data: DataFrame containing time series data
            
        Returns:
            TimeSeriesDataSet for training or inference
        """
        # Create time series dataset with the configuration settings
        max_encoder_length = self.config['model']['params']['max_encoder_length']
        max_prediction_length = self.config['model']['params']['max_prediction_length']
        
        # Identify categorical and continuous variables
        categorical_variables = [col for col in data.columns if data[col].dtype == 'category']
        continuous_variables = [col for col in data.columns if data[col].dtype in ['float64', 'int64']]
        
        # Create the time series dataset
        dataset = TimeSeriesDataSet(
            data=data,
            time_idx="time_idx",  
            target=self.config['data']['label_column'],
            group_ids=["group_id"],  # Typically this would be a market identifier
            max_encoder_length=max_encoder_length,
            max_prediction_length=max_prediction_length,
            categorical_encoders={cat: data[cat].unique().tolist() for cat in categorical_variables},
            time_varying_known_categoricals=categorical_variables,
            time_varying_known_reals=continuous_variables,
            time_varying_unknown_reals=[self.config['data']['label_column']]
        )
        
        return dataset
    
    def train(self, data: pd.DataFrame) -> Dict:
        """
        Train the Market Regime Predictor model.
        
        Args:
            data: DataFrame containing training data
            
        Returns:
            Dictionary with training metrics
        """
        # Prepare data
        self.training_data = self._prepare_data(data)
        train_dataloader = self.training_data.to_dataloader(
            batch_size=self.config['training']['batch_size'],
            train=True
        )
        
        # Create validation set
        validation_data = TimeSeriesDataSet.from_dataset(
            self.training_data, 
            data, 
            min_prediction_idx=data["time_idx"].min() + self.config['model']['params']['max_encoder_length']
        )
        val_dataloader = validation_data.to_dataloader(
            batch_size=self.config['training']['batch_size'],
            train=False
        )
        
        # Initialize TFT model
        self.model = TemporalFusionTransformer.from_dataset(
            self.training_data,
            hidden_size=self.config['model']['params']['hidden_size'],
            attention_head_size=self.config['model']['params']['attention_heads'],
            dropout=self.config['model']['params']['dropout'],
            hidden_continuous_size=self.config['model']['params']['hidden_continuous_size'],
            learning_rate=self.config['model']['params']['learning_rate'],
            lstm_layers=self.config['model']['params']['lstm_layers'],
            hidden_layers=self.config['model']['params']['hidden_layers']
        )
        
        # Configure trainer with callbacks
        early_stop_callback = EarlyStopping(
            monitor="val_loss",
            min_delta=1e-4,
            patience=self.config['training']['early_stopping_patience'],
            verbose=True,
            mode="min"
        )
        lr_monitor = LearningRateMonitor(logging_interval="epoch")
        
        self.trainer = Trainer(
            max_epochs=self.config['training']['epochs'],
            gpus=1 if torch.cuda.is_available() else 0,
            gradient_clip_val=self.config['training']['gradient_clip_val'],
            callbacks=[early_stop_callback, lr_monitor]
        )
        
        # Train the model
        self.trainer.fit(
            self.model,
            train_dataloaders=train_dataloader,
            val_dataloaders=val_dataloader
        )
        
        # Return training metrics
        return {
            "training_loss": self.trainer.callback_metrics["train_loss"].item(),
            "validation_loss": self.trainer.callback_metrics["val_loss"].item(),
            "epochs_trained": self.trainer.current_epoch
        }
    
    def predict(self, data: pd.DataFrame) -> Dict[str, Union[str, float, Dict]]:
        """
        Predict market regime from input data.
        
        Args:
            data: DataFrame containing input features
            
        Returns:
            Dictionary with predicted regime and confidence scores
        """
        if self.model is None:
            raise ValueError("Model has not been trained or loaded yet.")
        
        # Prepare data for prediction
        dataset = self._prepare_data(data)
        dataloader = dataset.to_dataloader(batch_size=1, train=False)
        
        # Get prediction
        prediction = self.model.predict(dataloader)
        
        # Process prediction - assuming classification output
        predicted_class = prediction.argmax(dim=1).item()
        probabilities = torch.softmax(prediction, dim=1)[0].tolist()
        
        # Get regime name from class
        regime = self.regime_labels.get(predicted_class, "unknown")
        
        # Calculate confidence based on probabilities
        confidence = probabilities[predicted_class]
        
        # Return prediction with confidence and probabilities
        return {
            "predicted_regime": regime,
            "confidence": confidence,
            "probabilities": {
                label: probabilities[idx] for idx, label in self.regime_labels.items()
            },
            "raw_prediction": prediction.tolist()[0]
        }
    
    def save(self, path: str) -> None:
        """
        Save the model to disk.
        
        Args:
            path: Directory to save the model
        """
        if self.model is None:
            raise ValueError("No model to save.")
        
        os.makedirs(path, exist_ok=True)
        model_path = os.path.join(path, f"{self.name}_v{self.version}.pt")
        config_path = os.path.join(path, f"{self.name}_config_v{self.version}.yaml")
        
        # Save the PyTorch model
        torch.save(self.model.state_dict(), model_path)
        
        # Save the configuration
        with open(config_path, 'w') as file:
            yaml.dump(self.config, file)
            
        print(f"Model saved to {model_path}")
    
    def load(self, path: str) -> None:
        """
        Load a model from disk.
        
        Args:
            path: Path to the model file
        """
        model_path = os.path.join(path, f"{self.name}_v{self.version}.pt")
        if not os.path.exists(model_path):
            raise FileNotFoundError(f"Model file not found at {model_path}")
        
        # Need to initialize the model first with the original dataset
        if self.training_data is None:
            raise ValueError("Cannot load model without training data. Call load_training_data() first.")
        
        # Initialize the model architecture
        self.model = TemporalFusionTransformer.from_dataset(
            self.training_data,
            hidden_size=self.config['model']['params']['hidden_size'],
            attention_head_size=self.config['model']['params']['attention_heads'],
            dropout=self.config['model']['params']['dropout'],
            hidden_continuous_size=self.config['model']['params']['hidden_continuous_size'],
            learning_rate=self.config['model']['params']['learning_rate'],
            lstm_layers=self.config['model']['params']['lstm_layers'],
            hidden_layers=self.config['model']['params']['hidden_layers']
        )
        
        # Load weights
        self.model.load_state_dict(torch.load(model_path))
        self.model.eval()
        
        print(f"Model loaded from {model_path}")
    
    def load_training_data(self, data: pd.DataFrame) -> None:
        """
        Load training data needed for model initialization when loading saved models.
        
        Args:
            data: DataFrame containing training data
        """
        self.training_data = self._prepare_data(data)
        print("Training data loaded for model initialization") 