"""
Risk scoring models using N-BEATS and DeepAR for different market regimes.
N-BEATS Reference: https://pytorch-forecasting.readthedocs.io/en/v1.2.0/api/pytorch_forecasting.models.nbeats.NBeats.html
DeepAR Reference: https://pytorch-forecasting.readthedocs.io/en/v1.2.0/api/pytorch_forecasting.models.deepar.DeepAR.html
"""

import os
import yaml
import torch
import numpy as np
import pytorch_lightning as pl
from pytorch_forecasting.models import NBeats, DeepAR, TemporalFusionTransformer
from pytorch_forecasting.metrics import QuantileLoss, SMAPE
from typing import Dict, List, Optional, Tuple, Union, Any
import logging
import pandas as pd
import pickle
from sklearn.ensemble import GradientBoostingRegressor
from pytorch_forecasting.data import TimeSeriesDataSet
from pytorch_lightning import Trainer
from pytorch_lightning.callbacks import EarlyStopping, LearningRateMonitor

logger = logging.getLogger(__name__)

class ModelFactory:
    """Factory for creating different model types."""
    
    @staticmethod
    def create_model(model_type: str, config: Dict, training_data):
        """Create a model instance based on type."""
        if model_type.lower() == "nbeats":
            return NBEATSModel(config, training_data)
        elif model_type.lower() == "deepar":
            return DeepARModel(config, training_data)
        elif model_type.lower() == "tcn":
            return TCNModel(config, training_data)
        else:
            raise ValueError(f"Unknown model type: {model_type}")


class NBEATSModel:
    """N-BEATS model for risk score prediction."""
    
    def __init__(self, config: Dict, training_data=None):
        self.config = config
        self.training_data = training_data
        self.model = None
    
    def build_model(self):
        """Build the N-BEATS model."""
        if self.training_data is None:
            raise ValueError("Training data must be provided to build model")
        
        # Get configuration
        interpretation = self.config.get("interpretation", "trend_seasonal")
        num_blocks = self.config.get("num_blocks", [3, 3])
        num_block_layers = self.config.get("num_block_layers", 4)
        expansion_coefficient_length = self.config.get("expansion_coefficient_length", 5)
        dropout = self.config.get("dropout", 0.1)
        
        # Metrics
        loss = SMAPE()
        
        # Create the model
        self.model = NBeats.from_dataset(
            self.training_data,
            learning_rate=self.config.get("learning_rate", 0.001),
            log_interval=10,
            log_val_interval=1,
            widths=[32, 512],
            backcast_loss_ratio=0.1,
            loss=loss,
            interpretation=interpretation,
            num_blocks=num_blocks,
            num_block_layers=num_block_layers,
            expansion_coefficient_length=expansion_coefficient_length,
            dropout=dropout,
        )
        
        logger.info(f"Built N-BEATS model with {self.model.hparams}")
        return self.model
    
    def predict(self, data, num_samples: int = 100) -> Dict[str, Any]:
        """Make predictions with the model."""
        if self.model is None:
            raise ValueError("Model must be built before prediction")
        
        # Generate predictions
        predictions = self.model.predict(data, mode="prediction", return_x=True)
        
        # For uncertainty estimation, use Monte Carlo sampling
        mc_samples = []
        for _ in range(num_samples):
            # Enable dropout
            self.model.train()
            mc_sample = self.model.predict(data, mode="prediction")
            mc_samples.append(mc_sample.unsqueeze(0))
        
        # Stack samples
        mc_samples = torch.cat(mc_samples, dim=0)
        
        # Calculate mean and variance
        mean_prediction = mc_samples.mean(dim=0)
        variance = mc_samples.var(dim=0)
        
        # Calculate confidence intervals
        lower_bound = mean_prediction - 1.96 * torch.sqrt(variance)
        upper_bound = mean_prediction + 1.96 * torch.sqrt(variance)
        
        # Set model back to evaluation mode
        self.model.eval()
        
        # Return predictions with confidence intervals
        return {
            "prediction": predictions.cpu().numpy(),
            "mean": mean_prediction.cpu().numpy(),
            "lower_bound": lower_bound.cpu().numpy(),
            "upper_bound": upper_bound.cpu().numpy(),
            "variance": variance.cpu().numpy()
        }
    
    def save(self, path: str):
        """Save model to disk."""
        if self.model is None:
            raise ValueError("Model must be built before saving")
        
        torch.save(self.model.state_dict(), path)
        logger.info(f"Model saved to {path}")
    
    def load(self, path: str):
        """Load model from disk."""
        if self.model is None:
            raise ValueError("Model must be built before loading weights")
        
        self.model.load_state_dict(torch.load(path))
        self.model.eval()
        logger.info(f"Model loaded from {path}")
        return self.model


class DeepARModel:
    """DeepAR model for risk score prediction."""
    
    def __init__(self, config: Dict, training_data=None):
        self.config = config
        self.training_data = training_data
        self.model = None
    
    def build_model(self):
        """Build the DeepAR model."""
        if self.training_data is None:
            raise ValueError("Training data must be provided to build model")
        
        # Get configuration
        cell_type = self.config.get("cell_type", "LSTM")
        hidden_size = self.config.get("hidden_size", 64)
        rnn_layers = self.config.get("rnn_layers", 2)
        dropout = self.config.get("dropout", 0.1)
        loss = QuantileLoss()
        
        # Create the model
        self.model = DeepAR.from_dataset(
            self.training_data,
            learning_rate=self.config.get("learning_rate", 0.001),
            log_interval=10,
            log_val_interval=1,
            hidden_size=hidden_size,
            rnn_layers=rnn_layers,
            dropout=dropout,
            loss=loss,
            cell_type=cell_type,
        )
        
        logger.info(f"Built DeepAR model with {self.model.hparams}")
        return self.model
    
    def predict(self, data, num_samples: int = 100) -> Dict[str, Any]:
        """Make predictions with the model."""
        if self.model is None:
            raise ValueError("Model must be built before prediction")
        
        # Generate predictions with sample=True to get distribution
        predictions = self.model.predict(
            data, 
            mode="sample", 
            n_samples=num_samples,
            return_x=True
        )
        
        # Calculate statistics from samples
        mean_prediction = predictions.mean(dim=0)
        variance = predictions.var(dim=0)
        
        # Calculate confidence intervals
        lower_bound = mean_prediction - 1.96 * torch.sqrt(variance)
        upper_bound = mean_prediction + 1.96 * torch.sqrt(variance)
        
        # Return predictions with confidence intervals
        return {
            "prediction": mean_prediction.cpu().numpy(),
            "samples": predictions.cpu().numpy(),
            "lower_bound": lower_bound.cpu().numpy(),
            "upper_bound": upper_bound.cpu().numpy(),
            "variance": variance.cpu().numpy()
        }
    
    def save(self, path: str):
        """Save model to disk."""
        if self.model is None:
            raise ValueError("Model must be built before saving")
        
        torch.save(self.model.state_dict(), path)
        logger.info(f"Model saved to {path}")
    
    def load(self, path: str):
        """Load model from disk."""
        if self.model is None:
            raise ValueError("Model must be built before loading weights")
        
        self.model.load_state_dict(torch.load(path))
        self.model.eval()
        logger.info(f"Model loaded from {path}")
        return self.model


class TCNModel:
    """Temporal Convolutional Network model for risk prediction."""
    
    def __init__(self, config: Dict, training_data=None):
        self.config = config
        self.training_data = training_data
        self.model = None
    
    def build_model(self):
        """Build the TCN model."""
        if self.training_data is None:
            raise ValueError("Training data must be provided to build model")
        
        # Since PyTorch Forecasting doesn't have a native TCN model,
        # we would typically implement a custom TCN model here.
        # For this example, we'll use TFT as a placeholder
        self.model = TemporalFusionTransformer.from_dataset(
            self.training_data,
            learning_rate=self.config.get("learning_rate", 0.001),
            hidden_size=self.config.get("num_filters", 64),
            attention_head_size=1,
            dropout=self.config.get("dropout", 0.1),
            hidden_continuous_size=self.config.get("num_filters", 64) // 2,
            loss=SMAPE(),
        )
        
        logger.info(f"Built TCN model with {self.model.hparams}")
        return self.model
    
    def predict(self, data, num_samples: int = 100) -> Dict[str, Any]:
        """Make predictions with the model."""
        if self.model is None:
            raise ValueError("Model must be built before prediction")
        
        # Generate predictions
        predictions = self.model.predict(data, mode="prediction", return_x=True)
        
        # For uncertainty estimation, use Monte Carlo dropout
        self.model.train()  # Enable dropout
        mc_samples = []
        for _ in range(num_samples):
            sample = self.model.predict(data, mode="prediction")
            mc_samples.append(sample.unsqueeze(0))
        
        # Stack samples
        mc_samples = torch.cat(mc_samples, dim=0)
        
        # Calculate statistics
        mean_prediction = mc_samples.mean(dim=0)
        variance = mc_samples.var(dim=0)
        
        # Calculate confidence intervals
        lower_bound = mean_prediction - 1.96 * torch.sqrt(variance)
        upper_bound = mean_prediction + 1.96 * torch.sqrt(variance)
        
        # Set model back to evaluation mode
        self.model.eval()
        
        # Return predictions with confidence intervals
        return {
            "prediction": predictions.cpu().numpy(),
            "mean": mean_prediction.cpu().numpy(),
            "lower_bound": lower_bound.cpu().numpy(),
            "upper_bound": upper_bound.cpu().numpy(),
            "variance": variance.cpu().numpy()
        }
    
    def save(self, path: str):
        """Save model to disk."""
        if self.model is None:
            raise ValueError("Model must be built before saving")
        
        torch.save(self.model.state_dict(), path)
        logger.info(f"Model saved to {path}")
    
    def load(self, path: str):
        """Load model from disk."""
        if self.model is None:
            raise ValueError("Model must be built before loading weights")
        
        self.model.load_state_dict(torch.load(path))
        self.model.eval()
        logger.info(f"Model loaded from {path}")
        return self.model


class AdaptiveRiskScoringModel:
    """
    Adaptive Risk Scoring Model using a meta-learner to select and combine 
    different models based on market regimes and conditions.
    
    This model leverages gradient boosted trees, N-BEATS, and DeepAR models
    to provide accurate risk predictions with uncertainty estimates.
    """
    
    def __init__(self, config_path: str):
        """
        Initialize the Adaptive Risk Scoring Model.
        
        Args:
            config_path: Path to the configuration YAML file
        """
        with open(config_path, 'r') as file:
            self.config = yaml.safe_load(file)
            
        self.name = self.config['name']
        self.version = self.config['version']
        
        # Initialize model containers
        self.base_models = {}
        self.meta_learner = None
        self.training_data = None
        self.feature_importance = None
        
        # Initialize regime-specific model mapping
        self.regime_model_mapping = {
            regime_config['regime']: regime_config['model_type']
            for regime_config in self.config['model']['regime_specific_models']
        }
    
    def _prepare_tabular_data(self, data: pd.DataFrame) -> Tuple[np.ndarray, np.ndarray]:
        """
        Prepare tabular data for gradient boosted tree models.
        
        Args:
            data: DataFrame containing features and target
            
        Returns:
            Tuple of features array and target array
        """
        # Extract features and target
        X = data.drop(columns=[self.config['data']['label_column']])
        y = data[self.config['data']['label_column']]
        
        return X.values, y.values
    
    def _prepare_time_series_data(self, data: pd.DataFrame, model_type: str) -> TimeSeriesDataSet:
        """
        Prepare time series data for neural forecasting models.
        
        Args:
            data: DataFrame containing time series data
            model_type: Type of model ("n_beats" or "deep_ar")
            
        Returns:
            TimeSeriesDataSet for training or inference
        """
        # Find model config
        model_config = None
        for model in self.config['model']['base_models']:
            if model.get('type', model['name']) == model_type:
                model_config = model
                break
        
        if model_config is None:
            raise ValueError(f"Model type {model_type} not found in configuration")
        
        # Create time series dataset
        if model_type == "n_beats":
            context_length = 60  # Default context length for N-BEATS
        elif model_type == "deep_ar":
            context_length = model_config['params']['context_length']
        else:
            context_length = 30  # Default fallback
            
        prediction_length = model_config['params'].get('prediction_length', 5)
        
        # Identify categorical and continuous variables
        categorical_variables = [col for col in data.columns if data[col].dtype == 'category']
        continuous_variables = [col for col in data.columns 
                               if data[col].dtype in ['float64', 'int64'] 
                               and col != self.config['data']['label_column']]
        
        # Create the time series dataset
        dataset = TimeSeriesDataSet(
            data=data,
            time_idx="time_idx",
            target=self.config['data']['label_column'],
            group_ids=["group_id"],  # Typically security identifier
            max_encoder_length=context_length,
            max_prediction_length=prediction_length,
            categorical_encoders={cat: data[cat].unique().tolist() for cat in categorical_variables},
            time_varying_known_categoricals=categorical_variables,
            time_varying_known_reals=continuous_variables,
            time_varying_unknown_reals=[self.config['data']['label_column']]
        )
        
        return dataset
    
    def train(self, data: pd.DataFrame, market_regime: Optional[str] = None) -> Dict:
        """
        Train the Adaptive Risk Scoring model.
        
        Args:
            data: DataFrame containing training data
            market_regime: Current market regime (if None, will train all models)
            
        Returns:
            Dictionary with training metrics
        """
        self.training_data = data
        training_metrics = {}
        
        # Determine which models to train based on market regime
        models_to_train = []
        if market_regime is None:
            # Train all base models
            models_to_train = self.config['model']['base_models']
        else:
            # Train the specific model for this regime
            model_type = self.regime_model_mapping.get(market_regime)
            if model_type is None:
                raise ValueError(f"No model defined for regime {market_regime}")
                
            for model_config in self.config['model']['base_models']:
                if model_config.get('type', model_config['name']) == model_type:
                    models_to_train = [model_config]
                    break
        
        # Train each selected model
        for model_config in models_to_train:
            model_name = model_config['name']
            model_type = model_config.get('type', model_name)
            
            if model_type == "gradient_boosted_trees":
                # Train GBT model
                X, y = self._prepare_tabular_data(data)
                
                model = GradientBoostingRegressor(
                    n_estimators=model_config['params']['n_estimators'],
                    learning_rate=model_config['params']['learning_rate'],
                    max_depth=model_config['params']['max_depth'],
                    subsample=model_config['params']['subsample'],
                    max_features=model_config['params']['colsample_bytree'],
                    random_state=42
                )
                
                model.fit(X, y)
                
                # Store feature importance
                if hasattr(model, 'feature_importances_'):
                    self.feature_importance = {
                        feature: importance 
                        for feature, importance in zip(data.drop(columns=[self.config['data']['label_column']]).columns, 
                                                      model.feature_importances_)
                    }
                
                # Calculate training metrics
                train_score = model.score(X, y)
                training_metrics[model_name] = {
                    "r2_score": train_score,
                    "model_type": "gradient_boosted_trees"
                }
                
                # Store the model
                self.base_models[model_name] = model
                
            elif model_type in ["n_beats", "neural_forecaster"]:
                # Train N-BEATS model
                ts_dataset = self._prepare_time_series_data(data, "n_beats")
                train_dataloader = ts_dataset.to_dataloader(
                    batch_size=self.config['training']['batch_size'],
                    train=True
                )
                
                # Create validation set
                validation_data = TimeSeriesDataSet.from_dataset(
                    ts_dataset, 
                    data, 
                    min_prediction_idx=data["time_idx"].min() + 60  # Context length
                )
                val_dataloader = validation_data.to_dataloader(
                    batch_size=self.config['training']['batch_size'],
                    train=False
                )
                
                # Initialize N-BEATS model
                model = NBeats.from_dataset(
                    ts_dataset,
                    stack_types=model_config['params']['stack_types'],
                    num_blocks=[model_config['params']['num_blocks_per_stack']] * len(model_config['params']['stack_types']),
                    num_block_layers=4,  # Default
                    widths=[model_config['params']['hidden_layer_units']] * 4,
                    sharing=model_config['params']['share_weights_in_stack'],
                    expansion_coefficient_lengths=model_config['params']['thetas_dims']
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
                
                trainer = Trainer(
                    max_epochs=self.config['training']['epochs'],
                    gpus=1 if torch.cuda.is_available() else 0,
                    callbacks=[early_stop_callback, lr_monitor]
                )
                
                # Train the model
                trainer.fit(
                    model,
                    train_dataloaders=train_dataloader,
                    val_dataloaders=val_dataloader
                )
                
                # Store training metrics
                training_metrics[model_name] = {
                    "training_loss": trainer.callback_metrics["train_loss"].item(),
                    "validation_loss": trainer.callback_metrics["val_loss"].item(),
                    "epochs_trained": trainer.current_epoch,
                    "model_type": "n_beats"
                }
                
                # Store the model
                self.base_models[model_name] = model
                
            elif model_type == "deep_ar":
                # Train DeepAR model
                ts_dataset = self._prepare_time_series_data(data, "deep_ar")
                train_dataloader = ts_dataset.to_dataloader(
                    batch_size=self.config['training']['batch_size'],
                    train=True
                )
                
                # Create validation set
                validation_data = TimeSeriesDataSet.from_dataset(
                    ts_dataset, 
                    data, 
                    min_prediction_idx=data["time_idx"].min() + model_config['params']['context_length']
                )
                val_dataloader = validation_data.to_dataloader(
                    batch_size=self.config['training']['batch_size'],
                    train=False
                )
                
                # Initialize DeepAR model
                model = DeepAR.from_dataset(
                    ts_dataset,
                    hidden_size=model_config['params']['hidden_size'],
                    rnn_layers=model_config['params']['rnn_layers'],
                    dropout=model_config['params']['dropout'],
                    learning_rate=0.001,  # Default
                    log_interval=10,
                    log_val_interval=1
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
                
                trainer = Trainer(
                    max_epochs=self.config['training']['epochs'],
                    gpus=1 if torch.cuda.is_available() else 0,
                    callbacks=[early_stop_callback, lr_monitor]
                )
                
                # Train the model
                trainer.fit(
                    model,
                    train_dataloaders=train_dataloader,
                    val_dataloaders=val_dataloader
                )
                
                # Store training metrics
                training_metrics[model_name] = {
                    "training_loss": trainer.callback_metrics["train_loss"].item(),
                    "validation_loss": trainer.callback_metrics["val_loss"].item(),
                    "epochs_trained": trainer.current_epoch,
                    "model_type": "deep_ar"
                }
                
                # Store the model
                self.base_models[model_name] = model
        
        # Train meta-learner if all base models are trained
        if len(self.base_models) == len(self.config['model']['base_models']) and self.meta_learner is None:
            self._train_meta_learner(data)
            training_metrics["meta_learner"] = {
                "status": "trained",
                "model_type": self.config['model']['meta_learner']['type']
            }
        
        return training_metrics
    
    def _train_meta_learner(self, data: pd.DataFrame) -> None:
        """
        Train the meta-learner model that decides which base model to use.
        
        Args:
            data: DataFrame containing training data
        """
        # Get market features for meta-learner
        market_features = []
        for feature_category in ['market', 'security']:
            if feature_category in self.config['features']:
                market_features.extend(self.config['features'][feature_category])
        
        # Keep only available columns
        market_features = [col for col in market_features if col in data.columns]
        
        if not market_features:
            # If no specific market features are available, use all features
            X = data.drop(columns=[self.config['data']['label_column']])
        else:
            X = data[market_features]
        
        y = data[self.config['data']['label_column']]
        
        # Initialize and train meta-learner (using GBT as default)
        meta_params = self.config['model']['meta_learner']['params']
        self.meta_learner = GradientBoostingRegressor(
            n_estimators=meta_params['n_estimators'],
            learning_rate=meta_params['learning_rate'],
            max_depth=meta_params['max_depth'],
            random_state=42
        )
        
        self.meta_learner.fit(X, y)
    
    def predict(self, data: pd.DataFrame, market_regime: Optional[str] = None) -> Dict[str, Any]:
        """
        Generate risk predictions using the appropriate model(s) for the given data.
        
        Args:
            data: DataFrame containing input features
            market_regime: Current market regime (if provided, uses the corresponding model)
            
        Returns:
            Dictionary with risk prediction, confidence intervals, and contributing factors
        """
        if not self.base_models:
            raise ValueError("No trained models available. Call train() first.")
        
        # Get predictions from each relevant model
        predictions = {}
        
        if market_regime is not None:
            # Use the specific model for this regime
            model_type = self.regime_model_mapping.get(market_regime)
            if model_type is None:
                raise ValueError(f"No model defined for regime {market_regime}")
            
            # Find the model of this type
            target_model = None
            for model_name, model in self.base_models.items():
                for model_config in self.config['model']['base_models']:
                    if model_config['name'] == model_name and model_config.get('type', model_name) == model_type:
                        target_model = (model_name, model)
                        break
                if target_model:
                    break
            
            if target_model is None:
                raise ValueError(f"No trained model found for type {model_type}")
            
            # Generate prediction from the specific model
            model_name, model = target_model
            if isinstance(model, GradientBoostingRegressor):
                # GBT prediction
                X, _ = self._prepare_tabular_data(data)
                predictions[model_name] = model.predict(X)
            elif isinstance(model, (NBeats, DeepAR)):
                # Neural model prediction
                model_type = "n_beats" if isinstance(model, NBeats) else "deep_ar"
                ts_dataset = self._prepare_time_series_data(data, model_type)
                dataloader = ts_dataset.to_dataloader(batch_size=1, train=False)
                raw_preds = model.predict(dataloader)
                
                # Process prediction
                if isinstance(model, NBeats):
                    # Point prediction
                    predictions[model_name] = raw_preds.mean().item()
                else:  # DeepAR
                    # Probabilistic prediction
                    quantiles = self.config['inference']['quantiles']
                    quantile_preds = model.predict_quantiles(
                        dataloader, quantiles=quantiles
                    )
                    predictions[model_name] = {
                        "mean": raw_preds.mean().item(),
                        "quantiles": {str(q): val.item() for q, val in zip(quantiles, quantile_preds[0])}
                    }
        else:
            # Use meta-learner to combine predictions or select the best model
            for model_name, model in self.base_models.items():
                if isinstance(model, GradientBoostingRegressor):
                    # GBT prediction
                    X, _ = self._prepare_tabular_data(data)
                    predictions[model_name] = model.predict(X)
                elif isinstance(model, (NBeats, DeepAR)):
                    # Neural model prediction - skip for meta-learner case
                    continue
        
        # Final prediction
        if market_regime is not None:
            # Return prediction from the specific model
            model_name = list(predictions.keys())[0]
            if isinstance(predictions[model_name], dict):
                # DeepAR probabilistic prediction
                risk_score = predictions[model_name]["mean"]
                quantiles = predictions[model_name]["quantiles"]
            else:
                # Point prediction
                risk_score = predictions[model_name].item() if hasattr(predictions[model_name], 'item') else predictions[model_name]
                quantiles = None
        else:
            # Use meta-learner or simple average
            if self.meta_learner is not None:
                # Get market features for meta-learner
                market_features = []
                for feature_category in ['market', 'security']:
                    if feature_category in self.config['features']:
                        market_features.extend(self.config['features'][feature_category])
                
                # Keep only available columns
                market_features = [col for col in market_features if col in data.columns]
                
                if not market_features:
                    # If no specific market features are available, use all features
                    X = data.drop(columns=[self.config['data']['label_column']] if self.config['data']['label_column'] in data.columns else [])
                else:
                    X = data[market_features]
                
                risk_score = self.meta_learner.predict(X)[0]
            else:
                # Simple average of available predictions
                values = [p if not isinstance(p, dict) else p["mean"] for p in predictions.values()]
                risk_score = sum(values) / len(values)
            
            # No quantiles for meta-learner
            quantiles = None
        
        # Compile the result
        result = {
            "risk_score": risk_score,
            "confidence": 0.85,  # Default confidence
            "market_regime": market_regime,
        }
        
        # Add quantiles if available
        if quantiles:
            result["prediction_intervals"] = quantiles
        
        # Add feature importance if available
        if self.feature_importance:
            # Sort features by importance
            sorted_features = sorted(self.feature_importance.items(), key=lambda x: x[1], reverse=True)
            result["contributing_factors"] = {feature: importance for feature, importance in sorted_features[:5]}
        
        return result
    
    def save(self, path: str) -> None:
        """
        Save the model to disk.
        
        Args:
            path: Directory to save the model
        """
        if not self.base_models:
            raise ValueError("No trained models to save.")
        
        os.makedirs(path, exist_ok=True)
        
        # Save configuration
        config_path = os.path.join(path, f"{self.name}_config_v{self.version}.yaml")
        with open(config_path, 'w') as file:
            yaml.dump(self.config, file)
        
        # Save each base model
        for model_name, model in self.base_models.items():
            model_path = os.path.join(path, f"{self.name}_{model_name}_v{self.version}")
            
            if isinstance(model, GradientBoostingRegressor):
                # Save sklearn model
                with open(f"{model_path}.pkl", 'wb') as file:
                    pickle.dump(model, file)
            elif isinstance(model, (NBeats, DeepAR)):
                # Save PyTorch model
                model_checkpoint = {
                    "state_dict": model.state_dict(),
                    "hparams": model.hparams
                }
                torch.save(model_checkpoint, f"{model_path}.pt")
        
        # Save meta-learner if available
        if self.meta_learner is not None:
            meta_path = os.path.join(path, f"{self.name}_meta_learner_v{self.version}.pkl")
            with open(meta_path, 'wb') as file:
                pickle.dump(self.meta_learner, file)
        
        # Save feature importance if available
        if self.feature_importance:
            importance_path = os.path.join(path, f"{self.name}_feature_importance_v{self.version}.pkl")
            with open(importance_path, 'wb') as file:
                pickle.dump(self.feature_importance, file)
        
        print(f"Model saved to {path}")
    
    def load(self, path: str) -> None:
        """
        Load the model from disk.
        
        Args:
            path: Directory containing the saved model
        """
        # Load configuration
        config_path = os.path.join(path, f"{self.name}_config_v{self.version}.yaml")
        if not os.path.exists(config_path):
            raise FileNotFoundError(f"Config file not found at {config_path}")
        
        with open(config_path, 'r') as file:
            self.config = yaml.safe_load(file)
        
        # Initialize empty containers
        self.base_models = {}
        
        # Load base models
        for model_config in self.config['model']['base_models']:
            model_name = model_config['name']
            model_type = model_config.get('type', model_name)
            
            if model_type == "gradient_boosted_trees":
                # Load sklearn model
                model_path = os.path.join(path, f"{self.name}_{model_name}_v{self.version}.pkl")
                if os.path.exists(model_path):
                    with open(model_path, 'rb') as file:
                        model = pickle.load(file)
                    self.base_models[model_name] = model
            elif model_type in ["n_beats", "neural_forecaster", "deep_ar"]:
                # Load PyTorch model
                model_path = os.path.join(path, f"{self.name}_{model_name}_v{self.version}.pt")
                if os.path.exists(model_path):
                    # Need to initialize with training data before loading weights
                    print(f"Note: PyTorch models need training data to initialize. Use load_pytorch_model() for {model_name}.")
        
        # Load meta-learner
        meta_path = os.path.join(path, f"{self.name}_meta_learner_v{self.version}.pkl")
        if os.path.exists(meta_path):
            with open(meta_path, 'rb') as file:
                self.meta_learner = pickle.load(file)
        
        # Load feature importance
        importance_path = os.path.join(path, f"{self.name}_feature_importance_v{self.version}.pkl")
        if os.path.exists(importance_path):
            with open(importance_path, 'rb') as file:
                self.feature_importance = pickle.load(file)
        
        print(f"Model loaded from {path}")
    
    def load_pytorch_model(self, model_name: str, path: str, training_data: pd.DataFrame) -> None:
        """
        Load a PyTorch model (N-BEATS or DeepAR) with the required training data.
        
        Args:
            model_name: Name of the model to load
            path: Directory containing the saved model
            training_data: DataFrame containing training data for model initialization
        """
        # Find model config
        model_config = None
        for config in self.config['model']['base_models']:
            if config['name'] == model_name:
                model_config = config
                break
        
        if model_config is None:
            raise ValueError(f"Model {model_name} not found in configuration")
        
        model_type = model_config.get('type', model_name)
        model_path = os.path.join(path, f"{self.name}_{model_name}_v{self.version}.pt")
        
        if not os.path.exists(model_path):
            raise FileNotFoundError(f"Model file not found at {model_path}")
        
        # Prepare dataset for initialization
        ts_dataset = self._prepare_time_series_data(training_data, model_type)
        
        # Load model based on type
        if model_type in ["n_beats", "neural_forecaster"]:
            # Initialize N-BEATS model
            model = NBeats.from_dataset(
                ts_dataset,
                stack_types=model_config['params']['stack_types'],
                num_blocks=[model_config['params']['num_blocks_per_stack']] * len(model_config['params']['stack_types']),
                num_block_layers=4,  # Default
                widths=[model_config['params']['hidden_layer_units']] * 4,
                sharing=model_config['params']['share_weights_in_stack'],
                expansion_coefficient_lengths=model_config['params']['thetas_dims']
            )
        elif model_type == "deep_ar":
            # Initialize DeepAR model
            model = DeepAR.from_dataset(
                ts_dataset,
                hidden_size=model_config['params']['hidden_size'],
                rnn_layers=model_config['params']['rnn_layers'],
                dropout=model_config['params']['dropout'],
                learning_rate=0.001  # Default
            )
        else:
            raise ValueError(f"Unsupported model type: {model_type}")
        
        # Load weights
        checkpoint = torch.load(model_path)
        model.load_state_dict(checkpoint["state_dict"])
        model.eval()
        
        # Store in base_models
        self.base_models[model_name] = model
        
        print(f"PyTorch model {model_name} loaded from {model_path}") 