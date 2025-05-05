"""
Confidence Calibration for Risk Models using Conformal Prediction
"""

import numpy as np
import pandas as pd
from typing import List, Tuple, Optional, Dict, Any, Union
import logging

logger = logging.getLogger(__name__)

class ConformalCalibrator:
    """
    Calibrates model confidence using conformal prediction techniques.
    
    This class implements conformal prediction methods to calibrate 
    prediction intervals for risk models.
    """
    
    def __init__(self, alpha: float = 0.1):
        """
        Initialize the conformal calibrator.
        
        Args:
            alpha: Significance level (e.g., 0.1 for 90% confidence intervals)
        """
        self.alpha = alpha
        self.calibration_scores = None
        self.fitted = False
    
    def fit(self, predictions: np.ndarray, ground_truth: np.ndarray) -> None:
        """
        Fit the calibrator using calibration data.
        
        Args:
            predictions: Model predictions on calibration data
            ground_truth: True values for calibration data
        """
        if len(predictions) != len(ground_truth):
            raise ValueError("Predictions and ground truth must have the same length")
        
        # Calculate nonconformity scores (absolute error for regression)
        self.calibration_scores = np.abs(ground_truth - predictions)
        self.fitted = True
        
        logger.info(f"Fitted conformal calibrator with {len(predictions)} samples")
    
    def get_prediction_interval(self, prediction: float, 
                               std_dev: Optional[float] = None) -> Tuple[float, float]:
        """
        Get calibrated prediction interval for a single prediction.
        
        Args:
            prediction: The point prediction from the model
            std_dev: Optional standard deviation estimate
            
        Returns:
            Tuple containing (lower_bound, upper_bound)
        """
        if not self.fitted:
            logger.warning("Calibrator not fitted, using fallback interval")
            return self._fallback_interval(prediction, std_dev)
        
        # Calculate the quantile based on the calibration scores
        n = len(self.calibration_scores)
        q_level = np.ceil((1 - self.alpha) * (n + 1)) / n
        q_hat = np.quantile(self.calibration_scores, min(q_level, 1.0))
        
        # Construct the prediction interval
        lower_bound = prediction - q_hat
        upper_bound = prediction + q_hat
        
        return (lower_bound, upper_bound)
    
    def get_prediction_intervals(self, predictions: np.ndarray, 
                                std_devs: Optional[np.ndarray] = None) -> np.ndarray:
        """
        Get calibrated prediction intervals for multiple predictions.
        
        Args:
            predictions: Array of point predictions
            std_devs: Optional array of standard deviation estimates
            
        Returns:
            Array of tuples containing (lower_bound, upper_bound) for each prediction
        """
        intervals = []
        for i, pred in enumerate(predictions):
            std_dev = std_devs[i] if std_devs is not None else None
            interval = self.get_prediction_interval(pred, std_dev)
            intervals.append(interval)
        
        return np.array(intervals)
    
    def _fallback_interval(self, prediction: float, std_dev: Optional[float] = None) -> Tuple[float, float]:
        """
        Generate a fallback interval when calibration data is not available.
        
        Args:
            prediction: The point prediction
            std_dev: Optional standard deviation estimate
            
        Returns:
            Tuple containing (lower_bound, upper_bound)
        """
        # Use Gaussian assumption with provided standard deviation
        if std_dev is not None:
            # For 90% confidence (alpha=0.1), use z-score of approximately 1.645
            z_score = 1.645
            return (prediction - z_score * std_dev, prediction + z_score * std_dev)
        
        # Fallback with default width of 20% of prediction value
        width = max(abs(prediction) * 0.2, 1.0)
        return (prediction - width, prediction + width)


def calibrate_probabilities(prob_predictions: np.ndarray, 
                          true_labels: np.ndarray, 
                          method: str = 'temperature') -> callable:
    """
    Calibrate classification probability outputs.
    
    Args:
        prob_predictions: Predicted probabilities [n_samples, n_classes]
        true_labels: True class labels
        method: Calibration method ('temperature' or 'isotonic')
        
    Returns:
        Calibration function that can be applied to new probability predictions
    """
    if method not in ['temperature', 'isotonic']:
        raise ValueError(f"Unknown calibration method: {method}")
    
    if method == 'temperature':
        # Simple temperature scaling implementation
        from scipy.optimize import minimize
        
        def nll_loss(T, probs, true_labels):
            # Negative log likelihood loss for temperature scaling
            n_samples = len(probs)
            calibrated_probs = probs ** (1 / T)
            calibrated_probs = calibrated_probs / calibrated_probs.sum(axis=1, keepdims=True)
            
            # Get probability of the true class for each sample
            correct_probs = calibrated_probs[np.arange(n_samples), true_labels]
            nll = -np.log(correct_probs).mean()
            return nll
        
        # Find optimal temperature
        result = minimize(lambda T: nll_loss(T, prob_predictions, true_labels), 
                         x0=1.0, method='Nelder-Mead')
        T_opt = result.x[0]
        
        # Create calibration function
        def calibrate_func(probs: np.ndarray) -> np.ndarray:
            calibrated = probs ** (1 / T_opt)
            return calibrated / calibrated.sum(axis=1, keepdims=True)
        
        logger.info(f"Fitted temperature scaling with T={T_opt:.4f}")
        return calibrate_func
    
    else:  # isotonic regression
        from sklearn.isotonic import IsotonicRegression
        
        # For simplicity, only calibrate the probability of the positive class
        # in binary classification
        if prob_predictions.shape[1] == 2:
            ir = IsotonicRegression(out_of_bounds='clip')
            ir.fit(prob_predictions[:, 1], (true_labels == 1).astype(int))
            
            def calibrate_func(probs: np.ndarray) -> np.ndarray:
                calibrated = probs.copy()
                calibrated[:, 1] = ir.predict(probs[:, 1])
                calibrated[:, 0] = 1 - calibrated[:, 1]
                return calibrated
            
            logger.info("Fitted isotonic regression calibration")
            return calibrate_func
        else:
            # For multi-class, we'd need more complex implementation
            # Fallback to identity for now
            logger.warning("Isotonic calibration for multi-class not implemented")
            return lambda x: x 