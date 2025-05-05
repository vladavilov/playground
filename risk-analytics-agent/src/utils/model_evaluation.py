"""
Model Evaluation Utilities for Risk Analytics
"""

import numpy as np
import pandas as pd
from typing import Dict, List, Optional, Union, Tuple, Any
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.metrics import (
    accuracy_score, 
    precision_score, 
    recall_score, 
    f1_score, 
    classification_report,
    confusion_matrix,
    mean_squared_error,
    mean_absolute_error,
    r2_score
)
import torch
import logging

logger = logging.getLogger(__name__)

class ModelEvaluator:
    """
    Base class for model evaluation.
    
    Provides methods to evaluate and visualize model performance.
    """
    
    def __init__(self, model_name: str):
        """
        Initialize model evaluator.
        
        Args:
            model_name: Name of the model
        """
        self.model_name = model_name
        self.evaluation_results = {}
    
    def evaluate(self, y_true: np.ndarray, y_pred: np.ndarray, 
                y_proba: Optional[np.ndarray] = None) -> Dict[str, Any]:
        """
        Evaluate model performance.
        
        Args:
            y_true: Ground truth values/labels
            y_pred: Predicted values/labels
            y_proba: Optional probability predictions (for classification)
            
        Returns:
            Dictionary of evaluation metrics
        """
        # To be implemented by subclasses
        raise NotImplementedError("Subclasses must implement evaluate()")
    
    def plot_results(self, output_path: Optional[str] = None) -> None:
        """
        Plot evaluation results.
        
        Args:
            output_path: Optional path to save the plots
        """
        # To be implemented by subclasses
        raise NotImplementedError("Subclasses must implement plot_results()")
    
    def save_results(self, output_path: str) -> None:
        """
        Save evaluation results to file.
        
        Args:
            output_path: Path to save the results
        """
        if not self.evaluation_results:
            logger.warning("No evaluation results to save")
            return
        
        # Convert to DataFrame for saving
        results_df = pd.DataFrame([self.evaluation_results])
        results_df.to_csv(f"{output_path}/{self.model_name}_evaluation.csv", index=False)
        
        # Save as JSON as well for better readability
        import json
        with open(f"{output_path}/{self.model_name}_evaluation.json", 'w') as f:
            json.dump(self.evaluation_results, f, indent=4)
        
        logger.info(f"Saved evaluation results to {output_path}")


class ClassificationEvaluator(ModelEvaluator):
    """
    Evaluator for classification models.
    
    Provides methods specific to evaluating classification performance.
    """
    
    def __init__(self, model_name: str, class_names: Optional[List[str]] = None):
        """
        Initialize classification evaluator.
        
        Args:
            model_name: Name of the model
            class_names: Optional list of class names
        """
        super().__init__(model_name)
        self.class_names = class_names
    
    def evaluate(self, y_true: np.ndarray, y_pred: np.ndarray, 
                y_proba: Optional[np.ndarray] = None) -> Dict[str, Any]:
        """
        Evaluate classification model performance.
        
        Args:
            y_true: Ground truth labels
            y_pred: Predicted labels
            y_proba: Optional probability predictions
            
        Returns:
            Dictionary of evaluation metrics
        """
        # Calculate basic metrics
        accuracy = accuracy_score(y_true, y_pred)
        
        # Check if binary or multiclass
        unique_classes = np.unique(np.concatenate([y_true, y_pred]))
        is_binary = len(unique_classes) <= 2
        
        if is_binary:
            precision = precision_score(y_true, y_pred, average='binary')
            recall = recall_score(y_true, y_pred, average='binary')
            f1 = f1_score(y_true, y_pred, average='binary')
        else:
            precision = precision_score(y_true, y_pred, average='weighted')
            recall = recall_score(y_true, y_pred, average='weighted')
            f1 = f1_score(y_true, y_pred, average='weighted')
        
        # Calculate confusion matrix
        cm = confusion_matrix(y_true, y_pred)
        
        # Create detailed classification report
        class_names = self.class_names if self.class_names else unique_classes
        report = classification_report(y_true, y_pred, target_names=class_names, output_dict=True)
        
        # Get accuracy per class
        accuracy_per_class = {}
        for i, class_name in enumerate(class_names):
            class_idx = np.where(y_true == i)[0]
            if len(class_idx) > 0:
                class_accuracy = sum(y_pred[class_idx] == i) / len(class_idx)
                accuracy_per_class[class_name] = class_accuracy
        
        # Store results
        self.evaluation_results = {
            "accuracy": accuracy,
            "precision": precision,
            "recall": recall,
            "f1_score": f1,
            "confusion_matrix": cm.tolist(),
            "classification_report": report,
            "accuracy_per_class": accuracy_per_class
        }
        
        # Add additional metrics for binary classification
        if is_binary and y_proba is not None and y_proba.shape[1] >= 2:
            from sklearn.metrics import roc_auc_score, average_precision_score
            try:
                # ROC AUC
                if y_proba.shape[1] == 2:
                    roc_auc = roc_auc_score(y_true, y_proba[:, 1])
                else:
                    roc_auc = roc_auc_score(y_true, y_proba, multi_class='ovr')
                
                # PR AUC (Average Precision)
                if y_proba.shape[1] == 2:
                    pr_auc = average_precision_score(y_true, y_proba[:, 1])
                else:
                    # One-vs-Rest approach for multiclass
                    pr_auc = average_precision_score(
                        pd.get_dummies(y_true).values, y_proba, average='macro'
                    )
                
                self.evaluation_results["roc_auc"] = roc_auc
                self.evaluation_results["pr_auc"] = pr_auc
            except Exception as e:
                logger.warning(f"Error calculating ROC/PR AUC: {e}")
        
        return self.evaluation_results
    
    def plot_results(self, output_path: Optional[str] = None) -> None:
        """
        Plot classification evaluation results.
        
        Args:
            output_path: Optional path to save the plots
        """
        if not self.evaluation_results:
            logger.warning("No evaluation results to plot")
            return
        
        # Plot confusion matrix
        plt.figure(figsize=(10, 8))
        cm = np.array(self.evaluation_results["confusion_matrix"])
        class_names = self.class_names if self.class_names else [f"Class {i}" for i in range(cm.shape[0])]
        
        # Create normalized confusion matrix
        cm_norm = cm.astype('float') / cm.sum(axis=1)[:, np.newaxis]
        
        sns.heatmap(cm_norm, annot=cm, fmt='d', cmap='Blues', 
                   xticklabels=class_names, yticklabels=class_names)
        plt.title(f"{self.model_name} - Confusion Matrix")
        plt.ylabel('True Label')
        plt.xlabel('Predicted Label')
        
        if output_path:
            plt.savefig(f"{output_path}/{self.model_name}_confusion_matrix.png", dpi=300, bbox_inches='tight')
        else:
            plt.show()
        
        # Plot accuracy by class
        plt.figure(figsize=(12, 6))
        accuracy_by_class = self.evaluation_results.get("accuracy_per_class", {})
        if accuracy_by_class:
            classes = list(accuracy_by_class.keys())
            accuracies = list(accuracy_by_class.values())
            
            plt.bar(classes, accuracies)
            plt.axhline(y=self.evaluation_results["accuracy"], color='r', linestyle='-', label=f'Overall Accuracy: {self.evaluation_results["accuracy"]:.2f}')
            plt.ylabel('Accuracy')
            plt.xlabel('Class')
            plt.title(f"{self.model_name} - Accuracy by Class")
            plt.xticks(rotation=45)
            plt.ylim(0, 1.0)
            plt.legend()
            
            if output_path:
                plt.savefig(f"{output_path}/{self.model_name}_accuracy_by_class.png", dpi=300, bbox_inches='tight')
            else:
                plt.show()


class RegressionEvaluator(ModelEvaluator):
    """
    Evaluator for regression models.
    
    Provides methods specific to evaluating regression performance.
    """
    
    def evaluate(self, y_true: np.ndarray, y_pred: np.ndarray, 
                prediction_intervals: Optional[np.ndarray] = None) -> Dict[str, Any]:
        """
        Evaluate regression model performance.
        
        Args:
            y_true: Ground truth values
            y_pred: Predicted values
            prediction_intervals: Optional prediction intervals (lower, upper)
            
        Returns:
            Dictionary of evaluation metrics
        """
        # Calculate basic metrics
        mse = mean_squared_error(y_true, y_pred)
        rmse = np.sqrt(mse)
        mae = mean_absolute_error(y_true, y_pred)
        r2 = r2_score(y_true, y_pred)
        
        # Calculate mean absolute percentage error (MAPE)
        mape = np.mean(np.abs((y_true - y_pred) / np.maximum(1e-6, np.abs(y_true)))) * 100
        
        # Calculate median absolute error
        median_ae = np.median(np.abs(y_true - y_pred))
        
        # Store results
        self.evaluation_results = {
            "mse": mse,
            "rmse": rmse,
            "mae": mae,
            "r2": r2,
            "mape": mape,
            "median_ae": median_ae
        }
        
        # Evaluate prediction intervals if provided
        if prediction_intervals is not None:
            lower_bounds = prediction_intervals[:, 0]
            upper_bounds = prediction_intervals[:, 1]
            
            # Calculate interval width
            interval_width = upper_bounds - lower_bounds
            avg_interval_width = np.mean(interval_width)
            
            # Calculate coverage (proportion of true values within interval)
            coverage = np.mean((y_true >= lower_bounds) & (y_true <= upper_bounds))
            
            # Add to results
            self.evaluation_results["avg_interval_width"] = avg_interval_width
            self.evaluation_results["interval_coverage"] = coverage
        
        return self.evaluation_results
    
    def plot_results(self, output_path: Optional[str] = None) -> None:
        """
        Plot regression evaluation results.
        
        Args:
            output_path: Optional path to save the plots
        """
        if not hasattr(self, 'y_true') or not hasattr(self, 'y_pred'):
            logger.warning("Need to store y_true and y_pred for plotting. Call plot_with_data() instead.")
            return
        
        self.plot_with_data(self.y_true, self.y_pred, output_path)
    
    def plot_with_data(self, y_true: np.ndarray, y_pred: np.ndarray, 
                      prediction_intervals: Optional[np.ndarray] = None,
                      output_path: Optional[str] = None) -> None:
        """
        Plot regression results with provided data.
        
        Args:
            y_true: Ground truth values
            y_pred: Predicted values
            prediction_intervals: Optional prediction intervals (lower, upper)
            output_path: Optional path to save the plots
        """
        # Store data for later use
        self.y_true = y_true
        self.y_pred = y_pred
        
        # Create scatter plot of actual vs predicted
        plt.figure(figsize=(10, 8))
        plt.scatter(y_true, y_pred, alpha=0.5)
        
        # Add diagonal line (perfect predictions)
        min_val = min(np.min(y_true), np.min(y_pred))
        max_val = max(np.max(y_true), np.max(y_pred))
        plt.plot([min_val, max_val], [min_val, max_val], 'r--')
        
        plt.xlabel('Actual Values')
        plt.ylabel('Predicted Values')
        plt.title(f"{self.model_name} - Actual vs Predicted")
        
        # Add metrics text
        if self.evaluation_results:
            metrics_text = f"RMSE: {self.evaluation_results['rmse']:.4f}\n"
            metrics_text += f"MAE: {self.evaluation_results['mae']:.4f}\n"
            metrics_text += f"R²: {self.evaluation_results['r2']:.4f}"
            
            plt.annotate(metrics_text, xy=(0.05, 0.95), xycoords='axes fraction',
                        bbox=dict(boxstyle="round,pad=0.5", fc="yellow", alpha=0.5),
                        va='top')
        
        if output_path:
            plt.savefig(f"{output_path}/{self.model_name}_actual_vs_predicted.png", dpi=300, bbox_inches='tight')
        else:
            plt.show()
        
        # Plot residuals
        plt.figure(figsize=(10, 6))
        residuals = y_true - y_pred
        plt.scatter(y_pred, residuals, alpha=0.5)
        plt.axhline(y=0, color='r', linestyle='-')
        
        plt.xlabel('Predicted Values')
        plt.ylabel('Residuals')
        plt.title(f"{self.model_name} - Residual Plot")
        
        if output_path:
            plt.savefig(f"{output_path}/{self.model_name}_residuals.png", dpi=300, bbox_inches='tight')
        else:
            plt.show()
        
        # Plot prediction intervals if provided
        if prediction_intervals is not None:
            plt.figure(figsize=(12, 8))
            
            # Sort by true values for better visualization
            idx = np.argsort(y_true)
            sorted_true = y_true[idx]
            sorted_pred = y_pred[idx]
            sorted_lower = prediction_intervals[idx, 0]
            sorted_upper = prediction_intervals[idx, 1]
            
            # Plot a subset if there are too many points
            n_points = len(sorted_true)
            if n_points > 500:
                step = n_points // 500
                indices = np.arange(0, n_points, step)
                sorted_true = sorted_true[indices]
                sorted_pred = sorted_pred[indices]
                sorted_lower = sorted_lower[indices]
                sorted_upper = sorted_upper[indices]
            
            # Plot data
            plt.plot(range(len(sorted_true)), sorted_true, 'b.', label='Actual')
            plt.plot(range(len(sorted_pred)), sorted_pred, 'r.', label='Predicted')
            
            # Plot intervals
            plt.fill_between(range(len(sorted_lower)), sorted_lower, sorted_upper, 
                           color='gray', alpha=0.3, label='Prediction Interval')
            
            plt.xlabel('Sorted Sample Index')
            plt.ylabel('Value')
            plt.title(f"{self.model_name} - Prediction Intervals")
            plt.legend()
            
            # Add coverage information
            if 'interval_coverage' in self.evaluation_results:
                coverage_text = f"Interval Coverage: {self.evaluation_results['interval_coverage']:.2%}"
                plt.annotate(coverage_text, xy=(0.05, 0.95), xycoords='axes fraction',
                           bbox=dict(boxstyle="round,pad=0.5", fc="yellow", alpha=0.5),
                           va='top')
            
            if output_path:
                plt.savefig(f"{output_path}/{self.model_name}_prediction_intervals.png", dpi=300, bbox_inches='tight')
            else:
                plt.show()


def evaluate_torch_model(model, dataloader, device='cpu', classification=True):
    """
    Evaluate PyTorch model on a dataloader.
    
    Args:
        model: PyTorch model
        dataloader: DataLoader with evaluation data
        device: Device to run evaluation on
        classification: Whether this is a classification model
        
    Returns:
        Dictionary with evaluation results
    """
    model.eval()
    y_true_list = []
    y_pred_list = []
    y_proba_list = []
    
    with torch.no_grad():
        for batch in dataloader:
            # Extract inputs and targets (might need to adapt based on your dataloader structure)
            if isinstance(batch, (list, tuple)) and len(batch) >= 2:
                x, y = batch[0], batch[1]
            elif hasattr(batch, 'x') and hasattr(batch, 'y'):
                x, y = batch.x, batch.y
            else:
                logger.error(f"Unsupported batch format: {type(batch)}")
                continue
            
            # Move to device
            if isinstance(x, torch.Tensor):
                x = x.to(device)
            if isinstance(y, torch.Tensor):
                y = y.to(device)
            
            # Forward pass
            outputs = model(x)
            
            # Process outputs based on model type
            if classification:
                # Extract probabilities and predictions
                if isinstance(outputs, torch.Tensor):
                    if outputs.shape[-1] > 1:  # Multi-class
                        probabilities = torch.softmax(outputs, dim=-1)
                        _, predicted = torch.max(outputs, dim=-1)
                    else:  # Binary
                        probabilities = torch.sigmoid(outputs)
                        predicted = (probabilities > 0.5).long()
                else:
                    logger.warning("Output is not a tensor, using custom processing")
                    probabilities = outputs
                    predicted = (probabilities > 0.5).long()
                
                # Store results
                y_true_list.append(y.cpu().numpy())
                y_pred_list.append(predicted.cpu().numpy())
                y_proba_list.append(probabilities.cpu().numpy())
            else:
                # Regression - just store predictions
                y_true_list.append(y.cpu().numpy())
                y_pred_list.append(outputs.cpu().numpy())
    
    # Concatenate batches
    y_true = np.concatenate(y_true_list, axis=0)
    y_pred = np.concatenate(y_pred_list, axis=0)
    
    # Create appropriate evaluator
    if classification:
        y_proba = np.concatenate(y_proba_list, axis=0)
        evaluator = ClassificationEvaluator(model.__class__.__name__)
        results = evaluator.evaluate(y_true, y_pred, y_proba)
    else:
        evaluator = RegressionEvaluator(model.__class__.__name__)
        results = evaluator.evaluate(y_true, y_pred)
    
    return results, evaluator 