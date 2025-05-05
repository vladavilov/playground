"""Market Regime Prediction schemas.

This module defines the Pydantic models for market regime predictions used in risk analytics.
"""

from datetime import datetime
from enum import Enum
from typing import Dict, List, Optional, Union

from pydantic import Field, validator, root_validator

from .base import BaseSchema, TimestampMixin


class MarketRegimeType(str, Enum):
    """Enumeration of market regime types."""
    
    CALM_NORMAL = "CALM_NORMAL"
    VOLATILITY_RISK_OFF = "VOLATILITY_RISK_OFF"
    LIQUIDITY_CRISIS = "LIQUIDITY_CRISIS"
    CREDIT_DRIVEN_SELLOFF = "CREDIT_DRIVEN_SELLOFF"
    RATE_DRIVEN_SELLOFF = "RATE_DRIVEN_SELLOFF"
    STRONG_INFLOW_RALLY = "STRONG_INFLOW_RALLY"
    TECHNICAL_DISLOCATION = "TECHNICAL_DISLOCATION"


class RegimeIndicator(BaseSchema):
    """Model for individual regime indicators."""
    
    name: str = Field(
        ...,
        description="Name of the regime indicator",
        example="RATE_VOLATILITY"
    )
    
    value: float = Field(
        ...,
        description="Value of the indicator",
        example=0.85
    )
    
    importance: float = Field(
        ...,
        description="Importance weight of this indicator (0-1)",
        example=0.7,
        ge=0,
        le=1
    )
    
    threshold: Optional[float] = Field(
        None,
        description="Threshold value for this indicator",
        example=0.5
    )
    
    @validator("name")
    def validate_name(cls, v: str) -> str:
        """Validate that the indicator name is valid."""
        if not v or len(v) > 100:
            raise ValueError(f"Indicator name must be between 1 and 100 characters: {v}")
        return v


class RegimePrediction(BaseSchema, TimestampMixin):
    """Model for market regime predictions."""
    
    prediction_id: str = Field(
        ...,
        description="Unique identifier for this prediction",
        example="regime-pred-2023-07-21-001"
    )
    
    current_regime: MarketRegimeType = Field(
        ...,
        description="Current predicted market regime",
        example="CALM_NORMAL"
    )
    
    regime_probabilities: Dict[MarketRegimeType, float] = Field(
        ...,
        description="Probabilities for each possible regime",
        example={
            "CALM_NORMAL": 0.75,
            "VOLATILITY_RISK_OFF": 0.15,
            "LIQUIDITY_CRISIS": 0.02,
            "CREDIT_DRIVEN_SELLOFF": 0.03,
            "RATE_DRIVEN_SELLOFF": 0.03,
            "STRONG_INFLOW_RALLY": 0.01,
            "TECHNICAL_DISLOCATION": 0.01
        }
    )
    
    confidence_score: float = Field(
        ...,
        description="Confidence in the prediction (0-1)",
        example=0.85,
        ge=0,
        le=1
    )
    
    regime_indicators: List[RegimeIndicator] = Field(
        ...,
        description="Key indicators influencing the regime prediction",
        min_items=1
    )
    
    model_version: str = Field(
        ...,
        description="Version of the model used for prediction",
        example="1.2.0"
    )
    
    valid_until: Optional[datetime] = Field(
        None,
        description="Timestamp until which this prediction is considered valid",
        example="2023-07-22T14:30:15.123Z"
    )
    
    previous_regime: Optional[MarketRegimeType] = Field(
        None,
        description="Previous market regime",
        example="VOLATILITY_RISK_OFF"
    )
    
    regime_transition_probability: Optional[float] = Field(
        None,
        description="Probability of regime transition in next period (0-1)",
        example=0.15,
        ge=0,
        le=1
    )
    
    @root_validator
    def validate_probabilities(cls, values: Dict) -> Dict:
        """Validate that regime probabilities sum to approximately 1."""
        probabilities = values.get("regime_probabilities")
        if probabilities:
            total = sum(probabilities.values())
            if not 0.99 <= total <= 1.01:
                raise ValueError(f"Regime probabilities must sum to approximately 1.0: {total}")
                
            # Ensure the current_regime has the highest probability
            current_regime = values.get("current_regime")
            if current_regime and probabilities:
                max_regime = max(probabilities, key=probabilities.get)
                if current_regime != max_regime:
                    raise ValueError(
                        f"Current regime ({current_regime}) should have the highest probability, "
                        f"but {max_regime} has higher probability"
                    )
        
        return values
    
    @validator("valid_until")
    def validate_valid_until(cls, v: Optional[datetime], values: Dict) -> Optional[datetime]:
        """Validate that valid_until is after the prediction timestamp."""
        if v and "timestamp" in values and v <= values["timestamp"]:
            raise ValueError(f"Valid until date must be after prediction timestamp: {v} <= {values['timestamp']}")
        return v
    
    class Config:
        """Configuration for the RegimePrediction model."""
        schema_extra = {
            "example": {
                "predictionId": "regime-pred-2023-07-21-001",
                "timestamp": "2023-07-21T14:30:15.123Z",
                "currentRegime": "CALM_NORMAL",
                "regimeProbabilities": {
                    "CALM_NORMAL": 0.75,
                    "VOLATILITY_RISK_OFF": 0.15,
                    "LIQUIDITY_CRISIS": 0.02,
                    "CREDIT_DRIVEN_SELLOFF": 0.03,
                    "RATE_DRIVEN_SELLOFF": 0.03,
                    "STRONG_INFLOW_RALLY": 0.01,
                    "TECHNICAL_DISLOCATION": 0.01
                },
                "confidenceScore": 0.85,
                "regimeIndicators": [
                    {
                        "name": "RATE_VOLATILITY",
                        "value": 0.85,
                        "importance": 0.7,
                        "threshold": 0.5
                    },
                    {
                        "name": "CREDIT_SPREADS",
                        "value": 0.25,
                        "importance": 0.6,
                        "threshold": 0.7
                    },
                    {
                        "name": "MARKET_LIQUIDITY",
                        "value": 0.65,
                        "importance": 0.8,
                        "threshold": 0.4
                    }
                ],
                "modelVersion": "1.2.0",
                "validUntil": "2023-07-22T14:30:15.123Z",
                "previousRegime": "VOLATILITY_RISK_OFF",
                "regimeTransitionProbability": 0.15
            }
        } 