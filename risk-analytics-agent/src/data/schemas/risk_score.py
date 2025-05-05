"""Risk Score schemas for trade risk assessment.

This module defines the Pydantic models for risk scores and related data used in risk analytics.
"""

from datetime import datetime
from enum import Enum
from typing import Dict, List, Optional, Any, Union

from pydantic import Field, validator, root_validator

from .base import BaseSchema, TimestampMixin
from .regime_prediction import MarketRegimeType
from .trade_data import TradeDirection


class RiskFactorType(str, Enum):
    """Enumeration of risk factor types."""
    
    LIQUIDITY = "LIQUIDITY"
    CREDIT = "CREDIT"
    INTEREST_RATE = "INTEREST_RATE"
    VOLATILITY = "VOLATILITY"
    MOMENTUM = "MOMENTUM"
    TECHNICAL = "TECHNICAL"
    SUPPLY_DEMAND = "SUPPLY_DEMAND"
    CONCENTRATION = "CONCENTRATION"
    MARKET_SENTIMENT = "MARKET_SENTIMENT"
    REGULATORY = "REGULATORY"


class RiskFactorSeverity(str, Enum):
    """Enumeration of risk factor severity levels."""
    
    LOW = "LOW"
    MEDIUM = "MEDIUM"
    HIGH = "HIGH"
    CRITICAL = "CRITICAL"


class MitigationAction(str, Enum):
    """Enumeration of risk mitigation actions."""
    
    REDUCE_SIZE = "REDUCE_SIZE"
    STAGE_EXECUTION = "STAGE_EXECUTION"
    ALTERNATIVE_SECURITY = "ALTERNATIVE_SECURITY"
    WAIT_FOR_BETTER_CONDITIONS = "WAIT_FOR_BETTER_CONDITIONS"
    USE_DIFFERENT_VENUE = "USE_DIFFERENT_VENUE"
    IMPROVE_PRICING = "IMPROVE_PRICING"
    HEDGE_POSITION = "HEDGE_POSITION"
    SPREAD_TRADE = "SPREAD_TRADE"


class HistoricalScenario(BaseSchema):
    """Model for historical scenarios similar to the current situation."""
    
    scenario_date: datetime = Field(
        ...,
        description="Date of the historical scenario",
        example="2022-03-15T10:30:00Z"
    )
    
    scenario_description: str = Field(
        ...,
        description="Description of the historical scenario",
        example="March 2022 Fed rate hike cycle"
    )
    
    market_regime: MarketRegimeType = Field(
        ...,
        description="Market regime during the scenario",
        example="RATE_DRIVEN_SELLOFF"
    )
    
    similarity_score: float = Field(
        ...,
        description="Similarity score to current situation (0-1)",
        example=0.85,
        ge=0,
        le=1
    )
    
    risk_score: int = Field(
        ...,
        description="Risk score during the scenario (0-100)",
        example=75,
        ge=0,
        le=100
    )
    
    key_differences: Optional[List[str]] = Field(
        None,
        description="Key differences from current situation",
        example=["Lower overall market volatility", "Higher dealer inventory levels"]
    )
    
    outcome_summary: Optional[str] = Field(
        None,
        description="Summary of the outcome in this scenario",
        example="15% price decline over 2 weeks followed by gradual recovery"
    )


class RiskFactor(BaseSchema):
    """Model for individual risk factors contributing to the risk score."""
    
    factor_type: RiskFactorType = Field(
        ...,
        description="Type of risk factor",
        example="LIQUIDITY"
    )
    
    factor_name: str = Field(
        ...,
        description="Specific name of the risk factor",
        example="BID_ASK_SPREAD_WIDENING"
    )
    
    severity: RiskFactorSeverity = Field(
        ...,
        description="Severity level of the risk factor",
        example="HIGH"
    )
    
    contribution: float = Field(
        ...,
        description="Contribution to overall risk score (0-1)",
        example=0.35,
        ge=0,
        le=1
    )
    
    description: str = Field(
        ...,
        description="Description of the risk factor",
        example="Current bid-ask spread is 2.5x the 30-day average"
    )
    
    raw_values: Optional[Dict[str, Any]] = Field(
        None,
        description="Raw values related to this risk factor",
        example={"current_spread": 1.25, "average_spread": 0.5, "percentile": 95}
    )
    
    regime_specific: bool = Field(
        False,
        description="Whether this factor is specific to the current market regime",
        example=True
    )
    
    @validator("factor_name")
    def validate_factor_name(cls, v: str) -> str:
        """Validate that the factor name is properly formatted."""
        if not v or len(v) > 100:
            raise ValueError(f"Factor name must be between 1 and 100 characters: {v}")
        return v


class RiskMitigation(BaseSchema):
    """Model for risk mitigation recommendations."""
    
    action: MitigationAction = Field(
        ...,
        description="Recommended mitigation action",
        example="REDUCE_SIZE"
    )
    
    description: str = Field(
        ...,
        description="Description of the mitigation action",
        example="Reduce trade size by approximately 40% to improve liquidity profile"
    )
    
    impact_estimate: float = Field(
        ...,
        description="Estimated impact on risk score (0-1 reduction factor)",
        example=0.3,
        ge=0,
        le=1
    )
    
    target_factors: List[RiskFactorType] = Field(
        ...,
        description="Risk factors targeted by this mitigation",
        min_items=1
    )
    
    details: Optional[Dict[str, Any]] = Field(
        None,
        description="Additional details for the mitigation",
        example={"suggested_size": 600000, "expected_price_improvement": "15 bps"}
    )
    
    @validator("target_factors")
    def validate_target_factors(cls, v: List[RiskFactorType]) -> List[RiskFactorType]:
        """Validate that there is at least one target factor."""
        if not v:
            raise ValueError("At least one target factor must be specified")
        return v


class RiskScore(BaseSchema, TimestampMixin):
    """Model for trade risk scores."""
    
    prediction_id: str = Field(
        ...,
        description="Unique identifier for this risk assessment",
        example="risk-score-2023-07-21-001"
    )
    
    security_id: str = Field(
        ...,
        description="Security identifier (CUSIP/ISIN)",
        min_length=9,
        max_length=12,
        example="912828ZQ6"
    )
    
    trade_direction: TradeDirection = Field(
        ...,
        description="Direction of the trade",
        example="BUY"
    )
    
    trade_size: float = Field(
        ...,
        description="Size of the trade (par amount)",
        example=1000000.0,
        gt=0
    )
    
    risk_score: int = Field(
        ...,
        description="Risk score (0-100, higher = more risky)",
        example=75,
        ge=0,
        le=100
    )
    
    market_regime: MarketRegimeType = Field(
        ...,
        description="Current market regime",
        example="VOLATILITY_RISK_OFF"
    )
    
    risk_factors: List[RiskFactor] = Field(
        ...,
        description="Risk factors contributing to the score",
        min_items=1
    )
    
    confidence_score: float = Field(
        ...,
        description="Confidence in the risk assessment (0-1)",
        example=0.85,
        ge=0,
        le=1
    )
    
    similar_scenarios: Optional[List[HistoricalScenario]] = Field(
        None,
        description="Historical scenarios similar to the current situation"
    )
    
    mitigation_suggestions: Optional[List[RiskMitigation]] = Field(
        None,
        description="Suggested risk mitigation actions"
    )
    
    model_version: str = Field(
        ...,
        description="Version of the model used for assessment",
        example="1.3.0"
    )
    
    valid_until: Optional[datetime] = Field(
        None,
        description="Timestamp until which this score is considered valid",
        example="2023-07-21T15:30:15.123Z"
    )
    
    @validator("risk_factors")
    def validate_risk_factors(cls, v: List[RiskFactor]) -> List[RiskFactor]:
        """Validate that there is at least one risk factor."""
        if not v:
            raise ValueError("At least one risk factor must be specified")
        
        # Check that contributions don't exceed 1.0
        total_contribution = sum(factor.contribution for factor in v)
        if total_contribution > 1.01:  # Allow small rounding error
            raise ValueError(f"Total risk factor contribution exceeds 1.0: {total_contribution}")
        
        return v
    
    @validator("valid_until")
    def validate_valid_until(cls, v: Optional[datetime], values: Dict) -> Optional[datetime]:
        """Validate that valid_until is after the prediction timestamp."""
        if v and "timestamp" in values and v <= values["timestamp"]:
            raise ValueError(f"Valid until date must be after prediction timestamp: {v} <= {values['timestamp']}")
        return v
    
    @validator("security_id")
    def validate_security_id(cls, v: str) -> str:
        """Validate security ID format."""
        if len(v) not in [9, 12]:
            raise ValueError(f"Security ID should be 9 chars (CUSIP) or 12 chars (ISIN): {v}")
        return v
    
    class Config:
        """Configuration for the RiskScore model."""
        schema_extra = {
            "example": {
                "predictionId": "risk-score-2023-07-21-001",
                "timestamp": "2023-07-21T14:30:15.123Z",
                "securityId": "912828ZQ6",
                "tradeDirection": "BUY",
                "tradeSize": 1000000.0,
                "riskScore": 75,
                "marketRegime": "VOLATILITY_RISK_OFF",
                "riskFactors": [
                    {
                        "factorType": "LIQUIDITY",
                        "factorName": "BID_ASK_SPREAD_WIDENING",
                        "severity": "HIGH",
                        "contribution": 0.35,
                        "description": "Current bid-ask spread is 2.5x the 30-day average",
                        "rawValues": {
                            "current_spread": 1.25,
                            "average_spread": 0.5,
                            "percentile": 95
                        },
                        "regimeSpecific": True
                    },
                    {
                        "factorType": "VOLATILITY",
                        "factorName": "PRICE_VOLATILITY",
                        "severity": "MEDIUM",
                        "contribution": 0.25,
                        "description": "30-day price volatility is in the 85th percentile",
                        "rawValues": {
                            "current_volatility": 3.2,
                            "average_volatility": 2.1,
                            "percentile": 85
                        },
                        "regimeSpecific": True
                    },
                    {
                        "factorType": "SUPPLY_DEMAND",
                        "factorName": "RECENT_OUTFLOWS",
                        "severity": "MEDIUM",
                        "contribution": 0.2,
                        "description": "Recent sector outflows of $1.2B over the past week",
                        "rawValues": {
                            "weekly_outflow": 1200000000,
                            "percentile": 80
                        },
                        "regimeSpecific": False
                    }
                ],
                "confidenceScore": 0.85,
                "similarScenarios": [
                    {
                        "scenarioDate": "2022-03-15T10:30:00Z",
                        "scenarioDescription": "March 2022 Fed rate hike cycle",
                        "marketRegime": "RATE_DRIVEN_SELLOFF",
                        "similarityScore": 0.85,
                        "riskScore": 82,
                        "keyDifferences": [
                            "Lower overall market volatility",
                            "Higher dealer inventory levels"
                        ],
                        "outcomeSummary": "15% price decline over 2 weeks followed by gradual recovery"
                    }
                ],
                "mitigationSuggestions": [
                    {
                        "action": "REDUCE_SIZE",
                        "description": "Reduce trade size by approximately 40% to improve liquidity profile",
                        "impactEstimate": 0.3,
                        "targetFactors": ["LIQUIDITY", "SUPPLY_DEMAND"],
                        "details": {
                            "suggested_size": 600000,
                            "expected_price_improvement": "15 bps"
                        }
                    },
                    {
                        "action": "STAGE_EXECUTION",
                        "description": "Split execution into 3 tranches over 2 days",
                        "impactEstimate": 0.25,
                        "targetFactors": ["LIQUIDITY", "VOLATILITY"],
                        "details": {
                            "suggested_stages": [
                                {"size": 400000, "timing": "Today"},
                                {"size": 300000, "timing": "Tomorrow AM"},
                                {"size": 300000, "timing": "Tomorrow PM"}
                            ]
                        }
                    }
                ],
                "modelVersion": "1.3.0",
                "validUntil": "2023-07-21T15:30:15.123Z"
            }
        } 