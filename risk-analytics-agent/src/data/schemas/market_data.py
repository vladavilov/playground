"""Market Data schemas for fixed income analytics.

This module defines the Pydantic models for market data used in risk analytics,
based on the data contract specifications.
"""

from datetime import datetime
from enum import Enum
from typing import Dict, List, Optional, Union

from pydantic import Field, validator

from .base import BaseSchema, SourceMetadataMixin, TimestampMixin


class DataType(str, Enum):
    """Enumeration of supported data types."""
    
    MARKET_DATA = "MARKET_DATA"
    TRADE_HISTORY = "TRADE_HISTORY"
    SECURITY_DATA = "SECURITY_DATA"


class AssetClass(str, Enum):
    """Enumeration of supported asset classes."""
    
    FIXED_INCOME = "FIXED_INCOME"
    EQUITIES = "EQUITIES"
    FX = "FX"
    COMMODITIES = "COMMODITIES"


class MetricType(str, Enum):
    """Enumeration of market data metric types."""
    
    YIELD_CURVE = "YIELD_CURVE"
    INDEX = "INDEX"
    FUND_FLOW = "FUND_FLOW"
    LIQUIDITY_MEASURE = "LIQUIDITY_MEASURE"
    VOLATILITY_MEASURE = "VOLATILITY_MEASURE"
    CREDIT_SPREAD = "CREDIT_SPREAD"
    RISK_PREMIUM = "RISK_PREMIUM"
    TECHNICAL_INDICATOR = "TECHNICAL_INDICATOR"


class Unit(str, Enum):
    """Enumeration of units used for market data metrics."""
    
    PERCENT = "PERCENT"
    BPS = "BPS"
    USD = "USD"
    EUR = "EUR"
    JPY = "JPY"
    GBP = "GBP"
    RATIO = "RATIO"
    INDEX = "INDEX"
    BOOLEAN = "BOOLEAN"
    COUNT = "COUNT"
    DAYS = "DAYS"


class MarketDataPoint(BaseSchema):
    """Model for a single market data point."""
    
    metric_type: MetricType = Field(
        ...,
        description="Type of market data metric",
        example="YIELD_CURVE"
    )
    
    key: str = Field(
        ...,
        description="Specific key for the metric",
        example="US_TREASURY_10Y"
    )
    
    value: float = Field(
        ...,
        description="Value of the metric",
        example=3.45
    )
    
    unit: Unit = Field(
        ...,
        description="Unit of measurement for the value",
        example="PERCENT"
    )
    
    @validator("key")
    def validate_key(cls, v: str) -> str:
        """Validate that the key is properly formatted."""
        if not v or len(v) > 100:
            raise ValueError(f"Key must be between 1 and 100 characters: {v}")
        return v
    
    @validator("value")
    def validate_value(cls, v: float, values: Dict) -> float:
        """Validate that the value is appropriate for the unit."""
        if "unit" in values:
            unit = values["unit"]
            
            # Percentage values should generally be between 0 and 100
            if unit == Unit.PERCENT and (v < 0 or v > 100):
                raise ValueError(f"Percentage value should be between 0 and 100: {v}")
                
            # Basis points are typically positive and below 10000
            if unit == Unit.BPS and (v < 0 or v > 10000):
                raise ValueError(f"BPS value should be between 0 and 10000: {v}")
                
        return v


class MarketData(BaseSchema, TimestampMixin, SourceMetadataMixin):
    """Model for market data messages from the fixed income Kafka stream."""
    
    data_type: DataType = Field(
        DataType.MARKET_DATA,
        description="Type of data",
        example="MARKET_DATA"
    )
    
    asset_class: AssetClass = Field(
        AssetClass.FIXED_INCOME,
        description="Asset class for the market data",
        example="FIXED_INCOME"
    )
    
    data_points: List[MarketDataPoint] = Field(
        ...,
        description="Collection of market data points",
        min_items=1
    )
    
    @validator("data_points")
    def validate_data_points(cls, v: List[MarketDataPoint]) -> List[MarketDataPoint]:
        """Validate that there's at least one data point."""
        if not v:
            raise ValueError("At least one data point is required")
            
        # Check for duplicate keys
        keys = [dp.key for dp in v]
        if len(keys) != len(set(keys)):
            raise ValueError("Duplicate keys found in data points")
            
        return v
    
    class Config:
        """Configuration for the MarketData model."""
        schema_extra = {
            "example": {
                "dataType": "MARKET_DATA",
                "assetClass": "FIXED_INCOME",
                "timestamp": "2023-07-21T14:30:15.123Z",
                "dataPoints": [
                    {
                        "metricType": "YIELD_CURVE",
                        "key": "US_TREASURY_10Y",
                        "value": 3.45,
                        "unit": "PERCENT"
                    },
                    {
                        "metricType": "INDEX",
                        "key": "MOVE_INDEX",
                        "value": 120.5,
                        "unit": "INDEX"
                    },
                    {
                        "metricType": "FUND_FLOW",
                        "key": "MUNI_FUND_WEEKLY",
                        "value": 1.2e9,
                        "unit": "USD"
                    }
                ],
                "metadata": {
                    "source": "BLOOMBERG",
                    "confidence": 0.95,
                    "processedAt": "2023-07-21T14:30:16.001Z"
                }
            }
        } 