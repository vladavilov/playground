"""Trade Data schemas for trade history and execution.

This module defines the Pydantic models for trade data used in risk analytics,
based on the data contract specifications.
"""

from datetime import datetime
from enum import Enum
from typing import Dict, List, Optional, Union
from uuid import UUID

from pydantic import Field, validator, constr

from .base import BaseSchema, TimestampMixin
from .market_data import DataType


class TradeDirection(str, Enum):
    """Enumeration of trade directions."""
    
    BUY = "BUY"
    SELL = "SELL"


class CounterpartyType(str, Enum):
    """Enumeration of counterparty types."""
    
    CUSTOMER = "CUSTOMER"
    DEALER = "DEALER"
    INTERDEALER = "INTERDEALER"


class ExecutionMethod(str, Enum):
    """Enumeration of execution methods."""
    
    ELECTRONIC = "ELECTRONIC"
    VOICE = "VOICE"
    AUCTION = "AUCTION"


class TradeDisposition(str, Enum):
    """Enumeration of final trade dispositions."""
    
    MATURITY = "MATURITY"
    SALE = "SALE"
    STILL_HELD = "STILL_HELD"


class ExecutionDetails(BaseSchema):
    """Model for trade execution details."""
    
    timestamp: datetime = Field(
        ...,
        description="Timestamp of the trade execution",
        example="2023-07-21T14:30:15.123Z"
    )
    
    price: float = Field(
        ...,
        description="Execution price (per 100 par)",
        example=99.75,
        gt=0
    )
    
    direction: TradeDirection = Field(
        ...,
        description="Trade direction (buy/sell)",
        example="BUY"
    )
    
    quantity: float = Field(
        ...,
        description="Trade quantity/par amount",
        example=1000000.0,
        gt=0
    )
    
    counterparty_type: Optional[CounterpartyType] = Field(
        None,
        description="Type of counterparty",
        example="DEALER"
    )
    
    masked_counterparty_id: Optional[str] = Field(
        None,
        description="Masked identifier for the counterparty",
        example="DEALER_123"
    )
    
    masked_trader_id: Optional[str] = Field(
        None,
        description="Masked identifier for the trader",
        example="TRADER_456"
    )
    
    execution_method: Optional[ExecutionMethod] = Field(
        None,
        description="Method of execution",
        example="ELECTRONIC"
    )
    
    yield_value: Optional[float] = Field(
        None,
        description="Execution yield (percentage)",
        example=3.45
    )
    
    spread_to_benchmark: Optional[float] = Field(
        None,
        description="Spread to benchmark (basis points)",
        example=45.5
    )
    
    @validator("price")
    def validate_price(cls, v: float) -> float:
        """Validate that the price is within a reasonable range for fixed income."""
        # Most fixed income is priced per 100 par, typically between 0 and 200
        if v <= 0 or v > 200:
            raise ValueError(f"Price should be between 0 and 200 for fixed income: {v}")
        return v
    
    @validator("yield_value")
    def validate_yield(cls, v: Optional[float]) -> Optional[float]:
        """Validate that the yield is within a reasonable range."""
        if v is not None and (v < 0 or v > 100):
            raise ValueError(f"Yield should be between 0 and 100 percent: {v}")
        return v


class PerformanceMetrics(BaseSchema):
    """Model for post-trade performance metrics."""
    
    t1_price_change: Optional[float] = Field(
        None,
        description="Price change after 1 day (%)",
        example=0.25
    )
    
    t5_price_change: Optional[float] = Field(
        None,
        description="Price change after 5 days (%)",
        example=0.75
    )
    
    realized_pnl: Optional[float] = Field(
        None,
        description="Realized profit/loss (USD)",
        example=5000.0
    )
    
    mark_to_market_pnl: Optional[float] = Field(
        None,
        description="Mark-to-market profit/loss (USD)",
        example=7500.0
    )
    
    price_impact: Optional[float] = Field(
        None,
        description="Estimated price impact (bps)",
        example=3.5
    )
    
    opportunity_cost: Optional[float] = Field(
        None,
        description="Estimated opportunity cost (bps)",
        example=2.0
    )
    
    holding_period_volatility: Optional[float] = Field(
        None,
        description="Volatility during holding period (annualized %)",
        example=8.5
    )
    
    holding_duration: Optional[int] = Field(
        None,
        description="Holding duration (days)",
        example=45,
        ge=0
    )
    
    final_disposition: Optional[TradeDisposition] = Field(
        None,
        description="Final disposition of the trade",
        example="SALE"
    )


class TradeData(BaseSchema):
    """Model for trade history data."""
    
    data_type: DataType = Field(
        DataType.TRADE_HISTORY,
        description="Type of data",
        example="TRADE_HISTORY"
    )
    
    trade_id: str = Field(
        ...,
        description="Unique identifier for the trade",
        example="12345678-1234-5678-1234-567812345678"
    )
    
    security_id: str = Field(
        ...,
        description="Security identifier (CUSIP/ISIN)",
        min_length=6,
        max_length=12,
        example="912828ZQ6"
    )
    
    execution_details: ExecutionDetails = Field(
        ...,
        description="Details of the trade execution"
    )
    
    performance_metrics: Optional[PerformanceMetrics] = Field(
        None,
        description="Post-trade performance metrics"
    )
    
    @validator("trade_id")
    def validate_trade_id(cls, v: str) -> str:
        """Validate that the trade ID is properly formatted."""
        # Try to parse as UUID to validate format
        try:
            UUID(v)
        except ValueError:
            # If not a UUID, ensure it's at least a non-empty string
            if not v or len(v) < 8:
                raise ValueError(f"Trade ID must be a UUID or at least 8 characters: {v}")
        return v
    
    @validator("security_id")
    def validate_security_id(cls, v: str) -> str:
        """Validate that the security ID is properly formatted."""
        # Simple validation for CUSIP (9 chars) or ISIN (12 chars)
        if len(v) not in [9, 12]:
            raise ValueError(f"Security ID should be 9 chars (CUSIP) or 12 chars (ISIN): {v}")
        return v
    
    class Config:
        """Configuration for the TradeData model."""
        schema_extra = {
            "example": {
                "dataType": "TRADE_HISTORY",
                "tradeId": "12345678-1234-5678-1234-567812345678",
                "securityId": "912828ZQ6",
                "executionDetails": {
                    "timestamp": "2023-07-21T14:30:15.123Z",
                    "price": 99.75,
                    "direction": "BUY",
                    "quantity": 1000000.0,
                    "counterpartyType": "DEALER",
                    "maskedCounterpartyId": "DEALER_123",
                    "maskedTraderId": "TRADER_456",
                    "executionMethod": "ELECTRONIC",
                    "yieldValue": 3.45,
                    "spreadToBenchmark": 45.5
                },
                "performanceMetrics": {
                    "t1PriceChange": 0.25,
                    "t5PriceChange": 0.75,
                    "realizedPnl": 5000.0,
                    "markToMarketPnl": 7500.0,
                    "priceImpact": 3.5,
                    "opportunityCost": 2.0,
                    "holdingPeriodVolatility": 8.5,
                    "holdingDuration": 45,
                    "finalDisposition": "SALE"
                }
            }
        } 