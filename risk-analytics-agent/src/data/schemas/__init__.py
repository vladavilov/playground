"""Data schemas for risk analytics system.

This module provides Pydantic schemas for all data models used in the system.
"""

from .base import BaseSchema, SourceMetadataMixin, TimestampMixin
from .market_data import (
    AssetClass,
    DataType,
    MarketData,
    MarketDataPoint,
    MetricType,
    Unit,
)
from .trade_data import (
    CounterpartyType,
    ExecutionDetails,
    ExecutionMethod,
    PerformanceMetrics,
    TradeData,
    TradeDirection,
    TradeDisposition,
)
from .security_data import (
    CallFeature,
    CalculatedMetrics,
    CoreAttributes,
    CouponType,
    CreditRating,
    SecurityData,
    SecurityType,
    TaxStatus,
)
from .regime_prediction import (
    MarketRegimeType,
    RegimeIndicator,
    RegimePrediction,
)
from .risk_score import (
    HistoricalScenario,
    MitigationAction,
    RiskFactor,
    RiskFactorSeverity,
    RiskFactorType,
    RiskMitigation,
    RiskScore,
)

__all__ = [
    # Base schemas
    'BaseSchema',
    'TimestampMixin',
    'SourceMetadataMixin',
    
    # Market data
    'DataType',
    'AssetClass',
    'MetricType',
    'Unit',
    'MarketDataPoint',
    'MarketData',
    
    # Trade data
    'TradeDirection',
    'CounterpartyType',
    'ExecutionMethod',
    'TradeDisposition',
    'ExecutionDetails',
    'PerformanceMetrics',
    'TradeData',
    
    # Security data
    'CreditRating',
    'TaxStatus',
    'CouponType',
    'SecurityType',
    'CallFeature',
    'CoreAttributes',
    'CalculatedMetrics',
    'SecurityData',
    
    # Regime prediction
    'MarketRegimeType',
    'RegimeIndicator',
    'RegimePrediction',
    
    # Risk score
    'RiskFactorType',
    'RiskFactorSeverity',
    'MitigationAction',
    'HistoricalScenario',
    'RiskFactor',
    'RiskMitigation',
    'RiskScore',
] 