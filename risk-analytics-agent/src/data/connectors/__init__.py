"""Data connector package for risk analytics.

This package provides connectors for various data sources, including Kafka streams and REST APIs.
"""

from .base_connector import (
    BaseConnector,
    MessageProcessor,
    MessageSchema,
    PermanentError,
    RetryableError,
    RetryStrategy,
)
from .error_handling import (
    CircuitBreaker,
    ErrorClassifier,
    ErrorSeverity,
    ExponentialBackoff,
    RetryPolicy,
)
from .market_data_connector import DeadLetterQueueHandler, MarketDataConnector
from .trade_history_connector import TradeHistoryConnector

__all__ = [
    # Base connector interfaces
    'BaseConnector',
    'MessageProcessor',
    'MessageSchema',
    'RetryStrategy',
    'PermanentError',
    'RetryableError',
    
    # Error handling
    'CircuitBreaker',
    'ErrorClassifier',
    'ErrorSeverity',
    'ExponentialBackoff',
    'RetryPolicy',
    
    # Connectors
    'MarketDataConnector',
    'TradeHistoryConnector',
    'DeadLetterQueueHandler',
] 