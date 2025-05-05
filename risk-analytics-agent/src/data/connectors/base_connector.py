"""Base interfaces and abstract classes for data connectors.

This module defines the core interfaces and abstract base classes for all data 
connectors in the system, ensuring consistent behavior across different data sources.
"""

import logging
import time
from abc import ABC, abstractmethod
from datetime import datetime
from typing import Any, Dict, Generic, List, Optional, TypeVar, Union

# Configure logging
logger = logging.getLogger(__name__)

# Generic type for message content
T = TypeVar('T')


class MessageSchema(ABC):
    """Base class for message schema definitions and validation."""
    
    @classmethod
    @abstractmethod
    def validate(cls, message: Dict[str, Any]) -> bool:
        """Validate that a message conforms to the expected schema.
        
        Args:
            message: The message to validate
            
        Returns:
            bool: True if the message is valid, False otherwise
        """
        pass
    
    @classmethod
    @abstractmethod
    def get_schema_definition(cls) -> Dict[str, Any]:
        """Get the schema definition as a dictionary.
        
        Returns:
            Dict[str, Any]: Schema definition in JSON schema format
        """
        pass


class MessageProcessor(Generic[T], ABC):
    """Interface for processing messages received from data connectors."""
    
    @abstractmethod
    def process_message(self, message: T, metadata: Dict[str, Any]) -> bool:
        """Process a message received from a data connector.
        
        Args:
            message: The deserialized message content
            metadata: Additional information about the message (topic, timestamp, etc.)
            
        Returns:
            bool: True if processing was successful, False otherwise
        """
        pass


class BaseConnector(ABC):
    """Abstract base class for all data connectors."""
    
    def __init__(self, name: str, message_processor: Optional[MessageProcessor] = None):
        """Initialize the base connector.
        
        Args:
            name: Unique name for this connector instance
            message_processor: Component to process received messages
        """
        self.name = name
        self.message_processor = message_processor
        self.connected = False
        self.start_time = None
        self.metrics = {
            "messages_processed": 0,
            "messages_failed": 0,
            "connection_attempts": 0,
            "connection_failures": 0,
            "last_connected_time": None,
            "last_message_time": None
        }
        
    @abstractmethod
    def connect(self) -> bool:
        """Connect to the data source.
        
        Returns:
            bool: True if connection was successful, False otherwise
        """
        pass
    
    @abstractmethod
    def disconnect(self) -> bool:
        """Disconnect from the data source.
        
        Returns:
            bool: True if disconnection was successful, False otherwise
        """
        pass
    
    def health_check(self) -> Dict[str, Any]:
        """Check the health of the connector.
        
        Returns:
            Dict[str, Any]: Health status information including connection state and metrics
        """
        uptime = None
        if self.start_time:
            uptime = datetime.now().timestamp() - self.start_time
            
        health_info = {
            "name": self.name,
            "connected": self.connected,
            "uptime_seconds": uptime,
            "metrics": self.metrics
        }
        
        return health_info
    
    def _update_metrics(self, successful: bool) -> None:
        """Update message processing metrics.
        
        Args:
            successful: Whether message processing was successful
        """
        if successful:
            self.metrics["messages_processed"] += 1
        else:
            self.metrics["messages_failed"] += 1
            
        self.metrics["last_message_time"] = datetime.now().timestamp()


class RetryableError(Exception):
    """Exception indicating that an operation should be retried."""
    
    def __init__(self, message: str, retry_after: Optional[float] = None):
        """Initialize a retryable error.
        
        Args:
            message: Error message
            retry_after: Suggested time to wait before retrying (in seconds)
        """
        super().__init__(message)
        self.retry_after = retry_after


class PermanentError(Exception):
    """Exception indicating that an operation should not be retried."""
    pass


class RetryStrategy:
    """Strategy for retrying operations with exponential backoff and jitter."""
    
    def __init__(
        self,
        max_retries: int = 3,
        initial_backoff: float = 1.0,
        max_backoff: float = 60.0,
        backoff_multiplier: float = 2.0,
        jitter_factor: float = 0.2
    ):
        """Initialize the retry strategy.
        
        Args:
            max_retries: Maximum number of retry attempts
            initial_backoff: Initial backoff time in seconds
            max_backoff: Maximum backoff time in seconds
            backoff_multiplier: Factor to multiply backoff by after each attempt
            jitter_factor: Random factor to add to backoff to prevent thundering herd
        """
        self.max_retries = max_retries
        self.initial_backoff = initial_backoff
        self.max_backoff = max_backoff
        self.backoff_multiplier = backoff_multiplier
        self.jitter_factor = jitter_factor
        
    def execute(self, operation: callable, *args, **kwargs) -> Any:
        """Execute an operation with retry logic.
        
        Args:
            operation: The function to execute
            *args: Positional arguments to pass to the operation
            **kwargs: Keyword arguments to pass to the operation
            
        Returns:
            Any: The result of the operation
            
        Raises:
            PermanentError: If operation fails with a permanent error
            Exception: If operation fails after all retries
        """
        retries = 0
        last_exception = None
        
        while retries <= self.max_retries:
            try:
                return operation(*args, **kwargs)
            except PermanentError as e:
                # Don't retry permanent errors
                raise e
            except RetryableError as e:
                last_exception = e
                retry_after = e.retry_after
                logger.warning(f"Retryable error (attempt {retries}/{self.max_retries}): {e}")
            except Exception as e:
                # Treat all other exceptions as retryable
                last_exception = e
                retry_after = None
                logger.warning(f"Error (attempt {retries}/{self.max_retries}): {e}")
                
            retries += 1
            
            if retries > self.max_retries:
                break
                
            # Calculate backoff time
            if retry_after is not None:
                # Use suggested retry time if provided
                backoff = retry_after
            else:
                # Otherwise use exponential backoff with jitter
                backoff = min(
                    self.max_backoff,
                    self.initial_backoff * (self.backoff_multiplier ** (retries - 1))
                )
                
                # Add jitter to prevent thundering herd
                import random
                jitter = random.uniform(-self.jitter_factor * backoff, self.jitter_factor * backoff)
                backoff = max(0.1, backoff + jitter)
                
            logger.info(f"Retrying in {backoff:.2f} seconds")
            time.sleep(backoff)
            
        # If we get here, all retries failed
        logger.error(f"Operation failed after {self.max_retries} retries")
        if last_exception:
            raise last_exception
        else:
            raise Exception("Operation failed after all retries") 