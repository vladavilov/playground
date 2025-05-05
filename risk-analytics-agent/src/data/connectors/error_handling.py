"""Error handling and retry utilities for data connectors.

This module provides specialized error handling mechanisms, retry policies,
circuit breakers, and other resilience utilities for data connectors.
"""

import functools
import logging
import random
import threading
import time
from datetime import datetime
from enum import Enum
from typing import Any, Callable, Dict, List, Optional, Set, Tuple, Type, Union

from .base_connector import PermanentError, RetryableError

# Configure logging
logger = logging.getLogger(__name__)


class ErrorSeverity(Enum):
    """Enumeration of error severity levels."""
    
    # Minor errors that don't affect core functionality
    LOW = 1
    
    # Errors that partially affect functionality but system can still operate
    MEDIUM = 2
    
    # Critical errors that prevent core functionality from working
    HIGH = 3
    
    # Fatal errors requiring immediate attention and system shutdown
    FATAL = 4


class ErrorClassifier:
    """Classifies exceptions into retryable vs permanent errors."""
    
    def __init__(self):
        """Initialize the error classifier."""
        # Exceptions that are always considered permanent (non-retryable)
        self.permanent_exception_types: Set[Type[Exception]] = {
            ValueError,
            TypeError,
            AssertionError,
            KeyError,
            AttributeError,
            NotImplementedError,
            PermissionError,
        }
        
        # Exceptions that are always considered retryable
        self.retryable_exception_types: Set[Type[Exception]] = {
            ConnectionError,
            TimeoutError,
            ConnectionRefusedError,
            ConnectionResetError,
            ConnectionAbortedError,
        }
        
        # Custom classification functions for more complex cases
        self.custom_classifiers: List[Tuple[Callable[[Exception], bool], bool]] = []
        
    def add_permanent_exception(self, exception_type: Type[Exception]) -> None:
        """Add an exception type to the permanent exceptions list.
        
        Args:
            exception_type: The exception type to add
        """
        self.permanent_exception_types.add(exception_type)
        
    def add_retryable_exception(self, exception_type: Type[Exception]) -> None:
        """Add an exception type to the retryable exceptions list.
        
        Args:
            exception_type: The exception type to add
        """
        self.retryable_exception_types.add(exception_type)
        
    def add_custom_classifier(self, classifier_func: Callable[[Exception], bool], is_retryable: bool) -> None:
        """Add a custom classifier function.
        
        Args:
            classifier_func: Function that returns True if it can classify the exception
            is_retryable: Whether exceptions matched by this classifier are retryable
        """
        self.custom_classifiers.append((classifier_func, is_retryable))
        
    def is_retryable(self, exception: Exception) -> bool:
        """Determine if an exception should be retried.
        
        Args:
            exception: The exception to classify
            
        Returns:
            bool: True if the exception is retryable, False otherwise
        """
        # First, check if it's a known permanent or retryable exception
        if isinstance(exception, PermanentError):
            return False
        if isinstance(exception, RetryableError):
            return True
            
        # Check if it's a known permanent exception type
        for exc_type in self.permanent_exception_types:
            if isinstance(exception, exc_type):
                return False
                
        # Check if it's a known retryable exception type
        for exc_type in self.retryable_exception_types:
            if isinstance(exception, exc_type):
                return True
                
        # Apply custom classifiers
        for classifier_func, is_retryable in self.custom_classifiers:
            if classifier_func(exception):
                return is_retryable
                
        # Default to being retryable for unknown exceptions
        return True
        
    def classify_exception(self, exception: Exception) -> Tuple[bool, Optional[float]]:
        """Classify an exception as retryable or permanent and suggest a retry delay.
        
        Args:
            exception: The exception to classify
            
        Returns:
            Tuple[bool, Optional[float]]: (is_retryable, suggested_retry_delay)
        """
        is_retryable = self.is_retryable(exception)
        
        # Determine suggested retry delay if it's retryable
        retry_delay = None
        if is_retryable:
            if isinstance(exception, RetryableError) and exception.retry_after is not None:
                retry_delay = exception.retry_after
            else:
                # Default delay based on exception type
                if isinstance(exception, TimeoutError):
                    # Timeouts often benefit from a longer delay
                    retry_delay = 5.0
                elif isinstance(exception, ConnectionError):
                    # Connection errors might need some time for system to recover
                    retry_delay = 3.0
                    
        return is_retryable, retry_delay


class CircuitBreaker:
    """Implements the Circuit Breaker pattern to prevent repeated calls to failing services."""
    
    def __init__(
        self,
        failure_threshold: int = 5,
        reset_timeout: float = 60.0,
        half_open_max_calls: int = 1,
        name: str = "default",
        excluded_exceptions: Optional[List[Type[Exception]]] = None
    ):
        """Initialize the circuit breaker.
        
        Args:
            failure_threshold: Number of failures before opening the circuit
            reset_timeout: Time in seconds before transitioning from open to half-open
            half_open_max_calls: Number of calls allowed in half-open state
            name: Name of this circuit breaker for logging purposes
            excluded_exceptions: Exceptions that should not count toward failures
        """
        self.name = name
        self.failure_threshold = failure_threshold
        self.reset_timeout = reset_timeout
        self.half_open_max_calls = half_open_max_calls
        self.excluded_exceptions = excluded_exceptions or []
        
        # State tracking
        self.failures = 0
        self._state = "closed"  # closed, open, half-open
        self.last_failure_time = None
        self.half_open_calls = 0
        
        # Thread safety
        self._lock = threading.RLock()
        
    @property
    def state(self) -> str:
        """Get the current state of the circuit breaker.
        
        Returns:
            str: Current state (closed, open, half-open)
        """
        with self._lock:
            # Check if we should transition from open to half-open based on timeout
            if (
                self._state == "open"
                and self.last_failure_time is not None
                and (datetime.now().timestamp() - self.last_failure_time) > self.reset_timeout
            ):
                self._state = "half-open"
                self.half_open_calls = 0
                logger.info(f"Circuit breaker '{self.name}' transitioned from open to half-open")
                
            return self._state
            
    def record_success(self) -> None:
        """Record a successful operation, potentially closing the circuit."""
        with self._lock:
            current_state = self.state
            
            if current_state == "half-open":
                self.half_open_calls += 1
                
                # If we've had enough successful half-open calls, close the circuit
                if self.half_open_calls >= self.half_open_max_calls:
                    self._state = "closed"
                    self.failures = 0
                    logger.info(f"Circuit breaker '{self.name}' closed after successful half-open calls")
            elif current_state == "closed":
                # Reset failure count on success in closed state
                self.failures = 0
                
    def record_failure(self, exception: Optional[Exception] = None) -> None:
        """Record a failed operation, potentially opening the circuit.
        
        Args:
            exception: The exception that caused the failure
        """
        # Don't count excluded exceptions as failures
        if exception and any(isinstance(exception, exc_type) for exc_type in self.excluded_exceptions):
            return
            
        with self._lock:
            current_state = self.state
            
            if current_state == "closed":
                self.failures += 1
                
                if self.failures >= self.failure_threshold:
                    self._state = "open"
                    self.last_failure_time = datetime.now().timestamp()
                    logger.warning(f"Circuit breaker '{self.name}' opened after {self.failures} failures")
                    
            elif current_state == "half-open":
                self._state = "open"
                self.last_failure_time = datetime.now().timestamp()
                logger.warning(f"Circuit breaker '{self.name}' reopened after failure in half-open state")
                
    def allow_request(self) -> bool:
        """Check if a request should be allowed based on the circuit state.
        
        Returns:
            bool: True if the request should be allowed, False otherwise
        """
        with self._lock:
            current_state = self.state
            
            if current_state == "closed":
                return True
            elif current_state == "half-open" and self.half_open_calls < self.half_open_max_calls:
                return True
            else:
                return False
                
    def __call__(self, func: Callable) -> Callable:
        """Decorator to apply circuit breaker to a function.
        
        Args:
            func: The function to wrap with circuit breaker logic
            
        Returns:
            Callable: The wrapped function
        """
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            if not self.allow_request():
                raise RetryableError(
                    f"Circuit breaker '{self.name}' is open",
                    retry_after=self.reset_timeout
                )
                
            try:
                result = func(*args, **kwargs)
                self.record_success()
                return result
            except Exception as e:
                self.record_failure(e)
                raise
                
        return wrapper


class ExponentialBackoff:
    """Implements exponential backoff strategy with jitter for retry delays."""
    
    def __init__(
        self,
        initial_delay: float = 1.0,
        max_delay: float = 60.0,
        multiplier: float = 2.0,
        jitter_factor: float = 0.25
    ):
        """Initialize the exponential backoff strategy.
        
        Args:
            initial_delay: Initial delay in seconds
            max_delay: Maximum delay in seconds
            multiplier: Multiplier for exponential backoff
            jitter_factor: Random factor to add to prevent thundering herd
        """
        self.initial_delay = initial_delay
        self.max_delay = max_delay
        self.multiplier = multiplier
        self.jitter_factor = jitter_factor
        
    def get_delay(self, retry_count: int) -> float:
        """Calculate the delay for a given retry count.
        
        Args:
            retry_count: Current retry attempt number (0-based)
            
        Returns:
            float: Delay in seconds
        """
        # Calculate base delay with exponential backoff
        delay = min(
            self.max_delay,
            self.initial_delay * (self.multiplier ** retry_count)
        )
        
        # Add jitter to prevent thundering herd
        jitter = random.uniform(-self.jitter_factor * delay, self.jitter_factor * delay)
        delay = max(0.001, delay + jitter)  # Ensure positive delay
        
        return delay


class RetryPolicy:
    """Comprehensive retry policy with support for various retry strategies."""
    
    def __init__(
        self,
        max_retries: int = 3,
        backoff_strategy: ExponentialBackoff = None,
        error_classifier: ErrorClassifier = None,
        circuit_breaker: Optional[CircuitBreaker] = None,
        on_retry_callback: Optional[Callable[[Exception, int, float], None]] = None
    ):
        """Initialize the retry policy.
        
        Args:
            max_retries: Maximum number of retry attempts
            backoff_strategy: Strategy for calculating retry delays
            error_classifier: Classifier for determining which errors to retry
            circuit_breaker: Optional circuit breaker to prevent calls to failing services
            on_retry_callback: Optional callback function called before each retry
        """
        self.max_retries = max_retries
        self.backoff_strategy = backoff_strategy or ExponentialBackoff()
        self.error_classifier = error_classifier or ErrorClassifier()
        self.circuit_breaker = circuit_breaker
        self.on_retry_callback = on_retry_callback
        
    def execute(self, func: Callable, *args, **kwargs) -> Any:
        """Execute a function with retry logic.
        
        Args:
            func: The function to execute
            *args: Positional arguments for the function
            **kwargs: Keyword arguments for the function
            
        Returns:
            Any: The result of the function
            
        Raises:
            Exception: If the function fails after all retries
        """
        retries = 0
        last_exception = None
        
        while True:
            try:
                # Check circuit breaker if present
                if self.circuit_breaker and not self.circuit_breaker.allow_request():
                    raise RetryableError(
                        f"Circuit breaker '{self.circuit_breaker.name}' is open",
                        retry_after=self.circuit_breaker.reset_timeout
                    )
                    
                # Execute the function
                result = func(*args, **kwargs)
                
                # Record success with circuit breaker if present
                if self.circuit_breaker:
                    self.circuit_breaker.record_success()
                    
                return result
                
            except Exception as e:
                last_exception = e
                
                # Record failure with circuit breaker if present
                if self.circuit_breaker:
                    self.circuit_breaker.record_failure(e)
                    
                # Check if we've hit the retry limit
                if retries >= self.max_retries:
                    logger.error(f"Operation failed after {retries} retries: {e}")
                    raise
                    
                # Classify the exception
                is_retryable, suggested_delay = self.error_classifier.classify_exception(e)
                
                if not is_retryable:
                    logger.error(f"Non-retryable error: {e}")
                    raise
                    
                # Calculate delay
                if suggested_delay is not None:
                    delay = suggested_delay
                else:
                    delay = self.backoff_strategy.get_delay(retries)
                    
                # Call retry callback if present
                if self.on_retry_callback:
                    self.on_retry_callback(e, retries, delay)
                    
                logger.warning(f"Retry {retries + 1}/{self.max_retries} after error: {e}. Waiting {delay:.2f}s")
                time.sleep(delay)
                
                retries += 1
                
    def __call__(self, func: Callable) -> Callable:
        """Decorator to apply retry policy to a function.
        
        Args:
            func: The function to wrap with retry logic
            
        Returns:
            Callable: The wrapped function
        """
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            return self.execute(func, *args, **kwargs)
        return wrapper 