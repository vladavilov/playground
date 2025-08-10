import logging
import sys
import structlog

SENSITIVE_FIELDS = ['password', 'security_key', 'secret', 'token']

def filter_sensitive_data(logger, log_method, event_dict):
    """
    A structlog processor to filter sensitive data from the event dictionary.
    """
    for field in SENSITIVE_FIELDS:
        if field in event_dict:
            event_dict[field] = '[FILTERED]'
    return event_dict

def configure_logging(log_level=logging.INFO, stream=None, force_reconfigure=False):
    """Configure structlog-based JSON logging."""
    if stream is None:
        stream = sys.stdout
    
    # Skip if already configured (unless force_reconfigure is True)
    if not force_reconfigure and hasattr(structlog, '_configured'):
        return
    
    # Clear any existing handlers to ensure clean setup
    for handler in logging.root.handlers[:]:
        logging.root.removeHandler(handler)
    
    # Configure Python's logging with force=True to override any existing configuration
    logging.basicConfig(
        format="%(message)s",
        stream=stream,
        level=log_level,
        force=True
    )
    
    # Ensure root logger level is set correctly
    logging.root.setLevel(log_level)
    
    shared_processors = [
        # Filter by level first for performance
        structlog.stdlib.filter_by_level,
        # Merge context variables (thread-safe)
        structlog.contextvars.merge_contextvars,
        # Add logger name and level
        structlog.stdlib.add_logger_name,
        structlog.processors.add_log_level,
        # Filter sensitive data
        filter_sensitive_data,
        # Add timestamp
        structlog.processors.TimeStamper(fmt="iso", utc=True),
        # Handle exceptions
        structlog.processors.format_exc_info,
        # Final JSON rendering
        structlog.processors.JSONRenderer(),
    ]
    
    # Configure structlog with thread-safe and multiprocess-safe settings
    structlog.configure(
        processors=shared_processors,
        wrapper_class=structlog.stdlib.BoundLogger,
        logger_factory=structlog.stdlib.LoggerFactory(),
        # Don't cache loggers to ensure configuration applies to all threads/processes
        cache_logger_on_first_use=False,
    )
    
    structlog._configured = True
