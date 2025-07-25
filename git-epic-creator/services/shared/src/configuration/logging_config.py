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

def configure_logging(log_level=logging.INFO, stream=None):
    """
    Configures logging for the application using structlog.
    
    Args:
        log_level: The minimum log level to output.
        stream: The stream to log to. Defaults to sys.stdout.
    """
    if stream is None:
        stream = sys.stdout
        
    logging.basicConfig(
        format="%(message)s",
        stream=stream,
        level=log_level,
        force=True
    )
    
    structlog.configure(
        processors=[
            filter_sensitive_data,
            structlog.stdlib.add_logger_name,
            structlog.processors.add_log_level,
            structlog.processors.TimeStamper(fmt="iso", utc=True),
            structlog.processors.JSONRenderer(),
        ],
        wrapper_class=structlog.stdlib.BoundLogger,
        logger_factory=structlog.stdlib.LoggerFactory(),
        cache_logger_on_first_use=True,
    )