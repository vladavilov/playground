import logging
import sys
from src.config import get_settings

DEFAULT_LOG_FORMAT = "%(asctime)s - %(name)s - %(levelname)s - %(module)s:%(funcName)s:%(lineno)d - %(message)s"

def setup_logging(stream=None):
    """
    Configures basic logging for the application.
    Sets the root logger's level and adds a console handler with a predefined format.
    The handler will write to the provided stream, or sys.stdout if stream is None.
    """

    settings = get_settings()
    log_level_str = settings.LOG_LEVEL.upper()
    
    level_map = {
        "DEBUG": logging.DEBUG,
        "INFO": logging.INFO,
        "WARNING": logging.WARNING,
        "ERROR": logging.ERROR,
        "CRITICAL": logging.CRITICAL
    }
    numeric_level = level_map.get(log_level_str, logging.INFO)

    root_logger = logging.getLogger()
    root_logger.setLevel(numeric_level)

    # Remove existing handlers from the root logger to ensure a clean state before adding new ones.
    # This makes the function idempotent in terms of handler count if called multiple times.
    for handler in root_logger.handlers[:]:
        root_logger.removeHandler(handler)
        handler.close() # Close handler before removing

    output_stream = stream if stream is not None else sys.stdout
    console_handler = logging.StreamHandler(output_stream)
    console_handler.setLevel(numeric_level)

    formatter = logging.Formatter(DEFAULT_LOG_FORMAT)
    console_handler.setFormatter(formatter)

    root_logger.addHandler(console_handler)

def get_logger(name: str) -> logging.Logger:
    """
    Retrieves a logger instance with the specified name.
    Relies on explicit setup_logging() call from application/tests.
    """
    return logging.getLogger(name)