"""
Pytest configuration for document processing service tests.
"""

import warnings
import pytest


def pytest_configure(config):
    """Configure pytest to suppress known deprecation warnings from dependencies."""
    # Suppress Tika pkg_resources warnings - these come from the tika library itself
    warnings.filterwarnings(
        "ignore",
        message="pkg_resources is deprecated as an API.*",
        category=UserWarning
    )
    
    warnings.filterwarnings(
        "ignore", 
        message="Deprecated call to `pkg_resources.declare_namespace.*",
        category=DeprecationWarning
    )


@pytest.fixture(autouse=True)
def suppress_dependency_warnings():
    """Automatically suppress dependency warnings for all tests."""
    # Apply global warning filters
    warnings.filterwarnings(
        "ignore",
        message="pkg_resources is deprecated as an API.*",
        category=UserWarning
    )
    warnings.filterwarnings(
        "ignore",
        message="Deprecated call to `pkg_resources.declare_namespace.*",
        category=DeprecationWarning
    )