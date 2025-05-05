#!/usr/bin/env python
from setuptools import setup, find_packages

setup(
    name="risk-analytics-agent",
    version="0.1.0",
    description="Risk analytics agent for trading systems",
    author="Risk Analytics Team",
    author_email="risk-analytics@example.com",
    url="https://github.com/example/risk-analytics-agent",
    packages=find_packages(where="src"),
    package_dir={"": "src"},
    python_requires=">=3.9,<3.12",
    install_requires=[
        # Core ML dependencies
        "torch>=1.12.0",
        "pytorch-forecasting>=0.10.3",
        "pytorch-lightning>=1.8.0",
        "scikit-learn>=1.0.2",
        "pandas>=1.4.0",
        "numpy>=1.22.0",
        
        # Data infrastructure
        "kafka-python>=2.0.2",
        "redis>=4.3.4",
        "pinotdb>=0.3.3",
        "pydantic>=1.10.4",
        
        # API framework
        "fastapi>=0.89.0",
        "uvicorn>=0.20.0",
        "python-dotenv>=0.21.0",
        "python-jose>=3.3.0",  # For JWT tokens
        "passlib>=1.7.4",      # For password hashing
        
        # Utilities
        "loguru>=0.6.0",       # Enhanced logging
        "prometheus-client>=0.15.0",
        "matplotlib>=3.6.0",
        "seaborn>=0.12.0",
    ],
    extras_require={
        "dev": [
            "pytest>=7.2.0",
            "pytest-asyncio>=0.20.0",
            "pytest-cov>=4.0.0",
            "black>=22.12.0",
            "isort>=5.11.0",
            "mypy>=0.991",
            "flake8>=6.0.0",
            "pre-commit>=2.21.0",
        ],
        "notebook": [
            "jupyter>=1.0.0",
            "ipywidgets>=8.0.0",
            "jupyterlab>=3.5.0",
        ],
    },
    entry_points={
        "console_scripts": [
            "risk-analytics-serve=risk_analytics_agent.main:serve_api",
            "risk-analytics-train=risk_analytics_agent.scripts.train:main",
        ],
    },
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Financial and Insurance Industry",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "License :: OSI Approved :: MIT License",
    ],
) 