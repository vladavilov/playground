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
    python_requires=">=3.9,<3.14",
    install_requires=[
        # Core ML dependencies
        "torch>=2.7.0,<2.8.0",
        "pytorch-forecasting>=1.3.0",
        "pytorch-lightning>=2.2.0",
        "scikit-learn>=1.6.0",
        "pandas>=2.2.3",
        "numpy>=2.0.0",
        
        # Data infrastructure
        "kafka-python>=2.0.2",
        "redis>=5.1.0",
        "pinotdb>=0.3.3",
        "pydantic>=2.7.0",
        
        # API framework
        "fastapi>=0.111.0",
        "uvicorn>=0.30.0",
        "python-dotenv>=1.0.1",
        "python-jose>=3.3.0",
        "passlib>=1.7.4",
        
        # Utilities
        "loguru>=0.7.2",
        "prometheus-client>=0.20.0",
        "matplotlib>=3.9.0",
        "seaborn>=0.13.2",
        
        # Development tools
        "pytest>=8.1.0",
        "pytest-asyncio>=0.24.0",
        "pytest-cov>=4.2.0",
        "black>=24.4.0",
        "isort>=5.14.0",
        "mypy>=1.10.0",
        "flake8>=7.1.0",
        "pre-commit>=3.8.0",
        
        # Jupyter environment
        "jupyter>=1.1.0",
        "ipywidgets>=8.1.7",
        "jupyterlab>=4.2.0",
        
        # Documentation
        "sphinx>=7.3.0",
        "sphinx-rtd-theme>=2.1.0",
    ],
    extras_require={
        "dev": [
            "pytest>=8.1.0",
            "pytest-asyncio>=0.24.0",
            "pytest-cov>=4.2.0",
            "black>=24.4.0",
            "isort>=5.14.0",
            "mypy>=1.10.0",
            "flake8>=7.1.0",
            "pre-commit>=3.8.0",
        ],
        "notebook": [
            "jupyter>=1.0.0,<1.1.0",
            "ipywidgets>=8.1.7",
            "jupyterlab>=4.1.0,<4.2.0",
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