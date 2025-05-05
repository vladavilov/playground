"""Main entry point for the Risk Analytics Agent API server."""

import os

import uvicorn
from dotenv import load_dotenv
from fastapi import FastAPI
from loguru import logger

# Load environment variables
load_dotenv()

app = FastAPI(
    title="Risk Analytics Agent API",
    description="API for risk analytics and market regime prediction",
    version="0.1.0",
)


@app.get("/")
async def root():
    """API root endpoint."""
    return {"message": "Risk Analytics Agent API"}


@app.get("/api/v1/health")
async def health_check():
    """Health check endpoint."""
    return {"status": "healthy"}


def serve_api():
    """Start the API server."""
    host = os.getenv("API_HOST", "0.0.0.0")
    port = int(os.getenv("API_PORT", "8000"))
    debug = os.getenv("API_DEBUG", "False").lower() == "true"
    
    logger.info(f"Starting Risk Analytics Agent API server on {host}:{port}")
    
    uvicorn.run(
        "risk_analytics_agent.main:app",
        host=host,
        port=port,
        reload=debug,
    )


if __name__ == "__main__":
    serve_api() 