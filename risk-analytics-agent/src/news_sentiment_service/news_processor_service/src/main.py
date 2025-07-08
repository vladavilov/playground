import asyncio
import logging
from contextlib import asynccontextmanager

import uvicorn
from fastapi import FastAPI
from openai import AzureOpenAI

from config import settings
from enrichment.article_enricher import ArticleEnricher
from processing.message_processor import MessageProcessor

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Global instances
azure_openai_client: AzureOpenAI = None
article_enricher: ArticleEnricher = None
message_processor: MessageProcessor = None


@asynccontextmanager
async def lifespan(app: FastAPI):
    """
    Lifespan context manager for FastAPI application.
    Handles startup and shutdown events.
    """
    # Startup
    logger.info("Starting News Processor Service...")
    
    global azure_openai_client, article_enricher, message_processor
    
    # Initialize Azure OpenAI client
    if settings.AZURE_OPENAI_ENDPOINT and settings.AZURE_OPENAI_API_KEY:
        try:
            azure_openai_client = AzureOpenAI(
                api_key=settings.AZURE_OPENAI_API_KEY,
                api_version=settings.AZURE_OPENAI_API_VERSION,
                azure_endpoint=settings.AZURE_OPENAI_ENDPOINT
            )
            logger.info("Azure OpenAI client initialized successfully")
        except Exception as e:
            logger.error(f"Failed to initialize Azure OpenAI client: {e}")
            raise
    else:
        logger.info("Azure OpenAI not configured - client not initialized")
    
    # Initialize ArticleEnricher
    if azure_openai_client:
        article_enricher = ArticleEnricher(azure_openai_client, settings.AZURE_OPENAI_DEPLOYMENT)
        logger.info("ArticleEnricher initialized successfully")
    
    # Initialize and start MessageProcessor
    try:
        message_processor = MessageProcessor(
            article_enricher=article_enricher,
            settings=settings
        )
        
        # Start background message processing
        await message_processor.start_processing()
        logger.info("Message processor started successfully")
            
    except Exception as e:
        logger.error(f"Failed to initialize message processor: {e}")
        raise
    
    logger.info("News Processor Service startup complete")
    
    yield
    
    # Shutdown
    logger.info("Shutting down News Processor Service...")
    
    if message_processor:
        await message_processor.stop_processing()
        logger.info("Message processor stopped")
    
    logger.info("News Processor Service shutdown complete")


# Create FastAPI app with lifespan
app = FastAPI(
    title="News Processor Service",
    description="AI-powered news article enrichment and processing service",
    version="1.0.0",
    lifespan=lifespan
)


@app.get("/health")
async def health_check():
    """Health check endpoint."""
    status = {
        "status": "healthy",
        "service": "news-processor-service",
        "azure_openai_configured": azure_openai_client is not None,
        "message_processor_active": message_processor is not None and message_processor.is_processing(),
        "settings": {
            "api_port": settings.API_PORT
        }
    }
    return status


@app.get("/health/detailed")
async def detailed_health_check():
    """Detailed health check with service dependencies."""
    status = {
        "status": "healthy",
        "service": "news-processor-service",
        "timestamp": asyncio.get_event_loop().time(),
        "dependencies": {
            "azure_openai": {
                "configured": azure_openai_client is not None,
                "endpoint": settings.AZURE_OPENAI_ENDPOINT or "Not configured"
            },
            "message_processor": {
                "active": message_processor is not None and message_processor.is_processing(),
                "queue_name": settings.SERVICE_BUS_QUEUE_NAME
            },
            "cosmos_db": {
                "configured": bool(settings.AZURE_COSMOSDB_ENDPOINT),
                "endpoint": settings.AZURE_COSMOSDB_ENDPOINT or "Not configured"
            }
        }
    }
    
    # Check if any critical dependency is missing
    if not message_processor or not message_processor.is_processing():
        status["status"] = "degraded"
        status["warning"] = "Message processor not active"
    
    return status


@app.get("/stats")
async def get_processing_stats():
    """Get processing statistics."""
    if not message_processor:
        return {
            "message": "Message processor not initialized",
            "error": "Service not fully started"
        }
    
    return await message_processor.get_stats()


def start():
    """Starts the Uvicorn server."""
    uvicorn.run(
        "src.main:app",
        host="0.0.0.0",
        port=settings.API_PORT,
        reload=False,
        workers=1  # Single worker for background processing
    )


if __name__ == "__main__":
    start() 