import asyncio
import logging
import httpx
import hashlib

from config import Settings
from models.models import RawNewsArticle
from service_bus.service_bus_client import ServiceBusClient

logging.basicConfig(
    level=logging.INFO, 
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


async def run_ingestion_cycle(settings: Settings, service_bus_client: ServiceBusClient):
    """
    Orchestrates one cycle of fetching news from adapters and enqueuing articles for processing.
    Duplicate checking is handled downstream by the News Processor Service.
    """
    logger.info("Starting ingestion cycle...")
    
    async with httpx.AsyncClient() as client:
        for adapter_url in settings.adapter_urls_list:
            try:
                logger.info(f"Fetching news from adapter: {adapter_url}")
                response = await client.get(adapter_url, timeout=30.0)
                response.raise_for_status()

                articles_data = response.json()
                logger.info(f"Retrieved {len(articles_data)} articles from {adapter_url}")
                
                for article_data in articles_data:
                    try:
                        if isinstance(article_data, str):
                            article = RawNewsArticle.model_validate_json(article_data)
                        else:
                            article = RawNewsArticle.model_validate(article_data)
                        
                        # Calculate article hash for downstream duplicate checking
                        hash_content = f"{article.title}:{article.source_name}"
                        article.article_hash = hashlib.sha256(hash_content.encode('utf-8')).hexdigest()

                        # Enqueue article for processing - no duplicate checking here
                        service_bus_client.send_message(article.model_dump_json())
                        logger.info(f"Article enqueued: '{article.title[:50]}...' (hash: {article.article_hash[:8]}...)")
                        
                    except Exception as e:
                        logger.error(f"Error processing article from {adapter_url}: {e}")
                        continue

            except httpx.HTTPStatusError as e:
                logger.error(f"HTTP error occurred while fetching from {adapter_url}: {e}")
            except httpx.RequestError as e:
                logger.error(f"Request error occurred while fetching from {adapter_url}: {e}")
            except Exception as e:
                logger.error(f"Unexpected error occurred while processing adapter {adapter_url}: {e}")


async def main():
    """
    Main entry point for the data ingestion orchestrator.
    Initializes Service Bus client and runs the ingestion cycle.
    """
    logger.info("=== Data Ingestion Orchestrator Starting ===")
    
    try:
        # Load settings
        settings = Settings()
        logger.info(f"Loaded configuration with {len(settings.adapter_urls_list)} adapter URLs")
        
        logger.info("Initializing Service Bus client...")
        service_bus_client = ServiceBusClient(
            namespace=settings.SERVICE_BUS_NAMESPACE,
            queue_name=settings.SERVICE_BUS_QUEUE_NAME,
        )
        
        await run_ingestion_cycle(settings, service_bus_client)
        logger.info("=== Ingestion cycle completed successfully ===")
        
    except Exception as e:
        logger.error(f"=== Fatal error in orchestrator: {e} ===")
        raise


if __name__ == "__main__":
    asyncio.run(main())
