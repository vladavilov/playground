import asyncio
import json
import logging
from datetime import datetime, timezone
from typing import Optional, Dict, Any

from azure.servicebus.aio import ServiceBusClient
from azure.servicebus import ServiceBusMessage
from azure.core.exceptions import AzureError

from models.models import RawNewsArticle, EnrichedNewsEvent
from enrichment.article_enricher import ArticleEnricher
from cosmos_db.cosmos_db_client import CosmosDBClient
from service_bus.service_bus_client import ServiceBusClient as CommonServiceBusClient

logger = logging.getLogger(__name__)


class MessageProcessor:
    """
    Handles background processing of news articles from Service Bus messages.
    
    This class:
    1. Listens to Service Bus queue for new article messages
    2. Deserializes messages to RawNewsArticle objects
    3. Uses ArticleEnricher to process articles
    4. Saves enriched results to Cosmos DB
    """
    
    def __init__(self, article_enricher: ArticleEnricher, settings):
        """
        Initialize MessageProcessor.
        
        Args:
            article_enricher: ArticleEnricher instance for processing articles
            settings: Application settings object
        """
        self.article_enricher = article_enricher
        self.settings = settings
        self.is_running = False
        self.processing_task: Optional[asyncio.Task] = None
        
        # Statistics
        self.stats = {
            "messages_processed": 0,
            "messages_failed": 0,
            "processing_errors": 0,
            "started_at": None,
            "last_processed_at": None
        }
        
        # Initialize clients
        self.service_bus_client: Optional[CommonServiceBusClient] = None
        self.cosmos_client: Optional[CosmosDBClient] = None
        
        self._initialize_clients()
    
    def _initialize_clients(self):
        """Initialize Azure service clients."""
        try:
            # Initialize Service Bus client
            # When in local fallback mode, let the client read connection string from environment
            if self.settings.USE_LOCAL_FALLBACK:
                self.service_bus_client = CommonServiceBusClient(
                    namespace=None,  # Let client use SERVICE_BUS_EMULATOR_CONNECTION_STRING from env
                    queue_name=self.settings.SERVICE_BUS_QUEUE_NAME
                )
            else:
                self.service_bus_client = CommonServiceBusClient(
                    namespace=self.settings.AZURE_SERVICEBUS_NAMESPACE,
                    queue_name=self.settings.SERVICE_BUS_QUEUE_NAME
                )
            logger.info("Service Bus client initialized")
            
            # Initialize Cosmos DB client
            self.cosmos_client = CosmosDBClient(
                database_name=self.settings.COSMOS_DB_DATABASE_NAME,
                container_name=self.settings.COSMOS_DB_CONTAINER_NAME
            )
            logger.info("Cosmos DB client initialized")
            
        except Exception as e:
            logger.error(f"Failed to initialize clients: {e}")
            raise
    
    async def start_processing(self):
        """Start background message processing."""
        if self.is_running:
            logger.warning("Message processor is already running")
            return
        
        if not self.article_enricher:
            raise ValueError("ArticleEnricher is required for message processing")
        
        self.is_running = True
        self.stats["started_at"] = datetime.now(timezone.utc).isoformat()
        
        # Start the background processing task
        self.processing_task = asyncio.create_task(self._process_messages_loop())
        logger.info("Message processing started")
    
    async def stop_processing(self):
        """Stop background message processing."""
        if not self.is_running:
            return
        
        self.is_running = False
        
        if self.processing_task:
            self.processing_task.cancel()
            try:
                await self.processing_task
            except asyncio.CancelledError:
                pass
        
        logger.info("Message processing stopped")
    
    def is_processing(self) -> bool:
        """Check if message processing is active."""
        return self.is_running and self.processing_task is not None and not self.processing_task.done()
    
    async def _process_messages_loop(self):
        """Main message processing loop."""
        logger.info("Starting message processing loop")
        
        try:
            while self.is_running:
                try:
                    # Use the callback-based receive_messages from ServiceBusClient
                    # Run in executor to make it async-compatible
                    await asyncio.get_event_loop().run_in_executor(
                        None, 
                        self.service_bus_client.receive_messages,
                        self._message_callback,
                        10,  # max_messages
                        30   # timeout
                    )
                    
                    # Brief pause to prevent tight loop
                    await asyncio.sleep(1)
                    
                except Exception as e:
                    logger.error(f"Error in message processing loop: {e}")
                    self.stats["processing_errors"] += 1
                    await asyncio.sleep(5)  # Longer pause on error
                    
        except asyncio.CancelledError:
            logger.info("Message processing loop cancelled")
        except Exception as e:
            logger.error(f"Fatal error in message processing loop: {e}")
    
    def _message_callback(self, message_body: bytes):
        """
        Callback function for processing individual messages from Service Bus.
        
        Args:
            message_body: Raw message body bytes from Service Bus
        """
        try:
            # Parse message body
            message_str = message_body.decode('utf-8')
            article_data = json.loads(message_str)
            
            # Convert to RawNewsArticle
            raw_article = RawNewsArticle(**article_data)
            
            # Enrich the article using AI
            enriched_event = self.article_enricher.enrich_article(raw_article)
            
            # Save to Cosmos DB - convert enriched event to dict and upsert
            item_dict = enriched_event.model_dump() if hasattr(enriched_event, 'model_dump') else enriched_event.__dict__
            
            # Ensure we have an id field for Cosmos DB
            if 'id' not in item_dict and 'article_hash' in item_dict:
                item_dict['id'] = item_dict['article_hash']
            
            self.cosmos_client.upsert_item(item_dict)
            logger.info(f"Saved enriched event with article_hash: {item_dict.get('article_hash', 'unknown')}")
            
            # Update statistics
            self.stats["messages_processed"] += 1
            self.stats["last_processed_at"] = datetime.now(timezone.utc).isoformat()
            
            logger.info(f"Successfully processed article: {raw_article.article_hash}")
            
        except json.JSONDecodeError as e:
            logger.error(f"Invalid JSON in message: {e}")
            self.stats["messages_failed"] += 1
            
        except Exception as e:
            logger.error(f"Error processing message: {e}")
            self.stats["messages_failed"] += 1
    
    
    async def get_stats(self) -> Dict[str, Any]:
        """Get processing statistics."""
        current_stats = dict(self.stats)
        current_stats.update({
            "is_running": self.is_running,
            "is_processing": self.is_processing(),
            "queue_name": self.settings.SERVICE_BUS_QUEUE_NAME
        })
        
        if current_stats["started_at"]:
            started_time = datetime.fromisoformat(current_stats["started_at"])
            current_time = datetime.now(timezone.utc)
            uptime_seconds = (current_time - started_time).total_seconds()
            current_stats["uptime_seconds"] = uptime_seconds
        
        return current_stats 