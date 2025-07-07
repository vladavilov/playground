import pytest
import asyncio
import json
from unittest.mock import Mock, patch, AsyncMock
from datetime import datetime, timezone
import sys
import os

# Add paths to import from src and common
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'src'))
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', 'common', 'src'))

from models.models import RawNewsArticle, EnrichedNewsEvent
from enrichment.article_enricher import ArticleEnricher
from processing.message_processor import MessageProcessor


class TestMessageProcessor:
    
    @pytest.fixture
    def mock_settings(self):
        """Mock settings object for testing."""
        settings = Mock()
        settings.AZURE_SERVICEBUS_NAMESPACE = "test-namespace"
        settings.SERVICE_BUS_QUEUE_NAME = "test-queue"
        settings.AZURE_COSMOSDB_ENDPOINT = "https://test-cosmos.documents.azure.com:443/"
        settings.COSMOS_DB_DATABASE_NAME = "test-db"
        settings.COSMOS_DB_CONTAINER_NAME = "test-container"
        return settings
    

    
    @pytest.fixture
    def mock_article_enricher(self):
        """Mock ArticleEnricher for testing."""
        enricher = Mock(spec=ArticleEnricher)
        return enricher
    
    @pytest.fixture
    def sample_raw_article(self):
        """Sample raw news article for testing."""
        return RawNewsArticle(
            article_text="Test article content",
            source_name="Test Source",
            publication_time=datetime(2024, 1, 15, 10, 30, 0),
            title="Test Article Title",
            url="https://example.com/test",
            article_hash="test123hash"
        )
    
    @pytest.fixture
    def sample_enriched_event(self, sample_raw_article):
        """Sample enriched news event for testing."""
        from models.models import Entities, Sentiment
        
        entities = Entities(
            issuer_name="Test Issuer",
            sector="municipal",
            state="CA",
            cusips=["12345ABC8"]
        )
        
        sentiment = Sentiment(
            score=0.75,
            magnitude=0.85
        )
        
        return EnrichedNewsEvent(
            id=sample_raw_article.article_hash,
            source=sample_raw_article.source_name,
            published_at=sample_raw_article.publication_time,
            ingested_at=datetime.now(timezone.utc),
            event_type="Credit_Rating_Upgrade",
            entities=entities,
            sentiment=sentiment,
            source_credibility_tier="TIER_3_GENERAL_FINANCIAL",
            summary_excerpt="Test article summary",
            raw_article_url=sample_raw_article.url
        )
    
    @pytest.fixture
    def mock_service_bus_message(self, sample_raw_article):
        """Mock Service Bus message for testing."""
        message = Mock()
        message.message_id = "test-message-id"
        message.body = sample_raw_article.model_dump_json().encode('utf-8')
        message.delivery_count = 1
        return message
    
    @patch('processing.message_processor.CommonServiceBusClient')
    @patch('processing.message_processor.CosmosDBClient')
    def test_message_processor_initialization(
        self, 
        mock_cosmos_client, 
        mock_service_bus_client, 
        mock_article_enricher, 
        mock_settings
    ):
        """Test MessageProcessor initialization with Azure clients."""
        processor = MessageProcessor(mock_article_enricher, mock_settings)
        
        assert processor.article_enricher == mock_article_enricher
        assert processor.settings == mock_settings
        assert not processor.is_running
        assert processor.processing_task is None
        assert processor.stats["messages_processed"] == 0
        assert processor.stats["messages_failed"] == 0
        
        # Verify clients were initialized
        mock_service_bus_client.assert_called_once()
        mock_cosmos_client.assert_called_once()
    

    
    @patch('processing.message_processor.CommonServiceBusClient')
    @patch('processing.message_processor.CosmosDBClient')
    @pytest.mark.asyncio
    async def test_start_processing_normal_mode(
        self, 
        mock_cosmos_client, 
        mock_service_bus_client, 
        mock_article_enricher, 
        mock_settings
    ):
        """Test starting message processing in normal mode."""
        processor = MessageProcessor(mock_article_enricher, mock_settings)
        
        # Mock the processing loop to avoid infinite loop
        with patch.object(processor, '_process_messages_loop') as mock_loop:
            mock_loop.return_value = AsyncMock()
            
            await processor.start_processing()
            
            assert processor.is_running
            assert processor.processing_task is not None
            assert processor.stats["started_at"] is not None
    

    
    @patch('processing.message_processor.CommonServiceBusClient')
    @patch('processing.message_processor.CosmosDBClient')
    @pytest.mark.asyncio
    async def test_start_processing_without_enricher(
        self, 
        mock_cosmos_client, 
        mock_service_bus_client, 
        mock_settings
    ):
        """Test starting message processing without ArticleEnricher raises error."""
        processor = MessageProcessor(None, mock_settings)
        
        with pytest.raises(ValueError, match="ArticleEnricher is required"):
            await processor.start_processing()
    
    @patch('processing.message_processor.CommonServiceBusClient')
    @patch('processing.message_processor.CosmosDBClient')
    @pytest.mark.asyncio
    async def test_stop_processing(
        self, 
        mock_cosmos_client, 
        mock_service_bus_client, 
        mock_article_enricher, 
        mock_settings
    ):
        """Test stopping message processing."""
        processor = MessageProcessor(mock_article_enricher, mock_settings)
        
        # Start processing first
        with patch.object(processor, '_process_messages_loop') as mock_loop:
            mock_loop.return_value = AsyncMock()
            await processor.start_processing()
            
            # Now stop processing
            await processor.stop_processing()
            
            assert not processor.is_running
            assert processor.processing_task.cancelled()
    
    @patch('processing.message_processor.CommonServiceBusClient')
    @patch('processing.message_processor.CosmosDBClient')
    def test_message_callback_success(
        self, 
        mock_cosmos_client, 
        mock_service_bus_client, 
        mock_article_enricher, 
        mock_settings, 
        sample_raw_article,
        sample_enriched_event
    ):
        """Test successful processing of a message callback."""
        processor = MessageProcessor(mock_article_enricher, mock_settings)
        
        # Mock the enricher to return a sample enriched event
        mock_article_enricher.enrich_article.return_value = sample_enriched_event
        
        # Mock the cosmos client upsert method
        processor.cosmos_client.upsert_item = Mock()
        
        # Create message body from sample article
        message_body = sample_raw_article.model_dump_json().encode('utf-8')
        
        # Process the message callback
        processor._message_callback(message_body)
        
        # Verify the enricher was called with correct article
        mock_article_enricher.enrich_article.assert_called_once()
        called_article = mock_article_enricher.enrich_article.call_args[0][0]
        assert called_article.article_hash == sample_raw_article.article_hash
        assert called_article.title == sample_raw_article.title
        
        # Verify the enriched event was saved via upsert_item
        processor.cosmos_client.upsert_item.assert_called_once()
        # Check that the upserted item contains the expected data
        upserted_item = processor.cosmos_client.upsert_item.call_args[0][0]
        assert upserted_item['id'] == sample_enriched_event.id
        assert upserted_item['source'] == sample_enriched_event.source
        
        # Verify statistics were updated
        assert processor.stats["messages_processed"] == 1
        assert processor.stats["last_processed_at"] is not None
    
    @patch('processing.message_processor.CommonServiceBusClient')
    @patch('processing.message_processor.CosmosDBClient')
    def test_message_callback_invalid_json(
        self, 
        mock_cosmos_client, 
        mock_service_bus_client, 
        mock_article_enricher, 
        mock_settings
    ):
        """Test processing of a message callback with invalid JSON."""
        processor = MessageProcessor(mock_article_enricher, mock_settings)
        
        # Create invalid JSON message body
        invalid_message_body = b"invalid json content"
        
        # Process the message callback (should handle error gracefully)
        processor._message_callback(invalid_message_body)
        
        # Verify the enricher was not called
        mock_article_enricher.enrich_article.assert_not_called()
        
        # Verify statistics were updated for failed message
        assert processor.stats["messages_failed"] == 1
        assert processor.stats["messages_processed"] == 0
    
    @patch('processing.message_processor.CommonServiceBusClient')
    @patch('processing.message_processor.CosmosDBClient')
    def test_message_callback_enrichment_error(
        self, 
        mock_cosmos_client, 
        mock_service_bus_client, 
        mock_article_enricher, 
        mock_settings, 
        sample_raw_article
    ):
        """Test processing of a message callback when enrichment fails."""
        processor = MessageProcessor(mock_article_enricher, mock_settings)
        
        # Mock the enricher to raise an exception
        mock_article_enricher.enrich_article.side_effect = Exception("Enrichment failed")
        
        # Create message body from sample article
        message_body = sample_raw_article.model_dump_json().encode('utf-8')
        
        # Process the message callback (should handle error gracefully)
        processor._message_callback(message_body)
        
        # Verify the enricher was called
        mock_article_enricher.enrich_article.assert_called_once()
        
        # Verify statistics were updated for failed message
        assert processor.stats["messages_failed"] == 1
        assert processor.stats["messages_processed"] == 0
    
    
    @patch('processing.message_processor.CommonServiceBusClient')
    @patch('processing.message_processor.CosmosDBClient')
    @pytest.mark.asyncio
    async def test_get_stats(
        self, 
        mock_cosmos_client, 
        mock_service_bus_client, 
        mock_article_enricher, 
        mock_settings
    ):
        """Test getting processing statistics."""
        processor = MessageProcessor(mock_article_enricher, mock_settings)
        
        # Set some test statistics
        processor.stats["messages_processed"] = 10
        processor.stats["messages_failed"] = 2
        processor.stats["started_at"] = datetime.now(timezone.utc).isoformat()
        
        # Get stats
        stats = await processor.get_stats()
        
        # Verify stats contain expected fields
        assert stats["messages_processed"] == 10
        assert stats["messages_failed"] == 2
        assert stats["is_running"] == False
        assert stats["is_processing"] == False
        assert stats["queue_name"] == mock_settings.SERVICE_BUS_QUEUE_NAME
        assert "uptime_seconds" in stats
    
    @patch('processing.message_processor.CommonServiceBusClient')
    @patch('processing.message_processor.CosmosDBClient')
    def test_is_processing_when_not_running(self, mock_cosmos_client, mock_service_bus_client, mock_article_enricher, mock_settings):
        """Test is_processing when processor is not running."""
        processor = MessageProcessor(mock_article_enricher, mock_settings)
        
        assert not processor.is_processing()
    
    @patch('processing.message_processor.CommonServiceBusClient')
    @patch('processing.message_processor.CosmosDBClient')
    @pytest.mark.asyncio
    async def test_is_processing_when_running(
        self, 
        mock_cosmos_client, 
        mock_service_bus_client, 
        mock_article_enricher, 
        mock_settings
    ):
        """Test is_processing when processor is running."""
        processor = MessageProcessor(mock_article_enricher, mock_settings)
        
        # Start processing
        with patch.object(processor, '_process_messages_loop') as mock_loop:
            mock_loop.return_value = AsyncMock()
            await processor.start_processing()
            
            assert processor.is_processing()
            
            # Stop processing
            await processor.stop_processing()
            assert not processor.is_processing() 
