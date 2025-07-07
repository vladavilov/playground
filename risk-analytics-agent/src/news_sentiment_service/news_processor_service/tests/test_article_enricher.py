import pytest
from unittest.mock import Mock, patch, MagicMock
from datetime import datetime
import json

from models.models import RawNewsArticle, EnrichedNewsEvent
from enrichment.article_enricher import ArticleEnricher


class TestArticleEnricher:
    
    @pytest.fixture
    def mock_azure_openai_client(self):
        """Mock Azure OpenAI client for testing."""
        mock_client = Mock()
        return mock_client
    
    @pytest.fixture
    def sample_raw_article(self):
        """Sample raw news article for testing."""
        return RawNewsArticle(
            article_text="The city of Springfield announced a new municipal bond issue to fund infrastructure improvements. The bond will be rated AAA by Moody's and has a 10-year maturity.",
            source_name="Municipal Finance Weekly",
            publication_time=datetime(2024, 1, 15, 10, 30, 0),
            title="Springfield Issues New Infrastructure Bond",
            url="https://example.com/news/springfield-bond",
            article_hash="abc123def456"
        )
    
    @pytest.fixture
    def mock_entity_extraction_response(self):
        """Mock response for entity extraction prompt."""
        return {
            "choices": [
                {
                    "message": {
                        "content": json.dumps({
                            "issuer_name": "City of Springfield",
                            "sector": "municipal",
                            "state": "Illinois",
                            "cusips": ["12345ABC8"],
                            "relevant_for_scoring": True
                        })
                    }
                }
            ]
        }
    
    @pytest.fixture
    def mock_scoring_response(self):
        """Mock response for scoring and classification prompt."""
        return {
            "choices": [
                {
                    "message": {
                        "content": json.dumps({
                            "event_type": "Credit_Rating_Upgrade",
                            "sentiment": {
                                "score": 0.75,
                                "magnitude": 0.85
                            },
                            "source_credibility_tier": "TIER_3_GENERAL_FINANCIAL",
                            "summary_excerpt": "Springfield issues AAA-rated municipal bond for infrastructure"
                        })
                    }
                }
            ]
        }
    
    @pytest.fixture
    def enricher(self, mock_azure_openai_client):
        """Create ArticleEnricher instance for testing."""
        return ArticleEnricher(mock_azure_openai_client)
    
    def test_article_enricher_initialization(self, mock_azure_openai_client):
        """Test ArticleEnricher can be initialized with Azure OpenAI client."""
        enricher = ArticleEnricher(mock_azure_openai_client)
        
        assert enricher.client == mock_azure_openai_client
        assert enricher is not None
    
    def test_enrich_article_successful_flow(
        self, 
        enricher, 
        sample_raw_article,
        mock_entity_extraction_response,
        mock_scoring_response
    ):
        """Test successful article enrichment with relevant article."""
        # Mock the chat.completions.create calls
        mock_entity_response = Mock()
        mock_entity_response.choices = [Mock()]
        mock_entity_response.choices[0].message = Mock()
        mock_entity_response.choices[0].message.content = mock_entity_extraction_response["choices"][0]["message"]["content"]
        
        mock_scoring_response_obj = Mock()
        mock_scoring_response_obj.choices = [Mock()]
        mock_scoring_response_obj.choices[0].message = Mock()
        mock_scoring_response_obj.choices[0].message.content = mock_scoring_response["choices"][0]["message"]["content"]
        
        enricher.client.chat.completions.create.side_effect = [
            mock_entity_response,
            mock_scoring_response_obj
        ]
        
        # Call the method under test
        result = enricher.enrich_article(sample_raw_article)
        
        # Verify the result
        assert isinstance(result, EnrichedNewsEvent)
        assert result.id == sample_raw_article.article_hash
        assert result.source == sample_raw_article.source_name
        assert result.published_at == sample_raw_article.publication_time
        assert result.event_type == "Credit_Rating_Upgrade"
        assert result.entities.issuer_name == "City of Springfield"
        assert result.entities.sector == "municipal"
        assert result.entities.state == "Illinois"
        assert result.entities.cusips == ["12345ABC8"]
        assert result.sentiment.score == 0.75
        assert result.sentiment.magnitude == 0.85
        assert result.source_credibility_tier == "TIER_3_GENERAL_FINANCIAL"
        assert result.summary_excerpt == "Springfield issues AAA-rated municipal bond for infrastructure"
        assert result.raw_article_url == sample_raw_article.url
        
        # Verify OpenAI client was called twice (entity extraction + scoring)
        assert enricher.client.chat.completions.create.call_count == 2
    
    def test_enrich_article_not_relevant(
        self, 
        enricher, 
        sample_raw_article,
        mock_azure_openai_client
    ):
        """Test article enrichment when article is not relevant for scoring."""
        # Mock entity extraction response indicating not relevant
        not_relevant_response = {
            "choices": [
                {
                    "message": {
                        "content": json.dumps({
                            "issuer_name": None,
                            "sector": "global_other",
                            "state": None,
                            "cusips": [],
                            "relevant_for_scoring": False
                        })
                    }
                }
            ]
        }
        
        mock_response = Mock()
        mock_response.choices = [Mock()]
        mock_response.choices[0].message = Mock()
        mock_response.choices[0].message.content = not_relevant_response["choices"][0]["message"]["content"]
        
        enricher.client.chat.completions.create.return_value = mock_response
        
        # Call the method under test
        result = enricher.enrich_article(sample_raw_article)
        
        # Verify the result
        assert isinstance(result, EnrichedNewsEvent)
        assert result.entities.sector == "global_other"
        assert result.entities.issuer_name == "N/A"  # Should default to N/A when None
        assert result.event_type == "General_News"
        assert result.sentiment.score == 0.0
        assert result.sentiment.magnitude == 0.0
        assert result.source_credibility_tier == "TIER_3_GENERAL_FINANCIAL"  # Default
        assert result.summary_excerpt == "Article not relevant for financial analysis"
        
        # Verify OpenAI client was called only once (entity extraction only)
        assert enricher.client.chat.completions.create.call_count == 1
    
    def test_enrich_article_openai_error(
        self, 
        enricher, 
        sample_raw_article
    ):
        """Test handling of OpenAI API errors."""
        # Mock OpenAI client to raise an exception
        enricher.client.chat.completions.create.side_effect = Exception("OpenAI API Error")
        
        # Test that the method raises an exception
        with pytest.raises(Exception) as exc_info:
            enricher.enrich_article(sample_raw_article)
        
        assert "OpenAI API Error" in str(exc_info.value)
    
    def test_enrich_article_invalid_json_response(
        self, 
        enricher, 
        sample_raw_article
    ):
        """Test handling of invalid JSON responses from OpenAI."""
        # Mock invalid JSON response
        invalid_response = {
            "choices": [
                {
                    "message": {
                        "content": "Invalid JSON response"
                    }
                }
            ]
        }
        
        mock_response = Mock()
        mock_response.choices = [Mock()]
        mock_response.choices[0].message = Mock()
        mock_response.choices[0].message.content = invalid_response["choices"][0]["message"]["content"]
        
        enricher.client.chat.completions.create.return_value = mock_response
        
        # Test that the method raises an exception
        with pytest.raises(json.JSONDecodeError):
            enricher.enrich_article(sample_raw_article)
    
    def test_build_entity_extraction_prompt(self, enricher, sample_raw_article):
        """Test the entity extraction prompt building logic."""
        # Call the private method (we'll make it public for testing)
        messages = enricher._build_entity_extraction_prompt(sample_raw_article)
        
        assert len(messages) == 2
        assert messages[0]["role"] == "system"
        assert "financial news article" in messages[0]["content"]
        assert messages[1]["role"] == "user"
        assert sample_raw_article.article_text in messages[1]["content"]
    
    def test_build_scoring_prompt(self, enricher, sample_raw_article):
        """Test the scoring prompt building logic."""
        entities = {
            "issuer_name": "City of Springfield",
            "sector": "municipal",
            "state": "Illinois",
            "cusips": ["12345ABC8"]
        }
        
        # Call the private method (we'll make it public for testing)
        messages = enricher._build_scoring_prompt(sample_raw_article, entities)
        
        assert len(messages) == 2
        assert messages[0]["role"] == "system"
        assert "sentiment analysis" in messages[0]["content"]
        assert messages[1]["role"] == "user"
        assert sample_raw_article.article_text in messages[1]["content"]
        assert entities["issuer_name"] in messages[1]["content"] 