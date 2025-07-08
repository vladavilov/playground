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
        return ArticleEnricher(mock_azure_openai_client, "test-deployment")
    
    def test_article_enricher_initialization(self, mock_azure_openai_client):
        """Test ArticleEnricher can be initialized with Azure OpenAI client."""
        enricher = ArticleEnricher(mock_azure_openai_client, "test-deployment")
        
        assert enricher.client == mock_azure_openai_client
        assert enricher.deployment_name == "test-deployment"
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
        """Test article enrichment when article has 'global_other' sector (should be filtered out)."""
        # Mock entity extraction response with 'global_other' sector
        not_relevant_response = {
            "choices": [
                {
                    "message": {
                        "content": json.dumps({
                            "issuer_name": None,
                            "sector": "global_other",
                            "state": None,
                            "cusips": []
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
        
        # Verify the result is None (filtered out)
        assert result is None
        
        # Verify OpenAI client was called only once (entity extraction only, no scoring)
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
        assert "Financial News Analyst" in messages[0]["content"]
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
        assert "SENTIMENT ANALYSIS" in messages[0]["content"]
        assert messages[1]["role"] == "user"
        assert sample_raw_article.article_text in messages[1]["content"]
        assert entities["issuer_name"] in messages[1]["content"]

    def test_determine_source_credibility_tier_tier1_regulator(self, enricher):
        """Test source credibility tier determination for TIER_1_REGULATOR sources."""
        tier1_sources = ["SEC.gov", "Federal Reserve", "S&P Global", "Moody's", "Fitch Ratings"]
        
        for source in tier1_sources:
            tier = enricher._determine_source_credibility_tier(source)
            assert tier == "TIER_1_REGULATOR", f"Expected TIER_1_REGULATOR for {source}, got {tier}"

    def test_determine_source_credibility_tier_tier2_premium(self, enricher):
        """Test source credibility tier determination for TIER_2_PREMIUM_FINANCIAL sources."""
        tier2_sources = ["Bloomberg", "Reuters", "Wall Street Journal", "Financial Times", "Bond Buyer"]
        
        for source in tier2_sources:
            tier = enricher._determine_source_credibility_tier(source)
            assert tier == "TIER_2_PREMIUM_FINANCIAL", f"Expected TIER_2_PREMIUM_FINANCIAL for {source}, got {tier}"

    def test_determine_source_credibility_tier_tier3_general(self, enricher):
        """Test source credibility tier determination for TIER_3_GENERAL_FINANCIAL sources."""
        tier3_sources = ["CNBC", "MarketWatch", "Yahoo Finance", "CNN Business"]
        
        for source in tier3_sources:
            tier = enricher._determine_source_credibility_tier(source)
            assert tier == "TIER_3_GENERAL_FINANCIAL", f"Expected TIER_3_GENERAL_FINANCIAL for {source}, got {tier}"

    def test_determine_source_credibility_tier_tier4_company_pr(self, enricher):
        """Test source credibility tier determination for TIER_4_COMPANY_PR sources."""
        tier4_sources = ["Apple Press Release", "Microsoft Investor Relations", "Tesla News"]
        
        for source in tier4_sources:
            tier = enricher._determine_source_credibility_tier(source)
            assert tier == "TIER_4_COMPANY_PR", f"Expected TIER_4_COMPANY_PR for {source}, got {tier}"

    def test_determine_source_credibility_tier_tier5_syndicated_default(self, enricher):
        """Test source credibility tier determination defaults to TIER_5_SYNDICATED for unknown sources."""
        unknown_sources = ["Unknown Source", "Random Blog", "Social Media Post"]
        
        for source in unknown_sources:
            tier = enricher._determine_source_credibility_tier(source)
            assert tier == "TIER_5_SYNDICATED", f"Expected TIER_5_SYNDICATED for {source}, got {tier}"

    @patch('enrichment.article_enricher.logger')
    def test_enrich_article_filters_global_other_with_warning(
        self, 
        mock_logger,
        enricher, 
        sample_raw_article
    ):
        """Test that articles with 'global_other' sector are filtered out with a warning log."""
        # Mock entity extraction response with global_other sector
        global_other_response = {
            "choices": [
                {
                    "message": {
                        "content": json.dumps({
                            "issuer_name": None,
                            "sector": "global_other",
                            "state": None,
                            "cusips": []
                        })
                    }
                }
            ]
        }
        
        mock_response = Mock()
        mock_response.choices = [Mock()]
        mock_response.choices[0].message = Mock()
        mock_response.choices[0].message.content = global_other_response["choices"][0]["message"]["content"]
        
        enricher.client.chat.completions.create.return_value = mock_response
        
        # Call the method under test
        result = enricher.enrich_article(sample_raw_article)
        
        # Verify warning was logged
        mock_logger.warning.assert_called_once()
        warning_call_args = mock_logger.warning.call_args[0][0]
        assert "global_other" in warning_call_args
        assert sample_raw_article.article_hash in warning_call_args
        
        # Verify result is None (filtered out)
        assert result is None
        
        # Verify OpenAI client was called only once (entity extraction only, no scoring)
        assert enricher.client.chat.completions.create.call_count == 1

    def test_entity_extraction_prompt_excludes_relevant_for_scoring(self, enricher, sample_raw_article):
        """Test that entity extraction prompt no longer includes relevant_for_scoring field."""
        messages = enricher._build_entity_extraction_prompt(sample_raw_article)
        
        system_prompt = messages[0]["content"]
        
        # Verify relevant_for_scoring is not mentioned in the prompt
        assert "relevant_for_scoring" not in system_prompt
        assert "relevant for scoring" not in system_prompt.lower()

    def test_scoring_prompt_excludes_source_credibility_tier(self, enricher, sample_raw_article):
        """Test that scoring prompt no longer includes source_credibility_tier field."""
        entities = {
            "issuer_name": "City of Springfield",
            "sector": "municipal",
            "state": "Illinois",
            "cusips": ["12345ABC8"]
        }
        
        messages = enricher._build_scoring_prompt(sample_raw_article, entities)
        
        system_prompt = messages[0]["content"]
        
        # Verify source_credibility_tier is not mentioned in the prompt
        assert "source_credibility_tier" not in system_prompt
        assert "source credibility tier" not in system_prompt.lower()
        assert "TIER_1_REGULATOR" not in system_prompt
        assert "TIER_2_PREMIUM_FINANCIAL" not in system_prompt

    def test_enrich_article_uses_script_determined_credibility_tier(
        self, 
        enricher, 
        mock_azure_openai_client
    ):
        """Test that enriched event uses script-determined credibility tier, not AI-provided one."""
        # Create a raw article with known source
        sample_article = RawNewsArticle(
            article_text="Test article content",
            source_name="Bloomberg",
            publication_time=datetime(2024, 1, 15, 10, 30, 0),
            title="Test Article",
            url="https://example.com/test",
            article_hash="test123"
        )
        
        # Mock entity extraction response
        mock_entity_response = Mock()
        mock_entity_response.choices = [Mock()]
        mock_entity_response.choices[0].message = Mock()
        mock_entity_response.choices[0].message.content = json.dumps({
            "issuer_name": "Test Corp",
            "sector": "corporate",
            "state": None,
            "cusips": []
        })
        
        # Mock scoring response with AI-provided credibility tier (should be ignored)
        mock_scoring_response = Mock()
        mock_scoring_response.choices = [Mock()]
        mock_scoring_response.choices[0].message = Mock()
        mock_scoring_response.choices[0].message.content = json.dumps({
            "event_type": "General_News",
            "sentiment": {"score": 0.1, "magnitude": 0.5},
            "source_credibility_tier": "TIER_5_SYNDICATED",  # This should be ignored
            "summary_excerpt": "Test summary"
        })
        
        enricher.client.chat.completions.create.side_effect = [
            mock_entity_response,
            mock_scoring_response
        ]
        
        # Call the method
        result = enricher.enrich_article(sample_article)
        
        # Verify that script-determined tier is used (Bloomberg = TIER_2_PREMIUM_FINANCIAL)
        # and AI-provided tier is ignored
        assert result.source_credibility_tier == "TIER_2_PREMIUM_FINANCIAL"
    
    def test_clean_openai_response_content_plain_json(self, enricher):
        """Test _clean_openai_response_content with plain JSON (no wrapping)."""
        content = '{"issuer_name": "Test Corp", "sector": "corporate"}'
        result = enricher._clean_openai_response_content(content)
        assert result == '{"issuer_name": "Test Corp", "sector": "corporate"}'
    
    def test_clean_openai_response_content_json_wrapped(self, enricher):
        """Test _clean_openai_response_content with markdown JSON wrapping."""
        content = '''```json
{"issuer_name": "Test Corp", "sector": "corporate"}
```'''
        result = enricher._clean_openai_response_content(content)
        assert result == '{"issuer_name": "Test Corp", "sector": "corporate"}'
    
    def test_clean_openai_response_content_json_wrapped_with_whitespace(self, enricher):
        """Test _clean_openai_response_content with wrapped JSON and extra whitespace."""
        content = '''   ```json
   {"issuer_name": "Test Corp", "sector": "corporate"}
   ```   '''
        result = enricher._clean_openai_response_content(content)
        assert result == '{"issuer_name": "Test Corp", "sector": "corporate"}'
    
    def test_clean_openai_response_content_multiline_json(self, enricher):
        """Test _clean_openai_response_content with multiline JSON in wrapping."""
        content = '''```json
{
    "issuer_name": "Test Corp",
    "sector": "corporate",
    "state": null
}
```'''
        expected = '''{
    "issuer_name": "Test Corp",
    "sector": "corporate",
    "state": null
}'''
        result = enricher._clean_openai_response_content(content)
        assert result == expected
    
    def test_clean_openai_response_content_javascript_language_tag(self, enricher):
        """Test _clean_openai_response_content with different language tag (should remove it)."""
        content = '''```javascript
{"issuer_name": "Test Corp", "sector": "corporate"}
```'''
        result = enricher._clean_openai_response_content(content)
        assert result == '{"issuer_name": "Test Corp", "sector": "corporate"}'
    
    def test_clean_openai_response_content_no_language_tag(self, enricher):
        """Test _clean_openai_response_content with backticks but no language tag."""
        content = '''```
{"issuer_name": "Test Corp", "sector": "corporate"}
```'''
        result = enricher._clean_openai_response_content(content)
        assert result == '{"issuer_name": "Test Corp", "sector": "corporate"}'
    
    def test_clean_openai_response_content_extra_backticks(self, enricher):
        """Test _clean_openai_response_content with more than 3 backticks."""
        content = '''````json
{"issuer_name": "Test Corp", "sector": "corporate"}
````'''
        result = enricher._clean_openai_response_content(content)
        assert result == '{"issuer_name": "Test Corp", "sector": "corporate"}'
    
    def test_clean_openai_response_content_empty_string(self, enricher):
        """Test _clean_openai_response_content with empty string."""
        content = ""
        result = enricher._clean_openai_response_content(content)
        assert result == ""
    
    def test_clean_openai_response_content_only_whitespace(self, enricher):
        """Test _clean_openai_response_content with only whitespace."""
        content = "   \n\t  "
        result = enricher._clean_openai_response_content(content)
        assert result == ""
    
    def test_enrich_article_with_wrapped_entity_response(
        self, 
        enricher, 
        sample_raw_article
    ):
        """Test successful article enrichment when entity extraction response is wrapped in markdown."""
        # Mock entity extraction response with markdown wrapping
        mock_entity_response = Mock()
        mock_entity_response.choices = [Mock()]
        mock_entity_response.choices[0].message = Mock()
        mock_entity_response.choices[0].message.content = '''```json
{
    "issuer_name": "City of Springfield",
    "sector": "municipal",
    "state": "IL",
    "cusips": ["12345ABC8"]
}
```'''
        
        # Mock scoring response (regular JSON)
        mock_scoring_response = Mock()
        mock_scoring_response.choices = [Mock()]
        mock_scoring_response.choices[0].message = Mock()
        mock_scoring_response.choices[0].message.content = json.dumps({
            "event_type": "Credit_Rating_Upgrade",
            "sentiment": {"score": 0.75, "magnitude": 0.85},
            "summary_excerpt": "Springfield issues AAA-rated municipal bond"
        })
        
        enricher.client.chat.completions.create.side_effect = [
            mock_entity_response,
            mock_scoring_response
        ]
        
        # Call the method under test
        result = enricher.enrich_article(sample_raw_article)
        
        # Verify the result is correctly parsed despite markdown wrapping
        assert isinstance(result, EnrichedNewsEvent)
        assert result.entities.issuer_name == "City of Springfield"
        assert result.entities.sector == "municipal"
        assert result.entities.state == "IL"
        assert result.entities.cusips == ["12345ABC8"]
    
    def test_enrich_article_with_wrapped_scoring_response(
        self, 
        enricher, 
        sample_raw_article
    ):
        """Test successful article enrichment when scoring response is wrapped in markdown."""
        # Mock entity extraction response (regular JSON)
        mock_entity_response = Mock()
        mock_entity_response.choices = [Mock()]
        mock_entity_response.choices[0].message = Mock()
        mock_entity_response.choices[0].message.content = json.dumps({
            "issuer_name": "City of Springfield",
            "sector": "municipal", 
            "state": "IL",
            "cusips": ["12345ABC8"]
        })
        
        # Mock scoring response with markdown wrapping
        mock_scoring_response = Mock()
        mock_scoring_response.choices = [Mock()]
        mock_scoring_response.choices[0].message = Mock()
        mock_scoring_response.choices[0].message.content = '''```json
{
    "event_type": "Credit_Rating_Upgrade",
    "sentiment": {
        "score": 0.75,
        "magnitude": 0.85
    },
    "summary_excerpt": "Springfield issues AAA-rated municipal bond"
}
```'''
        
        enricher.client.chat.completions.create.side_effect = [
            mock_entity_response,
            mock_scoring_response
        ]
        
        # Call the method under test
        result = enricher.enrich_article(sample_raw_article)
        
        # Verify the result is correctly parsed despite markdown wrapping
        assert isinstance(result, EnrichedNewsEvent)
        assert result.event_type == "Credit_Rating_Upgrade"
        assert result.sentiment.score == 0.75
        assert result.sentiment.magnitude == 0.85
        assert result.summary_excerpt == "Springfield issues AAA-rated municipal bond"
    
    def test_enrich_article_with_both_responses_wrapped(
        self, 
        enricher, 
        sample_raw_article
    ):
        """Test successful article enrichment when both responses are wrapped in markdown."""
        # Mock entity extraction response with markdown wrapping
        mock_entity_response = Mock()
        mock_entity_response.choices = [Mock()]
        mock_entity_response.choices[0].message = Mock()
        mock_entity_response.choices[0].message.content = '''```json
{
    "issuer_name": "City of Springfield",
    "sector": "municipal",
    "state": "IL", 
    "cusips": ["12345ABC8"]
}
```'''
        
        # Mock scoring response with markdown wrapping
        mock_scoring_response = Mock()
        mock_scoring_response.choices = [Mock()]
        mock_scoring_response.choices[0].message = Mock()
        mock_scoring_response.choices[0].message.content = '''```json
{
    "event_type": "Credit_Rating_Upgrade",
    "sentiment": {
        "score": 0.75,
        "magnitude": 0.85
    },
    "summary_excerpt": "Springfield issues AAA-rated municipal bond"
}
```'''
        
        enricher.client.chat.completions.create.side_effect = [
            mock_entity_response,
            mock_scoring_response
        ]
        
        # Call the method under test
        result = enricher.enrich_article(sample_raw_article)
        
        # Verify the result is correctly parsed despite both responses having markdown wrapping
        assert isinstance(result, EnrichedNewsEvent)
        assert result.entities.issuer_name == "City of Springfield"
        assert result.entities.sector == "municipal"
        assert result.entities.state == "IL"
        assert result.entities.cusips == ["12345ABC8"]
        assert result.event_type == "Credit_Rating_Upgrade"
        assert result.sentiment.score == 0.75
        assert result.sentiment.magnitude == 0.85
        assert result.summary_excerpt == "Springfield issues AAA-rated municipal bond" 