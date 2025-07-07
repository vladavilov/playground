import json
import logging
from datetime import datetime, timezone
from typing import Dict, Any, List

from openai import AzureOpenAI
from models.models import RawNewsArticle, EnrichedNewsEvent, Entities, Sentiment

logger = logging.getLogger(__name__)


class ArticleEnricher:
    """
    ArticleEnricher handles the AI-powered enrichment of raw news articles.
    
    This class implements a two-step process:
    1. Entity Extraction: Extracts structured entities from the article
    2. Sentiment Analysis: Scores and classifies the article if relevant
    """
    
    def __init__(self, client: AzureOpenAI):
        """
        Initialize the ArticleEnricher with an Azure OpenAI client.
        
        Args:
            client: Azure OpenAI client instance
        """
        self.client = client
    
    def enrich_article(self, article: RawNewsArticle) -> EnrichedNewsEvent:
        """
        Enrich a raw news article with structured analysis and sentiment scores.
        
        Args:
            article: The raw news article to enrich
            
        Returns:
            EnrichedNewsEvent: The enriched article with AI analysis
            
        Raises:
            Exception: If OpenAI API calls fail
            json.JSONDecodeError: If OpenAI responses contain invalid JSON
        """
        logger.info(f"Starting enrichment for article: {article.article_hash}")
        
        # Step 1: Entity Extraction
        entity_response = self._call_openai_entity_extraction(article)
        entities_data = json.loads(entity_response.choices[0].message.content)
        
        # Check if article is relevant for scoring
        if not entities_data.get("relevant_for_scoring", False):
            logger.info(f"Article {article.article_hash} marked as not relevant for scoring")
            return self._create_non_relevant_event(article, entities_data)
        
        # Step 2: Sentiment Analysis and Classification
        scoring_response = self._call_openai_scoring(article, entities_data)
        scoring_data = json.loads(scoring_response.choices[0].message.content)
        
        # Assemble the final EnrichedNewsEvent
        enriched_event = self._create_enriched_event(article, entities_data, scoring_data)
        
        logger.info(f"Successfully enriched article: {article.article_hash}")
        return enriched_event
    
    def _call_openai_entity_extraction(self, article: RawNewsArticle) -> Any:
        """Call Azure OpenAI for entity extraction."""
        messages = self._build_entity_extraction_prompt(article)
        
        response = self.client.chat.completions.create(
            model="gpt-4",  # This will be replaced by deployment name
            messages=messages,
            temperature=0.1,
            max_tokens=1000
        )
        
        return response
    
    def _call_openai_scoring(self, article: RawNewsArticle, entities: Dict[str, Any]) -> Any:
        """Call Azure OpenAI for sentiment scoring and classification."""
        messages = self._build_scoring_prompt(article, entities)
        
        response = self.client.chat.completions.create(
            model="gpt-4",  # This will be replaced by deployment name
            messages=messages,
            temperature=0.1,
            max_tokens=1000
        )
        
        return response
    
    def _build_entity_extraction_prompt(self, article: RawNewsArticle) -> List[Dict[str, str]]:
        """
        Build the prompt for entity extraction.
        
        Args:
            article: The raw news article
            
        Returns:
            List of message dictionaries for OpenAI API
        """
        system_prompt = """
        You are a financial news analysis expert. Your task is to extract structured entities from a financial news article.
        
        Analyze the article and extract the following information:
        - issuer_name: The specific company, municipality, or entity mentioned (null if not specific)
        - sector: One of [municipal, corporate, government, global_market, global_other]
        - state: US state if applicable for municipal bonds (null otherwise)
        - cusips: List of CUSIP identifiers if mentioned (empty list if none)
        - relevant_for_scoring: Boolean indicating if this article is relevant for financial sentiment analysis
        
        Rules:
        - For broad market news (e.g., Federal Reserve policy), use sector="global_market"
        - For unrelated news (e.g., sports, weather), use sector="global_other" and relevant_for_scoring=false
        - Only mark as relevant_for_scoring=true if the article directly impacts specific financial instruments or markets
        
        Return only valid JSON with these exact field names.
        """
        
        user_prompt = f"""
        Please analyze this financial news article:
        
        Title: {article.title}
        Content: {article.article_text}
        Source: {article.source_name}
        
        Extract the structured entities as JSON.
        """
        
        return [
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": user_prompt}
        ]
    
    def _build_scoring_prompt(self, article: RawNewsArticle, entities: Dict[str, Any]) -> List[Dict[str, str]]:
        """
        Build the prompt for sentiment analysis and classification.
        
        Args:
            article: The raw news article
            entities: Previously extracted entities
            
        Returns:
            List of message dictionaries for OpenAI API
        """
        system_prompt = """
        You are a financial sentiment analysis expert. Analyze the provided news article and provide:
        
        1. event_type: One of [Default, Bankruptcy, Credit_Rating_Downgrade, Bond_Insurer_Downgrade, 
           Central_Bank_Policy, State_Budget_Crisis, Natural_Disaster_Impact, Credit_Rating_Upgrade, 
           M&A_Announced, Pension_Funding_Status_Change, Credit_Outlook_Negative, Regulatory_Investigation,
           Macro_Economic_Data, Guidance_Change, Lawsuit_Filed, Credit_Outlook_Positive, Earnings_Miss,
           Earnings_Beat, Executive_Change, General_News]
           
        2. sentiment: Object with:
           - score: Float between -1.0 (very negative) and 1.0 (very positive)
           - magnitude: Float between 0.0 and 1.0 indicating confidence/strength
           
        3. source_credibility_tier: One of [TIER_1_REGULATOR, TIER_2_PREMIUM_FINANCIAL, 
           TIER_3_GENERAL_FINANCIAL, TIER_4_COMPANY_PR, TIER_5_SYNDICATED]
           
        4. summary_excerpt: Brief summary (max 100 chars) of the key financial impact
        
        Return only valid JSON with these exact field names.
        """
        
        user_prompt = f"""
        Analyze this financial news article for sentiment and classification:
        
        Title: {article.title}
        Content: {article.article_text}
        Source: {article.source_name}
        
        Context:
        - Issuer: {entities.get('issuer_name', 'N/A')}
        - Sector: {entities.get('sector', 'N/A')}
        - State: {entities.get('state', 'N/A')}
        - CUSIPs: {entities.get('cusips', [])}
        
        Provide sentiment analysis and classification as JSON.
        """
        
        return [
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": user_prompt}
        ]
    
    def _create_non_relevant_event(self, article: RawNewsArticle, entities_data: Dict[str, Any]) -> EnrichedNewsEvent:
        """Create an EnrichedNewsEvent for non-relevant articles."""
        entities = Entities(
            issuer_name=entities_data.get("issuer_name") or "N/A",
            sector=entities_data.get("sector", "global_other"),
            state=entities_data.get("state"),
            cusips=entities_data.get("cusips", [])
        )
        
        sentiment = Sentiment(score=0.0, magnitude=0.0)
        
        return EnrichedNewsEvent(
            id=article.article_hash,
            source=article.source_name,
            published_at=article.publication_time,
            ingested_at=datetime.now(timezone.utc),
            event_type="General_News",
            entities=entities,
            sentiment=sentiment,
            source_credibility_tier="TIER_3_GENERAL_FINANCIAL",  # Default
            summary_excerpt="Article not relevant for financial analysis",
            raw_article_url=article.url
        )
    
    def _create_enriched_event(
        self, 
        article: RawNewsArticle, 
        entities_data: Dict[str, Any], 
        scoring_data: Dict[str, Any]
    ) -> EnrichedNewsEvent:
        """Create an EnrichedNewsEvent from AI analysis results."""
        entities = Entities(
            issuer_name=entities_data.get("issuer_name") or "N/A",
            sector=entities_data.get("sector") or "global_other",
            state=entities_data.get("state"),
            cusips=entities_data.get("cusips", [])
        )
        
        sentiment_data = scoring_data.get("sentiment", {})
        sentiment = Sentiment(
            score=sentiment_data.get("score", 0.0),
            magnitude=sentiment_data.get("magnitude", 0.0)
        )
        
        return EnrichedNewsEvent(
            id=article.article_hash,
            source=article.source_name,
            published_at=article.publication_time,
            ingested_at=datetime.now(timezone.utc),
            event_type=scoring_data.get("event_type", "General_News"),
            entities=entities,
            sentiment=sentiment,
            source_credibility_tier=scoring_data.get("source_credibility_tier", "TIER_3_GENERAL_FINANCIAL"),
            summary_excerpt=scoring_data.get("summary_excerpt", ""),
            raw_article_url=article.url
        ) 