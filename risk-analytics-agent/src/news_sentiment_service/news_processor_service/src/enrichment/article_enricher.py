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
    
    # Source credibility tier mapping based on publication reliability
    SOURCE_CREDIBILITY_MAPPING = {
        # TIER_1_REGULATOR: Government agencies and rating agencies
        "SEC.gov": "TIER_1_REGULATOR",
        "Federal Reserve": "TIER_1_REGULATOR", 
        "FINRA": "TIER_1_REGULATOR",
        "Treasury": "TIER_1_REGULATOR",
        "S&P Global": "TIER_1_REGULATOR",
        "Moody's": "TIER_1_REGULATOR",
        "Fitch Ratings": "TIER_1_REGULATOR",
        "Moody's Analytics": "TIER_1_REGULATOR",
        
        # TIER_2_PREMIUM_FINANCIAL: Premium financial media
        "Bloomberg": "TIER_2_PREMIUM_FINANCIAL",
        "Reuters": "TIER_2_PREMIUM_FINANCIAL", 
        "Wall Street Journal": "TIER_2_PREMIUM_FINANCIAL",
        "Financial Times": "TIER_2_PREMIUM_FINANCIAL",
        "Bond Buyer": "TIER_2_PREMIUM_FINANCIAL",
        "Institutional Investor": "TIER_2_PREMIUM_FINANCIAL",
        "The Bond Buyer": "TIER_2_PREMIUM_FINANCIAL",
        
        # TIER_3_GENERAL_FINANCIAL: General financial media
        "CNBC": "TIER_3_GENERAL_FINANCIAL",
        "MarketWatch": "TIER_3_GENERAL_FINANCIAL", 
        "Yahoo Finance": "TIER_3_GENERAL_FINANCIAL",
        "CNN Business": "TIER_3_GENERAL_FINANCIAL",
        "Fox Business": "TIER_3_GENERAL_FINANCIAL",
        "Barron's": "TIER_3_GENERAL_FINANCIAL",
        "Municipal Finance Weekly": "TIER_3_GENERAL_FINANCIAL",
        
        # TIER_4_COMPANY_PR: Company communications
        "Apple Press Release": "TIER_4_COMPANY_PR",
        "Microsoft Investor Relations": "TIER_4_COMPANY_PR", 
        "Tesla News": "TIER_4_COMPANY_PR",
        "Investor Relations": "TIER_4_COMPANY_PR",
        "Company Press Release": "TIER_4_COMPANY_PR",
        
        # Note: TIER_5_SYNDICATED is the default for unknown sources
    }
    
    def __init__(self, client: AzureOpenAI, deployment_name: str = "gpt-4"):
        """
        Initialize the ArticleEnricher with an Azure OpenAI client.
        
        Args:
            client: Azure OpenAI client instance
            deployment_name: The name of the Azure OpenAI deployment to use
        """
        self.client = client
        self.deployment_name = deployment_name
    
    def _determine_source_credibility_tier(self, source_name: str) -> str:
        """
        Determine the credibility tier for a news source based on hardcoded mapping.
        
        Args:
            source_name: Name of the news source
            
        Returns:
            str: Credibility tier (TIER_1_REGULATOR through TIER_5_SYNDICATED)
        """
        # Direct lookup for exact matches
        if source_name in self.SOURCE_CREDIBILITY_MAPPING:
            return self.SOURCE_CREDIBILITY_MAPPING[source_name]
        
        # Fuzzy matching for sources that might have slight variations
        source_lower = source_name.lower()
        
        # Check for partial matches in source name
        for known_source, tier in self.SOURCE_CREDIBILITY_MAPPING.items():
            if known_source.lower() in source_lower or source_lower in known_source.lower():
                return tier
        
        # Check for common patterns indicating company PR
        if any(pattern in source_lower for pattern in ["press release", "investor relations", "pr newswire"]):
            return "TIER_4_COMPANY_PR"
        
        # Default to syndicated/wire service tier for unknown sources  
        return "TIER_5_SYNDICATED"
    
    def enrich_article(self, article: RawNewsArticle) -> EnrichedNewsEvent:
        """
        Enrich a raw news article with structured analysis and sentiment scores.
        
        Args:
            article: The raw news article to enrich
            
        Returns:
            EnrichedNewsEvent: The enriched article with AI analysis, or None if filtered out
            
        Raises:
            Exception: If OpenAI API calls fail
            json.JSONDecodeError: If OpenAI responses contain invalid JSON
        """
        logger.info(f"Starting enrichment for article: {article.article_hash}")
        
        # Step 1: Entity Extraction
        entity_response = self._call_openai_entity_extraction(article)
        entities_data = json.loads(entity_response.choices[0].message.content)
        
        # Check if article is 'global_other' sector (filter out with warning)
        if entities_data.get("sector") == "global_other":
            logger.warning(f"Article {article.article_hash} classified as 'global_other' sector - filtering out")
            return None
        
        # Step 2: Sentiment Analysis and Classification (for relevant articles)
        scoring_response = self._call_openai_scoring(article, entities_data)
        scoring_data = json.loads(scoring_response.choices[0].message.content)
        
        # Assemble the final EnrichedNewsEvent with script-determined credibility tier
        enriched_event = self._create_enriched_event(article, entities_data, scoring_data)
        
        logger.info(f"Successfully enriched article: {article.article_hash}")
        return enriched_event
    
    def _call_openai_entity_extraction(self, article: RawNewsArticle) -> Any:
        """Call Azure OpenAI for entity extraction."""
        messages = self._build_entity_extraction_prompt(article)
        
        response = self.client.chat.completions.create(
            model=self.deployment_name,
            messages=messages,
            temperature=0.1,
            max_tokens=1000
        )
        
        return response
    
    def _call_openai_scoring(self, article: RawNewsArticle, entities: Dict[str, Any]) -> Any:
        """Call Azure OpenAI for sentiment scoring and classification."""
        messages = self._build_scoring_prompt(article, entities)
        
        response = self.client.chat.completions.create(
            model=self.deployment_name,
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
        # ROLE & EXPERTISE
        You are a Senior Financial News Analyst with 15+ years of experience in municipal bond markets, corporate finance, and regulatory analysis. You specialize in extracting structured financial entities from news articles with 99%+ accuracy.

        # MISSION
        Extract structured financial entities from news articles to enable precise risk assessment and portfolio impact analysis.

        # TASK STRUCTURE (4-Step Process)
        
        ## STEP 1: RELEVANCE ASSESSMENT
        Determine if the article contains information that could impact financial instruments or investment decisions.
        
        ## STEP 2: ENTITY IDENTIFICATION  
        Extract specific financial entities using strict classification rules.
        
        ## STEP 3: VALIDATION
        Verify all extracted data meets quality standards.
        
        ## STEP 4: JSON FORMATTING
        Structure output in exact required format.

        # EXTRACTION SPECIFICATIONS

        ## Required Fields:
        - **issuer_name**: Specific company/municipality/entity name (null if broad/general)
        - **sector**: Must be exactly one of [municipal, corporate, government, global_market, global_other]
        - **state**: US state code for municipal bonds only (null otherwise)  
        - **cusips**: Array of 9-character CUSIP identifiers if mentioned (empty array if none)

        ## Classification Rules:
        - **municipal**: City, county, state government bonds/debt
        - **corporate**: Company-specific news affecting corporate bonds/equity
        - **government**: Federal government, treasury, federal agency news
        - **global_market**: Broad market factors (Fed policy, economic indicators, market-wide events)
        - **global_other**: Non-financial news (sports, weather, entertainment)

        ## Sector Classification Guidance:
        - **municipal**: Direct impact on city, county, or state government finances
        - **corporate**: Company-specific news affecting business operations or finances  
        - **government**: Federal policy, treasury operations, or regulatory changes
        - **global_market**: Broad economic factors affecting multiple sectors
        - **global_other**: Non-financial content (sports, weather, entertainment, general news)

        # FEW-SHOT EXAMPLES

        ## Example 1: Municipal Bond News
        **Input**: "City of Detroit announces $500M bond offering to refinance water system debt. Moody's rates the bonds A2."
        **Output**: 
        ```json
        {
            "issuer_name": "City of Detroit",
            "sector": "municipal", 
            "state": "MI",
            "cusips": []
        }
        ```

        ## Example 2: Corporate News
        **Input**: "Apple Inc. reports Q3 earnings beat with revenue of $81.8B, up 8% YoY. Strong iPhone sales drive results."
        **Output**:
        ```json
        {
            "issuer_name": "Apple Inc.",
            "sector": "corporate",
            "state": null,
            "cusips": []
        }
        ```

        ## Example 3: Federal Reserve Policy
        **Input**: "Federal Reserve raises interest rates by 0.25% to combat inflation, signals additional hikes possible."
        **Output**:
        ```json
        {
            "issuer_name": null,
            "sector": "global_market",
            "state": null,
            "cusips": []
        }
        ```

        ## Example 4: Non-Financial News
        **Input**: "Local high school football team wins state championship in overtime thriller."
        **Output**:
        ```json
        {
            "issuer_name": null,
            "sector": "global_other",
            "state": null,
            "cusips": []
        }
        ```

        # SELF-VALIDATION CHECKLIST
        Before responding, verify:
        □ Is sector classification correct according to rules?
        □ Are CUSIPs properly formatted (9 characters) if present?
        □ Is state code valid US state abbreviation if municipal?
        □ Is issuer_name specific entity or null for broad topics?
        □ Is JSON format exactly correct?

        # SUCCESS CRITERIA
        - 100% adherence to sector classification rules
        - Proper null handling for missing/inapplicable fields
        - Valid JSON format with exact field names
        - Logical consistency across all extracted fields

        Return ONLY valid JSON with exact field names specified above.
        """
        
        user_prompt = f"""
        Analyze this financial news article using the 4-step process:

        **ARTICLE DATA:**
        - Title: {article.title}
        - Content: {article.article_text}
        - Source: {article.source_name}
        
        **YOUR TASK:**
        Extract structured entities following the exact specifications and examples provided. Use the self-validation checklist before responding.

        Provide only the JSON output.
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
        # ROLE & EXPERTISE
        You are a Senior Fixed Income Analyst and Risk Manager with 20+ years of experience in credit analysis, market sentiment evaluation, and financial event classification. You have deep expertise in municipal bonds, corporate credit, and macroeconomic impact assessment.

        # MISSION
        Analyze financial news articles to provide precise sentiment scoring and event classification for investment risk assessment and portfolio management decisions.

        # ANALYTICAL FRAMEWORK (4-Step Process)

        ## STEP 1: EVENT CLASSIFICATION
        Categorize the financial event using established market taxonomy.

        ## STEP 2: SENTIMENT ANALYSIS
        Evaluate financial impact using quantitative sentiment scoring.

        ## STEP 3: CREDIBILITY ASSESSMENT  
        Determine source reliability for weighting in risk models.

        ## STEP 4: VALIDATION & SUMMARY
        Verify consistency and provide actionable summary.

        # OUTPUT SPECIFICATIONS

        ## Required Fields:
        1. **event_type**: Exact classification from predefined taxonomy
        2. **sentiment**: Object with precise scoring methodology
        3. **summary_excerpt**: Concise financial impact summary (≤100 chars)

        # EVENT TYPE TAXONOMY (Choose exactly one):

        **Credit Events:**
        - Default, Bankruptcy, Credit_Rating_Downgrade, Credit_Rating_Upgrade
        - Bond_Insurer_Downgrade, Credit_Outlook_Negative, Credit_Outlook_Positive

        **Corporate Events:**  
        - M&A_Announced, Earnings_Beat, Earnings_Miss, Guidance_Change
        - Executive_Change, Lawsuit_Filed, Regulatory_Investigation

        **Macro Events:**
        - Central_Bank_Policy, Macro_Economic_Data, State_Budget_Crisis
        - Natural_Disaster_Impact, Pension_Funding_Status_Change

        **Default:**
        - General_News (use only when no specific category applies)

        # SENTIMENT SCORING METHODOLOGY

        ## Score Range: -1.0 to +1.0
        - **+0.8 to +1.0**: Extremely positive (major upgrades, strong earnings beats)
        - **+0.4 to +0.7**: Positive (minor upgrades, earnings beats, positive guidance)
        - **+0.1 to +0.3**: Slightly positive (stable outlook improvements)
        - **-0.1 to -0.1**: Neutral (routine announcements, mixed signals)
        - **-0.1 to -0.3**: Slightly negative (minor concerns, guidance cuts)
        - **-0.4 to -0.7**: Negative (downgrades, earnings misses, investigations)
        - **-0.8 to -1.0**: Extremely negative (defaults, bankruptcies, major crises)

        ## Magnitude Range: 0.0 to 1.0 (Confidence/Impact Strength)
        - **0.9-1.0**: High confidence, clear financial impact
        - **0.7-0.8**: Good confidence, probable impact  
        - **0.5-0.6**: Moderate confidence, some uncertainty
        - **0.3-0.4**: Low confidence, ambiguous signals
        - **0.0-0.2**: Very low confidence, minimal impact

        # FEW-SHOT EXAMPLES

        ## Example 1: Credit Rating Downgrade
        **Input**: "S&P downgrades Chicago bonds to BBB+ from A-, citing pension obligations and declining tax base."
        **Output**:
        ```json
        {
            "event_type": "Credit_Rating_Downgrade",
            "sentiment": {
                "score": -0.6,
                "magnitude": 0.9
            },
            "summary_excerpt": "Chicago bonds downgraded BBB+ due to pension/tax issues"
        }
        ```

        ## Example 2: Strong Earnings Beat
        **Input**: "Microsoft reports Q4 earnings of $2.95/share vs $2.78 expected. Cloud revenue up 31% YoY."
        **Output**:
        ```json
        {
            "event_type": "Earnings_Beat", 
            "sentiment": {
                "score": 0.7,
                "magnitude": 0.8
            },
            "summary_excerpt": "MSFT beats earnings $2.95 vs $2.78, cloud growth 31%"
        }
        ```

        ## Example 3: Federal Reserve Policy
        **Input**: "Fed holds rates steady at 5.25-5.5%, signals potential cuts if inflation moderates further."
        **Output**:
        ```json
        {
            "event_type": "Central_Bank_Policy",
            "sentiment": {
                "score": 0.2,
                "magnitude": 0.7
            },
            "summary_excerpt": "Fed holds rates, signals potential cuts on inflation"
        }
        ```

        ## Example 4: Corporate Investigation
        **Input**: "SEC launches investigation into Tesla's autopilot claims and safety disclosures to investors."
        **Output**:
        ```json
        {
            "event_type": "Regulatory_Investigation",
            "sentiment": {
                "score": -0.5,
                "magnitude": 0.8
            },
            "summary_excerpt": "SEC investigates Tesla autopilot/safety disclosures"
        }
        ```

        # SENTIMENT REASONING PROCESS

        For each article, consider:
        1. **Direct Financial Impact**: How does this affect cash flows, credit quality, or market value?
        2. **Market Context**: Is this positive/negative relative to expectations?
        3. **Time Horizon**: Short-term reaction vs long-term fundamental impact
        4. **Scope**: Company-specific vs sector-wide vs market-wide implications
        5. **Certainty**: How definitive is the information vs speculation?

        # SELF-VALIDATION CHECKLIST

        Before responding, verify:
        □ Event type matches content and is from approved taxonomy?
        □ Sentiment score aligns with financial impact severity?
        □ Magnitude reflects confidence level in assessment?
        □ Summary excerpt captures key financial impact in ≤100 chars?
        □ JSON format is exactly correct with proper field names?
        □ Score and magnitude are within specified ranges?

        # SUCCESS CRITERIA
        - Accurate event classification using financial market standards
        - Sentiment scoring that reflects genuine financial impact magnitude
        - Concise summary highlighting investment-relevant information
        - Perfect JSON formatting with all required fields
        - Logical consistency across all output elements

        Return ONLY valid JSON with exact field names specified above.
        """
        
        user_prompt = f"""
        Perform comprehensive financial sentiment analysis using the 4-step framework:

        **ARTICLE DATA:**
        - Title: {article.title}
        - Content: {article.article_text}
        - Source: {article.source_name}

        **EXTRACTED CONTEXT:**
        - Issuer: {entities.get('issuer_name', 'N/A')}
        - Sector: {entities.get('sector', 'N/A')}
        - State: {entities.get('state', 'N/A')}
        - CUSIPs: {entities.get('cusips', [])}

        **YOUR ANALYSIS TASK:**
        1. Classify the financial event type using the provided taxonomy
        2. Score sentiment using the quantitative methodology and reasoning process
        3. Assess source credibility based on publication tier system
        4. Create actionable summary excerpt highlighting key financial impact
        5. Use self-validation checklist before responding

        **CRITICAL REQUIREMENTS:**
        - Consider the extracted context when assessing impact
        - Apply sector-specific sentiment considerations
        - Ensure magnitude reflects your confidence in the assessment
        - Provide reasoning that aligns with financial market standards

        Provide only the JSON output.
        """
        
        return [
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": user_prompt}
        ]
    

    
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
        
        # Determine credibility tier using script logic instead of AI
        credibility_tier = self._determine_source_credibility_tier(article.source_name)
        
        return EnrichedNewsEvent(
            id=article.article_hash,
            source=article.source_name,
            published_at=article.publication_time,
            ingested_at=datetime.now(timezone.utc),
            event_type=scoring_data.get("event_type", "General_News"),
            entities=entities,
            sentiment=sentiment,
            source_credibility_tier=credibility_tier,
            summary_excerpt=scoring_data.get("summary_excerpt", ""),
            raw_article_url=article.url
        ) 