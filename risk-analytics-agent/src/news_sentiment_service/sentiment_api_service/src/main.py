import uvicorn
from datetime import datetime, timezone
from typing import Optional, List, Dict, Any
from fastapi import FastAPI, HTTPException
from pydantic import BaseModel, Field
import logging


from config import settings
from sentiment_calculator import SentimentCalculator
from cosmos_db.cosmos_db_client import CosmosDBClient
from models.models import EnrichedNewsEvent, Entities, Sentiment, RealTimeSentimentResponse, HistoricalSentimentResponse

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Global components (initialized lazily)
cosmos_db_client = None
sentiment_calculator = None

def get_cosmos_client():
    """Get or initialize the Cosmos DB client."""
    global cosmos_db_client
    if cosmos_db_client is None:
        logger.info("Initializing Cosmos DB client")
        cosmos_db_client = CosmosDBClient(
            database_name=settings.COSMOS_DATABASE_NAME,
            container_name=settings.COSMOS_CONTAINER_NAME
        )
    return cosmos_db_client

def get_sentiment_calculator():
    """Get or initialize the sentiment calculator."""
    global sentiment_calculator
    if sentiment_calculator is None:
        logger.info("Initializing sentiment calculator")
        sentiment_calculator = SentimentCalculator()
    return sentiment_calculator

app = FastAPI(
    title="News Sentiment Score API Service (NSSS)",
    description="Calculates and serves aggregated sentiment scores for fixed-income instruments",
    version="1.0.0"
)

# Request models for parameter validation
class RealtimeSentimentRequest(BaseModel):
    cusip: Optional[str] = Field(None, description="CUSIP identifier")
    sector: Optional[str] = Field(None, description="Sector name") 
    issuer_name: Optional[str] = Field(None, description="Issuer name")

    def model_post_init(self, __context):
        # Ensure at least one parameter is provided
        if not any([self.cusip, self.sector, self.issuer_name]):
            raise ValueError("At least one of cusip, sector, or issuer_name must be provided")

class HistoricalSentimentRequest(BaseModel):
    as_of_date: str = Field(..., description="Date in YYYY-MM-DD format")
    cusip: Optional[str] = Field(None, description="CUSIP identifier")
    sector: Optional[str] = Field(None, description="Sector name")
    issuer_name: Optional[str] = Field(None, description="Issuer name")

    def model_post_init(self, __context):
        # Ensure at least one entity parameter is provided
        if not any([self.cusip, self.sector, self.issuer_name]):
            raise ValueError("At least one of cusip, sector, or issuer_name must be provided")


def _build_cosmos_query(cusip: Optional[str], sector: Optional[str], issuer_name: Optional[str], 
                       as_of_date: Optional[str] = None) -> tuple[str, List[Dict[str, Any]]]:
    """
    Build Cosmos DB SQL query and parameters based on filter criteria.
    Always includes global_market events and uses OR logic for entity parameters.
    
    Returns:
        tuple: (query_string, parameters_list)
    """
    logger.info(f"Building Cosmos query with cusip={cusip}, sector={sector}, issuer_name={issuer_name}, as_of_date={as_of_date}")
    
    # Build OR conditions for entity parameters using direct string interpolation
    entity_conditions = []
    
    if cusip:
        # Escape single quotes in cusip for SQL safety
        escaped_cusip = cusip.replace("'", "''")
        entity_conditions.append(f"ARRAY_CONTAINS(c.entities.cusips, '{escaped_cusip}')")
    
    if sector:
        # Escape single quotes in sector for SQL safety  
        escaped_sector = sector.replace("'", "''")
        entity_conditions.append(f"c.entities.sector = '{escaped_sector}'")
    
    if issuer_name:
        # Escape single quotes in issuer_name for SQL safety
        escaped_issuer = issuer_name.replace("'", "''")
        entity_conditions.append(f"c.entities.issuer_name = '{escaped_issuer}'")
    
    # Always include global_market events
    entity_conditions.append("c.entities.sector = 'global_market'")
    
    # Build the WHERE clause - simplified structure that works with emulator
    if entity_conditions:
        combined_conditions = " OR ".join(entity_conditions)
        where_clause = f"WHERE ({combined_conditions})"
    else:
        # Fallback to just global_market
        where_clause = "WHERE c.entities.sector = 'global_market'"
    
    # Build complete query
    query_parts = ["SELECT * FROM c", where_clause]
    
    # Add date filtering for historical queries
    if as_of_date:
        query_parts.append(f"AND c.published_at <= '{as_of_date}T23:59:59Z'")
    
    query = " ".join(query_parts)
    parameters = []  # Empty for direct queries
    
    logger.info(f"Built query: {query}")
    logger.info(f"Query parameters: {parameters}")
    
    return query, parameters


def _convert_cosmos_events_to_models(cosmos_events: List[Dict[str, Any]]) -> List[EnrichedNewsEvent]:
    """Convert Cosmos DB documents to EnrichedNewsEvent model objects."""
    logger.info(f"Converting {len(cosmos_events)} Cosmos events to model objects")
    events = []
    for doc in cosmos_events:
        try:
            # Convert datetime strings to datetime objects
            published_at = datetime.fromisoformat(doc["published_at"].replace('Z', '+00:00'))
            ingested_at = datetime.fromisoformat(doc["ingested_at"].replace('Z', '+00:00'))
            
            # Create model objects
            entities = Entities(**doc["entities"])
            sentiment = Sentiment(**doc["sentiment"])
            
            event = EnrichedNewsEvent(
                id=doc["id"],
                source=doc["source"],
                published_at=published_at,
                ingested_at=ingested_at,
                event_type=doc["event_type"],
                entities=entities,
                sentiment=sentiment,
                source_credibility_tier=doc["source_credibility_tier"],
                summary_excerpt=doc["summary_excerpt"],
                raw_article_url=doc["raw_article_url"]
            )
            events.append(event)
        except Exception as e:
            # Log the error but continue processing other events
            logger.error(f"Error converting event {doc.get('id', 'unknown')}: {e}")
            continue
    
    logger.info(f"Successfully converted {len(events)} events")
    return events


@app.get("/health")
async def health_check():
    """Health check endpoint."""
    logger.info("Health check requested")
    return {"status": "ok"}


@app.get("/sentiment/realtime", response_model=RealTimeSentimentResponse)
async def get_realtime_sentiment(
    cusip: Optional[str] = None,
    sector: Optional[str] = None, 
    issuer_name: Optional[str] = None
):
    """
    Calculate real-time sentiment score for a given entity.
    At least one of cusip, sector, or issuer_name is required.
    """
    logger.info(f"Realtime sentiment request: cusip={cusip}, sector={sector}, issuer_name={issuer_name}")
    
    # Validate parameters
    try:
        request = RealtimeSentimentRequest(cusip=cusip, sector=sector, issuer_name=issuer_name)
        logger.info("Parameter validation successful")
    except ValueError as e:
        logger.error(f"Parameter validation failed: {e}")
        raise HTTPException(status_code=422, detail=str(e))
    
    try:
        # Build and execute query
        logger.info("Building Cosmos DB query")
        query, parameters = _build_cosmos_query(cusip, sector, issuer_name)
        
        logger.info("Fetching events from Cosmos DB")
        cosmos_events = get_cosmos_client().query_items(query, None)
        
        # Convert to model objects
        logger.info("Converting Cosmos events to model objects")
        events = _convert_cosmos_events_to_models(cosmos_events)
        
        # Calculate sentiment score
        logger.info("Calculating aggregated sentiment score")
        current_time = datetime.now(timezone.utc)
        result = get_sentiment_calculator()._calculate_ass(events, "REALTIME", current_time)
        
        logger.info(f"Sentiment calculation complete. Score: {result['aggregated_sentiment_score']}, Articles: {result['contributing_articles_count']}")
        
        # Return response
        response = RealTimeSentimentResponse(
            aggregated_sentiment_score=result["aggregated_sentiment_score"],
            contributing_articles_count=result["contributing_articles_count"],
            articles=result["articles"]
        )
        
        logger.info("Realtime sentiment response generated successfully")
        return response
        
    except Exception as e:
        logger.error(f"Internal server error in realtime endpoint: {str(e)}")
        raise HTTPException(status_code=500, detail=f"Internal server error: {str(e)}")


@app.get("/sentiment/historical", response_model=HistoricalSentimentResponse)
async def get_historical_sentiment(
    as_of_date: str,
    cusip: Optional[str] = None,
    sector: Optional[str] = None,
    issuer_name: Optional[str] = None
):
    """
    Calculate historical sentiment score for a given entity as of a specific date.
    Requires as_of_date and at least one of cusip, sector, or issuer_name.
    """
    logger.info(f"Historical sentiment request: as_of_date={as_of_date}, cusip={cusip}, sector={sector}, issuer_name={issuer_name}")
    
    # Validate parameters
    try:
        request = HistoricalSentimentRequest(
            as_of_date=as_of_date, cusip=cusip, sector=sector, issuer_name=issuer_name
        )
        logger.info("Parameter validation successful")
    except ValueError as e:
        logger.error(f"Parameter validation failed: {e}")
        raise HTTPException(status_code=422, detail=str(e))
    
    # Validate date format
    try:
        datetime.fromisoformat(as_of_date)
        logger.info("Date format validation successful")
    except ValueError:
        logger.error(f"Invalid date format: {as_of_date}")
        raise HTTPException(status_code=422, detail="Invalid date format. Use YYYY-MM-DD.")
    
    try:
        # Build and execute query
        logger.info("Building Cosmos DB query for historical data")
        query, parameters = _build_cosmos_query(cusip, sector, issuer_name, as_of_date)
        
        logger.info("Fetching historical events from Cosmos DB")
        cosmos_events = get_cosmos_client().query_items(query, None)
        
        # Convert to model objects
        logger.info("Converting Cosmos events to model objects")
        events = _convert_cosmos_events_to_models(cosmos_events)
        
        # Calculate sentiment score (end of day for historical)
        logger.info("Calculating historical aggregated sentiment score")
        end_of_day = datetime.fromisoformat(f"{as_of_date}T23:59:59+00:00")
        result = get_sentiment_calculator()._calculate_ass(events, "HISTORICAL", end_of_day)
        
        logger.info(f"Historical sentiment calculation complete. Score: {result['aggregated_sentiment_score']}, Articles: {result['contributing_articles_count']}")
        
        # Return response
        response = HistoricalSentimentResponse(
            aggregated_sentiment_score=result["aggregated_sentiment_score"],
            contributing_articles_count=result["contributing_articles_count"]
        )
        
        logger.info("Historical sentiment response generated successfully")
        return response
        
    except Exception as e:
        logger.error(f"Internal server error in historical endpoint: {str(e)}")
        raise HTTPException(status_code=500, detail=f"Internal server error: {str(e)}")


def start():
    """Starts the Uvicorn server."""
    logger.info("Starting Uvicorn server")
    uvicorn.run(
        "src.main:app",
        host="0.0.0.0",
        port=settings.API_PORT,
        reload=False,
        workers=2
    )


if __name__ == "__main__":
    start() 