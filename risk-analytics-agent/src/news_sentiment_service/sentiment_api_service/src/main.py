import uvicorn
from datetime import datetime, timezone
from typing import Optional, List, Dict, Any
from fastapi import FastAPI, HTTPException, Depends
from pydantic import BaseModel, Field

import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..', 'common', 'src'))

from config import settings
from sentiment_calculator import SentimentCalculator
from cosmos_db.cosmos_db_client import CosmosDBClient
from models.models import EnrichedNewsEvent, Entities, Sentiment, RealTimeSentimentResponse, HistoricalSentimentResponse

# Global components (initialized lazily)
cosmos_db_client = None
sentiment_calculator = None

def get_cosmos_client():
    """Get or initialize the Cosmos DB client."""
    global cosmos_db_client
    if cosmos_db_client is None:
        cosmos_db_client = CosmosDBClient(
            database_name=settings.COSMOS_DATABASE_NAME,
            container_name=settings.COSMOS_CONTAINER_NAME
        )
    return cosmos_db_client

def get_sentiment_calculator():
    """Get or initialize the sentiment calculator."""
    global sentiment_calculator
    if sentiment_calculator is None:
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
    
    Returns:
        tuple: (query_string, parameters_list)
    """
    query_parts = ["SELECT * FROM c WHERE 1=1"]
    parameters = []
    
    if cusip:
        query_parts.append("AND ARRAY_CONTAINS(c.entities.cusips, @cusip)")
        parameters.append({"name": "@cusip", "value": cusip})
    
    if sector:
        query_parts.append("AND c.entities.sector = @sector")
        parameters.append({"name": "@sector", "value": sector})
    
    if issuer_name:
        query_parts.append("AND c.entities.issuer_name = @issuer_name")
        parameters.append({"name": "@issuer_name", "value": issuer_name})
    
    if as_of_date:
        # For historical queries, only include events published before end of the specified date
        query_parts.append("AND c.published_at <= @end_of_date")
        end_of_date = f"{as_of_date}T23:59:59Z"
        parameters.append({"name": "@end_of_date", "value": end_of_date})
    
    query = " ".join(query_parts)
    return query, parameters


def _convert_cosmos_events_to_models(cosmos_events: List[Dict[str, Any]]) -> List[EnrichedNewsEvent]:
    """Convert Cosmos DB documents to EnrichedNewsEvent model objects."""
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
            print(f"Error converting event {doc.get('id', 'unknown')}: {e}")
            continue
    
    return events


@app.get("/health")
async def health_check():
    """Health check endpoint."""
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
    # Validate parameters
    try:
        request = RealtimeSentimentRequest(cusip=cusip, sector=sector, issuer_name=issuer_name)
    except ValueError as e:
        raise HTTPException(status_code=422, detail=str(e))
    
    try:
        # Build and execute query
        query, parameters = _build_cosmos_query(cusip, sector, issuer_name)
        cosmos_events = get_cosmos_client().query_items(query, parameters)
        
        # Convert to model objects
        events = _convert_cosmos_events_to_models(cosmos_events)
        
        # Calculate sentiment score
        current_time = datetime.now(timezone.utc)
        result = get_sentiment_calculator()._calculate_ass(events, "REALTIME", current_time)
        
        # Return response
        return RealTimeSentimentResponse(
            aggregated_sentiment_score=result["aggregated_sentiment_score"],
            contributing_articles_count=result["contributing_articles_count"],
            articles=result["articles"]
        )
        
    except Exception as e:
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
    # Validate parameters
    try:
        request = HistoricalSentimentRequest(
            as_of_date=as_of_date, cusip=cusip, sector=sector, issuer_name=issuer_name
        )
    except ValueError as e:
        raise HTTPException(status_code=422, detail=str(e))
    
    # Validate date format
    try:
        datetime.fromisoformat(as_of_date)
    except ValueError:
        raise HTTPException(status_code=422, detail="Invalid date format. Use YYYY-MM-DD.")
    
    try:
        # Build and execute query
        query, parameters = _build_cosmos_query(cusip, sector, issuer_name, as_of_date)
        cosmos_events = get_cosmos_client().query_items(query, parameters)
        
        # Convert to model objects
        events = _convert_cosmos_events_to_models(cosmos_events)
        
        # Calculate sentiment score (end of day for historical)
        end_of_day = datetime.fromisoformat(f"{as_of_date}T23:59:59+00:00")
        result = get_sentiment_calculator()._calculate_ass(events, "HISTORICAL", end_of_day)
        
        # Return response
        return HistoricalSentimentResponse(
            aggregated_sentiment_score=result["aggregated_sentiment_score"],
            contributing_articles_count=result["contributing_articles_count"]
        )
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Internal server error: {str(e)}")


def start():
    """Starts the Uvicorn server."""
    uvicorn.run(
        "src.main:app",
        host="0.0.0.0",
        port=settings.API_PORT,
        reload=False,
        workers=2
    )


if __name__ == "__main__":
    start() 