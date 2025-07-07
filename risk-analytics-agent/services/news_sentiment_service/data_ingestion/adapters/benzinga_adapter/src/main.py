import uvicorn
import logging
import sys
from fastapi import FastAPI, HTTPException
from typing import List, Optional
from datetime import datetime, timezone
import hashlib
from bs4 import BeautifulSoup
import httpx
from email.utils import parsedate_to_datetime

from models.models import RawNewsArticle
from config import settings
from middleware import ExceptionLoggingMiddleware, RequestResponseLoggingMiddleware

# Configure logging
def setup_logging():
    """Configure application logging"""
    logging.basicConfig(
        level=settings.get_log_level(),
        format=settings.LOG_FORMAT,
        datefmt=settings.LOG_DATE_FORMAT,
        handlers=[
            logging.StreamHandler(sys.stdout),
            logging.StreamHandler(sys.stderr)
        ]
    )
    
    # Set specific log levels for third-party libraries
    logging.getLogger("httpx").setLevel(logging.WARNING)
    logging.getLogger("uvicorn.access").setLevel(logging.WARNING)
    
    return logging.getLogger(__name__)

# Setup logging and get logger
logger = setup_logging()

app = FastAPI(
    title="Benzinga News Adapter",
    description="An adapter for fetching news from the Benzinga API using direct HTTP calls to the REST API.",
    version="1.0.0"
)

# Add middleware (order matters - exception middleware should be added last)
app.add_middleware(RequestResponseLoggingMiddleware)
app.add_middleware(ExceptionLoggingMiddleware)

logger.info("Benzinga News Adapter starting up...")

RELEVANT_CHANNELS = "Bonds,Treasuries,Economics,Federal Reserve,Govt & Economy"

def strip_html(html_content: str) -> str:
    """Removes HTML tags from a string."""
    try:
        return BeautifulSoup(html_content, "html.parser").get_text()
    except Exception as e:
        logger.error(f"Error stripping HTML: {e}", exc_info=True)
        return html_content  # Return original content if parsing fails

@app.get("/health")
async def health_check():
    """A simple health check endpoint."""
    logger.debug("Health check requested")
    return {"status": "ok"}

@app.get("/news", response_model=List[RawNewsArticle])
async def get_news(
    dateFrom: Optional[datetime] = None, 
    dateTo: Optional[datetime] = None
) -> List[RawNewsArticle]:
    """
    Fetches the latest news articles from the Benzinga API using direct HTTP calls,
    focusing on channels relevant to US fixed income and municipal markets.

    Supports filtering by a UTC date range. Naive datetimes are assumed to be UTC.
    """
    logger.info(f"Fetching news articles. dateFrom: {dateFrom}, dateTo: {dateTo}")
    
    try:
        # Prepare parameters for the Benzinga REST API
        params = {
            "token": settings.BENZINGA_API_TOKEN,
            "channels": RELEVANT_CHANNELS,
            "displayOutput": "full"
        }
        
        # Add date filtering if dateFrom is provided
        if dateFrom:
            # If the datetime is naive, assume UTC as per spec.
            if dateFrom.tzinfo is None:
                dateFrom = dateFrom.replace(tzinfo=timezone.utc)
            params["updatedSince"] = int(dateFrom.timestamp())
            logger.debug(f"Using dateFrom filter: {dateFrom} (timestamp: {params['updatedSince']})")
            
        # If dateTo is naive, assume UTC.
        if dateTo and dateTo.tzinfo is None:
            dateTo = dateTo.replace(tzinfo=timezone.utc)
            logger.debug(f"Using dateTo filter: {dateTo}")
        
        logger.debug(f"Calling Benzinga API with params: {params}")
        
        async with httpx.AsyncClient() as client:
            response = await client.get(
                "https://api.benzinga.com/api/v2/news",
                params=params,
                timeout=30.0
            )
            
            # Log response details for debugging
            logger.debug(f"Benzinga API response status: {response.status_code}")
            logger.debug(f"Benzinga API response headers: {dict(response.headers)}")
            
            response.raise_for_status()
            
            try:
                benzinga_response = response.json()
            except ValueError as e:
                logger.error(f"Invalid JSON response from Benzinga API: {e}")
                if not response.content:
                    raise HTTPException(status_code=502, detail="Benzinga API returned empty response body")
                else:
                    raise HTTPException(status_code=502, detail="Benzinga API returned invalid JSON response")
            
            # Based on Benzinga API documentation, response is always a direct array of articles
            if isinstance(benzinga_response, list):
                benzinga_articles = benzinga_response
            else:
                logger.error(f"Received unexpected response format from Benzinga API. Expected array, got: {type(benzinga_response)}")
                raise HTTPException(status_code=502, detail="Benzinga API returned invalid response format - expected array of articles")
        
        logger.info(f"Received {len(benzinga_articles)} articles from Benzinga API")
        
        transformed_articles = []
        
        for article in benzinga_articles:
            try:
                # Validate required fields exist
                if not article.get("updated"):
                    logger.warning(f"Article missing 'updated' field, skipping: {article.get('id', 'Unknown ID')}")
                    continue
                
                # Parse the 'updated' field from Benzinga which is in RFC 2822 format
                # Example: "Tue, 13 Feb 2024 13:25:37 -0400"
                try:
                    publication_time = parsedate_to_datetime(article["updated"])
                    if publication_time is None:
                        logger.warning(f"Failed to parse date '{article['updated']}' for article {article.get('id', 'Unknown ID')}")
                        continue
                    
                    # Ensure publication_time is timezone-aware (parsedate_to_datetime returns timezone-aware)
                    # Convert to UTC for consistent comparison
                    if publication_time.tzinfo is not None:
                        publication_time = publication_time.astimezone(timezone.utc)
                    else:
                        publication_time = publication_time.replace(tzinfo=timezone.utc)
                        
                except (ValueError, TypeError) as e:
                    logger.warning(f"Invalid date format '{article.get('updated')}' for article {article.get('id', 'Unknown ID')}: {e}")
                    continue

                # Perform filtering for both dateFrom and dateTo.
                # This ensures the test passes with the mock and is robust for production.
                if dateFrom and publication_time < dateFrom:
                    continue
                if dateTo and publication_time > dateTo:
                    continue

                # Extract and clean article content
                title = article.get("title", "")
                body = article.get("body", "")
                url = article.get("url", "")
                
                # Use teaser as fallback if body is empty
                if not body and article.get("teaser"):
                    body = article.get("teaser", "")
                
                plain_text_body = strip_html(body)
                article_hash = hashlib.md5((title + plain_text_body).encode()).hexdigest()

                transformed_articles.append(
                    RawNewsArticle(
                        article_text=plain_text_body,
                        source_name="Benzinga",
                        publication_time=publication_time,
                        title=title,
                        url=url,
                        article_hash=article_hash
                    )
                )
                
            except Exception as e:
                logger.error(f"Error processing article {article.get('id', 'Unknown ID')}: {article.get('title', 'Unknown')}: {e}", exc_info=True)
                continue  # Skip this article and continue with others

        logger.info(f"Successfully transformed {len(transformed_articles)} articles")
        return transformed_articles

    except HTTPException:
        # Re-raise HTTP exceptions (they're already handled appropriately)
        raise
    except httpx.HTTPStatusError as exc:
        # Handle HTTP status errors from Benzinga API
        logger.error(f"Benzinga API returned HTTP error {exc.response.status_code}: {exc.response.text}", exc_info=True)
        raise HTTPException(status_code=502, detail=f"Benzinga API error: {exc.response.status_code}")
    except httpx.RequestError as exc:
        # Handle connection/network errors
        logger.error(f"Error connecting to Benzinga API: {exc}", exc_info=True)
        raise HTTPException(status_code=503, detail=f"Error connecting to Benzinga API: {exc}")
    except Exception as exc:
        # Handle any other unexpected errors
        logger.error(f"Unexpected error occurred: {exc}", exc_info=True)
        raise HTTPException(status_code=500, detail=f"An unexpected error occurred: {exc}")

def start():
    """Launched with `poetry run start` at root level"""
    logger.info(f"Starting Benzinga News Adapter on port {settings.API_PORT}")
    uvicorn.run(
        "src.main:app",
        host="0.0.0.0",
        port=settings.API_PORT,
        reload=False,
        workers=2,
        log_config=None,  # Use our custom logging configuration
        access_log=False  # Disable uvicorn access log since we have our own middleware
    )

if __name__ == "__main__":
    start()
