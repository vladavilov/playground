import structlog
from fastapi import FastAPI
import gitlab
import redis.asyncio as redis

from configuration.logging_config import configure_logging
from utils.app_factory import FastAPIFactory
from config import get_gitlab_client_settings
from dependencies import create_redis_client

from routers.gitlab_router import router as gitlab_router


configure_logging()
logger = structlog.get_logger(__name__)

app: FastAPI = FastAPIFactory.create_app(
    title="GitLab Client Service",
    description="Thin REST facade over GitLab with normalized models",
    version="1.0.0",
    enable_cors=True,
    enable_redis=True
)

app.include_router(gitlab_router)


@app.get("/health/gitlab", tags=["Health"])
async def health_gitlab():
    """
    Check GitLab connectivity.
    
    Note: This check uses a dummy token since actual auth is per-request.
    """
    settings = get_gitlab_client_settings()
    
    try:
        # Create a minimal client to check GitLab reachability
        test_client = gitlab.Gitlab(
            url=settings.GITLAB_BASE_URL,
            timeout=5.0
        )
        
        # Try to get version (public endpoint)
        version = test_client.version()
        
        return {
            "healthy": True,
            "gitlab_url": settings.GITLAB_BASE_URL,
            "gitlab_version": version
        }
    except Exception as e:
        logger.error("GitLab health check failed", error=str(e))
        return {
            "healthy": False,
            "gitlab_url": settings.GITLAB_BASE_URL,
            "error": str(e)
        }


@app.get("/health/embeddings", tags=["Health"])
async def health_embeddings():
    """Check embedding provider connectivity (OpenAI or Azure OpenAI)."""
    settings = get_gitlab_client_settings()
    llm_cfg = settings.llm
    
    if not llm_cfg.OAI_BASE_URL:
        return {
            "healthy": False,
            "error": "OAI_BASE_URL not configured"
        }
    
    try:
        from openai import OpenAI, AzureOpenAI
        
        # Detect Azure vs standard OpenAI
        is_azure = llm_cfg.OAI_API_VERSION is not None
        
        if is_azure:
            client = AzureOpenAI(
                api_key=llm_cfg.OAI_KEY or "dummy-key",
                azure_endpoint=llm_cfg.OAI_BASE_URL,
                api_version=llm_cfg.OAI_API_VERSION,
                timeout=5.0
            )
            provider = "Azure OpenAI"
        else:
            client = OpenAI(
                api_key=llm_cfg.OAI_KEY or "dummy-key",
                base_url=llm_cfg.OAI_BASE_URL,
                timeout=5.0
            )
            provider = "OpenAI"
        
        # Try a minimal embedding request
        response = client.embeddings.create(
            model=llm_cfg.OAI_EMBED_MODEL,
            input=["health check"]
        )
        
        embedding_dim = len(response.data[0].embedding) if response.data else 0
        
        return {
            "healthy": True,
            "provider": provider,
            "base_url": llm_cfg.OAI_BASE_URL,
            "model": llm_cfg.OAI_EMBED_MODEL,
            "embedding_dimension": embedding_dim,
            "api_version": llm_cfg.OAI_API_VERSION if is_azure else None
        }
    except Exception as e:
        logger.error("Embedding health check failed", error=str(e), error_type=type(e).__name__)
        return {
            "healthy": False,
            "base_url": llm_cfg.OAI_BASE_URL,
            "error": str(e)
        }


logger.info("GitLab Client Service ready")


