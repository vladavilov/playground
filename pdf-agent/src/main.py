from fastapi import FastAPI
from contextlib import asynccontextmanager
from src.logger_config import setup_logging, get_logger

# Initialize logging as early as possible
setup_logging()

log = get_logger(__name__)

# Define the lifespan context manager
@asynccontextmanager
async def lifespan(app: FastAPI):
    log.info("Application startup complete.")
    yield
    # Add any shutdown logic here if needed in the future
    log.info("Application shutdown complete.")

app = FastAPI(lifespan=lifespan)

@app.get("/health")
async def health_check():
    log.info("Health check endpoint was called.")
    return {"status": "ok"}

# To run this application (for local testing):
# uvicorn src.main:app --reload --log-level trace 