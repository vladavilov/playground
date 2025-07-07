import uvicorn
from fastapi import FastAPI

from config import settings

app = FastAPI(
    title="Generic Microservice",
    description="A boilerplate for generic microservices.",
    version="1.0.0"
)

@app.get("/health")
async def health_check():
    """A simple health check endpoint."""
    return {"status": "ok"}

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
