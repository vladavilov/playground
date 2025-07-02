from fastapi import FastAPI


SERVICE_NAME = "<YOUR_SERVICE_NAME>"

app = FastAPI(title=SERVICE_NAME)


@app.get("/health", tags=["health"])
def healthcheck():  # pragma: no cover
    """Liveness probe used by container HEALTHCHECK."""
    return {"status": "ok"}
