FROM python:3.12-slim AS builder

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

WORKDIR /app

# 1) Pre-install runtime dependencies to leverage Docker layer caching.
#    If only source code changes but dependencies stay the same, this step is reused.
RUN pip install --no-cache-dir \
    fastapi \
    uvicorn \
    pydantic \
    structlog \
    sentence-transformers \
    orjson \
    neo4j \
    python-dotenv \
    tenacity

# 2) Copy sources and install local packages (quick when deps unchanged)
COPY ./openai_mock_service/ ./openai_mock_service/

WORKDIR /app/openai_mock_service
RUN pip install --no-cache-dir .

# Ensure model directories exist so the final stage COPY always succeeds
RUN mkdir -p /models/hf-cache /models/embeddings/jina-embeddings-v2-base-en

# Pre-cache embeddings into a local cache to speed up runtime (optional)
ARG PRECACHE_MODELS=false
ENV HF_HOME=/models/hf-cache \
    TRANSFORMERS_CACHE=/models/hf-cache \
    EMBED_MODEL_DIR=/models/embeddings/jina-embeddings-v2-base-en
RUN if [ "$PRECACHE_MODELS" = "true" ]; then python -m src.build_init || true; fi

FROM python:3.12-slim AS final

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

RUN apt-get update && apt-get install -y --no-install-recommends curl && rm -rf /var/lib/apt/lists/*

COPY --from=builder /opt/venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"
ENV PYTHONPATH=/app/src
ENV HF_HOME=/app/models/hf-cache \
    TRANSFORMERS_CACHE=/app/models/hf-cache

WORKDIR /app

COPY --from=builder /app/openai_mock_service/src/ ./src/
COPY --from=builder /models /app/models

ENV API_PORT=8000
EXPOSE $API_PORT

HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:${API_PORT}/health || exit 1

CMD ["python", "-m", "src.main"] 