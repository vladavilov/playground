FROM python:3.12-slim-bookworm

# Build argument to control model precaching
ARG PRECACHE_MODELS=true

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PYTHONPATH=/app/openai_mock_service/src \
    HF_HOME=/app/models/hf-cache \
    TRANSFORMERS_CACHE=/app/models/hf-cache \
    EMBED_MODEL_DIR=/app/models/embeddings/jina-embeddings-v2-base-en

RUN apt-get update && apt-get install -y --no-install-recommends curl ca-certificates && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Ensure model directories exist
RUN mkdir -p /app/models/hf-cache /app/models/embeddings/jina-embeddings-v2-base-en

# Copy sources and install local packages
COPY ./shared/ ./shared/
COPY ./openai_mock_service/ ./openai_mock_service/

WORKDIR /app/shared
RUN pip install --retries 2 --prefer-binary --timeout 30 --no-cache-dir .

WORKDIR /app/openai_mock_service
RUN pip install --retries 2 --prefer-binary --timeout 30 --no-cache-dir .

WORKDIR /app

# Download and cache embeddings model during build if PRECACHE_MODELS is true
# This ensures the model is available offline at runtime
RUN if [ "$PRECACHE_MODELS" = "true" ]; then \
        echo "Downloading and caching embeddings model..."; \
        python -m build_init || exit 1; \
    else \
        echo "Skipping model precaching (PRECACHE_MODELS=$PRECACHE_MODELS)"; \
    fi

ENV API_PORT=8000
EXPOSE $API_PORT

HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:${API_PORT}/health || exit 1

CMD ["python", "-m", "main"]