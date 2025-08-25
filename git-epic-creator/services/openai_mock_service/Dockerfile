FROM python:3.12-slim AS builder

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

WORKDIR /app

COPY ./shared/ ./shared/
COPY ./openai_mock_service/ ./openai_mock_service/

WORKDIR /app/shared
RUN pip install --no-cache-dir .

WORKDIR /app/openai_mock_service
RUN pip install --no-cache-dir .

# Pre-cache embeddings into a local cache to speed up runtime
ENV HF_HOME=/models/hf-cache \
    TRANSFORMERS_CACHE=/models/hf-cache \
    EMBED_MODEL_DIR=/models/embeddings/jina-embeddings-v2-base-en
RUN python -m src.build_init || true

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