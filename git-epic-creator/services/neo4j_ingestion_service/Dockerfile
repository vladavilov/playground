FROM python:3.12-slim AS builder

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

RUN apt-get update \
 && apt-get install -y --no-install-recommends build-essential \
 && rm -rf /var/lib/apt/lists/*

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH" \
    PIP_DEFAULT_TIMEOUT=120 \
    PIP_DISABLE_PIP_VERSION_CHECK=1 \
    PIP_NO_CACHE_DIR=1

# Ensure modern packaging tools and be resilient to slow networks
RUN pip install --retries 10 --timeout 120 --upgrade pip setuptools wheel

WORKDIR /app

COPY ./shared/ ./shared/
COPY ./neo4j_ingestion_service/ ./neo4j_ingestion_service/

WORKDIR /app/shared
RUN pip install --retries 10 --prefer-binary --timeout 120 .

WORKDIR /app/neo4j_ingestion_service
RUN pip install --retries 10 --prefer-binary --timeout 120 .

FROM python:3.12-slim AS final

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
    curl \
    libstdc++6 \
    libgcc-s1 \
    libgomp1 \
 && rm -rf /var/lib/apt/lists/*

COPY --from=builder /opt/venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"
ENV PYTHONPATH=/app/src

WORKDIR /app

COPY --from=builder /app/neo4j_ingestion_service/src/ ./src/

# Pre-create the GraphRAG workspace root so CLI has a place to initialize
ENV RAG_WORKSPACE_ROOT=/workspace/graphrag
RUN mkdir -p ${RAG_WORKSPACE_ROOT}

ENV API_PORT=8000
EXPOSE $API_PORT

HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:${API_PORT}/health || exit 1

CMD ["python", "-m", "src.main"] 