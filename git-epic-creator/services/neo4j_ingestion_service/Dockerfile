FROM python:3.13-slim-bookworm AS builder-with-tools

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

# Install build tools with retry logic and better error handling
RUN for i in 1 2 3 4 5; do \
        apt-get update && \
        apt-get install -y --no-install-recommends --fix-missing \
            build-essential && \
        rm -rf /var/lib/apt/lists/* && \
        break || \
        (echo "Retry $i/5 failed, waiting..." && sleep 5); \
    done && \
    gcc --version

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH" \
    PIP_DEFAULT_TIMEOUT=120 \
    PIP_DISABLE_PIP_VERSION_CHECK=1 \
    PIP_NO_CACHE_DIR=1

RUN pip install --retries 10 --timeout 120 --upgrade pip setuptools wheel

WORKDIR /app

COPY ./shared/ ./shared/
WORKDIR /app/shared
RUN pip install --retries 10 --prefer-binary --timeout 120 .

WORKDIR /app
COPY ./neo4j_ingestion_service/ ./neo4j_ingestion_service/

WORKDIR /app/neo4j_ingestion_service
RUN pip install --retries 10 --prefer-binary --timeout 120 .

FROM python:3.13-slim-bookworm AS final

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

# Install runtime dependencies with retry logic
RUN for i in 1 2 3 4 5; do \
        apt-get update && \
        apt-get install -y --no-install-recommends --fix-missing \
            curl \
            libstdc++6 \
            libgcc-s1 \
            libgomp1 && \
        rm -rf /var/lib/apt/lists/* && \
        break || \
        (echo "Retry $i/5 failed, waiting..." && sleep 5); \
    done && \
    curl --version

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

WORKDIR /app

COPY --from=builder-with-tools /opt/venv /opt/venv
ENV PYTHONPATH=/app/src

COPY --from=builder-with-tools /app/neo4j_ingestion_service/src/ ./src/

# Pre-create the GraphRAG workspace root so CLI has a place to initialize
ENV RAG_WORKSPACE_ROOT=/workspace/graphrag
RUN mkdir -p ${RAG_WORKSPACE_ROOT}

ENV API_PORT=8000
EXPOSE $API_PORT

HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:${API_PORT}/health || exit 1

CMD ["python", "-m", "src.main"] 