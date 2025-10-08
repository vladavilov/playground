FROM python-service-base:latest AS builder-with-tools

# Add build tools for compiling native extensions (e.g., GraphRAG dependencies)
RUN apt-get update \
 && apt-get install -y --no-install-recommends build-essential \
 && rm -rf /var/lib/apt/lists/*

# Special pip settings for large/slow packages
ENV PIP_DEFAULT_TIMEOUT=120 \
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

FROM python-service-base:latest AS final

# Install runtime libraries for compiled C/C++ extensions
# Note: curl and ca-certificates already provided by base image
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
    libstdc++6 \
    libgcc-s1 \
    libgomp1 \
 && rm -rf /var/lib/apt/lists/*

# Note: PATH, PYTHONPATH, and WORKDIR already configured by base image

COPY --from=builder-with-tools /opt/venv /opt/venv

COPY --from=builder-with-tools /app/neo4j_ingestion_service/src/ ./src/

# Pre-create the GraphRAG workspace root so CLI has a place to initialize
ENV RAG_WORKSPACE_ROOT=/workspace/graphrag
RUN mkdir -p ${RAG_WORKSPACE_ROOT}

ENV API_PORT=8000
EXPOSE $API_PORT

HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:${API_PORT}/health || exit 1

CMD ["python", "-m", "src.main"] 