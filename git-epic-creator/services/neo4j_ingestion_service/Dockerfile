FROM python:3.12-slim-bookworm

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PIP_DEFAULT_TIMEOUT=120 \
    PIP_DISABLE_PIP_VERSION_CHECK=1 \
    PIP_NO_CACHE_DIR=1 \
    PYTHONPATH=/app/neo4j_ingestion_service/src

# Install build/runtime deps with retry logic
RUN for i in 1 2 3 4 5; do \
        find /etc/apt -type f \( -name 'sources.list' -o -name '*.list' -o -name '*.sources' \) -print0 | xargs -0 sed -i 's|http://|https://|g'; \
        apt-get update && \
        apt-get install -y --no-install-recommends --fix-missing \
            ca-certificates \
            build-essential \
            curl \
            libstdc++6 \
            libgcc-s1 \
            libgomp1 && \
        rm -rf /var/lib/apt/lists/* && \
        break || \
        (echo "Retry $i/5 failed, waiting..." && sleep 5); \
    done && \
    update-ca-certificates && \
    gcc --version && curl --version

WORKDIR /app

COPY ./shared/ ./shared/
WORKDIR /app/shared
RUN pip install --retries 10 --prefer-binary --timeout 120 .

WORKDIR /app
COPY ./neo4j_ingestion_service/ ./neo4j_ingestion_service/

WORKDIR /app/neo4j_ingestion_service
RUN pip install --retries 10 --prefer-binary --timeout 120 .

WORKDIR /app

ENV API_PORT=8000
EXPOSE $API_PORT

HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:${API_PORT}/health || exit 1

CMD ["python", "-m", "main"] 