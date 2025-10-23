FROM python:3.12-slim-bookworm

ARG PRECACHE_MODELS=false

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PYTHONPATH=/app/openai_mock_service/src

RUN for i in 1 2 3 4 5; do \
        find /etc/apt -type f \( -name 'sources.list' -o -name '*.list' -o -name '*.sources' \) -print0 | xargs -0 sed -i 's|http://|https://|g'; \
        apt-get update && \
        apt-get install -y --no-install-recommends --fix-missing \
            ca-certificates \
            curl && \
        rm -rf /var/lib/apt/lists/* && \
        break || \
        (echo "Retry $i/5 failed, waiting..." && sleep 5); \
    done && \
    update-ca-certificates && curl --version

WORKDIR /app

# No model/cache directories required

# Copy sources and install local packages
COPY ./shared/ ./shared/
COPY ./openai_mock_service/ ./openai_mock_service/

WORKDIR /app/shared
RUN pip install --retries 2 --prefer-binary --timeout 30 --no-cache-dir .

WORKDIR /app/openai_mock_service
RUN pip install --retries 2 --prefer-binary --timeout 30 --no-cache-dir .

WORKDIR /app

# No model pre-caching step required

ENV API_PORT=8000
EXPOSE $API_PORT

HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:${API_PORT}/health || exit 1

CMD ["python", "-m", "main"]