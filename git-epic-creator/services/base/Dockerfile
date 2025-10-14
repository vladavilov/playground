# Common runtime base for Python 3.12 services
# Services handle their own dependency installation
FROM python:3.12-slim-bookworm

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PIP_NO_CACHE_DIR=1 \
    PIP_DISABLE_PIP_VERSION_CHECK=1 \
    PIP_DEFAULT_TIMEOUT=120

RUN for i in 1 2 3 4 5; do \
      find /etc/apt -type f \( -name 'sources.list' -o -name '*.list' -o -name '*.sources' \) -print0 | xargs -0 sed -i 's|http://|https://|g'; \
      apt-get update && \
      apt-get install -y --no-install-recommends curl ca-certificates && \
      rm -rf /var/lib/apt/lists/* && \
      break || (echo "Retry $i/5 failed, waiting..." && sleep 5); \
    done && update-ca-certificates

WORKDIR /app
EXPOSE 8000

