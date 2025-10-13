FROM python:3.12-slim AS builder

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

WORKDIR /app

# Copy only this service context
COPY ./gpt_oss/ ./gpt_oss/

WORKDIR /app/gpt_oss
RUN pip install --no-cache-dir .

# Pre-cache embeddings into a local cache to speed up runtime
ENV HF_HOME=/models/hf-cache \
    TRANSFORMERS_CACHE=/models/hf-cache \
    EMBED_MODEL_DIR=/models/embeddings/jina-embeddings-v2-base-en
RUN python -m src.build_init || true

FROM ollama/ollama:0.3.14 AS final

ENV DEBIAN_FRONTEND=noninteractive \
    OLLAMA_HOST=127.0.0.1:11435 \
    OLLAMA_MODELS=/models \
    HOME=/home/appuser \
    API_PORT=11434 \
    PYTHONPATH=/app/src \
    PATH="/opt/venv/bin:$PATH"

LABEL org.opencontainers.image.title="gpt-oss (Ollama, CPU-only)" \
      org.opencontainers.image.description="OpenAI-compatible API serving gpt-oss:20b via Ollama (no GPU required)" \
      org.opencontainers.image.source="https://github.com/ollama/ollama" \
      org.opencontainers.image.licenses="Apache-2.0"

RUN set -eux; \
    find /etc/apt -type f \( -name 'sources.list' -o -name '*.list' -o -name '*.sources' \) -print0 \
      | xargs -0 sed -i 's|http://|https://|g'; \
    apt-get update && apt-get install -y --no-install-recommends \
      tini curl ca-certificates bash && \
    rm -rf /var/lib/apt/lists/*

# Non-root runtime user and model store
RUN groupadd --system app && useradd -m -u 10001 -g app -s /usr/sbin/nologin -d /home/appuser appuser && \
    mkdir -p ${OLLAMA_MODELS} && chown -R appuser:app ${OLLAMA_MODELS} /home/appuser

# Bring in Python runtime, app, and pre-fetched embeddings/HF cache
COPY --from=builder /opt/venv /opt/venv
COPY --from=builder /app/gpt_oss/src/ /app/src/
COPY --from=builder /models ${OLLAMA_MODELS}
RUN chown -R appuser:app ${OLLAMA_MODELS} /opt/venv /app/src

# Prefetch gpt-oss model to keep runtime isolated from network and host
USER appuser
RUN sh -lc "ollama serve & sleep 5 && ollama pull gpt-oss:20b && pkill ollama || true"

# Startup script to run Ollama on 11435 and FastAPI on 11434
USER root
RUN printf '%s\n' \
  '#!/usr/bin/env bash' \
  'set -euo pipefail' \
  'export OLLAMA_HOST=127.0.0.1:11435' \
  'ollama serve & ' \
  'sleep 2' \
  'exec uvicorn src.main:app --host 0.0.0.0 --port ${API_PORT}' \
  > /usr/local/bin/start.sh && \
  chmod +x /usr/local/bin/start.sh
USER appuser

EXPOSE 11434

HEALTHCHECK --interval=15s --timeout=3s --start-period=20s --retries=3 \
  CMD curl -fsS http://localhost:${API_PORT}/health || exit 1

ENTRYPOINT ["tini","-g","--","/usr/local/bin/start.sh"]

