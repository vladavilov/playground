FROM ollama/ollama:latest

ENV DEBIAN_FRONTEND=noninteractive \
    OLLAMA_HOST=0.0.0.0:11434 \
    OLLAMA_MODELS=/models \
    HOME=/home/appuser

LABEL org.opencontainers.image.title="gpt-oss (Ollama, CPU-only)" \
      org.opencontainers.image.description="OpenAI-compatible API serving gpt-oss:20b via Ollama (no GPU required)" \
      org.opencontainers.image.source="https://github.com/ollama/ollama" \
      org.opencontainers.image.licenses="Apache-2.0"

RUN set -eux; \
    find /etc/apt -type f \( -name 'sources.list' -o -name '*.list' -o -name '*.sources' \) -print0 \
      | xargs -0 sed -i 's|http://|https://|g'; \
    apt-get update && apt-get install -y --no-install-recommends \
      tini curl ca-certificates && \
    rm -rf /var/lib/apt/lists/*

# Non-root runtime user and model store
RUN groupadd --system app && useradd -m -u 10001 -g app -s /usr/sbin/nologin -d /home/appuser appuser && \
    mkdir -p ${OLLAMA_MODELS} && chown -R appuser:app ${OLLAMA_MODELS} /home/appuser

# Prefetch model to keep runtime isolated from network and host
USER appuser
RUN sh -lc "ollama serve & sleep 5 && ollama pull gpt-oss:20b && pkill ollama || true"

EXPOSE 11434

HEALTHCHECK --interval=15s --timeout=3s --start-period=20s --retries=3 \
  CMD curl -fsS http://localhost:11434/v1/models || exit 1

ENTRYPOINT ["tini","-g","--","ollama","serve"]

