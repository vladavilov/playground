FROM vllm/vllm-openai:latest

# Environment configuration
ENV DEBIAN_FRONTEND=noninteractive \
    MODEL_ID="openai/gpt-oss-20b" \
    HF_HOME="/opt/hf_cache" \
    PORT="8000" \
    VLLM_ARGS="--api-key=EMPTY" \
    HOME=/home/appuser

LABEL org.opencontainers.image.title="gpt-oss (vLLM, NVIDIA GPU)" \
      org.opencontainers.image.description="OpenAI-compatible API serving openai/gpt-oss-20b via vLLM (CUDA GPUs)" \
      org.opencontainers.image.source="https://github.com/vllm-project/vllm" \
      org.opencontainers.image.licenses="Apache-2.0"

# System tools and non-root user
RUN set -eux; \
    apt-get update && apt-get install -y --no-install-recommends \
      tini curl ca-certificates && \
    rm -rf /var/lib/apt/lists/* && \
    groupadd --system app && useradd -m -u 10001 -g app -s /usr/sbin/nologin -d /home/appuser appuser && \
    mkdir -p ${HF_HOME} && chown -R appuser:app ${HF_HOME} /home/appuser

# Install prefetch utility and pre-download model weights into HF cache
WORKDIR /opt/gpt_oss_gpu
COPY pyproject.toml README.md ./
COPY src ./src
RUN pip install --no-cache-dir . && \
    HF_TOKEN="${HF_TOKEN:-}" gpt-oss-prefetch || true

USER appuser
EXPOSE 8000

HEALTHCHECK --interval=20s --timeout=5s --start-period=30s --retries=5 \
  CMD curl -fsS http://localhost:8000/v1/models || exit 1

# Start vLLM OpenAI-compatible server
ENTRYPOINT ["tini","-g","--","bash","-lc","python -m vllm.entrypoints.openai.api_server --host 0.0.0.0 --port ${PORT} --model ${MODEL_ID} ${VLLM_ARGS}"]


