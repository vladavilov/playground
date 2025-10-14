# Document Processing Service Dockerfile
# 
# This Dockerfile supports conditional VLM (Vision Language Model) configuration:
# - DOCLING_VLM_MODE=local: Downloads and caches SmolVLM model locally, pre-warms docling plugins
# - DOCLING_VLM_MODE=remote: Skips local model download and pre-warming, uses remote VLM APIs
#
# Build with custom VLM mode:
# docker build --build-arg DOCLING_VLM_MODE=remote -t document-processing-service .
#
FROM python-service-base:latest

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PYTHONPATH=/app/document_processing_service/src

# Set DOCLING_VLM_MODE early to control conditional build steps
# Default to 'local' if not provided, but can be overridden via build args
ARG DOCLING_VLM_MODE=local
ENV DOCLING_VLM_MODE=${DOCLING_VLM_MODE}

WORKDIR /app

COPY ./shared/ ./shared/
COPY ./document_processing_service/ ./document_processing_service/

WORKDIR /app/shared
RUN pip install --no-cache-dir .

WORKDIR /app/document_processing_service
RUN pip install --no-cache-dir .

# Install Java Runtime Environment, wget for Tika download, curl for healthchecks, and git for HF downloads
RUN for i in 1 2 3 4 5; do \
    find /etc/apt -type f \( -name 'sources.list' -o -name '*.list' -o -name '*.sources' \) -print0 | xargs -0 sed -i 's|http://|https://|g'; \
    apt-get update && \
    apt-get install -y --no-install-recommends --fix-missing \
        openjdk-17-jre-headless \
        wget \
        libgl1 \
        git && \
    rm -rf /var/lib/apt/lists/* && \
    java -version && \
    break || (echo "Retry $i/5 failed, waiting..." && sleep 5); \
  done

# Set JAVA_HOME and update PATH to include Java
ENV JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64
ENV PATH="${JAVA_HOME}/bin:${PATH}"

# Pre-install Apache Tika server during build phase
ENV TIKA_VERSION=3.1.0
ENV TIKA_SERVER_JAR=/opt/tika-server/tika-server.jar
ENV TIKA_SERVER_ENDPOINT=http://localhost:9998

# Download and validate Tika Server JAR
RUN mkdir -p /opt/tika-server && \
    echo "Downloading Tika Server JAR version ${TIKA_VERSION}..." && \
    wget --quiet --timeout=60 --tries=3 \
    -O ${TIKA_SERVER_JAR} \
    "https://repo1.maven.org/maven2/org/apache/tika/tika-server-standard/${TIKA_VERSION}/tika-server-standard-${TIKA_VERSION}.jar" && \
    echo "Verifying download..." && \
    test -f ${TIKA_SERVER_JAR} && \
    JAR_SIZE=$(stat -c%s ${TIKA_SERVER_JAR}) && \
    echo "JAR downloaded successfully: $JAR_SIZE bytes" && \
    [ "$JAR_SIZE" -gt 50000000 ] && \
    chmod 644 ${TIKA_SERVER_JAR} && \
    echo "Tika JAR ready: ${TIKA_SERVER_JAR}"

# Set default Tika log path for build-time directory creation
ENV TIKA_LOG_PATH=/tmp/tika-logs

# Prepare Tika log directory and initial log file with proper permissions
RUN mkdir -p ${TIKA_LOG_PATH} && \
    touch ${TIKA_LOG_PATH}/tika.log && \
    chmod 755 ${TIKA_LOG_PATH} && \
    chmod 644 ${TIKA_LOG_PATH}/tika.log && \
    echo "Tika log directory and initial log file prepared: ${TIKA_LOG_PATH}/tika.log"

# Note: Tika environment variables are now configured via docker-compose.env or environments

WORKDIR /app

# Hugging Face caches (only create directories for local VLM mode)
ENV HF_HOME=/opt/hf-cache \
    HUGGINGFACE_HUB_CACHE=/opt/hf-cache/hub \
    TRANSFORMERS_CACHE=/opt/hf-cache/transformers \
    HF_REPO_ID=HuggingFaceTB/SmolVLM-256M-Instruct

# Conditionally create HF cache directories only when DOCLING_VLM_MODE=local
RUN if [ "$DOCLING_VLM_MODE" = "local" ]; then \
        echo "DOCLING_VLM_MODE=local: Creating Hugging Face cache directories..." && \
        mkdir -p "$HF_HOME" "$HUGGINGFACE_HUB_CACHE" "$TRANSFORMERS_CACHE" /opt/hf-cache/models; \
    else \
        echo "DOCLING_VLM_MODE=$DOCLING_VLM_MODE: Skipping Hugging Face cache directory creation (using remote VLM)"; \
    fi

# Conditionally predownload local ungated VLM only when DOCLING_VLM_MODE=local
RUN if [ "$DOCLING_VLM_MODE" = "local" ]; then \
        echo "DOCLING_VLM_MODE=local: Downloading and caching Hugging Face model..." && \
        python -m scripts.download_hf_model; \
    else \
        echo "DOCLING_VLM_MODE=$DOCLING_VLM_MODE: Skipping Hugging Face model download (using remote VLM)"; \
    fi

# Conditionally pre-warm docling plugins and models only when DOCLING_VLM_MODE=local
RUN if [ "$DOCLING_VLM_MODE" = "local" ]; then \
        echo "DOCLING_VLM_MODE=local: Pre-warming docling plugins and models..." && \
        python -m scripts.build_init; \
    else \
        echo "DOCLING_VLM_MODE=$DOCLING_VLM_MODE: Skipping docling pre-warm (using remote VLM)"; \
    fi

ENV API_PORT=8000
EXPOSE $API_PORT

# Health check validates both Tika environment (including pre-installed JAR) and Celery
HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:${API_PORT}/health/tika && curl -f http://localhost:${API_PORT}/health/celery || exit 1

CMD ["python", "-m", "main"] 