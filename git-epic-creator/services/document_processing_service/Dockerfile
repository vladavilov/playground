FROM python:3.13-slim-bookworm AS builder

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

WORKDIR /app

COPY ./shared/ ./shared/
COPY ./document_processing_service/ ./document_processing_service/

WORKDIR /app/shared
RUN pip install --no-cache-dir .

WORKDIR /app/document_processing_service
RUN pip install --no-cache-dir .

FROM python:3.13-slim-bookworm AS final

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

# Install Java Runtime Environment, wget for Tika download, curl for healthchecks, and git for HF downloads
RUN apt-get update && apt-get install -y \
    openjdk-17-jre-headless \
    wget \
    curl \
    git \
    && rm -rf /var/lib/apt/lists/* && \
    java -version

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

COPY --from=builder /opt/venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"
ENV PYTHONPATH=/app/src

WORKDIR /app

COPY --from=builder /app/document_processing_service/src/ ./src/

# Hugging Face caches and predownload
ENV HF_HOME=/opt/hf-cache \
    HUGGINGFACE_HUB_CACHE=/opt/hf-cache/hub \
    TRANSFORMERS_CACHE=/opt/hf-cache/transformers \
    HF_REPO_ID=HuggingFaceTB/SmolVLM-256M-Instruct

RUN mkdir -p "$HF_HOME" "$HUGGINGFACE_HUB_CACHE" "$TRANSFORMERS_CACHE" /opt/hf-cache/models

# Predownload local ungated VLM to bake into the image layer
RUN python -m src.scripts.download_hf_model

ENV API_PORT=8000
EXPOSE $API_PORT

# Health check validates both Tika environment (including pre-installed JAR) and Celery
HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:${API_PORT}/health/tika && curl -f http://localhost:${API_PORT}/health/celery || exit 1

CMD ["python", "-m", "src.main"] 