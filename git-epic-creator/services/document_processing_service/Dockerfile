# Document Processing Service - Application Layer
# 
# This service image contains application code and lightweight components.
# It extends the heavy base image which contains:
# - Java JRE 17, Apache Tika, RapidOCR models, heavy Python libs
#
# Build process:
# 1. First build base: docker build -f Dockerfile.base -t document-processing-base:latest .
# 2. Then build service: docker build -t document-processing-service:latest .
#
# For remote VLM mode (faster build):
# 1. docker build -f Dockerfile.base --build-arg DOCLING_VLM_MODE=remote -t document-processing-base:latest .
# 2. docker build -t document-processing-service:latest .
#
FROM document-processing-base:latest

ENV PYTHONPATH=/app/document_processing_service/src

# Set DOCLING_VLM_MODE from base image (can be overridden)
ARG DOCLING_VLM_MODE
ENV DOCLING_VLM_MODE=${DOCLING_VLM_MODE:-local}

WORKDIR /app

# Copy application code (this changes frequently)
COPY ./shared/ ./shared/
COPY ./document_processing_service/ ./document_processing_service/

# Install shared and service packages (dependencies already installed in base image)
# This reinstall is quick since heavy deps are cached
WORKDIR /app/shared
RUN pip install --no-cache-dir .

WORKDIR /app/document_processing_service
RUN pip install --no-cache-dir .

# Conditionally download HF model and pre-warm docling plugins (only for local VLM mode)
# This step requires the actual application code with scripts/
RUN if [ "$DOCLING_VLM_MODE" = "local" ]; then \
        echo "DOCLING_VLM_MODE=local: Downloading model and pre-warming docling..." && \
        echo "Step 1/2: Downloading and caching Hugging Face model..." && \
        python -m scripts.download_hf_model && \
        echo "Step 2/2: Pre-warming docling plugins and models..." && \
        python -m scripts.build_init && \
        echo "Local VLM environment initialized successfully."; \
    else \
        echo "DOCLING_VLM_MODE=${DOCLING_VLM_MODE}: Skipping local VLM model download"; \
    fi

WORKDIR /app

ENV API_PORT=8000
EXPOSE $API_PORT

# Health check validates all components (basic, Celery, processors, Tika) via consolidated endpoint
HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:${API_PORT}/health/celery || exit 1

CMD ["python", "-m", "main"]
