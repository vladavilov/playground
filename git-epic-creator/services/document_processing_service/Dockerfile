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

# Conditionally pre-warm docling plugins (only for local VLM mode)
# Model is already downloaded in base image, just need to pre-warm
RUN if [ "$DOCLING_VLM_MODE" = "local" ]; then \
        echo "DOCLING_VLM_MODE=local: Pre-warming docling plugins and models..." && \
        echo "SmolVLM model already downloaded in base image, pre-warming docling..." && \
        python -m scripts.build_init && \
        echo "Local VLM environment pre-warmed successfully."; \
    else \
        echo "DOCLING_VLM_MODE=${DOCLING_VLM_MODE}: Skipping local VLM pre-warming"; \
    fi

WORKDIR /app

ENV API_PORT=8000
EXPOSE $API_PORT

# Health check validates all components (basic, Celery, processors, Tika) via consolidated endpoint
HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:${API_PORT}/health/celery || exit 1

CMD ["python", "-m", "main"]
