#syntax=docker/dockerfile:1

# This Dockerfile is designed to be built from the parent 'news_sentiment_service' directory.
# It requires a build argument 'SERVICE_ROOT_IN_CONTEXT' to specify the service's location.
# Example Build Command:
# docker build -t my-service -f ./templates/data_ingestion_service_boilerplate/Dockerfile --build-arg SERVICE_ROOT_IN_CONTEXT=./data_ingestion/adapters/my_new_adapter .

FROM python:3.13-slim AS builder

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

WORKDIR /app

# This build argument points to the service's directory from the build context root.
ARG SERVICE_ROOT_IN_CONTEXT

# 1. Copy the necessary source directories.
COPY ./common/ ./common/
COPY ${SERVICE_ROOT_IN_CONTEXT} ${SERVICE_ROOT_IN_CONTEXT}/

# 2. Install the common library first from its own directory.
WORKDIR /app/common
RUN pip install --no-cache-dir .

# 3. Then, install the specific service from its own directory.
WORKDIR /app/${SERVICE_ROOT_IN_CONTEXT}
RUN pip install --no-cache-dir .

FROM python:3.13-slim AS final

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

COPY --from=builder /opt/venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"
ENV PYTHONPATH=/app/src

# Standardize the port.
ENV API_PORT=8000
EXPOSE $API_PORT

HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:${API_PORT}/health || exit 1

# In the final stage, only copy the application's own source code from the builder.
COPY --from=builder /app/${SERVICE_ROOT_IN_CONTEXT}/src/ ./src/

# The port is now configured via the settings model, no need to expose here.

# The CMD now simply executes the Python application.
CMD ["python", "-m", "src.main"]