# Multi-stage base image for Python 3.13 services
# Services reference python-service-base:latest which resolves to the final stage

# Builder stage with venv pre-configured
FROM python:3.13-slim-bookworm AS base-builder

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

RUN apt-get update \
  && apt-get install -y --no-install-recommends curl ca-certificates \
  && rm -rf /var/lib/apt/lists/*

# Pre-create venv for services to use
RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

WORKDIR /app

# Runtime stage - inherits from builder and adds PYTHONPATH
FROM base-builder AS base-runtime

ENV PYTHONPATH=/app/src
