FROM python:3.13-slim AS builder

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

WORKDIR /app

COPY ./common/ ./common/
COPY ./news_processor_service/ ./news_processor_service/

WORKDIR /app/common
RUN pip install --no-cache-dir .

WORKDIR /app/news_processor_service
RUN pip install --no-cache-dir .

FROM python:3.13-slim AS final

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

# Install curl for health check script
RUN apt-get update && apt-get install -y curl && rm -rf /var/lib/apt/lists/*

COPY --from=builder /opt/venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"
ENV PYTHONPATH=/app/src

WORKDIR /app

COPY --from=builder /app/news_processor_service/src/ ./src/

# Create non-root user for security
RUN groupadd -r appuser && useradd -r -g appuser appuser

# Copy and setup health check script
COPY ./news_processor_service/scripts/healthcheck.sh ./healthcheck.sh
RUN chmod +x ./healthcheck.sh

# Set proper ownership for appuser
RUN chown -R appuser:appuser /app
USER appuser

ENV API_PORT=8000
EXPOSE $API_PORT

CMD ["python", "-m", "src.main"]