FROM python:3.13-slim AS builder

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

WORKDIR /app

COPY ./shared/ ./shared/
COPY ./ai_workflow_service/ ./ai_workflow_service/

WORKDIR /app/shared
RUN pip install --no-cache-dir .

WORKDIR /app/ai_workflow_service
RUN pip install --no-cache-dir .

FROM python:3.13-slim AS final

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

COPY --from=builder /opt/venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"
ENV PYTHONPATH=/app/src

WORKDIR /app

COPY --from=builder /app/ai_workflow_service/src/ ./src/

ENV API_PORT=8000
EXPOSE $API_PORT

# Health check validates both Tika environment (including pre-installed JAR) and Celery
HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:${API_PORT}/health && curl -f http://localhost:${API_PORT}/health/celery || exit 1

CMD ["python", "-m", "src.main"] 