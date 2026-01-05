FROM python-service-base:latest

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PYTHONPATH=/app/sse_bridge_service/src

WORKDIR /app

COPY ./shared/ ./shared/
COPY ./sse_bridge_service/ ./sse_bridge_service/

WORKDIR /app/shared
RUN pip install --no-cache-dir .

WORKDIR /app/sse_bridge_service
RUN pip install --no-cache-dir .

WORKDIR /app

ENV API_PORT=8000
EXPOSE $API_PORT

HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:${API_PORT}/health || exit 1

CMD ["python", "-m", "main"]