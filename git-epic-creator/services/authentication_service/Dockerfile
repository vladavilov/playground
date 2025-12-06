FROM python-service-base:latest

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PYTHONPATH=/app/authentication_service/src

WORKDIR /app

COPY ./shared/ ./shared/
COPY ./authentication_service/ ./authentication_service/

WORKDIR /app/shared
RUN pip install --upgrade pip setuptools wheel && \
    pip install .

WORKDIR /app/authentication_service
RUN pip install --no-cache-dir .

WORKDIR /app

ENV API_PORT=8020
EXPOSE $API_PORT

# Basic API health check
HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:${API_PORT}/health || exit 1

CMD ["python", "-m", "main"]
