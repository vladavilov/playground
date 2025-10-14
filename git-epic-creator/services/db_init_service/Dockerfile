FROM python-service-base:latest

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PYTHONPATH=/app/db_init_service/src

WORKDIR /app

COPY ./shared/ ./shared/
COPY ./db_init_service/ ./db_init_service/

WORKDIR /app/shared
RUN pip install --no-cache-dir .

WORKDIR /app/db_init_service
RUN pip install --no-cache-dir .

WORKDIR /app

ENV API_PORT=8000
EXPOSE $API_PORT

HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:${API_PORT}/db/health || exit 1

CMD ["python", "-m", "main"]