FROM python:3.13-slim-bookworm AS builder

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

WORKDIR /app

COPY ./shared/ ./shared/
COPY ./ui_service/ ./ui_service/

WORKDIR /app/shared
RUN pip install --no-cache-dir .

WORKDIR /app/ui_service
RUN pip install --no-cache-dir .

FROM python:3.13-slim-bookworm AS final

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

RUN apt-get update && apt-get install -y curl && rm -rf /var/lib/apt/lists/*

COPY --from=builder /opt/venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"
ENV PYTHONPATH=/app/src

WORKDIR /app

COPY --from=builder /app/ui_service/src/ ./src/

ENV API_PORT=8000
EXPOSE $API_PORT

HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:${API_PORT}/health || exit 1

CMD ["python", "-m", "src.main"] 