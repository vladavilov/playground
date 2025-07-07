FROM python:3.13-slim AS builder

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

# Install curl for health checks and git for pip dependencies
RUN apt-get update && apt-get install -y curl git && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY ./common/ ./common/
COPY ./data_ingestion/adapters/benzinga_adapter/ ./data_ingestion/adapters/benzinga_adapter/

WORKDIR /app/common
RUN pip install --no-cache-dir .

WORKDIR /app/data_ingestion/adapters/benzinga_adapter
RUN pip install --no-cache-dir .

FROM python:3.13-slim AS final

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

COPY --from=builder /opt/venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"
ENV PYTHONPATH=/app/src

WORKDIR /app

COPY --from=builder /app/data_ingestion/adapters/benzinga_adapter/src/ ./src/

EXPOSE 8000

CMD ["python", "-m", "src.main"]