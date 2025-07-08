FROM python:3.13-slim AS builder

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

WORKDIR /app

COPY ./common/ ./common/
COPY ./data_ingestion/orchestrator_service ./data_ingestion/orchestrator_service/

WORKDIR /app/common
RUN pip install --no-cache-dir .

WORKDIR /app/data_ingestion/orchestrator_service
RUN pip install --no-cache-dir .

FROM python:3.13-slim AS final

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

COPY --from=builder /opt/venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"
ENV PYTHONPATH=/app/src

# In the final stage, only copy the application's own source code from the builder.
COPY --from=builder /app/data_ingestion/orchestrator_service/src/ ./src/

CMD ["python", "-m", "src.main"]