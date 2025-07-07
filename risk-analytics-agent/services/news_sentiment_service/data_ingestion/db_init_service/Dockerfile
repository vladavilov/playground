FROM python:3.13-slim AS builder

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

WORKDIR /app

COPY ./common/ ./common/
WORKDIR /app/common
RUN pip install --no-cache-dir .

WORKDIR /app/data_ingestion/db_init_service
COPY ./data_ingestion/db_init_service/ .
RUN pip install --no-cache-dir .

FROM python:3.13-slim AS final

COPY --from=builder /opt/venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

WORKDIR /app

COPY --from=builder /app/data_ingestion/db_init_service/src ./src

CMD ["python", "src/main.py"] 