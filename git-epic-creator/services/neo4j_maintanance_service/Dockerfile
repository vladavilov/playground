FROM python-service-base:latest AS builder

WORKDIR /app

COPY ./shared/ ./shared/
COPY ./neo4j_maintanance_service/ ./neo4j_maintanance_service/

WORKDIR /app/shared
RUN pip install --no-cache-dir .

WORKDIR /app/neo4j_maintanance_service
RUN pip install --no-cache-dir .

FROM python-service-base:latest

COPY --from=builder /opt/venv /opt/venv
COPY --from=builder /app/neo4j_maintanance_service/src/ ./src/

ENV API_PORT=8000
EXPOSE $API_PORT

HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:${API_PORT}/health || exit 1

CMD ["python", "-m", "src.main"] 