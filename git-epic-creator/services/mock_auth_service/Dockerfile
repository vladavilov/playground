FROM python:3.13-slim-bookworm AS builder

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

WORKDIR /app

COPY ./mock_auth_service/ ./mock_auth_service/

WORKDIR /app/mock_auth_service
RUN pip install --no-cache-dir .

FROM python:3.13-slim-bookworm AS final

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

RUN apt-get update && apt-get install -y curl && rm -rf /var/lib/apt/lists/*

COPY --from=builder /opt/venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"
ENV PYTHONPATH=/app/src

WORKDIR /app

COPY --from=builder /app/mock_auth_service/src/ ./src/
COPY --from=builder /app/mock_auth_service/certs/generate_certs.py ./certs/generate_certs.py

# Generate self-signed certificates for HTTPS
RUN mkdir -p /app/certs && \
    python /app/certs/generate_certs.py /app/certs/cert.pem /app/certs/key.pem

ENV API_PORT=8000
ENV SSL_CERTFILE=/app/certs/cert.pem
ENV SSL_KEYFILE=/app/certs/key.pem

EXPOSE $API_PORT

HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
  CMD curl -f -k https://localhost:${API_PORT}/health || exit 1

CMD ["python", "-m", "src.main"] 