FROM python-service-base:latest

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PYTHONPATH=/app/mock_auth_service/src

WORKDIR /app

COPY ./mock_auth_service/ ./mock_auth_service/

WORKDIR /app/mock_auth_service
RUN pip install --no-cache-dir .

WORKDIR /app

COPY ./mock_auth_service/certs/generate_certs.py /app/certs/generate_certs.py

# Generate self-signed certificates for HTTPS
RUN mkdir -p /app/certs && \
    python /app/certs/generate_certs.py /app/certs/cert.pem /app/certs/key.pem

ENV API_PORT=8000
ENV SSL_CERTFILE=/app/certs/cert.pem
ENV SSL_KEYFILE=/app/certs/key.pem

EXPOSE $API_PORT

HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
  CMD curl -f -k https://localhost:${API_PORT}/health || exit 1

CMD ["python", "-m", "main"] 