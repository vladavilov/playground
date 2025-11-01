FROM python-service-base:latest

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PYTHONPATH=/app/gitlab_mock_service/src

WORKDIR /app

COPY ./shared/ ./shared/
COPY ./gitlab_mock_service/ ./gitlab_mock_service/

# Copy SSL certificate and key for HTTPS
COPY ./gitlab_mock_service/gitlab.crt /app/gitlab_mock_service/gitlab.crt
COPY ./gitlab_mock_service/gitlab.key /app/gitlab_mock_service/gitlab.key

WORKDIR /app/shared
RUN pip install --no-cache-dir .

WORKDIR /app/gitlab_mock_service
RUN pip install --no-cache-dir .

WORKDIR /app

ENV API_PORT=8000
EXPOSE $API_PORT

HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
  CMD curl -k -f https://localhost:${API_PORT}/health || exit 1

CMD ["python", "-m", "main"]

