FROM python-service-base:latest

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PYTHONPATH=/app/gitlab_client_service/src

WORKDIR /app

COPY ./shared/ ./shared/
COPY ./gitlab_client_service/ ./gitlab_client_service/

# Copy SSL certificate for GitLab verification
COPY ./gitlab_client_service/gitlab.crt /app/gitlab_client_service/gitlab.crt

WORKDIR /app/shared
RUN pip install --no-cache-dir .

WORKDIR /app/gitlab_client_service
RUN pip install --no-cache-dir .

WORKDIR /app

ENV API_PORT=8000
EXPOSE $API_PORT

HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:${API_PORT}/health || exit 1

CMD ["python", "-m", "main"] 