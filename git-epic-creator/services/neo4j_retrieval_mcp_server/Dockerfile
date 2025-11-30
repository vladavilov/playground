FROM python-service-base:latest

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PYTHONPATH=/app/neo4j_retrieval_mcp_server/src

WORKDIR /app

# Copy shared library first (for dependency resolution)
COPY ./shared/ ./shared/

# Copy service code
COPY ./neo4j_retrieval_mcp_server/ ./neo4j_retrieval_mcp_server/

# Install shared library
WORKDIR /app/shared
RUN pip install --no-cache-dir .

# Install service dependencies
WORKDIR /app/neo4j_retrieval_mcp_server
RUN pip install --no-cache-dir .

WORKDIR /app

# Default port for HTTP transport
ENV PORT=8082
EXPOSE $PORT

# Health check for HTTP transport mode
HEALTHCHECK --interval=30s --timeout=30s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:${PORT}/health || exit 1

# Default entrypoint - supports both stdio and http transports
CMD ["python", "-m", "main"]
