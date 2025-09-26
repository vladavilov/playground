FROM python:3.13-slim-bookworm AS builder

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

WORKDIR /e2e-tests

COPY . .

WORKDIR /e2e-tests
RUN pip install --no-cache-dir .

FROM python:3.13-slim-bookworm AS final

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

COPY --from=builder /opt/venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"
ENV PYTHONPATH=/e2e-tests/src

WORKDIR /e2e-tests

COPY --from=builder /e2e-tests/src/ ./src/
COPY --from=builder /e2e-tests/resources ./resources

# Ensure test results directory exists for junitxml output
RUN mkdir -p /e2e-tests/test-results

# Optional wait before running tests to allow dependencies to be fully ready
ENV E2E_WAIT_SECS=10

# Run tests, record success/failure, then keep container alive for healthcheck
CMD ["/bin/sh", "-c", "echo 'Waiting for all services to be fully ready...' && \
    sleep ${E2E_WAIT_SECS:-10} && \
    echo 'Starting end-to-end tests...' && \
    pytest -v --tb=short --junitxml=/e2e-tests/test-results/results.xml; \
    code=$?; \
    if [ $code -eq 0 ]; then \
      echo 'E2E tests passed'; \
      touch /tmp/e2e_success; \
    else \
      echo 'E2E tests failed'; \
      touch /tmp/e2e_failure; \
    fi; \
    sleep infinity"]

# Healthcheck reports healthy only when tests passed
HEALTHCHECK --interval=30s --timeout=10s --start-period=300s --retries=3 CMD test -f /tmp/e2e_success || exit 1