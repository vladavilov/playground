FROM python:3.13-slim AS builder

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

WORKDIR /e2e-tests

COPY . .

WORKDIR /e2e-tests
RUN pip install --no-cache-dir .

FROM python:3.13-slim AS final

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

COPY --from=builder /opt/venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"
ENV PYTHONPATH=/e2e-tests/src

WORKDIR /e2e-tests

COPY --from=builder /e2e-tests/src/ ./src/

CMD ["pytest", "-s"] 