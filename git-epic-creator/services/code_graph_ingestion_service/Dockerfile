FROM rust:1.84-slim AS builder

WORKDIR /app/code_graph_ingestion_service

# Pre-copy manifests for better Docker layer caching.
COPY ./code_graph_ingestion_service/Cargo.toml ./Cargo.toml
COPY ./code_graph_ingestion_service/src ./src
COPY ./code_graph_ingestion_service/tests ./tests

RUN cargo build --release

FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends ca-certificates && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=builder /app/code_graph_ingestion_service/target/release/code_graph_ingestion_service /usr/local/bin/code-graph-ingestion-service

ENV API_PORT=8015
EXPOSE 8015

CMD ["/usr/local/bin/code-graph-ingestion-service"]
