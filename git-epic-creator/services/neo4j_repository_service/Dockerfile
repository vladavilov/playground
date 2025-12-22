FROM rust:1.85-bookworm AS builder

WORKDIR /app

# Dependencies (cached)
COPY Cargo.toml ./
RUN cargo fetch

# Build
COPY src ./src
RUN cargo build --release

FROM debian:bookworm-slim AS runtime

RUN apt-get update && apt-get install -y --no-install-recommends ca-certificates && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=builder /app/target/release/neo4j-repository-service /app/neo4j-repository-service
COPY queries /app/queries

ENV API_PORT=8080
ENV BIND_HOST=0.0.0.0
ENV NEO4J_REPOSITORY_QUERIES_DIR=/app/queries
ENV RUST_LOG=info,tower_http=info

EXPOSE 8080

CMD ["/app/neo4j-repository-service"]

