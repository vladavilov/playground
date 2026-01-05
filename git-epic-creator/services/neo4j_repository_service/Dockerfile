FROM rust:1.85-bookworm AS builder

WORKDIR /app/neo4j_repository_service

# Dependencies (cached)
COPY neo4j_repository_service/Cargo.toml neo4j_repository_service/Cargo.lock ./
RUN mkdir -p src && printf '%s\n' 'fn main() {}' > src/main.rs
RUN cargo fetch

# Build
COPY neo4j_repository_service/src ./src
RUN cargo build --release

FROM debian:bookworm-slim AS runtime

RUN apt-get update && apt-get install -y --no-install-recommends ca-certificates curl && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=builder /app/neo4j_repository_service/target/release/neo4j-repository-service /app/neo4j-repository-service
COPY neo4j_repository_service/queries /app/queries

ENV API_PORT=8080
ENV BIND_HOST=0.0.0.0
ENV NEO4J_REPOSITORY_QUERIES_DIR=/app/queries
ENV RUST_LOG=info,tower_http=info

EXPOSE 8080

CMD ["/app/neo4j-repository-service"]

