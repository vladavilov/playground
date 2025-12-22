FROM python-service-base:latest

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PYTHONPATH=/app/code_graph_ingestion_service/src

WORKDIR /app

COPY ./shared/ ./shared/
COPY ./tree_sitter_cobol_binding/ ./tree_sitter_cobol_binding/
COPY ./code_graph_ingestion_service/ ./code_graph_ingestion_service/

## Build + install precompiled COBOL Tree-sitter wheel from source.
## (tree_sitter_cobol_binding is the golden source; no runtime compilation at service runtime.)
WORKDIR /app/tree_sitter_cobol_binding
RUN pip install --upgrade pip setuptools wheel && \
    pip install --no-cache-dir "build==1.2.2.post1" && \
    python -m build --wheel --outdir /app/wheelhouse && \
    pip install --no-cache-dir --find-links=/app/wheelhouse "tree-sitter-cobol==0.1.0+grammar.8ba6692"

WORKDIR /app/shared
RUN pip install --no-cache-dir .

WORKDIR /app/code_graph_ingestion_service
RUN pip install --no-cache-dir .

WORKDIR /app

ENV API_PORT=8015
EXPOSE $API_PORT

# App wiring is added in Task 01 (src.main:app)
CMD ["python", "-m", "main"]
