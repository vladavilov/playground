# Stage 1: Build the virtual environment
FROM python:3.11-slim AS builder

ENV PYTHONDONTWRITEBYTECODE=1
ENV PYTHONUNBUFFERED=1

# Create a virtual environment
RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

WORKDIR /app

# Install Python dependencies
COPY requirements.txt .

# Upgrade pip and install dependencies into the venv
RUN pip install --no-cache-dir --upgrade pip
RUN pip install --no-cache-dir -r requirements.txt

# Download NLTK data required for text processing
COPY scripts/ ./scripts/
RUN python scripts/download_nltk_data.py

# Download and save the embedding model
RUN python scripts/download_model.py


# Stage 2: Create the final, slim image
FROM python:3.11-slim AS final

ENV PYTHONDONTWRITEBYTECODE=1
ENV PYTHONUNBUFFERED=1

# Copy the virtual environment from the builder stage
COPY --from=builder /opt/venv /opt/venv

# Set the path to use the venv
ENV PATH="/opt/venv/bin:$PATH"

# Default API port and workers, can be overridden
ENV API_PORT=8000
ENV WORKERS=2

WORKDIR /app

# Copy application code and configuration
COPY src/ ./src/
COPY config/ ./config/

# Copy the pre-downloaded embedding model from the builder stage
COPY --from=builder /app/embedding_model ./embedding_model/

# Expose the port the app runs on
EXPOSE $API_PORT

# Define the command to run the application
# For production, WORKERS should be set to (2 * <number of CPU cores>) + 1.
CMD ["sh", "-c", "exec uvicorn src.main:app --host 0.0.0.0 --port $API_PORT --workers $WORKERS"]