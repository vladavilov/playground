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

# Expose the port the app runs on
EXPOSE $API_PORT

# Define the command to run the application
# For production, WORKERS should be set to (2 * <number of CPU cores>) + 1.
CMD ["sh", "-c", "exec uvicorn src.main:app --host 0.0.0.0 --port $API_PORT --workers $WORKERS"]