FROM python:3.11-slim

ENV PYTHONDONTWRITEBYTECODE=1
ENV PYTHONUNBUFFERED=1

# Default API port, can be overridden by Docker run -e or Compose file
ENV API_PORT=8000

WORKDIR /app

# Install Python dependencies
# First, copy only the requirements file to leverage Docker cache
COPY requirements.txt .

# Upgrade pip and install dependencies
RUN pip install --no-cache-dir --upgrade pip && \
    pip install --no-cache-dir -r requirements.txt

COPY src/ ./src/
COPY config/ ./config/

# Expose the port the app runs on, using the environment variable
EXPOSE $API_PORT

# Define the command to run the application
CMD ["sh", "-c", "exec uvicorn src.main:app --host 0.0.0.0 --port $API_PORT"]