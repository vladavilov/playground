FROM python:3.13-slim

WORKDIR /app

RUN apt-get update
RUN apt-get install -y gcc build-essential

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

COPY app.py .
COPY static ./static

EXPOSE 8000

CMD ["uvicorn", "app:app", "--host", "0.0.0.0", "--port", "8000"]
