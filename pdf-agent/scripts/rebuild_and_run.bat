@echo off
ECHO Building Docker image...
docker build -t pdf-extractor-service .

ECHO Stopping existing container...
docker stop pdf-extractor-service > nul 2>&1

ECHO Removing existing container...
docker rm pdf-extractor-service > nul 2>&1

ECHO Starting new container...
docker run -d -p 8000:8000 --env-file env/.env --name pdf-extractor-service pdf-extractor-service

ECHO Waiting for container to initialize...
timeout /t 3 /nobreak > nul

ECHO Displaying initial logs:
docker logs pdf-extractor-service

ECHO Done. Script finished. 