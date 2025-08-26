### gpt-oss vLLM GPU Microservice

NVIDIA GPU-backed container serving `openai/gpt-oss-20b` via vLLM with an OpenAI-compatible API (`/v1`).

References:
- vLLM OpenAI server: https://docs.vllm.ai/en/latest/serving/openai_compatible_server.html
- Docker usage: https://docs.vllm.ai/en/latest/deployment/docker.html

#### Build
```bash
docker build -t gpt-oss-vllm-gpu:latest .
```

#### Run (docker)
```bash
docker run --rm -p 8000:8000 --gpus all --ipc=host \
  -e MODEL_ID=openai/gpt-oss-20b \
  -e HF_HOME=/opt/hf_cache \
  -e VLLM_ARGS="--api-key=EMPTY" \
  gpt-oss-vllm-gpu:latest
```

#### Test the API
```bash
curl -s http://localhost:8000/v1/models | jq

curl -s -X POST http://localhost:8000/v1/chat/completions \
  -H 'Content-Type: application/json' \
  -H 'Authorization: Bearer EMPTY' \
  -d '{
    "model": "openai/gpt-oss-20b",
    "messages": [
      {"role": "system", "content": "You are a helpful assistant."},
      {"role": "user", "content": "Explain what MXFP4 quantization is."}
    ]
  }' | jq
```


