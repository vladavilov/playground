### gpt-oss vLLM Microservice

Minimal, cloud-native container that serves `openai/gpt-oss-20b` via vLLM with an OpenAI-compatible API (`/v1`).

Reference: How to run gpt-oss with vLLM — https://cookbook.openai.com/articles/gpt-oss/run-vllm

#### Build
```bash
docker build -t gpt-oss-vllm:latest .
```

#### Run (docker)
```bash
# Requires NVIDIA GPU runtime
docker run --rm -p 8000:8000 --gpus all \
  -e MODEL_ID=openai/gpt-oss-20b \
  -e HF_HOME=/opt/hf_cache \
  -e VLLM_ARGS="--api-key=EMPTY" \
  gpt-oss-vllm:latest
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

#### Python SDK example
```python
from openai import OpenAI

client = OpenAI(base_url="http://localhost:8000/v1", api_key="EMPTY")

resp = client.chat.completions.create(
    model="openai/gpt-oss-20b",
    messages=[
        {"role": "system", "content": "You are a helpful assistant."},
        {"role": "user", "content": "Explain what MXFP4 quantization is."},
    ],
)

print(resp.choices[0].message.content)
```
