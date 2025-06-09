"""
Configuration and fixtures for pytest.
"""
import pytest
from unittest.mock import MagicMock, patch
from agno.models.openai import OpenAIChat
from agno.models.response import ModelResponse
from pathlib import Path
from sentence_transformers import SentenceTransformer
import os

from src.config import get_settings

# Suppress the symlink warning from huggingface_hub on Windows
os.environ["HF_HUB_DISABLE_SYMLINKS_WARNING"] = "1"


@pytest.fixture(scope="session", autouse=True)
def download_embedding_model():
    """
    Fixture to ensure the embedding model is downloaded before any tests run.
    This is session-scoped and runs automatically once per test session.
    It uses the sentence_transformers library directly to download the model.
    """
    settings = get_settings()
    model_name = "BAAI/bge-small-en-v1.5"  # This is a test-specific concern
    model_path = Path(settings.EMBEDDING_MODEL_PATH)
    
    if not model_path.exists():
        print(f"\nEmbedding model not found at '{model_path}'. Downloading...")
        try:
            model = SentenceTransformer(model_name)
            model.save(settings.EMBEDDING_MODEL_PATH)
            print("Model downloaded and saved successfully.")
        except Exception as e:
            pytest.fail(f"Failed to download or save embedding model: {e}")
    else:
        print("\nEmbedding model already exists. Skipping download.")


@pytest.fixture
def mock_chat_model():
    """
    Provides a mocked OpenAIChat model instance.

    This fixture creates a real `OpenAIChat` instance but patches its `response`
    method to return a predefined mock response. This ensures that the model's
    internal logic (like instruction generation) works as expected without
    making actual API calls.
    """
    with patch('os.getenv', return_value="fake_key"):
        model = OpenAIChat(id="gpt-4o")
        
        mock_model_response = MagicMock(spec=ModelResponse)
        mock_model_response.content = '{"property1": "value1", "property2": "value2"}'
        mock_model_response.tool_executions = None

        model.response = MagicMock(return_value=mock_model_response)
        yield model 