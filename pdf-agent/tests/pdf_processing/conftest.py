"""
Configuration and fixtures for pytest.
"""
import pytest
from unittest.mock import MagicMock, patch
from agno.models.openai import OpenAIChat
from agno.models.response import ModelResponse


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