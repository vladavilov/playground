import os
os.environ["AZURE_OPENAI_API_KEY"] = "fake_key"
os.environ["AZURE_OPENAI_ENDPOINT"] = "https://fake_endpoint"
os.environ["AZURE_OPENAI_LLM_DEPLOYMENT_NAME"] = "fake_llm"
os.environ["AZURE_OPENAI_EMBEDDINGS_DEPLOYMENT_NAME"] = "fake_embeddings"

import pytest
from httpx import AsyncClient, ASGITransport
from unittest.mock import patch, MagicMock
from fastapi.testclient import TestClient
from src.main import app
from src.config import get_settings
from agno.workflow import RunResponse

client = TestClient(app)

@pytest.fixture(autouse=True)
def override_settings():
    """
    Fixture to automatically override settings for each test.
    This ensures a clean, temporary directory for each test run.
    """
    settings = get_settings()
    with patch("src.main.get_settings") as mock_get_settings:
        # Point to a subdirectory in the temp_files to avoid conflicts
        test_temp_dir = settings.TEMP_FILE_DIR / "test_run"
        test_temp_dir.mkdir(parents=True, exist_ok=True)
        
        settings.TEMP_FILE_DIR = test_temp_dir
        settings.MAX_FILE_SIZE_MB = 1 # Keep it small for testing
        
        mock_get_settings.return_value = settings
        yield settings
        # Clean up the test-specific temp directory
        import shutil
        shutil.rmtree(test_temp_dir, ignore_errors=True)

def test_health_check():
    response = client.get("/health")
    assert response.status_code == 200
    assert response.json() == {"status": "ok"}

@patch("src.main.PDFExtractionWorkflow")
def test_extract_data_success(mock_workflow_class, override_settings):
    """Tests successful data extraction from a PDF."""
    mock_workflow_instance = MagicMock()
    mock_workflow_instance.run.return_value = RunResponse(content={"data": "success"})
    mock_workflow_class.return_value = mock_workflow_instance
    
    pdf_content = b"%PDF-1.4\n1 0 obj\n<<>>\nendobj"
    files = {"file": ("test.pdf", pdf_content, "application/pdf")}
    
    response = client.post("/extract_data", files=files)

    assert response.status_code == 200
    assert response.json() == {"data": "success"}
    mock_workflow_instance.run.assert_called_once()
    
    # Check that the temp file was created and then deleted
    temp_file_path = override_settings.TEMP_FILE_DIR / "test.pdf"
    assert not temp_file_path.exists()

def test_extract_data_invalid_content_type():
    """Tests rejection of files with incorrect content type."""
    files = {"file": ("test.txt", b"some text", "text/plain")}
    response = client.post("/extract_data", files=files)
    assert response.status_code == 400
    assert "Invalid file type" in response.json()["detail"]

def test_extract_data_file_too_large():
    """Tests rejection of files exceeding the size limit."""
    # 2MB > 1MB limit
    large_content = b"a" * (2 * 1024 * 1024)
    files = {"file": ("large.pdf", large_content, "application/pdf")}
    response = client.post("/extract_data", files=files)
    assert response.status_code == 413
    assert "File size exceeds the limit" in response.json()["detail"]

@patch("src.main.PDFExtractionWorkflow")
def test_extract_data_workflow_failure(mock_workflow_class, override_settings):
    """Tests server error response when the workflow fails."""
    mock_workflow_instance = MagicMock()
    mock_workflow_instance.run.side_effect = Exception("Workflow exploded")
    mock_workflow_class.return_value = mock_workflow_instance
    
    pdf_content = b"%PDF-1.4\n"
    files = {"file": ("fail.pdf", pdf_content, "application/pdf")}
    
    response = client.post("/extract_data", files=files)
    
    assert response.status_code == 500
    assert "An unexpected error occurred" in response.json()["detail"]

    # Check that the temp file was still cleaned up on failure
    temp_file_path = override_settings.TEMP_FILE_DIR / "fail.pdf"
    assert not temp_file_path.exists() 