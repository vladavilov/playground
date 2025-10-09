"""Integration tests for Docling-backed document processor (no local mocks).

Only external web calls are suppressed by setting HF_HUB_OFFLINE.
"""
import os
import tempfile
import shutil
from pathlib import Path
import pytest
from services.docling_processor import DoclingProcessor
from service_configuration.docling_config import DoclingSettings


def _write_bytes_temp(suffix: str, data: bytes) -> str:
    tmp = tempfile.NamedTemporaryFile(suffix=suffix, delete=False)
    tmp.write(data)
    tmp.flush()
    tmp.close()
    return tmp.name


def test_pdf_integration_local_sample(monkeypatch):
    tmp_root = tempfile.mkdtemp(prefix="hf-cache-")
    monkeypatch.setenv("HF_HOME", tmp_root)
    monkeypatch.setenv("HUGGINGFACE_HUB_CACHE", os.path.join(tmp_root, "hub"))
    monkeypatch.setenv("TRANSFORMERS_CACHE", os.path.join(tmp_root, "transformers"))
    monkeypatch.setenv("HF_HUB_DISABLE_SYMLINKS", "1")
    monkeypatch.setenv("HF_HUB_DISABLE_HARDLINKS", "1")
    monkeypatch.setenv("HF_HUB_DISABLE_PROGRESS_BARS", "1")

    path = str(Path(__file__).resolve().parent / "dummy.pdf")

    processor = DoclingProcessor()
    result = processor.extract_text_with_result(path)
    assert result.success is True
    assert result.file_type == "application/pdf"
    assert isinstance(result.extracted_text, str)
    assert "Welcome to Smallpdf" in result.extracted_text

def test_image_integration_png(monkeypatch):
    tmp_root = tempfile.mkdtemp(prefix="hf-cache-")
    monkeypatch.setenv("HF_HOME", tmp_root)
    monkeypatch.setenv("HUGGINGFACE_HUB_CACHE", os.path.join(tmp_root, "hub"))
    monkeypatch.setenv("TRANSFORMERS_CACHE", os.path.join(tmp_root, "transformers"))
    monkeypatch.setenv("HF_HUB_DISABLE_SYMLINKS", "1")
    monkeypatch.setenv("HF_HUB_DISABLE_HARDLINKS", "1")
    monkeypatch.setenv("HF_HUB_DISABLE_PROGRESS_BARS", "1")

    src_path = str(Path(__file__).resolve().parent / "01-sequence-diagram-example.png")
    tmp_img_dir = tempfile.mkdtemp(prefix="docling-img-")
    path = os.path.join(tmp_img_dir, "sample.png")
    shutil.copyfile(src_path, path)

    processor = DoclingProcessor()
    try:
        result = processor.extract_text_with_result(path)
        assert result.success is True
        assert "LifeLine" in result.extracted_text
        assert result.file_type == "image/*"
        assert isinstance(result.extracted_text, str)
    finally:
        try:
            shutil.rmtree(tmp_img_dir, ignore_errors=True)
        except Exception:
            pass


def test_pdf_unsupported_format_error():
    path = _write_bytes_temp(".txt", b"hello")
    try:
        processor = DoclingProcessor()
        result = processor.extract_text_with_result(path)
        assert result.success is False
        assert "Unsupported" in (result.error_message or "")
    finally:
        try:
            os.unlink(path)
        except Exception:
            pass


def test_nonexistent_file_error():
    processor = DoclingProcessor()
    result = processor.extract_text_with_result("/path/does/not/exist.pdf")
    assert result.success is False
    assert "File not found" in (result.error_message or "")


# ===== VLM Mode Configuration Tests =====

def test_local_vlm_mode_default(monkeypatch):
    """Test that local VLM mode is the default."""
    monkeypatch.setenv("DOCLING_VLM_MODE", "local")
    
    settings = DoclingSettings()
    processor = DoclingProcessor(settings=settings)
    
    assert processor.settings.DOCLING_VLM_MODE == "local"
    # Processor should be created without errors
    assert processor._converter is not None


def test_local_vlm_mode_explicit(monkeypatch):
    """Test explicit local VLM mode configuration."""
    monkeypatch.setenv("DOCLING_VLM_MODE", "local")
    
    tmp_root = tempfile.mkdtemp(prefix="hf-cache-")
    monkeypatch.setenv("HF_HOME", tmp_root)
    monkeypatch.setenv("HUGGINGFACE_HUB_CACHE", os.path.join(tmp_root, "hub"))
    monkeypatch.setenv("TRANSFORMERS_CACHE", os.path.join(tmp_root, "transformers"))
    
    settings = DoclingSettings()
    assert settings.DOCLING_VLM_MODE == "local"
    
    processor = DoclingProcessor(settings=settings)
    assert processor._converter is not None


def test_azure_openai_provider_configuration(monkeypatch):
    """Test Azure OpenAI provider configuration."""
    monkeypatch.setenv("DOCLING_VLM_MODE", "remote")
    monkeypatch.setenv("DOCLING_VLM_PROVIDER", "azure_openai")
    monkeypatch.setenv("AZURE_OPENAI_ENDPOINT", "https://test.openai.azure.com")
    monkeypatch.setenv("AZURE_OPENAI_DEPLOYMENT_NAME", "test-deployment")
    monkeypatch.setenv("AZURE_OPENAI_API_KEY", "test-key")
    monkeypatch.setenv("AZURE_OPENAI_API_VERSION", "2024-02-15-preview")
    
    settings = DoclingSettings()
    processor = DoclingProcessor(settings=settings)
    
    # Test that VLM options can be created without errors
    vlm_options = processor._create_azure_openai_vlm_options()
    
    assert vlm_options is not None
    url_str = str(vlm_options.url)
    assert "test.openai.azure.com" in url_str
    assert "test-deployment" in url_str
    assert "2024-02-15-preview" in url_str
    assert vlm_options.headers["api-key"] == "test-key"


def test_lm_studio_provider_configuration(monkeypatch):
    """Test LM Studio provider configuration."""
    monkeypatch.setenv("DOCLING_VLM_MODE", "remote")
    monkeypatch.setenv("DOCLING_VLM_PROVIDER", "lm_studio")
    monkeypatch.setenv("DOCLING_VLM_ENDPOINT", "http://localhost:1234")
    monkeypatch.setenv("DOCLING_VLM_MODEL", "granite-docling-258m-mlx")
    
    settings = DoclingSettings()
    processor = DoclingProcessor(settings=settings)
    
    vlm_options = processor._create_lm_studio_vlm_options()
    
    assert vlm_options is not None
    assert "localhost:1234" in str(vlm_options.url)
    assert vlm_options.params["model"] == "granite-docling-258m-mlx"


def test_ollama_provider_configuration(monkeypatch):
    """Test Ollama provider configuration."""
    monkeypatch.setenv("DOCLING_VLM_MODE", "remote")
    monkeypatch.setenv("DOCLING_VLM_PROVIDER", "ollama")
    monkeypatch.setenv("DOCLING_VLM_ENDPOINT", "http://localhost:11434")
    monkeypatch.setenv("DOCLING_VLM_MODEL", "llama3.2-vision:11b")
    
    settings = DoclingSettings()
    processor = DoclingProcessor(settings=settings)
    
    vlm_options = processor._create_ollama_vlm_options()
    
    assert vlm_options is not None
    assert "localhost:11434" in str(vlm_options.url)
    assert vlm_options.params["model"] == "llama3.2-vision:11b"


def test_openai_compatible_provider_configuration(monkeypatch):
    """Test generic OpenAI-compatible provider configuration."""
    monkeypatch.setenv("DOCLING_VLM_MODE", "remote")
    monkeypatch.setenv("DOCLING_VLM_PROVIDER", "openai_compatible")
    monkeypatch.setenv("DOCLING_VLM_ENDPOINT", "http://custom-api:8000")
    monkeypatch.setenv("DOCLING_VLM_MODEL", "custom-vision-model")
    monkeypatch.setenv("DOCLING_VLM_API_KEY", "custom-key")
    
    settings = DoclingSettings()
    processor = DoclingProcessor(settings=settings)
    
    vlm_options = processor._create_openai_compatible_vlm_options()
    
    assert vlm_options is not None
    assert "custom-api:8000" in str(vlm_options.url)
    assert vlm_options.params["model"] == "custom-vision-model"
    assert vlm_options.headers["Authorization"] == "Bearer custom-key"


def test_provider_factory_azure_openai(monkeypatch):
    """Test provider factory returns correct provider for azure_openai."""
    monkeypatch.setenv("DOCLING_VLM_MODE", "remote")
    monkeypatch.setenv("DOCLING_VLM_PROVIDER", "azure_openai")
    monkeypatch.setenv("AZURE_OPENAI_ENDPOINT", "https://test.openai.azure.com")
    monkeypatch.setenv("AZURE_OPENAI_DEPLOYMENT_NAME", "test-deployment")
    monkeypatch.setenv("AZURE_OPENAI_API_KEY", "test-key")
    
    settings = DoclingSettings()
    processor = DoclingProcessor(settings=settings)
    
    vlm_options = processor._get_remote_vlm_options()
    
    assert vlm_options is not None
    assert "test.openai.azure.com" in str(vlm_options.url)


def test_provider_factory_lm_studio(monkeypatch):
    """Test provider factory returns correct provider for lm_studio."""
    monkeypatch.setenv("DOCLING_VLM_MODE", "remote")
    monkeypatch.setenv("DOCLING_VLM_PROVIDER", "lm_studio")
    monkeypatch.setenv("DOCLING_VLM_ENDPOINT", "http://localhost:1234")
    monkeypatch.setenv("DOCLING_VLM_MODEL", "test-model")
    
    settings = DoclingSettings()
    processor = DoclingProcessor(settings=settings)
    
    vlm_options = processor._get_remote_vlm_options()
    
    assert vlm_options is not None
    assert "localhost:1234" in str(vlm_options.url)


def test_provider_switching(monkeypatch):
    """Test that provider can be switched via environment variable."""
    
    # First: Azure OpenAI
    monkeypatch.setenv("DOCLING_VLM_MODE", "remote")
    monkeypatch.setenv("DOCLING_VLM_PROVIDER", "azure_openai")
    monkeypatch.setenv("AZURE_OPENAI_ENDPOINT", "https://test.openai.azure.com")
    monkeypatch.setenv("AZURE_OPENAI_DEPLOYMENT_NAME", "test-deployment")
    monkeypatch.setenv("AZURE_OPENAI_API_KEY", "test-key")
    
    settings1 = DoclingSettings()
    processor1 = DoclingProcessor(settings=settings1)
    vlm_options1 = processor1._get_remote_vlm_options()
    
    assert "test.openai.azure.com" in str(vlm_options1.url)
    
    # Second: Switch to Ollama
    monkeypatch.setenv("DOCLING_VLM_PROVIDER", "ollama")
    monkeypatch.setenv("DOCLING_VLM_ENDPOINT", "http://localhost:11434")
    monkeypatch.setenv("DOCLING_VLM_MODEL", "llama3.2-vision")
    
    settings2 = DoclingSettings()
    processor2 = DoclingProcessor(settings=settings2)
    vlm_options2 = processor2._get_remote_vlm_options()
    
    assert "localhost:11434" in str(vlm_options2.url)


# ===== Error Handling Tests =====

def test_azure_openai_missing_endpoint(monkeypatch):
    """Test error when Azure OpenAI endpoint is missing."""
    monkeypatch.setenv("DOCLING_VLM_MODE", "remote")
    monkeypatch.setenv("DOCLING_VLM_PROVIDER", "azure_openai")
    monkeypatch.setenv("AZURE_OPENAI_ENDPOINT", "")
    monkeypatch.setenv("AZURE_OPENAI_DEPLOYMENT_NAME", "test-deployment")
    monkeypatch.setenv("AZURE_OPENAI_API_KEY", "test-key")
    
    settings = DoclingSettings()
    
    # Error is raised during processor initialization
    with pytest.raises(ValueError) as exc_info:
        DoclingProcessor(settings=settings)
    
    assert "AZURE_OPENAI_ENDPOINT is required" in str(exc_info.value)


def test_azure_openai_missing_deployment_name(monkeypatch):
    """Test error when Azure OpenAI deployment name is missing."""
    monkeypatch.setenv("DOCLING_VLM_MODE", "remote")
    monkeypatch.setenv("DOCLING_VLM_PROVIDER", "azure_openai")
    monkeypatch.setenv("AZURE_OPENAI_ENDPOINT", "https://test.openai.azure.com")
    monkeypatch.setenv("AZURE_OPENAI_DEPLOYMENT_NAME", "")
    monkeypatch.setenv("AZURE_OPENAI_API_KEY", "test-key")
    
    settings = DoclingSettings()
    
    # Error is raised during processor initialization
    with pytest.raises(ValueError) as exc_info:
        DoclingProcessor(settings=settings)
    
    assert "AZURE_OPENAI_DEPLOYMENT_NAME is required" in str(exc_info.value)


def test_azure_openai_missing_api_key(monkeypatch):
    """Test error when Azure OpenAI API key is missing."""
    monkeypatch.setenv("DOCLING_VLM_MODE", "remote")
    monkeypatch.setenv("DOCLING_VLM_PROVIDER", "azure_openai")
    monkeypatch.setenv("AZURE_OPENAI_ENDPOINT", "https://test.openai.azure.com")
    monkeypatch.setenv("AZURE_OPENAI_DEPLOYMENT_NAME", "test-deployment")
    monkeypatch.setenv("AZURE_OPENAI_API_KEY", "")
    
    settings = DoclingSettings()
    
    # Error is raised during processor initialization
    with pytest.raises(ValueError) as exc_info:
        DoclingProcessor(settings=settings)
    
    assert "AZURE_OPENAI_API_KEY is required" in str(exc_info.value)


def test_watsonx_missing_api_key(monkeypatch):
    """Test error when watsonx API key is missing."""
    monkeypatch.setenv("DOCLING_VLM_MODE", "remote")
    monkeypatch.setenv("DOCLING_VLM_PROVIDER", "watsonx")
    monkeypatch.setenv("WX_API_KEY", "")
    monkeypatch.setenv("WX_PROJECT_ID", "test-project")
    
    settings = DoclingSettings()
    
    # Error is raised during processor initialization
    with pytest.raises(ValueError) as exc_info:
        DoclingProcessor(settings=settings)
    
    assert "WX_API_KEY is required" in str(exc_info.value)


def test_watsonx_missing_project_id(monkeypatch):
    """Test error when watsonx project ID is missing."""
    monkeypatch.setenv("DOCLING_VLM_MODE", "remote")
    monkeypatch.setenv("DOCLING_VLM_PROVIDER", "watsonx")
    monkeypatch.setenv("WX_API_KEY", "test-key")
    monkeypatch.setenv("WX_PROJECT_ID", "")
    
    settings = DoclingSettings()
    
    # Error is raised during processor initialization
    with pytest.raises(ValueError) as exc_info:
        DoclingProcessor(settings=settings)
    
    assert "WX_PROJECT_ID is required" in str(exc_info.value)


def test_invalid_provider(monkeypatch):
    """Test error when invalid provider is specified."""
    monkeypatch.setenv("DOCLING_VLM_MODE", "remote")
    monkeypatch.setenv("DOCLING_VLM_PROVIDER", "invalid_provider")
    
    settings = DoclingSettings()
    
    # The error should be raised during processor initialization
    with pytest.raises(ValueError) as exc_info:
        DoclingProcessor(settings=settings)
    
    assert "Unsupported VLM provider: invalid_provider" in str(exc_info.value)
    assert "azure_openai" in str(exc_info.value)


def test_invalid_vlm_mode(monkeypatch):
    """Test error when invalid VLM mode is specified."""
    monkeypatch.setenv("DOCLING_VLM_MODE", "invalid_mode")
    
    settings = DoclingSettings()
    
    with pytest.raises(ValueError) as exc_info:
        DoclingProcessor(settings=settings)
    
    assert "Unsupported VLM mode: invalid_mode" in str(exc_info.value)


def test_response_format_doctags(monkeypatch):
    """Test that DOCTAGS response format is correctly configured."""
    monkeypatch.setenv("DOCLING_VLM_MODE", "remote")
    monkeypatch.setenv("DOCLING_VLM_PROVIDER", "azure_openai")
    monkeypatch.setenv("DOCLING_VLM_RESPONSE_FORMAT", "DOCTAGS")
    monkeypatch.setenv("AZURE_OPENAI_ENDPOINT", "https://test.openai.azure.com")
    monkeypatch.setenv("AZURE_OPENAI_DEPLOYMENT_NAME", "test-deployment")
    monkeypatch.setenv("AZURE_OPENAI_API_KEY", "test-key")
    
    settings = DoclingSettings()
    processor = DoclingProcessor(settings=settings)
    
    vlm_options = processor._create_azure_openai_vlm_options()
    
    from docling.datamodel.pipeline_options_vlm_model import ResponseFormat
    assert vlm_options.response_format == ResponseFormat.DOCTAGS


def test_response_format_markdown(monkeypatch):
    """Test that MARKDOWN response format is correctly configured."""
    monkeypatch.setenv("DOCLING_VLM_MODE", "remote")
    monkeypatch.setenv("DOCLING_VLM_PROVIDER", "azure_openai")
    monkeypatch.setenv("DOCLING_VLM_RESPONSE_FORMAT", "MARKDOWN")
    monkeypatch.setenv("AZURE_OPENAI_ENDPOINT", "https://test.openai.azure.com")
    monkeypatch.setenv("AZURE_OPENAI_DEPLOYMENT_NAME", "test-deployment")
    monkeypatch.setenv("AZURE_OPENAI_API_KEY", "test-key")
    
    settings = DoclingSettings()
    processor = DoclingProcessor(settings=settings)
    
    vlm_options = processor._create_azure_openai_vlm_options()
    
    from docling.datamodel.pipeline_options_vlm_model import ResponseFormat
    assert vlm_options.response_format == ResponseFormat.MARKDOWN
