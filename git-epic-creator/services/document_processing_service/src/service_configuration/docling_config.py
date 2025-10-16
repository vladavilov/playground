"""
Docling configuration settings for document processing service.
"""
from pydantic import Field
from pydantic_settings import BaseSettings, SettingsConfigDict


class DoclingSettings(BaseSettings):
    """Docling configuration settings for document processing."""

    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding='utf-8',
        case_sensitive=True,
        extra='ignore'
    )

    DOCLING_USE_OCR: bool = Field(
        default=True,
        description="Use OCR when extracting from images/PDFs if needed"
    )

    DOCLING_IMAGE_EXTENSIONS: str = Field(
        default=".png,.jpg,.jpeg,.tiff,.tif,.bmp,.gif,.webp,.heic,.heif,.ppm,.pbm,.pgm",
        description="Comma-separated list of image extensions handled by Docling"
    )

    DOCLING_TIMEOUT_SECONDS: int = Field(
        default=180,
        description="Timeout for Docling extraction"
    )

    # OCR languages as comma-separated list, e.g. "en,fr,de"
    DOCLING_OCR_LANGS: str = Field(
        default="en",
        description="Comma-separated list of OCR languages (e.g., 'en,fr,de')"
    )

    # Image scale for picture generation and VLM description quality
    DOCLING_IMAGES_SCALE: float = Field(
        default=2.0,
        description="Scaling factor for images used in picture description"
    )

    # ===== VLM Mode Configuration =====
    DOCLING_VLM_MODE: str = Field(
        default="local",
        description="VLM mode: 'local' (SmolVLM) or 'remote' (API-based)"
    )

    DOCLING_VLM_PROVIDER: str = Field(
        default="azure_openai",
        description="Remote VLM provider: azure_openai, lm_studio, ollama, watsonx, openai_compatible"
    )

    # ===== Azure OpenAI Configuration (PRIMARY Remote Provider) =====
    DOCLING_AZURE_OPENAI_ENDPOINT: str = Field(
        default="",
        description="Azure OpenAI endpoint URL (e.g., https://myresource.openai.azure.com)"
    )

    DOCLING_AZURE_OPENAI_DEPLOYMENT_NAME: str = Field(
        default="",
        description="Azure OpenAI deployment name (e.g., llama-32-vision)"
    )

    DOCLING_AZURE_OPENAI_API_KEY: str = Field(
        default="",
        description="Azure OpenAI API key"
    )

    DOCLING_AZURE_OPENAI_API_VERSION: str = Field(
        default="2024-02-15-preview",
        description="Azure OpenAI API version"
    )

    # ===== Generic Remote VLM Configuration (for other providers) =====
    DOCLING_VLM_ENDPOINT: str = Field(
        default="http://localhost:1234",
        description="Generic VLM API endpoint for non-Azure providers"
    )

    DOCLING_VLM_MODEL: str = Field(
        default="granite-docling-258m-mlx",
        description="Model name/ID for non-Azure providers"
    )

    DOCLING_VLM_API_KEY: str = Field(
        default="",
        description="API key for non-Azure providers (if required)"
    )

    # ===== watsonx.ai specific configuration =====
    WX_API_KEY: str = Field(
        default="",
        description="IBM Cloud API key for watsonx.ai"
    )

    WX_PROJECT_ID: str = Field(
        default="",
        description="watsonx.ai project ID"
    )

    # ===== Common Remote VLM Parameters =====
    DOCLING_VLM_PROMPT: str = Field(
        default="Convert this page to docling format with detailed descriptions.",
        description="Prompt template for VLM processing"
    )

    DOCLING_VLM_TIMEOUT: int = Field(
        default=90,
        description="Timeout for remote VLM API calls in seconds"
    )

    DOCLING_VLM_TEMPERATURE: float = Field(
        default=0.7,
        description="Temperature for VLM generation"
    )

    DOCLING_VLM_MAX_TOKENS: int = Field(
        default=4096,
        description="Maximum tokens for VLM response"
    )

    DOCLING_VLM_RESPONSE_FORMAT: str = Field(
        default="DOCTAGS",
        description="Response format: DOCTAGS or MARKDOWN"
    )

    # ===== Fallback Configuration =====
    DOCLING_ENABLE_EMPTY_FALLBACK: bool = Field(
        default=True,
        description="Enable automatic fallback to Tika if Docling extraction returns empty or minimal text"
    )

    DOCLING_MIN_TEXT_LENGTH: int = Field(
        default=50,
        description="Minimum text length (chars) to consider extraction successful. Below this triggers fallback if enabled."
    )


