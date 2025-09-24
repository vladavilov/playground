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


