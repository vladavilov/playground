"""Build-time initialization: pre-warm docling plugins and models.

This script ensures all docling plugins, dependencies, and models are loaded
and cached during the Docker build phase, preventing lazy initialization delays
at runtime when the first task is processed.
"""

from __future__ import annotations

import tempfile
import structlog
import sys
import time
from pathlib import Path

logger = structlog.get_logger(__name__)


def _print_progress(message: str) -> None:
    """Print progress message to stdout with flush to keep Docker connection alive."""
    print(f"[BUILD_INIT] {message}", flush=True)
    sys.stdout.flush()


def main() -> int:
    """
    Initialize docling plugins by creating a DocumentConverter and warming it.
    
    This forces docling to:
    - Load all pipeline plugins (OCR, table detection, picture description, etc.)
    - Download and initialize SmolVLM model
    - Cache all necessary components in the Docker image layer
    
    Returns:
        0 on success, 1 on failure (build will fail)
    """
    start_time = time.time()
    try:
        _print_progress("Starting docling plugin pre-load...")
        
        # Import here to ensure all dependencies are available
        from services.docling_processor import DoclingProcessor
        from service_configuration.docling_config import DoclingSettings
        
        logger.info("build_init_started", step="docling_plugin_preload")
        _print_progress("Imports successful, creating processor...")
        
        # Create processor with default settings
        settings = DoclingSettings()
        _print_progress(f"Settings loaded: VLM_MODE={settings.DOCLING_VLM_MODE}, OCR={settings.DOCLING_USE_OCR}")
        
        processor = DoclingProcessor(settings=settings)
        elapsed = time.time() - start_time
        
        logger.info("docling_processor_created", 
                   ocr_enabled=settings.DOCLING_USE_OCR,
                   image_scale=settings.DOCLING_IMAGES_SCALE)
        _print_progress(f"Processor created successfully ({elapsed:.1f}s elapsed)")
        
        # Create a minimal dummy PDF to warm the pipeline
        # This forces docling to initialize all plugins and models
        _print_progress("Creating temporary warmup PDF...")
        with tempfile.TemporaryDirectory() as tmpdir:
            dummy_pdf = Path(tmpdir) / "warmup.pdf"
            
            # Create a minimal valid PDF (simplest possible structure)
            # This is a bare-bones PDF with just enough to be recognized
            pdf_content = b"""%PDF-1.4
1 0 obj
<< /Type /Catalog /Pages 2 0 R >>
endobj
2 0 obj
<< /Type /Pages /Kids [3 0 R] /Count 1 >>
endobj
3 0 obj
<< /Type /Page /Parent 2 0 R /Resources 4 0 R /MediaBox [0 0 612 792] /Contents 5 0 R >>
endobj
4 0 obj
<< /Font << /F1 << /Type /Font /Subtype /Type1 /BaseFont /Helvetica >> >> >>
endobj
5 0 obj
<< /Length 44 >>
stream
BT
/F1 12 Tf
100 700 Td
(Warmup) Tj
ET
endstream
endobj
xref
0 6
0000000000 65535 f 
0000000009 00000 n 
0000000058 00000 n 
0000000115 00000 n 
0000000229 00000 n 
0000000329 00000 n 
trailer
<< /Size 6 /Root 1 0 R >>
startxref
422
%%EOF
"""
            dummy_pdf.write_bytes(pdf_content)
            
            logger.info("warmup_pdf_created", path=str(dummy_pdf), size=len(pdf_content))
            _print_progress(f"Warmup PDF created ({len(pdf_content)} bytes)")
            
            # Process the dummy PDF to trigger all plugin loading
            _print_progress("Processing warmup PDF (this may take several minutes)...")
            _print_progress("This will load ML models, OCR engines, and docling plugins...")
            
            process_start = time.time()
            result = processor.extract_text_with_result(str(dummy_pdf))
            process_elapsed = time.time() - process_start
            
            if result.success:
                total_elapsed = time.time() - start_time
                logger.info("build_init_success", 
                           message="Docling plugins and models pre-loaded successfully",
                           extracted_text_length=len(result.extracted_text or ""))
                _print_progress(f"Warmup successful! Processing took {process_elapsed:.1f}s, total time {total_elapsed:.1f}s")
            else:
                logger.error("build_init_failed", 
                            message="Warmup failed - docling processing unsuccessful",
                            error=result.error_message)
                _print_progress(f"ERROR: Warmup failed - {result.error_message}")
                return 1
        
        total_elapsed = time.time() - start_time
        logger.info("build_init_completed", step="docling_plugin_preload", total_time=f"{total_elapsed:.1f}s")
        _print_progress(f"Build initialization completed successfully in {total_elapsed:.1f}s")
        return 0
        
    except Exception as exc:
        # Fail the build if docling initialization fails
        elapsed = time.time() - start_time
        logger.error("build_init_failed", 
                   error=str(exc),
                   message="Plugin preload failed - build cannot continue",
                   elapsed_time=f"{elapsed:.1f}s")
        _print_progress(f"ERROR: Build initialization failed after {elapsed:.1f}s: {exc}")
        import traceback
        traceback.print_exc()
        return 1


if __name__ == "__main__":
    raise SystemExit(main())

