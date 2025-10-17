"""Download Docling layout models for offline operation.

This script downloads layout analysis models (including docling-layout-heron) 
used by Docling for document processing. Models are cached locally to enable
offline operation.
"""
from __future__ import annotations

import sys
import os
from pathlib import Path
import structlog

logger = structlog.get_logger(__name__)


def get_default_models_path() -> Path:
    """Get the default models directory path."""
    script_dir = Path(__file__).parent
    service_root = script_dir.parent.parent  # Go up to service root
    return service_root / "plugins" / "docling" / "models"


def main() -> int:
    """
    Download Docling layout models by initializing DocumentConverter.
    
    This triggers automatic model download to the specified artifacts path.
    Models include layout analysis models like docling-layout-heron.
    
    Returns:
        0 on success, 1 on failure
    """
    local_dir = get_default_models_path()
    
    print(f"Platform: {sys.platform}")
    print(f"Target directory: {local_dir}")
    print()
    
    # Create directory structure
    local_dir.mkdir(parents=True, exist_ok=True)
    
    print("=" * 80)
    print("Downloading Docling layout models...")
    print("=" * 80)
    print()
    print("NOTE: Docling models are automatically downloaded when DocumentConverter")
    print("      is first initialized with a specific artifacts_path.")
    print()
    
    try:
        # Set artifacts path before importing Docling
        os.environ["DOCLING_ARTIFACTS_PATH"] = str(local_dir)
        
        print("[1/3] Importing Docling components...")
        from docling.datamodel.base_models import InputFormat
        from docling.datamodel.pipeline_options import PdfPipelineOptions
        from docling.document_converter import DocumentConverter, PdfFormatOption
        print("      ✓ Imports successful")
        
        print()
        print("[2/3] Initializing DocumentConverter (this downloads models)...")
        print("      This may take several minutes...")
        
        # Initialize with artifacts_path to trigger download
        pipeline_options = PdfPipelineOptions(artifacts_path=str(local_dir))
        
        doc_converter = DocumentConverter(
            format_options={
                InputFormat.PDF: PdfFormatOption(pipeline_options=pipeline_options)
            }
        )
        
        print("      ✓ DocumentConverter initialized")
        
        print()
        print("[3/3] Verifying downloaded models...")
        
        # Check for downloaded artifacts
        if local_dir.exists():
            artifacts = list(local_dir.rglob("*"))
            model_files = [f for f in artifacts if f.is_file()]
            
            if model_files:
                total_size = sum(f.stat().st_size for f in model_files)
                size_mb = total_size / (1024 * 1024)
                
                print(f"      ✓ Found {len(model_files)} model file(s)")
                print(f"      Total size: {size_mb:.1f} MB")
                print()
                
                print("Downloaded artifacts:")
                for f in sorted(model_files)[:20]:  # Show first 20 files
                    rel_path = f.relative_to(local_dir)
                    file_size_mb = f.stat().st_size / (1024 * 1024)
                    print(f"  - {rel_path} ({file_size_mb:.2f} MB)")
                
                if len(model_files) > 20:
                    print(f"  ... and {len(model_files) - 20} more files")
                
                print()
                print("=" * 80)
                print(f"✓ SUCCESS! Downloaded Docling models to '{local_dir}'")
                print("=" * 80)
                return 0
            else:
                print("      ⚠ Warning: No model files found")
                print()
                print("Models may be stored in a different location.")
                print("Check Docling's default cache directory.")
                return 1
        else:
            print(f"      ✗ Directory not created: {local_dir}")
            return 1
            
    except ImportError as e:
        print()
        print(f"✗ Error: Required package not installed")
        print(f"  {e}")
        print()
        print("Install docling with:")
        print("  pip install docling[vlm]")
        return 1
        
    except Exception as e:
        print()
        print(f"✗ Error: {type(e).__name__}: {e}")
        print()
        import traceback
        traceback.print_exc()
        print()
        print("Troubleshooting:")
        print("  1. Ensure docling is installed: pip install docling[vlm]")
        print("  2. Check internet connection (models download on first use)")
        print(f"  3. Check write permissions for {local_dir}")
        return 1


if __name__ == "__main__":
    sys.exit(main())

