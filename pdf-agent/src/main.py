from fastapi import FastAPI, UploadFile, File, HTTPException, status
from contextlib import asynccontextmanager
import shutil
import tempfile
from src.logger_config import setup_logging, get_logger
from src.pdf_processing.workflow import PDFExtractionWorkflow
from src.config import get_settings

# Initialize logging as early as possible
setup_logging()

log = get_logger(__name__)

# Define the lifespan context manager
@asynccontextmanager
async def lifespan(app: FastAPI):
    log.info("Application startup complete.")
    yield
    # Add any shutdown logic here if needed in the future
    log.info("Application shutdown complete.")

app = FastAPI(lifespan=lifespan)

@app.get("/health")
async def health_check():
    log.info("Health check endpoint was called.")
    return {"status": "ok"}

@app.post("/extract_data")
async def extract_data(file: UploadFile = File(...)):
    """
    Extracts structured data from an uploaded PDF file.
    """
    settings = get_settings()
    
    # Security: Validate file type and size
    if file.content_type != "application/pdf":
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Invalid file type. Only PDF files are accepted.",
        )
        
    # Check file size
    max_size = settings.MAX_FILE_SIZE_MB * 1024 * 1024
    size = await file.read()
    if len(size) > max_size:
        raise HTTPException(
            status_code=status.HTTP_413_REQUEST_ENTITY_TOO_LARGE,
            detail=f"File size exceeds the limit of {settings.MAX_FILE_SIZE_MB}MB.",
        )
    await file.seek(0) # Reset file pointer after reading

    temp_dir = settings.TEMP_FILE_DIR
    temp_file_path = temp_dir / file.filename
    
    log.info(f"Receiving file: {file.filename} (Size: {len(size)} bytes)")

    try:
        with open(temp_file_path, "wb") as buffer:
            shutil.copyfileobj(file.file, buffer)
        
        log.info(f"File saved temporarily to {temp_file_path}")
        
        log.info("Initiating data extraction workflow...")
        workflow = PDFExtractionWorkflow()
        result = workflow.run(pdf_file_path=str(temp_file_path))

        log.info("Workflow execution complete.")
        return result.content

    except Exception as e:
        log.error(f"An error occurred during file processing: {e}", exc_info=True)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"An unexpected error occurred: {e}",
        )
    finally:
        if temp_file_path.exists():
            temp_file_path.unlink()
            log.info(f"Temporary file {temp_file_path} deleted.")

# To run this application (for local testing):
# uvicorn src.main:app --reload --log-level trace 