import os
import sys
from pathlib import Path
from sentence_transformers import SentenceTransformer

# Add the project root to the Python path to allow importing from 'src'
project_root = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(project_root))

from src.config import get_settings

def download_and_save_model():
    """
    Downloads a SentenceTransformer model and saves it to the path specified
    in the application settings. This is used to pre-load the model into a
    Docker image or environment.
    """
    settings = get_settings()
    model_name = "BAAI/bge-small-en-v1.5"
    save_path = settings.EMBEDDING_MODEL_PATH

    if os.path.exists(save_path):
        print(f"Model directory '{save_path}' already exists. Skipping download.")
        return

    print(f"Downloading sentence transformer model '{model_name}'...")
    model = SentenceTransformer(model_name)
    
    print(f"Saving model to '{save_path}'...")
    model.save(save_path)
    
    print("Model downloaded and saved successfully.")

if __name__ == "__main__":
    download_and_save_model() 