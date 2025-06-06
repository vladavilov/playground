import nltk

def download_punkt():
    """
    Downloads the 'punkt' tokenizer models for NLTK.
    This is required for sentence tokenization.
    """
    try:
        # Check if 'punkt' is already available
        nltk.data.find('tokenizers/punkt')
        print("NLTK 'punkt' tokenizer is already downloaded.")
    except LookupError:
        print("Downloading NLTK 'punkt' tokenizer...")
        nltk.download('punkt')
        print("'punkt' tokenizer downloaded successfully.")

if __name__ == "__main__":
    download_punkt() 