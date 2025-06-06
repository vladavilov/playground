import argparse
import httpx
import json
import os

def send_pdf(file_path: str, url: str):
    """
    Sends a PDF file to a specified URL and prints the response.

    Args:
        file_path (str): The path to the PDF file.
        url (str): The URL to send the file to.
    """
    try:
        # Resolve to absolute path from current working directory
        absolute_path = os.path.abspath(file_path)

        with open(absolute_path, "rb") as f:
            files = {"file": (os.path.basename(absolute_path), f, "application/pdf")}
            with httpx.Client() as client:
                print(f"Sending {absolute_path} to {url}...")
                response = client.post(url, files=files)
                response.raise_for_status()  # Raise an exception for bad status codes
                
                try:
                    # Try to parse and print JSON response
                    response_json = response.json()
                    print("Received JSON response:")
                    print(json.dumps(response_json, indent=2))
                except json.JSONDecodeError:
                    # If response is not JSON, print as text
                    print("Received non-JSON response:")
                    print(response.text)

    except FileNotFoundError:
        print(f"Error: The file was not found at the specified path: {file_path}")
        print(f"(Resolved to: {os.path.abspath(file_path)})")
    except httpx.RequestError as e:
        print(f"An error occurred while requesting {e.request.url!r}.")
        print(f"Error details: {e}")
    except httpx.HTTPStatusError as e:
        print(f"Error response {e.response.status_code} while requesting {e.request.url!r}.")
        print("Response content:")
        print(e.response.text)
    except Exception as e:
        print(f"An unexpected error occurred: {e}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Send a PDF to a server and get the response.")
    parser.add_argument("file_path", type=str, help="The path to the PDF file (can be relative to the current directory).")
    parser.add_argument("url", nargs='?', default="http://localhost:8000/extract_data", help="The URL to send the file to.")
    
    args = parser.parse_args()
    
    send_pdf(args.file_path, args.url)
