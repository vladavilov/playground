# Azure Blob Storage Service Documentation

## Overview

The `BlobStorageClient` provides a comprehensive interface for managing document storage in Azure Blob Storage. This service handles file upload, download, deletion, and organization operations with proper error handling and logging.

## Features

- **File Upload**: Upload files to Azure Blob Storage with optional project-based organization
- **File Download**: Download files from blob storage to local filesystem
- **File Deletion**: Remove files from blob storage
- **File Listing**: List files with optional filtering by project or prefix
- **Project Organization**: Organize files by project ID for better management
- **Cleanup Operations**: Bulk cleanup of project-specific files
- **Error Handling**: Comprehensive error handling with detailed logging

## Configuration

### Environment Variables

The service requires an Azure Storage connection string to be configured:

```bash
AZURE_STORAGE_CONNECTION_STRING="DefaultEndpointsProtocol=https;AccountName=youraccount;AccountKey=yourkey;EndpointSuffix=core.windows.net"
```

### Initialization

```python
from services.blob_storage import BlobStorageClient

# Using environment variable
client = BlobStorageClient()

# Using explicit connection string
client = BlobStorageClient(
    connection_string="your_connection_string",
    container_name="documents"  # Optional, defaults to "documents"
)
```

## Usage Examples

### File Upload

```python
from uuid import uuid4

# Basic file upload
result = client.upload_file(
    file_path="/path/to/document.pdf",
    blob_name="document.pdf"
)

# Upload with project organization
project_id = uuid4()
result = client.upload_file(
    file_path="/path/to/document.pdf",
    blob_name="document.pdf",
    project_id=project_id
)

if result.success:
    print(f"File uploaded successfully: {result.blob_url}")
else:
    print(f"Upload failed: {result.error_message}")
```

### File Download

```python
result = client.download_file(
    blob_name="project-id/document.pdf",
    local_path="/local/path/document.pdf"
)

if result.success:
    print(f"File downloaded to: {result.local_path}")
else:
    print(f"Download failed: {result.error_message}")
```

### File Deletion

```python
result = client.delete_file("project-id/document.pdf")

if result.success:
    print("File deleted successfully")
else:
    print(f"Deletion failed: {result.error_message}")
```

### File Listing

```python
# List all files
result = client.list_files()

# List files for specific project
project_id = uuid4()
result = client.list_files(project_id=project_id)

# List files with custom prefix
result = client.list_files(prefix="reports/")

if result.success:
    for file_name in result.file_list:
        print(f"Found file: {file_name}")
```

### Project Cleanup

```python
project_id = uuid4()
result = client.cleanup_project_files(project_id)

if result.success:
    print("Project files cleaned up successfully")
```

## API Reference

### BlobStorageClient

#### Constructor

```python
BlobStorageClient(connection_string: Optional[str] = None, container_name: str = "documents")
```

**Parameters:**
- `connection_string`: Azure Storage connection string (defaults to `AZURE_STORAGE_CONNECTION_STRING` environment variable)
- `container_name`: Name of the blob container (defaults to "documents")

**Raises:**
- `ValueError`: If no connection string is provided

#### Methods

##### upload_file

```python
upload_file(file_path: str, blob_name: str, project_id: Optional[UUID] = None) -> BlobStorageResult
```

Upload a file to Azure Blob Storage.

**Parameters:**
- `file_path`: Local path to the file to upload
- `blob_name`: Name for the blob in storage
- `project_id`: Optional project ID for organizing files

**Returns:** `BlobStorageResult` with upload operation details

##### download_file

```python
download_file(blob_name: str, local_path: str) -> BlobStorageResult
```

Download a file from Azure Blob Storage.

**Parameters:**
- `blob_name`: Name of the blob to download
- `local_path`: Local path where to save the file

**Returns:** `BlobStorageResult` with download operation details

##### delete_file

```python
delete_file(blob_name: str) -> BlobStorageResult
```

Delete a file from Azure Blob Storage.

**Parameters:**
- `blob_name`: Name of the blob to delete

**Returns:** `BlobStorageResult` with deletion operation details

##### list_files

```python
list_files(project_id: Optional[UUID] = None, prefix: Optional[str] = None) -> BlobStorageResult
```

List files in Azure Blob Storage.

**Parameters:**
- `project_id`: Optional project ID to filter files
- `prefix`: Optional prefix to filter files

**Returns:** `BlobStorageResult` with file list

##### cleanup_project_files

```python
cleanup_project_files(project_id: UUID) -> BlobStorageResult
```

Clean up all files for a specific project.

**Parameters:**
- `project_id`: Project ID

**Returns:** `BlobStorageResult` with cleanup operation details

### BlobStorageResult

Data class representing the result of blob storage operations.

#### Attributes

- `success: bool` - Whether the operation was successful
- `blob_name: Optional[str]` - Name of the blob
- `blob_url: Optional[str]` - URL of the blob (for uploads)
- `local_path: Optional[str]` - Local file path (for downloads)
- `file_list: Optional[List[str]]` - List of file names (for listing operations)
- `error_message: Optional[str]` - Error message if operation failed

## File Organization

The service supports project-based file organization:

- **Without project ID**: Files are stored directly in the container root
  - Example: `document.pdf`
- **With project ID**: Files are organized under project-specific prefixes
  - Example: `550e8400-e29b-41d4-a716-446655440000/document.pdf`

## Error Handling

The service provides comprehensive error handling:

- **File not found**: Returns failure result with descriptive error message
- **Azure SDK errors**: Catches and wraps Azure-specific exceptions
- **Network issues**: Handles connection and timeout errors
- **Permission errors**: Handles authentication and authorization failures

All errors are logged using structured logging for debugging and monitoring.

## Testing

### Test Setup

Tests use mocked Azure SDK dependencies to avoid requiring actual Azure Storage accounts:

```python
def setup_method(self):
    """Set up test fixtures."""
    # Provide a mock connection string to avoid Azure Storage dependency
    mock_connection_string = "DefaultEndpointsProtocol=https;AccountName=mockaccount;AccountKey=mockkey;EndpointSuffix=core.windows.net"
    self.client = BlobStorageClient(connection_string=mock_connection_string)
```

### Running Tests

```bash
# Run all blob storage tests
python -m pytest tests/test_blob_storage.py -v

# Run specific test
python -m pytest tests/test_blob_storage.py::TestBlobStorageClient::test_upload_file_success -v
```

### Test Coverage

The test suite covers:

- ✅ Successful file upload operations
- ✅ File upload with project organization
- ✅ Missing file handling
- ✅ File download operations
- ✅ File deletion operations
- ✅ File listing operations
- ✅ Blob name generation with and without project IDs

## Logging

The service uses structured logging with the following log levels:

- **INFO**: Successful operations and general flow
- **WARNING**: Non-critical issues (e.g., failed deletion during cleanup)
- **ERROR**: Operation failures and exceptions

Log entries include relevant context such as file paths, blob names, and project IDs.

## Best Practices

1. **Always check result.success** before proceeding with dependent operations
2. **Use project IDs** for better file organization in multi-tenant scenarios
3. **Handle cleanup** of temporary files after processing
4. **Monitor blob storage costs** by implementing retention policies
5. **Use appropriate container names** for different environments (dev, staging, prod)

## Dependencies

- `azure-storage-blob`: Azure Blob Storage SDK
- `structlog`: Structured logging
- `uuid`: UUID generation for project organization

## Security Considerations

- Store connection strings securely (environment variables, key vaults)
- Use managed identities when possible instead of connection strings
- Implement proper access controls at the Azure Storage account level
- Consider using SAS tokens for time-limited access
- Enable blob storage logging and monitoring for security auditing