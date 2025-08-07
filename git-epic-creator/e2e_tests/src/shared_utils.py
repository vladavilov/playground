"""
Shared utility functions for end-to-end tests.

This module provides common functionality used across multiple test modules
to eliminate code duplication and ensure consistency.
"""

import time
from typing import Dict, Any, Optional
import requests
from config import TestConstants


class HTTPUtils:
    """Utility class for common HTTP operations."""
    
    @staticmethod
    def make_request_with_retry(
        method: str,
        url: str,
        timeout: int = TestConstants.DEFAULT_TIMEOUT,
        headers: Optional[Dict[str, str]] = None,
        json_data: Optional[Dict[str, Any]] = None,
        files: Optional[Dict[str, Any]] = None,
        max_retries: int = 3,
        retry_delay: float = 1.0
    ) -> requests.Response:
        """
        Make HTTP request with retry logic and consistent error handling.
        
        Args:
            method: HTTP method (GET, POST, etc.)
            url: Request URL
            timeout: Request timeout in seconds
            headers: Optional HTTP headers
            json_data: Optional JSON data for request body
            files: Optional files for multipart upload
            max_retries: Maximum number of retry attempts
            retry_delay: Delay between retries in seconds
            
        Returns:
            requests.Response object
            
        Raises:
            requests.RequestException: If all retry attempts fail
        """
        last_exception = None
        
        for attempt in range(max_retries):
            try:
                return requests.request(
                    method=method,
                    url=url,
                    timeout=timeout,
                    headers=headers,
                    json=json_data,
                    files=files
                )
            except requests.RequestException as e:
                last_exception = e
                if attempt < max_retries - 1:
                    time.sleep(retry_delay)
                    
        raise last_exception
    
    @staticmethod
    def check_service_health(
        service_url: str, 
        endpoint: str = TestConstants.HEALTH_ENDPOINT,
        timeout: int = TestConstants.DEFAULT_TIMEOUT,
        expected_status: int = TestConstants.HTTP_OK
    ) -> bool:
        """
        Check if a service is healthy by calling its health endpoint.
        
        Args:
            service_url: Base URL of the service
            endpoint: Health check endpoint path
            timeout: Request timeout in seconds
            expected_status: Expected HTTP status code for healthy service
            
        Returns:
            True if service is healthy, False otherwise
        """
        try:
            response = requests.get(
                f"{service_url}{endpoint}",
                timeout=timeout
            )
            return response.status_code == expected_status
        except requests.RequestException:
            return False
    
    @staticmethod
    def wait_for_service_health(
        service_url: str,
        endpoint: str = TestConstants.HEALTH_ENDPOINT,
        timeout: int = TestConstants.SERVICE_HEALTH_TIMEOUT,
        check_interval: float = 2.0
    ) -> bool:
        """
        Wait for a service to become healthy within the specified timeout.
        
        Args:
            service_url: Base URL of the service
            endpoint: Health check endpoint path
            timeout: Maximum time to wait in seconds
            check_interval: Time between health checks in seconds
            
        Returns:
            True if service becomes healthy, False if timeout reached
        """
        start_time = time.time()
        
        while time.time() - start_time < timeout:
            if HTTPUtils.check_service_health(service_url, endpoint):
                return True
            time.sleep(check_interval)
            
        return False


class ServiceHealthChecker:
    """Centralized service health checking functionality."""
    
    @staticmethod
    def check_all_services_health(
        service_urls: Dict[str, str],
        timeout: int = TestConstants.SERVICE_HEALTH_TIMEOUT
    ) -> Dict[str, bool]:
        """
        Check health of all configured services.
        
        Args:
            service_urls: Dictionary mapping service names to URLs
            timeout: Maximum time to wait for each service
            
        Returns:
            Dictionary mapping service names to health status (True/False)
        """
        health_status = {}
        
        for service_name, service_url in service_urls.items():
            health_status[service_name] = HTTPUtils.wait_for_service_health(
                service_url=service_url,
                timeout=timeout
            )
            
        return health_status
    
    @staticmethod
    def wait_for_all_services(
        service_urls: Dict[str, str],
        timeout: int = TestConstants.SERVICE_HEALTH_TIMEOUT
    ) -> None:
        """
        Wait for all services to become healthy or raise an error.
        
        Args:
            service_urls: Dictionary mapping service names to URLs
            timeout: Maximum time to wait for all services
            
        Raises:
            RuntimeError: If any service fails to become healthy
        """
        health_status = ServiceHealthChecker.check_all_services_health(
            service_urls, timeout
        )
        
        failed_services = [
            service_name for service_name, is_healthy in health_status.items()
            if not is_healthy
        ]
        
        if failed_services:
            raise RuntimeError(
                f"The following services failed to become healthy: {', '.join(failed_services)}. "
                f"Make sure services are running before running tests."
            )


class DatabaseUtils:
    """Utility functions for database operations."""
    
    @staticmethod
    def execute_query_with_retry(
        connection,
        query: str,
        params: Optional[tuple] = None,
        max_retries: int = 3,
        retry_delay: float = 1.0
    ) -> Any:
        """
        Execute database query with retry logic.
        
        Args:
            connection: Database connection object
            query: SQL query to execute
            params: Optional query parameters
            max_retries: Maximum number of retry attempts
            retry_delay: Delay between retries in seconds
            
        Returns:
            Query result
            
        Raises:
            Exception: If all retry attempts fail
        """
        last_exception = None
        
        for attempt in range(max_retries):
            try:
                cursor = connection.cursor()
                if params:
                    cursor.execute(query, params)
                else:
                    cursor.execute(query)
                    
                result = cursor.fetchall() if cursor.description else None
                cursor.close()
                return result
                
            except Exception as e:
                last_exception = e
                if attempt < max_retries - 1:
                    time.sleep(retry_delay)
                    
        raise last_exception
    
    @staticmethod
    def verify_table_exists(connection, table_name: str) -> bool:
        """
        Verify that a database table exists.
        
        Args:
            connection: Database connection object
            table_name: Name of the table to check
            
        Returns:
            True if table exists, False otherwise
        """
        try:
            query = """
                SELECT table_name 
                FROM information_schema.tables 
                WHERE table_schema = 'public' 
                AND table_name = %s
                AND table_type = 'BASE TABLE'
            """
            result = DatabaseUtils.execute_query_with_retry(
                connection, query, (table_name,)
            )
            return len(result) > 0
        except Exception:
            return False


class ProjectTestUtils:
    """Utility functions for project-related test operations."""
    
    @staticmethod
    def build_project_url(base_url: str, project_id: str, endpoint: str = "") -> str:
        """
        Build a standardized project API URL.
        
        Args:
            base_url: Base service URL
            project_id: Project identifier
            endpoint: Optional additional endpoint path
            
        Returns:
            Complete project URL
        """
        url = f"{base_url}/projects/{project_id}"
        if endpoint:
            url = f"{url}{endpoint if endpoint.startswith('/') else '/' + endpoint}"
        return url
    
    @staticmethod
    def build_upload_url(base_url: str, project_id: str) -> str:
        """
        Build document upload URL for a project.
        
        Args:
            base_url: Base service URL
            project_id: Project identifier
            
        Returns:
            Document upload URL
        """
        return ProjectTestUtils.build_project_url(
            base_url, project_id, "/documents/upload"
        )
