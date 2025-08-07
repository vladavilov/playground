"""
Redis-based test monitoring utilities for end-to-end tests.

This module provides comprehensive monitoring capabilities for Celery tasks,
Redis streams, and UI progress messages during document processing workflows.
"""

import json
import time
from typing import Dict, Any
import redis
import requests

from config import TestConstants


class RedisTestMonitor:
    """
    Centralized Celery task monitoring for end-to-end tests.
    
    Provides comprehensive task monitoring using multiple verification strategies:
    1. Redis Streams monitoring for inter-service communication
    2. Celery task state inspection for actual task execution
    3. Traditional queue monitoring as fallback
    4. Redis Pub/Sub monitoring for project progress updates
    """
    
    def __init__(self, redis_config: Dict[str, Any], queue_name: str = 'celery'):
        """
        Initialize RedisTestMonitor with Redis connection and multiple monitoring strategies.
        
        Args:
            redis_config: Redis configuration dictionary
            queue_name: Name of the Celery queue to monitor (default: 'celery')
        """
        self.redis_client = redis.Redis(**redis_config)
        self.queue_name = queue_name
        self.stream_name = "task_streams:document_processing"
        self.consumer_group = "document_processors"
        self.ui_progress_channel = "ui:project_progress"  # Unified UI progress channel
        self.tracked_task_ids = set()  # Track Celery task IDs for verification
        
        # UI Progress monitoring state (consolidated from UIProgressMonitor)
        self.pubsub = None
        self.ui_messages_received = []
        self.ui_monitoring_active = False
        self.ui_project_id = None
    
    def get_current_task_count(self) -> int:
        """
        Get current number of tasks in the Celery queue.
        
        Returns:
            Number of tasks currently in the queue
            
        Raises:
            redis.RedisError: If Redis connection fails
        """
        return self.redis_client.llen(self.queue_name)
    
    def verify_task_queued(
        self, 
        initial_count: int, 
        expected_increase: int = 1, 
        timeout: int = TestConstants.DEFAULT_TIMEOUT
    ) -> bool:
        """
        Verify that the task queue increased by the expected amount.
        
        This method maintains strict exact count matching for test compatibility.
        For production use, consider verify_task_published_comprehensive for better reliability.
        
        Args:
            initial_count: Initial task count before operation
            expected_increase: Expected increase in task count
            timeout: Maximum time to wait for task to be queued
            
        Returns:
            True if task count increased by EXACTLY the expected amount, False otherwise
        """
        start_time = time.time()
        expected_count = initial_count + expected_increase
        
        while time.time() - start_time < timeout:
            try:
                current_count = self.get_current_task_count()
                if current_count == expected_count:
                    return True
                elif current_count > expected_count:
                    # More tasks queued than expected - return False for strict compatibility
                    return False
                    
            except redis.RedisError:
                # Ignore individual Redis errors during monitoring
                pass
                
            time.sleep(0.5)
            
        return False
    
    def wait_for_task_completion(self, timeout: int = TestConstants.DOCUMENT_PROCESSING_TIMEOUT) -> None:
        """
        Wait for all Celery tasks to complete.
        
        Args:
            timeout: Maximum time to wait in seconds
            
        Raises:
            AssertionError: If tasks don't complete within timeout
        """
        start_time = time.time()
        
        while time.time() - start_time < timeout:
            try:
                active_tasks = self.get_current_task_count()
                if active_tasks == 0:
                    return  # All tasks completed
                    
            except redis.RedisError:
                # Ignore individual Redis errors during monitoring
                pass
                
            time.sleep(5)
            
        raise AssertionError(f"Tasks did not complete within {timeout} seconds")
    
    def wait_for_task_completion_with_project_status(
        self,
        project_id: str,
        service_urls: Dict[str, str],
        auth_headers: Dict[str, str],
        timeout: int = TestConstants.DOCUMENT_PROCESSING_TIMEOUT
    ) -> None:
        """
        Wait for task completion using comprehensive monitoring strategies.
        
        Args:
            project_id: ID of the project being processed
            service_urls: Service URL configuration
            auth_headers: Authentication headers
            timeout: Maximum time to wait in seconds
            
        Raises:
            AssertionError: If processing doesn't complete within timeout
        """
        start_time = time.time()
        last_status_check = 0
        
        while time.time() - start_time < timeout:
            try:
                # Strategy 1: Check traditional Celery queue
                active_tasks = self.get_current_task_count()
                
                # Strategy 2: Check Redis Streams consumer group status
                stream_pending = self._get_stream_pending_count()
                
                # Strategy 3: Check project status (less frequently to avoid spam)
                current_time = time.time()
                project_status = None
                if current_time - last_status_check > 10:  # Check every 10 seconds
                    try:
                        response = requests.get(
                            f"{service_urls['project_management']}/projects/{project_id}",
                            headers=auth_headers,
                            timeout=TestConstants.DEFAULT_TIMEOUT
                        )
                        if response.status_code == TestConstants.HTTP_OK:
                            project = response.json()
                            project_status = project["status"]
                            last_status_check = current_time
                    except requests.RequestException:
                        # Ignore individual status check errors during monitoring
                        pass
                
                # Determine if processing is complete
                celery_idle = active_tasks == 0
                stream_idle = stream_pending == 0
                project_not_processing = project_status and project_status != TestConstants.PROJECT_STATUS_PROCESSING
                
                # Processing is considered complete if multiple indicators agree
                completion_indicators = sum([
                    celery_idle,
                    stream_idle,
                    project_not_processing
                ])
                
                if completion_indicators >= 2:
                    return  # Processing completed

            except (redis.RedisError, requests.RequestException):
                # Ignore individual errors during monitoring
                pass

            time.sleep(5)

        raise AssertionError(f"Document processing did not complete within {timeout} seconds")
    
    def _get_stream_pending_count(self) -> int:
        """
        Get the number of pending messages in the Redis stream consumer group.
        
        Returns:
            Number of pending messages, or -1 if unable to determine
        """
        try:
            pending_info = self.redis_client.xpending(self.stream_name, self.consumer_group)
            
            if isinstance(pending_info, dict):
                return pending_info.get('pending', 0)
            elif isinstance(pending_info, list) and len(pending_info) > 0:
                return pending_info[0]
            else:
                return 0
                
        except redis.ResponseError as e:
            if "nogroup" in str(e).lower():
                # Consumer group doesn't exist - likely no pending messages
                return 0
            else:
                return -1
        except Exception:
            return -1
    
    def get_stream_message_count(self) -> int:
        """
        Get the total number of messages in the Redis stream.
        
        Returns:
            Number of messages in the task request stream
        """
        try:
            stream_info = self.redis_client.xinfo_stream(self.stream_name)
            return stream_info.get('length', 0)
        except redis.ResponseError:
            # Stream doesn't exist yet
            return 0
    
    def get_recent_stream_messages(self, count: int = 10) -> list:
        """
        Get recent messages from the Redis stream.
        
        Args:
            count: Number of recent messages to retrieve
            
        Returns:
            List of recent stream messages
        """
        try:
            messages = self.redis_client.xrevrange(self.stream_name, count=count)
            return messages
        except redis.ResponseError:
            # Stream doesn't exist yet
            return []
    
    def verify_stream_message_published(self, project_id: str, timeout: int = 10) -> bool:
        """
        Verify that a task request message was published to Redis streams for the given project.
        
        Args:
            project_id: Project ID to look for in stream messages
            timeout: Maximum time to wait for message to appear
            
        Returns:
            True if message was found, False otherwise
        """
        start_time = time.time()
        
        while time.time() - start_time < timeout:
            try:
                # Get recent messages from the stream
                recent_messages = self.get_recent_stream_messages(count=5)
                
                for message_id, message_data in recent_messages:
                    # Check if this message is for our project
                    if message_data.get('project_id') == project_id:
                        # Verify message structure
                        if (message_data.get('message_type') == 'task_request' and 
                            message_data.get('task_type') == 'process_project_documents'):
                            return True
                            
            except Exception:
                # Ignore individual errors during monitoring
                pass
                
            time.sleep(0.5)
            
        return False
    
    def get_celery_task_keys(self) -> list:
        """
        Discover actual Celery task-related Redis keys.
        
        Returns:
            List of Redis keys related to Celery tasks
        """
        try:
            # Look for common Celery/Kombu key patterns
            patterns = [
                f"celery-task-meta-*",
                f"_kombu.binding.*",
                f"{self.queue_name}*",
                f"*{self.queue_name}*"
            ]
            
            all_keys = []
            for pattern in patterns:
                keys = self.redis_client.keys(pattern)
                all_keys.extend(keys)
            
            return list(set(all_keys))  # Remove duplicates
            
        except Exception:
            return []
    
    def verify_task_published_comprehensive(
        self, 
        project_id: str,
        initial_stream_count: int,
        timeout: int = TestConstants.DEFAULT_TIMEOUT
    ) -> Dict[str, Any]:
        """
        Comprehensive verification that combines multiple monitoring strategies.
        
        Args:
            project_id: Project ID being processed
            initial_stream_count: Initial stream message count
            timeout: Maximum time to wait for verification
            
        Returns:
            Dict with verification results from different strategies
        """
        start_time = time.time()
        results = {
            'stream_message_found': False,
            'stream_count_increased': False,
            'celery_keys_found': False,
            'task_processing_detected': False,
            'verification_successful': False
        }
        
        while time.time() - start_time < timeout:
            try:
                # Strategy 1: Check if stream message was published
                if not results['stream_message_found']:
                    results['stream_message_found'] = self.verify_stream_message_published(
                        project_id, timeout=2
                    )
                
                # Strategy 2: Check if stream count increased
                current_stream_count = self.get_stream_message_count()
                results['stream_count_increased'] = current_stream_count > initial_stream_count
                
                # Strategy 3: Check for Celery-related Redis keys
                celery_keys = self.get_celery_task_keys()
                results['celery_keys_found'] = len(celery_keys) > 0
                
                # Strategy 4: Look for any sign of task processing activity
                # This could be project status changes, Redis activity, etc.
                results['task_processing_detected'] = (
                    results['stream_message_found'] or 
                    results['stream_count_increased']
                )
                
                # Consider verification successful if we have strong evidence
                results['verification_successful'] = (
                    results['stream_message_found'] and results['stream_count_increased']
                ) or (
                    results['stream_count_increased'] and results['celery_keys_found']
                )
                
                if results['verification_successful']:
                    return results
                    
            except Exception:
                # Ignore individual errors during monitoring
                pass
                
            time.sleep(0.5)
        
        return results 

    def start_ui_progress_monitoring(self, project_id: str) -> None:
        """
        Start monitoring the UI progress channel for a specific project.
        
        This method starts listening BEFORE processing begins to capture
        all pub/sub messages published during the workflow.
        
        Args:
            project_id: Project ID to monitor
        """
        if self.ui_monitoring_active and self.ui_project_id == project_id:
            return
            
        # Stop any existing monitoring
        self.stop_ui_progress_monitoring()
        
        # Set project context
        self.ui_project_id = project_id
        self.ui_messages_received = []
        
        # Create pubsub connection
        self.pubsub = self.redis_client.pubsub()
        self.pubsub.subscribe(self.ui_progress_channel)
        
        # Get subscription confirmation
        initial_message = self.pubsub.get_message(timeout=2)
        if initial_message and initial_message['type'] == 'subscribe':
            self.ui_monitoring_active = True
        else:
            raise RuntimeError(f"Failed to subscribe to UI progress channel '{self.ui_progress_channel}'")
    
    def stop_ui_progress_monitoring(self) -> None:
        """
        Stop monitoring and clean up UI progress monitoring resources.
        """
        if self.pubsub:
            try:
                self.pubsub.close()
            except Exception:
                # Ignore cleanup errors
                pass
            finally:
                self.pubsub = None
                self.ui_monitoring_active = False
                self.ui_project_id = None
                self.ui_messages_received = []
    
    def check_for_ui_messages(self, timeout: float = 0.1) -> int:
        """
        Check for new UI progress messages and add them to the received list.
        
        Args:
            timeout: Timeout for message check
            
        Returns:
            Number of new messages received for this project
        """
        if not self.ui_monitoring_active or not self.ui_project_id:
            return 0
            
        new_messages = 0
        start_time = time.time()
        
        try:
            # Process all available messages within the timeout window
            while time.time() - start_time < timeout:
                message = self.pubsub.get_message(timeout=0.05)  # Short timeout for responsive checking
                
                if message and message['type'] == 'message':
                    try:
                        message_data = json.loads(message['data'])
                        
                        # Check if this message is for our project
                        if message_data.get('project_id') == self.ui_project_id:
                            if message_data.get('message_type') == 'project_progress':
                                self.ui_messages_received.append(message_data)
                                new_messages += 1
                                
                    except (json.JSONDecodeError, KeyError):
                        # Ignore invalid messages
                        pass
                elif message is None:
                    # No more messages available
                    break
                    
        except Exception:
            # Ignore individual errors during monitoring
            pass
            
        return new_messages
    
    def get_ui_messages_count(self) -> int:
        """
        Get the total number of UI progress messages received for the current project.
        
        Returns:
            Number of messages received
        """
        return len(self.ui_messages_received)
    
    def get_ui_messages(self) -> list:
        """
        Get all UI progress messages received for the current project.
        
        Returns:
            List of message data dictionaries
        """
        return self.ui_messages_received.copy()
    
    def wait_for_ui_messages(
        self, 
        min_messages: int = 1, 
        timeout: int = 30,
        check_interval: float = 0.5
    ) -> bool:
        """
        Wait for a minimum number of UI progress messages.
        
        Args:
            min_messages: Minimum number of messages to wait for
            timeout: Maximum time to wait in seconds
            check_interval: How often to check for messages
            
        Returns:
            True if minimum messages received, False if timeout
        """
        if not self.ui_monitoring_active:
            return False
            
        start_time = time.time()
        
        while time.time() - start_time < timeout:
            self.check_for_ui_messages(timeout=check_interval)
            
            if len(self.ui_messages_received) >= min_messages:
                return True
                
            time.sleep(check_interval)
            
        return False

    def monitor_ui_during_processing(
        self,
        processing_check_func,
        timeout: int = 60,
        check_interval: float = 0.5
    ) -> bool:
        """
        Monitor UI progress messages during document processing.
        
        This method continuously monitors for UI progress messages while
        also checking if processing is complete using the provided function.
        
        Args:
            processing_check_func: Function that returns True when processing is complete
            timeout: Maximum time to wait in seconds
            check_interval: How often to check for messages and processing status
            
        Returns:
            True if processing completed and messages were received, False if timeout
        """
        if not self.ui_monitoring_active:
            return False
            
        start_time = time.time()
        messages_received = 0
        
        while time.time() - start_time < timeout:
            # Check for new UI progress messages
            new_messages = self.check_for_ui_messages(timeout=0.1)
            messages_received += new_messages
            
            # Check if processing is complete
            try:
                if processing_check_func():
                    # Give a bit more time for final messages
                    time.sleep(2)
                    final_messages = self.check_for_ui_messages(timeout=1.0)
                    messages_received += final_messages
                    
                    return messages_received > 0
                    
            except Exception:
                # Ignore errors in processing check function
                pass
            
            time.sleep(check_interval)
            
        return False

    def get_ui_project_progress_channel(self) -> str:
        """
        Get the unified UI project progress channel name.
        
        Returns:
            str: UI project progress channel name
        """
        return self.ui_progress_channel

    def wait_for_ui_project_progress_message(
        self, 
        project_id: str, 
        expected_status: str = None,
        timeout: int = TestConstants.DEFAULT_TIMEOUT
    ) -> Dict[str, Any]:
        """
        Wait for a project progress message on the UI channel and return its data.
        
        Args:
            project_id: Project ID to look for in progress messages
            expected_status: Expected project status in the message (optional)
            timeout: Maximum time to wait for message
            
        Returns:
            Dict with message data if found, empty dict if not found
            
        Raises:
            AssertionError: If message is not received within timeout
        """
        # Start monitoring temporarily if not already active for this project
        was_monitoring = self.ui_monitoring_active and self.ui_project_id == project_id
        
        if not was_monitoring:
            self.start_ui_progress_monitoring(project_id)
        
        try:
            start_time = time.time()
            while time.time() - start_time < timeout:
                # Check for new messages
                new_messages = self.check_for_ui_messages(timeout=0.5)
                
                if new_messages > 0 or len(self.ui_messages_received) > 0:
                    # Get the most recent message
                    latest_message = self.ui_messages_received[-1]
                    
                    # Check status if specified
                    if expected_status is None or latest_message.get('status') == expected_status:
                        return latest_message
                        
                time.sleep(0.1)
                
        finally:
            # Only stop monitoring if we started it temporarily
            if not was_monitoring:
                self.stop_ui_progress_monitoring()
            
        raise AssertionError(
            f"No UI progress message received for project {project_id} within {timeout} seconds"
        )

    def __enter__(self):
        """Context manager entry for UI progress monitoring."""
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit - stop UI progress monitoring."""
        self.stop_ui_progress_monitoring()
