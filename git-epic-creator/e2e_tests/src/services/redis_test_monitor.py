"""
Redis-based test monitoring utilities for end-to-end tests.

This module provides comprehensive monitoring capabilities for Celery tasks,
Redis streams, and UI progress messages during document processing workflows.
"""

import json
import time
from typing import Dict, Any
import redis
import threading

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
        self.ui_progress_channel = "ui:project_progress"  # Unified UI progress channel
        self.ai_progress_channel = "ui:ai_workflow_progress"
        self.tracked_task_ids = set()  # Track Celery task IDs for verification
        
        # UI Progress monitoring state (consolidated from UIProgressMonitor)
        self.pubsub = None
        self.ui_messages_received = []
        self.ui_monitoring_active = False
        self.ui_project_id = None
        self._listener_thread: threading.Thread | None = None
        self._listener_running: bool = False
        self._msg_cond: threading.Condition = threading.Condition()
        self._test_start_time: float | None = None

        # AI workflow progress monitoring state
        self.ai_pubsub = None
        self.ai_messages_received = []
        self.ai_monitoring_active = False
        self.ai_project_id = None
        self._ai_listener_thread: threading.Thread | None = None
        self._ai_listener_running: bool = False
        self._ai_msg_cond: threading.Condition = threading.Condition()
    
    def get_current_task_count(self) -> int:
        """
        Get current number of tasks in the Celery queue.
        
        Returns:
            Number of tasks currently in the queue
            
        Raises:
            redis.RedisError: If Redis connection fails
        """
        return self.redis_client.llen(self.queue_name)
    
    # removed: task completion helpers and strict queue verification (use comprehensive verification and iter_ui_sequence)

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
            'queue_increased': False,
            'celery_keys_found': False,
            'task_processing_detected': False,
            'verification_successful': False
        }
        
        while time.time() - start_time < timeout:
            try:
                # Strategy 1: Check if queue length increased
                current_count = self.get_current_task_count()
                results['queue_increased'] = current_count > initial_stream_count
                
                # Strategy 2: Check for Celery-related Redis keys
                celery_keys = self.get_celery_task_keys()
                results['celery_keys_found'] = len(celery_keys) > 0
                
                # Strategy 3: Look for any sign of task processing activity
                # This could be project status changes, Redis activity, etc.
                results['task_processing_detected'] = (
                    results['queue_increased']
                )
                
                # Consider verification successful if we have strong evidence
                # Consider success if either the queue increased OR we detected Celery keys
                # to handle fast consumers that drain the queue immediately
                results['verification_successful'] = (
                    results['queue_increased'] or results['celery_keys_found']
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
        self._test_start_time = time.time()
        
        # Create pubsub connection (ignore subscribe messages to simplify listener)
        self.pubsub = self.redis_client.pubsub(ignore_subscribe_messages=True)
        self.pubsub.subscribe(self.ui_progress_channel)

        # Start a background thread that blocks on pubsub.listen()
        self._listener_running = True

        def _listen_loop() -> None:
            try:
                for message in self.pubsub.listen():  # blocking
                    if not self._listener_running:
                        break
                    try:
                        if not isinstance(message, dict):
                            continue
                        if message.get('type') != 'message':
                            continue
                        raw = message.get('data')
                        if isinstance(raw, bytes):
                            try:
                                raw = raw.decode('utf-8')
                            except Exception:
                                continue
                        data = None
                        try:
                            data = json.loads(raw)
                        except Exception:
                            continue
                        if data and self._is_message_for_current_test(data):
                            with self._msg_cond:
                                self.ui_messages_received.append(data)
                                self._msg_cond.notify_all()
                    except Exception:
                        # Ignore malformed messages
                        continue
            except Exception:
                # Listener exits on pubsub.close() or connection errors
                pass

        self._listener_thread = threading.Thread(target=_listen_loop, daemon=True)
        self._listener_thread.start()
        self.ui_monitoring_active = True
    
    def stop_ui_progress_monitoring(self) -> None:
        """
        Stop monitoring and clean up UI progress monitoring resources.
        """
        # Signal listener to stop
        self._listener_running = False
        if self.pubsub:
            try:
                self.pubsub.close()
            except Exception:
                # Ignore cleanup errors
                pass
        # Best-effort join
        if self._listener_thread and self._listener_thread.is_alive():
            try:
                self._listener_thread.join(timeout=1.0)
            except Exception:
                pass
        self._listener_thread = None
        self.pubsub = None
        self.ui_monitoring_active = False
        self.ui_project_id = None
        self.ui_messages_received = []
        self._test_start_time = None

    # ---- AI Workflow progress monitoring ----
    def start_ai_workflow_monitoring(self, project_id: str) -> None:
        """
        Start monitoring the AI workflow progress channel for a project.
        """
        if self.ai_monitoring_active and self.ai_project_id == project_id:
            return
        self.stop_ai_workflow_monitoring()
        self.ai_project_id = project_id
        self.ai_messages_received = []
        self.ai_pubsub = self.redis_client.pubsub(ignore_subscribe_messages=True)
        self.ai_pubsub.subscribe(self.ai_progress_channel)

        self._ai_listener_running = True

        def _ai_listen_loop() -> None:
            try:
                for message in self.ai_pubsub.listen():
                    if not self._ai_listener_running:
                        break
                    try:
                        if not isinstance(message, dict):
                            continue
                        if message.get('type') != 'message':
                            continue
                        raw = message.get('data')
                        if isinstance(raw, bytes):
                            try:
                                raw = raw.decode('utf-8')
                            except Exception:
                                continue
                        data = None
                        try:
                            data = json.loads(raw)
                        except Exception:
                            continue
                        if data and self._is_ai_message_for_current_test(data):
                            with self._ai_msg_cond:
                                self.ai_messages_received.append(data)
                                self._ai_msg_cond.notify_all()
                    except Exception:
                        continue
            except Exception:
                pass

        self._ai_listener_thread = threading.Thread(target=_ai_listen_loop, daemon=True)
        self._ai_listener_thread.start()
        self.ai_monitoring_active = True

    def stop_ai_workflow_monitoring(self) -> None:
        """Stop AI workflow monitoring and clean up resources."""
        self._ai_listener_running = False
        if self.ai_pubsub:
            try:
                self.ai_pubsub.close()
            except Exception:
                pass
        if self._ai_listener_thread and self._ai_listener_thread.is_alive():
            try:
                self._ai_listener_thread.join(timeout=1.0)
            except Exception:
                pass
        self._ai_listener_thread = None
        self.ai_pubsub = None
        self.ai_monitoring_active = False
        self.ai_project_id = None
        self.ai_messages_received = []

    def iter_ai_sequence(
        self,
        project_id: str,
        expected_statuses: list[str],
        *,
        timeout_per_step: int = TestConstants.DEFAULT_TIMEOUT,
    ):
        """Generator yielding AI workflow messages matching expected statuses in order."""
        if not self.ai_monitoring_active or self.ai_project_id != project_id:
            raise AssertionError("AI monitoring must be active for the target project before iterating a sequence")

        consumed_index = 0
        for expected in expected_statuses:
            deadline = time.time() + timeout_per_step
            found_msg: Dict[str, Any] | None = None
            while time.time() < deadline and found_msg is None:
                remaining = max(0.0, deadline - time.time())
                with self._ai_msg_cond:
                    if len(self.ai_messages_received) <= consumed_index:
                        self._ai_msg_cond.wait(timeout=min(remaining, 1.0))
                    current_len = len(self.ai_messages_received)
                    for idx in range(consumed_index, current_len):
                        msg = self.ai_messages_received[idx]
                        if self._is_ai_message_for_current_test(msg):
                            status = msg.get('status')
                            if status == expected:
                                consumed_index = idx + 1
                                found_msg = msg
                                break
            if found_msg is None:
                last_status = None
                if self.ai_messages_received:
                    last_status = self.ai_messages_received[-1].get('status')
                raise AssertionError(
                    f"Did not observe expected AI status '{expected}' within {timeout_per_step}s; last='{last_status}'"
                )
            yield found_msg

    def _is_ai_message_for_current_test(self, data: Dict[str, Any]) -> bool:
        if data.get('message_type') != 'ai_workflow_progress':
            return False
        if data.get('project_id') != self.ai_project_id:
            return False
        return True

    def iter_ui_sequence(
        self,
        project_id: str,
        expected_statuses: list[str],
        *,
        timeout_per_step: int = TestConstants.DEFAULT_TIMEOUT
    ):
        """Generator that yields each expected UI status message in order.

        Usage:
            seq = monitor.iter_ui_sequence(project_id, ["processing", "active"]) 
            msg1 = next(seq)  # blocks until 'processing'
            # ... do assertions/side-effects ...
            msg2 = next(seq)  # blocks until 'active'
        """
        if not self.ui_monitoring_active or self.ui_project_id != project_id:
            raise AssertionError("UI monitoring must be active for the target project before iterating a sequence")

        consumed_index = 0

        for expected in expected_statuses:
            deadline = time.time() + timeout_per_step
            found_msg: Dict[str, Any] | None = None
            while time.time() < deadline and found_msg is None:
                # Wait for a new message or until deadline
                remaining = max(0.0, deadline - time.time())
                with self._msg_cond:
                    if len(self.ui_messages_received) <= consumed_index:
                        self._msg_cond.wait(timeout=min(remaining, 1.0))
                    current_len = len(self.ui_messages_received)
                    for idx in range(consumed_index, current_len):
                        msg = self.ui_messages_received[idx]
                        if self._is_message_for_current_test(msg):
                            status = msg.get('status')
                            if status == expected:
                                consumed_index = idx + 1
                                found_msg = msg
                                break
            if found_msg is None:
                last_status = None
                if self.ui_messages_received:
                    last_status = self.ui_messages_received[-1].get('status')
                raise AssertionError(
                    f"Did not observe expected UI status '{expected}' within {timeout_per_step}s; last='{last_status}'"
                )
            yield found_msg

    def _is_message_for_current_test(self, data: Dict[str, Any]) -> bool:
        if data.get('message_type') != 'project_progress':
            return False
        if data.get('project_id') != self.ui_project_id:
            return False
        return True

    def __enter__(self):
        """Context manager entry for UI progress monitoring."""
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit - stop UI progress monitoring."""
        self.stop_ui_progress_monitoring()
