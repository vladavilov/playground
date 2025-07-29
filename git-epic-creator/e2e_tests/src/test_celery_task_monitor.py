"""
Tests for CeleryTaskMonitor functionality.

This module tests the enhanced Celery task monitoring capabilities
for ensuring proper task queue validation during document processing.
"""

import pytest
import redis
from unittest.mock import Mock, patch
from typing import Dict, Any

from conftest import CeleryTaskMonitor
from config import TestConstants


class TestCeleryTaskMonitor:
    """Test suite for CeleryTaskMonitor class."""

    def test_celery_task_monitor_initialization(self, redis_config: Dict[str, Any]) -> None:
        """Test CeleryTaskMonitor can be initialized with Redis config."""
        monitor = CeleryTaskMonitor(redis_config)
        
        assert monitor is not None
        assert monitor.redis_client is not None
        assert monitor.queue_name == 'celery'

    def test_get_current_task_count_success(self, redis_config: Dict[str, Any]) -> None:
        """Test getting current task count from Redis."""
        monitor = CeleryTaskMonitor(redis_config)
        
        with patch.object(monitor.redis_client, 'llen', return_value=5) as mock_llen:
            task_count = monitor.get_current_task_count()
            
        assert task_count == 5
        mock_llen.assert_called_once_with('celery')

    def test_get_current_task_count_redis_error(self, redis_config: Dict[str, Any]) -> None:
        """Test handling Redis connection errors when getting task count."""
        monitor = CeleryTaskMonitor(redis_config)
        
        with patch.object(monitor.redis_client, 'llen', side_effect=redis.RedisError("Connection failed")):
            with pytest.raises(redis.RedisError):
                monitor.get_current_task_count()

    def test_verify_task_queued_success(self, redis_config: Dict[str, Any]) -> None:
        """Test successful verification that tasks were queued."""
        monitor = CeleryTaskMonitor(redis_config)
        
        initial_count = 3
        final_count = 4
        expected_increase = 1
        
        with patch.object(monitor, 'get_current_task_count', side_effect=[initial_count, final_count]):
            result = monitor.verify_task_queued(initial_count, expected_increase, timeout=5)
            
        assert result is True

    def test_verify_task_queued_timeout(self, redis_config: Dict[str, Any]) -> None:
        """Test timeout when task is not queued within expected time."""
        monitor = CeleryTaskMonitor(redis_config)
        
        initial_count = 3
        # Task count doesn't increase
        with patch.object(monitor, 'get_current_task_count', return_value=initial_count):
            with patch('time.sleep'):  # Speed up the test
                result = monitor.verify_task_queued(initial_count, expected_increase=1, timeout=1)
                
        assert result is False

    def test_verify_task_queued_incorrect_increase(self, redis_config: Dict[str, Any]) -> None:
        """Test when task count increases by wrong amount."""
        monitor = CeleryTaskMonitor(redis_config)
        
        initial_count = 3
        wrong_final_count = 6  # Increased by 3 instead of 1
        expected_increase = 1
        
        with patch.object(monitor, 'get_current_task_count', side_effect=[initial_count, wrong_final_count]):
            result = monitor.verify_task_queued(initial_count, expected_increase, timeout=5)
            
        assert result is False

    def test_wait_for_task_completion_success(self, redis_config: Dict[str, Any]) -> None:
        """Test successful waiting for task completion."""
        monitor = CeleryTaskMonitor(redis_config)
        
        # Simulate tasks finishing: 3 -> 2 -> 1 -> 0
        task_counts = [3, 2, 1, 0]
        
        with patch.object(monitor, 'get_current_task_count', side_effect=task_counts):
            with patch('time.sleep'):  # Speed up the test
                monitor.wait_for_task_completion(timeout=10)
                
        # Should succeed without raising an exception

    def test_wait_for_task_completion_timeout(self, redis_config: Dict[str, Any]) -> None:
        """Test timeout when tasks don't complete within expected time."""
        monitor = CeleryTaskMonitor(redis_config)
        
        # Tasks never finish
        with patch.object(monitor, 'get_current_task_count', return_value=2):
            with patch('time.sleep'):  # Speed up the test
                with pytest.raises(AssertionError, match="Tasks did not complete within .* seconds"):
                    monitor.wait_for_task_completion(timeout=1)

    def test_wait_for_task_completion_with_project_status_check(
        self, 
        redis_config: Dict[str, Any]
    ) -> None:
        """Test task completion verification with project status checking."""
        monitor = CeleryTaskMonitor(redis_config)
        project_id = "test-project-123"
        service_urls = {"project_management": "http://localhost:8003"}
        auth_headers = {"Authorization": "Bearer test-token"}
        
        # Mock successful completion scenario
        with patch.object(monitor, 'get_current_task_count', return_value=0):
            with patch('requests.get') as mock_get:
                mock_response = Mock()
                mock_response.status_code = TestConstants.HTTP_OK
                mock_response.json.return_value = {"status": TestConstants.PROJECT_STATUS_ACTIVE}
                mock_get.return_value = mock_response
                
                monitor.wait_for_task_completion_with_project_status(
                    project_id=project_id,
                    service_urls=service_urls,
                    auth_headers=auth_headers,
                    timeout=10
                )
                
        # Verify project status was checked
        mock_get.assert_called_once()

    def test_wait_for_task_completion_redis_error_handling(self, redis_config: Dict[str, Any]) -> None:
        """Test proper handling of Redis errors during task completion wait."""
        monitor = CeleryTaskMonitor(redis_config)
        
        with patch.object(monitor, 'get_current_task_count', side_effect=redis.RedisError("Connection lost")):
            with patch('time.sleep'):
                with patch('builtins.print') as mock_print:  # Capture warning prints
                    with pytest.raises(AssertionError):  # Should timeout due to errors
                        monitor.wait_for_task_completion(timeout=1)
                        
        # Should have printed warning about Redis error
        mock_print.assert_called() 