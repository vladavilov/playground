"""
Tests for RedisTestMonitor functionality.

This module tests the enhanced Celery task monitoring capabilities
for ensuring proper task queue validation during document processing.
"""

import pytest
import redis
import json
from unittest.mock import Mock, patch, MagicMock
from typing import Dict, Any

from services.redis_test_monitor import RedisTestMonitor
from config import TestConstants


@pytest.fixture
def redis_config() -> Dict[str, Any]:
    """Mock Redis configuration for unit tests."""
    return {
        'host': 'localhost',
        'port': 6379,
        'db': 0,
        'decode_responses': True
    }


class TestRedisTestMonitor:
    """Test suite for RedisTestMonitor class."""

    def test_celery_task_monitor_initialization(self, redis_config: Dict[str, Any]) -> None:
        """Test RedisTestMonitor can be initialized with Redis config."""
        monitor = RedisTestMonitor(redis_config)
        
        assert monitor is not None
        assert monitor.redis_client is not None
        assert monitor.queue_name == 'celery'

    def test_get_current_task_count_success(self, redis_config: Dict[str, Any]) -> None:
        """Test getting current task count from Redis."""
        monitor = RedisTestMonitor(redis_config)
        
        with patch.object(monitor.redis_client, 'llen', return_value=5) as mock_llen:
            task_count = monitor.get_current_task_count()
            
        assert task_count == 5
        mock_llen.assert_called_once_with('celery')

    def test_get_current_task_count_redis_error(self, redis_config: Dict[str, Any]) -> None:
        """Test handling Redis connection errors when getting task count."""
        monitor = RedisTestMonitor(redis_config)
        
        with patch.object(monitor.redis_client, 'llen', side_effect=redis.RedisError("Connection failed")):
            with pytest.raises(redis.RedisError):
                monitor.get_current_task_count()

    def test_verify_task_queued_success(self, redis_config: Dict[str, Any]) -> None:
        """Test successful verification that tasks were queued."""
        monitor = RedisTestMonitor(redis_config)
        
        initial_count = 3
        final_count = 4
        expected_increase = 1
        
        with patch.object(monitor, 'get_current_task_count', side_effect=[initial_count, final_count]):
            result = monitor.verify_task_queued(initial_count, expected_increase, timeout=5)
            
        assert result is True

    def test_verify_task_queued_timeout(self, redis_config: Dict[str, Any]) -> None:
        """Test timeout when task is not queued within expected time."""
        monitor = RedisTestMonitor(redis_config)
        
        initial_count = 3
        # Task count doesn't increase
        with patch.object(monitor, 'get_current_task_count', return_value=initial_count):
            with patch('time.sleep'):  # Speed up the test
                result = monitor.verify_task_queued(initial_count, expected_increase=1, timeout=1)
                
        assert result is False

    def test_verify_task_queued_incorrect_increase(self, redis_config: Dict[str, Any]) -> None:
        """Test when task count increases by wrong amount."""
        monitor = RedisTestMonitor(redis_config)
        
        initial_count = 3
        wrong_final_count = 6  # Increased by 3 instead of 1
        expected_increase = 1
        
        with patch.object(monitor, 'get_current_task_count', side_effect=[initial_count, wrong_final_count]):
            result = monitor.verify_task_queued(initial_count, expected_increase, timeout=5)
            
        assert result is False

    def test_wait_for_task_completion_success(self, redis_config: Dict[str, Any]) -> None:
        """Test successful waiting for task completion."""
        monitor = RedisTestMonitor(redis_config)
        
        # Simulate tasks finishing: 3 -> 2 -> 1 -> 0
        task_counts = [3, 2, 1, 0]
        
        with patch.object(monitor, 'get_current_task_count', side_effect=task_counts):
            with patch('time.sleep'):  # Speed up the test
                monitor.wait_for_task_completion(timeout=10)
                
        # Should succeed without raising an exception

    def test_wait_for_task_completion_timeout(self, redis_config: Dict[str, Any]) -> None:
        """Test timeout when tasks don't complete within expected time."""
        monitor = RedisTestMonitor(redis_config)
        
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
        monitor = RedisTestMonitor(redis_config)
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
        monitor = RedisTestMonitor(redis_config)
        
        with patch.object(monitor, 'get_current_task_count', side_effect=redis.RedisError("Connection lost")):
            with patch('time.sleep'):
                with pytest.raises(AssertionError):
                    monitor.wait_for_task_completion(timeout=1)


class TestRedisTestMonitorUIProgress:
    """Test suite for UI progress monitoring functionality in RedisTestMonitor."""

    def test_start_ui_progress_monitoring_success(self, redis_config: Dict[str, Any]) -> None:
        """Test successful start of UI progress monitoring."""
        monitor = RedisTestMonitor(redis_config)
        project_id = "test-project-123"
        
        # Mock pubsub and subscription
        mock_pubsub = MagicMock()
        mock_message = {'type': 'subscribe', 'pattern': None, 'channel': b'ui:project_progress', 'data': 1}
        mock_pubsub.get_message.return_value = mock_message
        
        with patch.object(monitor.redis_client, 'pubsub', return_value=mock_pubsub):
            monitor.start_ui_progress_monitoring(project_id)
            
        assert monitor.ui_monitoring_active is True
        assert monitor.ui_project_id == project_id
        assert monitor.ui_messages_received == []
        mock_pubsub.subscribe.assert_called_once_with("ui:project_progress")

    def test_start_ui_progress_monitoring_subscription_failure(self, redis_config: Dict[str, Any]) -> None:
        """Test handling of subscription failure during UI monitoring start."""
        monitor = RedisTestMonitor(redis_config)
        project_id = "test-project-123"
        
        # Mock failed subscription
        mock_pubsub = MagicMock()
        mock_pubsub.get_message.return_value = None  # No subscription confirmation
        
        with patch.object(monitor.redis_client, 'pubsub', return_value=mock_pubsub):
            with pytest.raises(RuntimeError, match="Failed to subscribe to UI progress channel"):
                monitor.start_ui_progress_monitoring(project_id)

    def test_stop_ui_progress_monitoring(self, redis_config: Dict[str, Any]) -> None:
        """Test stopping UI progress monitoring."""
        monitor = RedisTestMonitor(redis_config)
        project_id = "test-project-123"
        
        # Start monitoring first
        mock_pubsub = MagicMock()
        mock_message = {'type': 'subscribe', 'pattern': None, 'channel': b'ui:project_progress', 'data': 1}
        mock_pubsub.get_message.return_value = mock_message
        
        with patch.object(monitor.redis_client, 'pubsub', return_value=mock_pubsub):
            monitor.start_ui_progress_monitoring(project_id)
            
        # Now stop monitoring
        monitor.stop_ui_progress_monitoring()
        
        assert monitor.ui_monitoring_active is False
        assert monitor.ui_project_id is None
        assert monitor.ui_messages_received == []
        mock_pubsub.close.assert_called_once()

    def test_check_for_ui_messages_success(self, redis_config: Dict[str, Any]) -> None:
        """Test successful checking for UI progress messages."""
        monitor = RedisTestMonitor(redis_config)
        project_id = "test-project-123"
        
        # Set up monitoring state
        monitor.ui_monitoring_active = True
        monitor.ui_project_id = project_id
        monitor.ui_messages_received = []
        
        # Mock pubsub with message that returns once then None
        mock_pubsub = MagicMock()
        test_message_data = {
            'project_id': project_id,
            'message_type': 'project_progress',
            'status': 'processing',
            'progress_pct': 50
        }
        mock_message = {
            'type': 'message',
            'data': json.dumps(test_message_data)
        }
        # Return message once, then None for subsequent calls
        mock_pubsub.get_message.side_effect = [mock_message, None, None, None]
        monitor.pubsub = mock_pubsub
        
        new_messages = monitor.check_for_ui_messages(timeout=0.1)
        
        assert new_messages == 1
        assert len(monitor.ui_messages_received) == 1
        assert monitor.ui_messages_received[0] == test_message_data

    def test_check_for_ui_messages_wrong_project(self, redis_config: Dict[str, Any]) -> None:
        """Test filtering messages for wrong project ID."""
        monitor = RedisTestMonitor(redis_config)
        project_id = "test-project-123"
        wrong_project_id = "wrong-project-456"
        
        # Set up monitoring state
        monitor.ui_monitoring_active = True
        monitor.ui_project_id = project_id
        monitor.ui_messages_received = []
        
        # Mock pubsub with message for wrong project
        mock_pubsub = MagicMock()
        test_message_data = {
            'project_id': wrong_project_id,  # Wrong project ID
            'message_type': 'project_progress',
            'status': 'processing'
        }
        mock_message = {
            'type': 'message',
            'data': json.dumps(test_message_data)
        }
        # Return message once, then None for subsequent calls
        mock_pubsub.get_message.side_effect = [mock_message, None, None, None]
        monitor.pubsub = mock_pubsub
        
        new_messages = monitor.check_for_ui_messages(timeout=0.1)
        
        assert new_messages == 0
        assert len(monitor.ui_messages_received) == 0

    def test_get_ui_messages_count(self, redis_config: Dict[str, Any]) -> None:
        """Test getting UI messages count."""
        monitor = RedisTestMonitor(redis_config)
        
        # Add some mock messages
        monitor.ui_messages_received = [
            {'project_id': 'test', 'message_type': 'project_progress', 'status': 'processing'},
            {'project_id': 'test', 'message_type': 'project_progress', 'status': 'completed'}
        ]
        
        count = monitor.get_ui_messages_count()
        assert count == 2

    def test_get_ui_messages(self, redis_config: Dict[str, Any]) -> None:
        """Test getting UI messages list."""
        monitor = RedisTestMonitor(redis_config)
        
        test_messages = [
            {'project_id': 'test', 'message_type': 'project_progress', 'status': 'processing'},
            {'project_id': 'test', 'message_type': 'project_progress', 'status': 'completed'}
        ]
        monitor.ui_messages_received = test_messages
        
        messages = monitor.get_ui_messages()
        assert messages == test_messages
        # Verify it returns a copy
        assert messages is not monitor.ui_messages_received

    def test_wait_for_ui_messages_success(self, redis_config: Dict[str, Any]) -> None:
        """Test successful waiting for UI messages."""
        monitor = RedisTestMonitor(redis_config)
        monitor.ui_monitoring_active = True
        
        # Mock check_for_ui_messages to simulate messages arriving
        with patch.object(monitor, 'check_for_ui_messages') as mock_check:
            # First call returns 0, second call adds 2 messages
            mock_check.side_effect = [0, 2]
            # Mock the messages being added
            monitor.ui_messages_received = [
                {'project_id': 'test', 'status': 'processing'},
                {'project_id': 'test', 'status': 'completed'}
            ]
            
            with patch('time.sleep'):  # Speed up test
                result = monitor.wait_for_ui_messages(min_messages=2, timeout=5, check_interval=0.1)
                
        assert result is True

    def test_wait_for_ui_messages_timeout(self, redis_config: Dict[str, Any]) -> None:
        """Test timeout when waiting for UI messages."""
        monitor = RedisTestMonitor(redis_config)
        monitor.ui_monitoring_active = True
        
        # Mock check_for_ui_messages to never find enough messages
        with patch.object(monitor, 'check_for_ui_messages', return_value=0):
            with patch('time.sleep'):  # Speed up test
                result = monitor.wait_for_ui_messages(min_messages=2, timeout=1, check_interval=0.1)
                
        assert result is False

    def test_monitor_ui_during_processing_success(self, redis_config: Dict[str, Any]) -> None:
        """Test successful UI monitoring during processing."""
        monitor = RedisTestMonitor(redis_config)
        monitor.ui_monitoring_active = True
        
        # Mock processing check function - complete on first call for simplicity
        processing_complete = Mock(side_effect=[True])  # Complete immediately
        
        # Mock check_for_ui_messages to simulate messages
        with patch.object(monitor, 'check_for_ui_messages') as mock_check:
            # Return 1 message initially, then 0 for final check
            mock_check.side_effect = [1, 0]
            
            with patch('time.sleep'):  # Speed up test
                result = monitor.monitor_ui_during_processing(
                    processing_check_func=processing_complete,
                    timeout=10,
                    check_interval=0.1
                )
                
        assert result is True
        assert processing_complete.call_count == 1

    def test_monitor_ui_during_processing_timeout(self, redis_config: Dict[str, Any]) -> None:
        """Test timeout during UI monitoring while processing."""
        monitor = RedisTestMonitor(redis_config)
        monitor.ui_monitoring_active = True
        
        # Mock processing check function that never completes
        processing_incomplete = Mock(return_value=False)
        
        with patch.object(monitor, 'check_for_ui_messages', return_value=0):
            with patch('time.sleep'):  # Speed up test
                result = monitor.monitor_ui_during_processing(
                    processing_check_func=processing_incomplete,
                    timeout=1,
                    check_interval=0.1
                )
                
        assert result is False

    def test_wait_for_ui_project_progress_message_success(self, redis_config: Dict[str, Any]) -> None:
        """Test successful waiting for specific project progress message."""
        monitor = RedisTestMonitor(redis_config)
        project_id = "test-project-123"
        
        test_message = {
            'project_id': project_id,
            'message_type': 'project_progress',
            'status': 'completed'
        }
        
        # Mock the UI monitoring methods
        with patch.object(monitor, 'start_ui_progress_monitoring') as mock_start:
            with patch.object(monitor, 'stop_ui_progress_monitoring') as mock_stop:
                with patch.object(monitor, 'check_for_ui_messages', return_value=1):
                    monitor.ui_messages_received = [test_message]
                    monitor.ui_monitoring_active = False  # Simulate not already monitoring
                    monitor.ui_project_id = None
                    
                    result = monitor.wait_for_ui_project_progress_message(
                        project_id=project_id,
                        expected_status='completed',
                        timeout=5
                    )
                    
        assert result == test_message
        mock_start.assert_called_once_with(project_id)
        mock_stop.assert_called_once()

    def test_context_manager_support(self, redis_config: Dict[str, Any]) -> None:
        """Test context manager support for RedisTestMonitor."""
        monitor = RedisTestMonitor(redis_config)
        
        with patch.object(monitor, 'stop_ui_progress_monitoring') as mock_stop:
            with monitor:
                pass  # Just test context manager entry/exit
                
        mock_stop.assert_called_once() 