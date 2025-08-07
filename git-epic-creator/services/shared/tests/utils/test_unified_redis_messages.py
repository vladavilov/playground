"""
Tests for unified Redis message classes that replace duplicated message types.
Tests for TaskRequestMessage and ProjectProgressMessage.
"""

import pytest
from unittest.mock import AsyncMock
from uuid import uuid4
from datetime import datetime

from utils.unified_redis_messages import (
    TaskRequestMessage,
    ProjectProgressMessage
)


class TestTaskRequestMessage:
    """Test the unified TaskRequestMessage class."""
    
    def test_task_request_message_creation(self):
        """Test creating a TaskRequestMessage with all required fields."""
        message_id = uuid4()
        timestamp = datetime.now()
        project_id = uuid4()
        correlation_id = uuid4()
        
        message = TaskRequestMessage(
            message_id=message_id,
            timestamp=timestamp,
            task_type="process_project_documents",
            project_id=project_id,
            correlation_id=correlation_id,
            parameters={"key": "value"}
        )
        
        assert message.message_id == message_id
        assert message.timestamp == timestamp
        assert message.task_type == "process_project_documents"
        assert message.project_id == project_id
        assert message.correlation_id == correlation_id
        assert message.parameters == {"key": "value"}
    
    def test_task_request_message_to_dict(self):
        """Test serializing TaskRequestMessage to dictionary."""
        message_id = uuid4()
        timestamp = datetime.now()
        project_id = uuid4()
        correlation_id = uuid4()
        
        message = TaskRequestMessage(
            message_id=message_id,
            timestamp=timestamp,
            task_type="process_project_documents",
            project_id=project_id,
            correlation_id=correlation_id,
            parameters={"test": "data"}
        )
        
        result = message.to_dict()
        
        assert result["message_id"] == str(message_id)
        assert result["timestamp"] == timestamp.isoformat()
        assert result["task_type"] == "process_project_documents"
        assert result["project_id"] == str(project_id)
        assert result["correlation_id"] == str(correlation_id)
        assert result["parameters"] == {"test": "data"}
    
    def test_task_request_message_from_dict(self):
        """Test deserializing TaskRequestMessage from dictionary."""
        message_id = uuid4()
        timestamp = datetime.now()
        project_id = uuid4()
        correlation_id = uuid4()
        
        data = {
            "message_id": str(message_id),
            "timestamp": timestamp.isoformat(),
            "task_type": "process_project_documents",
            "project_id": str(project_id),
            "correlation_id": str(correlation_id),
            "parameters": {"key": "value"}
        }
        
        message = TaskRequestMessage.from_dict(data)
        
        assert message.message_id == message_id
        assert message.timestamp == timestamp
        assert message.task_type == "process_project_documents"
        assert message.project_id == project_id
        assert message.correlation_id == correlation_id
        assert message.parameters == {"key": "value"}
    
    def test_task_request_message_empty_parameters(self):
        """Test TaskRequestMessage with empty parameters."""
        message = TaskRequestMessage(
            message_id=uuid4(),
            timestamp=datetime.now(),
            task_type="test_task",
            project_id=uuid4(),
            correlation_id=uuid4(),
            parameters={}
        )
        
        result = message.to_dict()
        assert result["parameters"] == {}


class TestProjectProgressMessage:
    """Test the unified ProjectProgressMessage class."""
    
    def test_project_progress_message_creation(self):
        """Test creating a ProjectProgressMessage with all fields."""
        message_id = uuid4()
        timestamp = datetime.now()
        project_id = uuid4()
        
        message = ProjectProgressMessage(
            message_id=message_id,
            timestamp=timestamp,
            project_id=project_id,
            status="processing",
            processed_count=5,
            total_count=10,
            processed_pct=50.0
        )
        
        assert message.message_id == message_id
        assert message.timestamp == timestamp
        assert message.project_id == project_id
        assert message.status == "processing"
        assert message.processed_count == 5
        assert message.total_count == 10
        assert message.processed_pct == 50.0
    
    def test_project_progress_message_optional_fields(self):
        """Test ProjectProgressMessage with optional fields as None."""
        message = ProjectProgressMessage(
            message_id=uuid4(),
            timestamp=datetime.now(),
            project_id=uuid4(),
            status="started",
            processed_count=None,
            total_count=None,
            processed_pct=None
        )
        
        result = message.to_dict()
        assert result["processed_count"] is None
        assert result["total_count"] is None
        assert result["processed_pct"] is None
    
    def test_project_progress_message_from_dict(self):
        """Test deserializing ProjectProgressMessage from dictionary."""
        message_id = uuid4()
        timestamp = datetime.now()
        project_id = uuid4()
        
        data = {
            "message_id": str(message_id),
            "timestamp": timestamp.isoformat(),
            "project_id": str(project_id),
            "status": "completed",
            "processed_count": 10,
            "total_count": 10,
            "processed_pct": 100.0
        }
        
        message = ProjectProgressMessage.from_dict(data)
        
        assert message.message_id == message_id
        assert message.timestamp == timestamp
        assert message.project_id == project_id
        assert message.status == "completed"
        assert message.processed_count == 10
        assert message.total_count == 10
        assert message.processed_pct == 100.0 
