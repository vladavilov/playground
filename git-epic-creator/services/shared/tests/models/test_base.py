"""
Tests for base model.
"""

from sqlalchemy.dialects.postgresql import UUID
from models.project_db import BaseModel

def test_base_model_attributes():
    """Test that BaseModel has the expected attributes."""
    assert BaseModel.__abstract__ is True
    assert hasattr(BaseModel, "id")
    assert hasattr(BaseModel, "created_at")
    assert hasattr(BaseModel, "updated_at")
    assert hasattr(BaseModel, "created_by")
    assert isinstance(BaseModel.id.type, UUID)
    assert BaseModel.created_by.nullable is False
