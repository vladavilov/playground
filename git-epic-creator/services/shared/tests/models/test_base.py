"""
Tests for base model.
"""

import uuid
from datetime import datetime
import pytest
from sqlalchemy import Column, String, Integer
from sqlalchemy.dialects.postgresql import UUID
from models.project_db import Base, BaseModel

def test_base_model_attributes():
    """Test that BaseModel has the expected attributes."""
    assert BaseModel.__abstract__ is True
    assert hasattr(BaseModel, "id")
    assert hasattr(BaseModel, "created_at")
    assert hasattr(BaseModel, "updated_at")
    assert hasattr(BaseModel, "created_by")
    assert isinstance(BaseModel.id.type, UUID)
    assert BaseModel.created_by.nullable is False

def test_to_dict_method():
    """Test to_dict method."""
    class ConcreteModel(BaseModel):
        __tablename__ = "concrete_model"
        name = Column(String(255))
        value = Column(Integer)
    
    model = ConcreteModel(
        id=uuid.uuid4(),
        created_by="test_user",
        name="Test",
        value=42
    )
    
    model_dict = model.to_dict()
    
    assert model_dict["id"] == model.id
    assert model_dict["created_by"] == "test_user"
    assert model_dict["name"] == "Test"
    assert model_dict["value"] == 42
    assert "created_at" in model_dict
    assert "updated_at" in model_dict

def test_from_dict_method():
    """Test from_dict method."""
    class TestModel(BaseModel):
        __tablename__ = "test_model"
        name = Column(String(255))
        value = Column(Integer)
    
    model_id = uuid.uuid4()
    model_data = {
        "id": model_id,
        "created_by": "test_user",
        "name": "Test",
        "value": 42,
        "extra_field": "This should be ignored"
    }
    
    model = TestModel.from_dict(model_data)
    
    assert model.id == model_id
    assert model.created_by == "test_user"
    assert model.name == "Test"
    assert model.value == 42
    assert not hasattr(model, "extra_field")