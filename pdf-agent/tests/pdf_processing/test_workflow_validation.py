import pytest
import json
from unittest.mock import patch
import os
from src.pdf_processing.workflow import PDFExtractionWorkflow

# Set dummy env vars to allow instantiation of the workflow object
# The tested method `_validate_agent_output` does not use them.
os.environ["AZURE_OPENAI_API_KEY"] = "test_key"
os.environ["AZURE_OPENAI_CHAT_ENDPOINT"] = "https://test-chat.openai.azure.com/"

# Sample property definitions with validation rules for testing
SAMPLE_PROPERTIES = [
    {"name": "instrument_type", "validation": {"required": True, "enum": ["Stock", "Bond"]}},
    {"name": "isin_code", "validation": {"required": True, "regex": "^[A-Z]{2}[A-Z0-9]{9}[0-9]$"}},
    {"name": "issuer", "validation": {"required": True}},
    {"name": "country_code", "validation": {"required": False, "regex": "^[A-Z]{2}$"}}
]

@pytest.fixture
def workflow_instance():
    """Provides a direct instance of PDFExtractionWorkflow for testing its methods."""
    return PDFExtractionWorkflow()

def test_valid_json_and_rules(workflow_instance):
    """
    Tests successful validation when JSON is valid and all rules are met.
    """
    valid_data = {
        "instrument_type": "Stock",
        "isin_code": "US0378331005",
        "issuer": "Apple Inc."
    }
    json_string = json.dumps(valid_data)
    
    parsed_data, errors = workflow_instance._validate_agent_output(json_string, SAMPLE_PROPERTIES)
    
    assert parsed_data == valid_data
    assert not errors, f"Expected no validation errors, but got: {errors}"

def test_invalid_json_structure(workflow_instance):
    """
    Tests failure when the string is not valid JSON.
    """
    invalid_json_string = '{"key": "value",}' # trailing comma
    
    parsed_data, errors = workflow_instance._validate_agent_output(invalid_json_string, SAMPLE_PROPERTIES)
    
    assert parsed_data is None
    assert len(errors) == 1
    assert "Invalid JSON structure" in errors[0]

def test_missing_required_property(workflow_instance):
    """
    Tests failure when a required property is missing.
    """
    data = {
        "instrument_type": "Bond",
        # "isin_code" is missing
        "issuer": "Some Government"
    }
    json_string = json.dumps(data)
    
    parsed_data, errors = workflow_instance._validate_agent_output(json_string, SAMPLE_PROPERTIES)
    
    assert parsed_data is not None
    assert len(errors) == 1
    assert "Missing required property: 'isin_code'" in errors[0]

def test_unexpected_property_is_removed(workflow_instance):
    """
    Tests that an unexpected property is silently removed without causing a
    validation error.
    """
    data = {
        "instrument_type": "Stock",
        "isin_code": "US0378331005",
        "issuer": "Apple Inc.",
        "extra_field": "some value"
    }
    json_string = json.dumps(data)
    
    parsed_data, errors = workflow_instance._validate_agent_output(json_string, SAMPLE_PROPERTIES)
    
    # Check that the extra field was removed
    expected_data = data.copy()
    del expected_data["extra_field"]
    
    assert parsed_data == expected_data
    assert not errors, "Expected no validation errors for an unexpected property."

def test_enum_validation_failure(workflow_instance):
    """
    Tests failure when a property's value is not in its defined enum.
    """
    data = {
        "instrument_type": "Mutual Fund", # Not in ["Stock", "Bond"]
        "isin_code": "US0378331005",
        "issuer": "Apple Inc."
    }
    json_string = json.dumps(data)
    
    parsed_data, errors = workflow_instance._validate_agent_output(json_string, SAMPLE_PROPERTIES)
    
    assert parsed_data is not None
    assert len(errors) == 1
    assert "Value 'Mutual Fund' for property 'instrument_type' is not in the allowed enum" in errors[0]

def test_regex_validation_failure(workflow_instance):
    """
    Tests failure when a property's value does not match its defined regex.
    """
    data = {
        "instrument_type": "Stock",
        "isin_code": "INVALID-CODE", # Does not match ISIN regex
        "issuer": "Apple Inc."
    }
    json_string = json.dumps(data)
    
    parsed_data, errors = workflow_instance._validate_agent_output(json_string, SAMPLE_PROPERTIES)
    
    assert parsed_data is not None
    assert len(errors) == 1
    assert "Value 'INVALID-CODE' for property 'isin_code' does not match regex pattern" in errors[0]

def test_multiple_validation_errors(workflow_instance):
    """
    Tests that multiple validation errors are reported correctly.
    """
    data = {
        # isin_code is missing
        "instrument_type": "Crypto", # Enum violation
        "issuer": "Apple Inc.",
        "another_extra": "field"
    }
    json_string = json.dumps(data)
    
    parsed_data, errors = workflow_instance._validate_agent_output(json_string, SAMPLE_PROPERTIES)
    
    assert parsed_data is not None
    assert len(errors) == 2
    # Order isn't guaranteed, so check for presence of each error
    assert any("Missing required property: 'isin_code'" in e for e in errors)
    assert any("not in the allowed enum" in e for e in errors)
    assert "another_extra" not in parsed_data

def test_optional_property_validation_success(workflow_instance):
    """
    Tests that an optional property passes validation when present and correct.
    """
    data = {
        "instrument_type": "Stock",
        "isin_code": "US0378331005",
        "issuer": "Apple Inc.",
        "country_code": "US"
    }
    json_string = json.dumps(data)
    
    parsed_data, errors = workflow_instance._validate_agent_output(json_string, SAMPLE_PROPERTIES)
    
    assert parsed_data is not None
    assert not errors

def test_optional_property_validation_failure(workflow_instance):
    """
    Tests that an optional property fails validation when present but incorrect.
    """
    data = {
        "instrument_type": "Stock",
        "isin_code": "US0378331005",
        "issuer": "Apple Inc.",
        "country_code": "USA" # Fails regex
    }
    json_string = json.dumps(data)
    
    parsed_data, errors = workflow_instance._validate_agent_output(json_string, SAMPLE_PROPERTIES)
    
    assert parsed_data is not None
    assert len(errors) == 1
    assert "Value 'USA' for property 'country_code' does not match regex pattern" in errors[0] 