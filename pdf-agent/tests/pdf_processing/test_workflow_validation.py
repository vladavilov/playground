import pytest
import json
from unittest.mock import patch
from src.pdf_processing.workflow import PDFExtractionWorkflow

# Sample property definitions with validation rules for testing
SAMPLE_PROPERTIES = [
    {"name": "instrument_type", "validation": {"required": True, "enum": ["Stock", "Bond"]}},
    {"name": "isin_code", "validation": {"required": True, "regex": "^[A-Z]{2}[A-Z0-9]{9}[0-9]$"}},
    {"name": "issuer", "validation": {"required": True}},
    {"name": "country_code", "validation": {"required": False, "regex": "^[A-Z]{2}$"}}
]

@pytest.fixture
def workflow():
    """Provides a PDFExtractionWorkflow instance for testing."""
    # Mock get_settings to avoid dependency on environment variables for tests
    with patch('src.pdf_processing.workflow.get_settings') as mock_get_settings:
        # Mock the return value of get_settings if needed by the workflow's __init__
        # For now, a simple mock object might suffice if settings are not used heavily in __init__
        # that is not relevant to the validation logic.
        from unittest.mock import MagicMock
        mock_settings = MagicMock()
        mock_settings.property_groups = []
        mock_settings.TOP_K = 3
        mock_settings.AZURE_OPENAI_API_KEY = "test"
        mock_settings.AZURE_OPENAI_ENDPOINT = "test"
        mock_settings.AZURE_OPENAI_EMBEDDINGS_DEPLOYMENT_NAME = "test"
        mock_settings.AZURE_OPENAI_LLM_DEPLOYMENT_NAME = "test"
        mock_get_settings.return_value = mock_settings
        instance = PDFExtractionWorkflow()
    return instance

def test_valid_json_and_rules(workflow):
    """
    Tests successful validation when JSON is valid and all rules are met.
    """
    valid_data = {
        "instrument_type": "Stock",
        "isin_code": "US0378331005",
        "issuer": "Apple Inc."
    }
    json_string = json.dumps(valid_data)
    
    parsed_data, errors = workflow._validate_agent_output(json_string, SAMPLE_PROPERTIES)
    
    assert parsed_data == valid_data
    assert not errors, f"Expected no validation errors, but got: {errors}"

def test_invalid_json_structure(workflow):
    """
    Tests failure when the string is not valid JSON.
    """
    invalid_json_string = '{"key": "value",}' # trailing comma
    
    parsed_data, errors = workflow._validate_agent_output(invalid_json_string, SAMPLE_PROPERTIES)
    
    assert parsed_data is None
    assert len(errors) == 1
    assert "Invalid JSON structure" in errors[0]

def test_missing_required_property(workflow):
    """
    Tests failure when a required property is missing.
    """
    data = {
        "instrument_type": "Bond",
        # "isin_code" is missing
        "issuer": "Some Government"
    }
    json_string = json.dumps(data)
    
    parsed_data, errors = workflow._validate_agent_output(json_string, SAMPLE_PROPERTIES)
    
    assert parsed_data is not None
    assert len(errors) == 1
    assert "Missing required property: 'isin_code'" in errors[0]

def test_unexpected_property(workflow):
    """
    Tests failure when an unexpected property is included in the output.
    """
    data = {
        "instrument_type": "Stock",
        "isin_code": "US0378331005",
        "issuer": "Apple Inc.",
        "extra_field": "some value"
    }
    json_string = json.dumps(data)
    
    parsed_data, errors = workflow._validate_agent_output(json_string, SAMPLE_PROPERTIES)
    
    assert parsed_data is not None
    assert len(errors) == 1
    assert "Unexpected property: 'extra_field'" in errors[0]

def test_enum_validation_failure(workflow):
    """
    Tests failure when a property's value is not in its defined enum.
    """
    data = {
        "instrument_type": "Mutual Fund", # Not in ["Stock", "Bond"]
        "isin_code": "US0378331005",
        "issuer": "Apple Inc."
    }
    json_string = json.dumps(data)
    
    parsed_data, errors = workflow._validate_agent_output(json_string, SAMPLE_PROPERTIES)
    
    assert parsed_data is not None
    assert len(errors) == 1
    assert "Value 'Mutual Fund' for property 'instrument_type' is not in the allowed enum" in errors[0]

def test_regex_validation_failure(workflow):
    """
    Tests failure when a property's value does not match its defined regex.
    """
    data = {
        "instrument_type": "Stock",
        "isin_code": "INVALID-CODE", # Does not match ISIN regex
        "issuer": "Apple Inc."
    }
    json_string = json.dumps(data)
    
    parsed_data, errors = workflow._validate_agent_output(json_string, SAMPLE_PROPERTIES)
    
    assert parsed_data is not None
    assert len(errors) == 1
    assert "Value 'INVALID-CODE' for property 'isin_code' does not match regex pattern" in errors[0]

def test_multiple_validation_errors(workflow):
    """
    Tests that multiple validation errors are reported correctly.
    """
    data = {
        # isin_code is missing
        "instrument_type": "Crypto", # Enum violation
        "issuer": "Apple Inc.",
        "another_extra": "field" # Unexpected property
    }
    json_string = json.dumps(data)
    
    parsed_data, errors = workflow._validate_agent_output(json_string, SAMPLE_PROPERTIES)
    
    assert parsed_data is not None
    assert len(errors) == 3
    # Order isn't guaranteed, so check for presence of each error
    assert any("Missing required property: 'isin_code'" in e for e in errors)
    assert any("Unexpected property: 'another_extra'" in e for e in errors)
    assert any("not in the allowed enum" in e for e in errors)

def test_optional_property_validation_success(workflow):
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
    
    parsed_data, errors = workflow._validate_agent_output(json_string, SAMPLE_PROPERTIES)
    
    assert parsed_data is not None
    assert not errors

def test_optional_property_validation_failure(workflow):
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
    
    parsed_data, errors = workflow._validate_agent_output(json_string, SAMPLE_PROPERTIES)
    
    assert parsed_data is not None
    assert len(errors) == 1
    assert "Value 'USA' for property 'country_code' does not match regex pattern" in errors[0] 