"""
Neo4j drift search and retrieval service end-to-end tests.

Tests the complete retrieval pipeline including:
- Neo4j graph data seeding
- Query execution
- Response validation
- DRIFT tree structure validation
"""

from __future__ import annotations

import pytest

from shared_utils import HTTPUtils
from services.validators import ContentValidators
from config import TestConstants


def test_retrieval_service(
    neo4j_driver,
    target_db_name,
    cyphers_path,
    service_urls,
    auth_headers,
    services_ready,
    wa,
):
    """
    Test Neo4j retrieval service with comprehensive response validation.
    
    This test:
    1. Seeds Neo4j with test data
    2. Sends retrieval query
    3. Validates response structure and content quality
    4. Validates embedded DRIFT JSON tree structure
    
    Args:
        neo4j_driver: Neo4j driver fixture
        target_db_name: Target Neo4j database name
        cyphers_path: Path to cypher script for seeding
        service_urls: Service URL configuration
        auth_headers: Authentication headers
        services_ready: Service health check fixture
        wa: WorkflowAssertions facade
    """
    # Seed Neo4j graph for deterministic retrieval behavior
    wa.load_cypher_script(neo4j_driver, target_db_name, cyphers_path)

    # Ensure retrieval service is healthy
    assert HTTPUtils.wait_for_service_health(service_urls['neo4j_retrieval']), (
        "neo4j_retrieval service is not healthy"
    )

    # Test query and expected project
    question = "what are the main components of the bridge?"
    project_id = "11111111-1111-1111-1111-111111111111"  # Fixed ID from cypher script

    # Execute retrieval query
    resp = HTTPUtils.make_request_with_retry(
        method="POST",
        url=service_urls['neo4j_retrieval'].rstrip('/') + "/retrieve",
        timeout=TestConstants.DEFAULT_TIMEOUT,
        headers=auth_headers,
        json_data={"query": question, "project_id": project_id},
    )

    # Validate HTTP response
    assert resp.status_code == 200, (
        f"retrieval status {resp.status_code}, body={resp.text[:200]}"
    )

    # Parse JSON response
    try:
        body = resp.json()
    except Exception:
        pytest.fail(f"Retrieval service returned non-JSON: {resp.text[:200]}")

    # Validate response structure (top-level keys)
    assert "final_answer" in body, "Response missing 'final_answer' field"
    assert "key_facts" in body, "Response missing 'key_facts' field"
    assert "residual_uncertainty" in body, "Response missing 'residual_uncertainty' field"

    # Comprehensive content quality validation (delegates to ContentValidators)
    ContentValidators.validate_retrieval_response_quality(body)
    
    # Validate embedded DRIFT JSON tree structure (optional deep validation)
    ContentValidators.validate_drift_json_tree(body)
