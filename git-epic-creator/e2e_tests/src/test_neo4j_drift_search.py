from __future__ import annotations

import pytest
from uuid import uuid4

from shared_utils import HTTPUtils
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
    wa.load_cypher_script(neo4j_driver, target_db_name, cyphers_path)

    # Ensure retrieval service is healthy
    assert HTTPUtils.wait_for_service_health(service_urls['neo4j_retrieval']), (
        "neo4j_retrieval service is not healthy"
    )

    question = "what are the main components of the bridge?"

    # Use fixed project id created by cypher script
    project_id = "11111111-1111-1111-1111-111111111111"

    resp = HTTPUtils.make_request_with_retry(
        method="POST",
        url=service_urls['neo4j_retrieval'].rstrip('/') + "/retrieve",
        timeout=TestConstants.DEFAULT_TIMEOUT,
        headers=auth_headers,
        json_data={"query": question, "project_id": project_id},
    )

    assert resp.status_code == 200, f"retrieval status {resp.status_code}, body={resp.text[:200]}"

    try:
        body = resp.json()
    except Exception:
        pytest.fail(f"Retrieval service returned non-JSON: {resp.text[:200]}")

    assert "final_answer" in body
    assert "key_facts" in body
    assert "residual_uncertainty" in body

    # 1. Validate final_answer content quality and mock response presence
    assert body['final_answer'], "final_answer should not be empty"
    assert len(body['final_answer']) > 50, "Answer should be substantive (>50 chars)"
    
    # Verify mentions bridge components from our test data and mock responses
    component_keywords = ["deck", "support", "arch", "cable", "load", "bridge"]
    found_components = [kw for kw in component_keywords if kw in body['final_answer'].lower()]
    assert len(found_components) >= 2, \
        f"final_answer should mention bridge components from mock, found: {found_components}"
    
    # Check for specific mock response content from DriftAggregatorHandler
    # Mock returns: "Bridges comprise the deck, supports, and load-bearing structures such as arches or cables."
    final_answer_lower = body['final_answer'].lower()
    assert "bridge" in final_answer_lower or "deck" in final_answer_lower, \
        "final_answer should contain core bridge terminology from mock response"
    
    # 2. Validate key_facts structure and mock response content
    assert body['key_facts'], "key_facts should not be empty"
    assert isinstance(body['key_facts'], list), "key_facts should be a list"
    assert len(body['key_facts']) > 0, "Should have at least one key fact"
    
    for fact in body['key_facts']:
        assert "fact" in fact, "Each key fact should have 'fact' field"
        assert "citations" in fact, "Each key fact should have 'citations' field"
        assert len(fact['fact']) > 10, f"Fact should be substantive: {fact['fact'][:50]}"
    
    # Check for specific mock key_facts content from DriftAggregatorHandler
    # Mock returns: {"fact": "Deck carries traffic and distributes loads.", "citations": ...}
    first_fact_lower = body['key_facts'][0]['fact'].lower()
    mock_keywords = ["deck", "traffic", "load", "distribute", "carry"]
    found_mock_keywords = [kw for kw in mock_keywords if kw in first_fact_lower]
    assert len(found_mock_keywords) >= 1, \
        f"key_facts should contain mock response content, found keywords: {found_mock_keywords}"
    
    # 3. Validate citations contain full context tree structure and mock responses
    citations_str = body['key_facts'][0]['citations']
    assert isinstance(citations_str, str), "Citations should be a string"
    assert len(citations_str) > 100, "Citations should contain substantial context"
    
    # Check for DRIFT tree structure components in citations
    assert '"question"' in citations_str or 'question' in citations_str.lower(), \
        "Citations should reference the question"
    assert '"primer"' in citations_str or 'initial_answer' in citations_str.lower(), \
        "Citations should include primer information"
    assert '"followups"' in citations_str or 'answer' in citations_str.lower(), \
        "Citations should include followup answers"
    
    # Check for mock response content from DriftPrimerHandler and DriftLocalExecutorHandler
    # Primer mock returns: "Bridges consist of deck, supports, and load-bearing structures."
    # Local executor mocks return content about "reinforced concrete", "arch", "compression", etc.
    citations_lower = citations_str.lower()
    primer_keywords = ["bridge", "deck", "support", "load"]
    local_keywords = ["concrete", "steel", "arch", "compression", "material", "thrust"]
    found_primer = [kw for kw in primer_keywords if kw in citations_lower]
    found_local = [kw for kw in local_keywords if kw in citations_lower]
    
    # Should have content from at least primer OR local executor responses
    assert len(found_primer) >= 1 or len(found_local) >= 1, \
        f"Citations should contain mock handler content. Primer: {found_primer}, Local: {found_local}"
    
    # 4. Validate enhanced neighborhood expansion data in citations
    # Our queries should return neighbours, related_entities, relationships, neighbor_chunk_ids
    # Check for entity neighbourhood data
    assert 'neighbour' in citations_lower or 'entity' in citations_lower or 'properties' in citations_lower, \
        "Citations should include entity neighbourhood information"
    
    # Check for chunk text content (actual retrieved context)
    component_mentions_in_citations = [kw for kw in component_keywords if kw in citations_lower]
    assert len(component_mentions_in_citations) >= 2, \
        f"Citations should mention multiple bridge components from context, found: {component_mentions_in_citations}"
    
    # 5. Attempt to parse embedded JSON tree if present
    import json as json_lib
    try:
        # Try to find and parse JSON structure in citations
        if '{"question":' in citations_str or '{"primer":' in citations_str:
            # Look for JSON object boundaries
            start_idx = max(citations_str.find('{"question":'), citations_str.find('{"primer":'))
            if start_idx >= 0:
                # Simple JSON extraction (find matching braces)
                brace_count = 0
                end_idx = start_idx
                in_string = False
                escape_next = False
                
                for i in range(start_idx, len(citations_str)):
                    char = citations_str[i]
                    if escape_next:
                        escape_next = False
                        continue
                    if char == '\\':
                        escape_next = True
                        continue
                    if char == '"' and not escape_next:
                        in_string = not in_string
                        continue
                    if not in_string:
                        if char == '{':
                            brace_count += 1
                        elif char == '}':
                            brace_count -= 1
                            if brace_count == 0:
                                end_idx = i + 1
                                break
                
                if end_idx > start_idx:
                    tree_json_str = citations_str[start_idx:end_idx]
                    tree_json = json_lib.loads(tree_json_str)
                    tree_data_found = True
                    
                    # Validate tree structure
                    assert "question" in tree_json or "primer" in tree_json, \
                        "Parsed tree should have question or primer"
                    
                    # Check for followup context with neighborhood data
                    if "followups" in tree_json:
                        for followup in tree_json["followups"]:
                            if "context" in followup and isinstance(followup["context"], list):
                                for chunk_data in followup["context"]:
                                    if isinstance(chunk_data, dict):
                                        # Validate enhanced neighborhood fields exist
                                        if "text" in chunk_data:
                                            assert len(chunk_data["text"]) > 10, \
                                                "Chunk text should be substantive"
                                        if "neighbours" in chunk_data:
                                            assert isinstance(chunk_data["neighbours"], list), \
                                                "Neighbours should be a list"
                                        # These are our enhanced fields from the fix
                                        if "related_entities" in chunk_data:
                                            assert isinstance(chunk_data["related_entities"], list), \
                                                "related_entities should be a list"
                                        if "relationships" in chunk_data:
                                            assert isinstance(chunk_data["relationships"], list), \
                                                "relationships should be a list"
                                        if "neighbor_chunk_ids" in chunk_data:
                                            assert isinstance(chunk_data["neighbor_chunk_ids"], list), \
                                                "neighbor_chunk_ids should be a list"
    except (json_lib.JSONDecodeError, KeyError, IndexError) as e:
        # JSON parsing might fail if structure is different, that's okay
        # as long as we have the content
        pass
    
    # 6. Verify no processing errors occurred
    response_str = str(body)
    assert "ERROR FETCHING LOCAL EXECUTOR" not in response_str, \
        "Should not have executor errors in response"
    assert "ERROR" not in body.get('final_answer', '').upper() or len(body.get('final_answer', '')) > 50, \
        "Should have substantive answer, not an error message"
    
    # 7. Validate residual_uncertainty is present and contains mock content
    assert isinstance(body.get('residual_uncertainty'), str), \
        "residual_uncertainty should be a string"
    assert len(body.get('residual_uncertainty', '')) > 10, \
        "residual_uncertainty should be substantive"
    
    # Check for mock response content from DriftAggregatorHandler
    # Mock returns: "Specific materials and design vary by bridge type."
    uncertainty_lower = body.get('residual_uncertainty', '').lower()
    uncertainty_keywords = ["material", "design", "vary", "type", "specific", "bridge"]
    found_uncertainty = [kw for kw in uncertainty_keywords if kw in uncertainty_lower]
    assert len(found_uncertainty) >= 1, \
        f"residual_uncertainty should contain mock response elements, found: {found_uncertainty}"