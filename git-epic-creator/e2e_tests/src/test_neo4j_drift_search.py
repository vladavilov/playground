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

    assert "Bridges comprise the deck, supports, and load-bearing structures such as arches or cables." in body['final_answer']

    citations = body['key_facts'][0]['citations']

    assert "citations" in citations
    assert "neighbours" in citations
    assert "4. Types of Bridges:" in citations
    assert "1. Introduction:" in citations
    assert "Clarify deck materials and structural role." in citations
    assert "Explain arch mechanics in load distribution." in citations
    assert "List common deck materials and how they influence load distribution." in citations

    assert "Arches carry deck loads p" in citations
    assert "Bridge decks are commonly built from reinforced" in citations
    assert "Reinforced concrete decks spread loa" not in citations # no answers for follow-ups of the second level

    assert "ERROR FETCHING LOCAL EXECUTOR" not in citations # no errors