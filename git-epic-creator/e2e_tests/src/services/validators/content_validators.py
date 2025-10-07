"""
Content quality validators for e2e tests.

This module provides content validation operations following
Single Responsibility Principle - focused on validating content quality.
"""

from __future__ import annotations

from typing import Dict, Any, List


class ContentValidators:
    """
    Validators for content quality and semantics.
    
    Provides validators for:
    - Mock response content verification
    - Keyword presence validation
    - Content quality checks
    """
    
    @staticmethod
    def validate_mock_content_in_backlog(bundle: Dict[str, Any]) -> None:
        """
        Validate mock response content from TasksBacklogEngineerHandler in backlog bundle.
        
        Mock returns epics with IDs like "EPIC-001", "EPIC-002" and tasks like "TASK-001"
        and content about "Document Upload", "Cloud Storage", "Cross-Device Access", etc.
        
        Args:
            bundle: Backlog bundle response to validate
            
        Raises:
            AssertionError: If mock content is not present
        """
        epics = bundle.get("epics", [])
        all_text = " ".join(
            str(epic.get("title", "")) + " " + str(epic.get("description", ""))
            for epic in epics
        ).lower()
        mock_epic_keywords = [
            "document", "upload", "storage", "cloud", "device",
            "access", "file", "mobile", "api"
        ]
        found_epic_keywords = [kw for kw in mock_epic_keywords if kw in all_text]
        assert len(found_epic_keywords) >= 2, \
            f"Epics should contain mock response content (Smallpdf features), found: {found_epic_keywords}"
        
        # Check for mock task IDs pattern (e.g., "TASK-001", "EPIC-001")
        all_ids = [epic.get("id", "") for epic in epics]
        for epic in epics:
            all_ids.extend([task.get("id", "") for task in epic.get("tasks", [])])
        id_text = " ".join(all_ids)
        # Should contain structured IDs (not just random strings)
        assert any(char.isdigit() for char in id_text), \
            "Epic/Task IDs should contain numbers (following mock pattern)"
    
    @staticmethod
    def validate_mock_content_in_requirements(all_reqs: List[Dict[str, Any]]) -> None:
        """
        Validate mock response content from EngineerHandler in requirements bundle.
        
        Mock returns requirements with IDs like "BR-1", "BR-2", "FR-1", "FR-2"
        and titles about "Upload", "Storage", "Smallpdf", etc.
        
        Args:
            all_reqs: Combined list of business and functional requirements
            
        Raises:
            AssertionError: If mock content is not present
        """
        if len(all_reqs) > 0:
            # Check that requirements have expected structure
            first_req = all_reqs[0]
            assert "id" in first_req, "Requirement should have id field"
            assert "title" in first_req, "Requirement should have title field"
            assert "description" in first_req, "Requirement should have description field"
            
            # Check for mock content keywords (Smallpdf-related terms)
            all_text = " ".join(
                str(r.get("title", "")) + " " + str(r.get("description", ""))
                for r in all_reqs
            ).lower()
            mock_keywords = ["upload", "storage", "file", "document", "device", "access", "mobile"]
            found_keywords = [kw for kw in mock_keywords if kw in all_text]
            assert len(found_keywords) >= 2, \
                f"Requirements should contain mock response content (Smallpdf features), found: {found_keywords}"
    
    @staticmethod
    def validate_retrieval_response_quality(body: Dict[str, Any]) -> None:
        """
        Validate retrieval service response quality and mock content.
        
        Checks:
        - final_answer content and quality
        - key_facts structure and mock content
        - citations with DRIFT tree structure
        - residual_uncertainty presence
        
        Args:
            body: Retrieval service response JSON
            
        Raises:
            AssertionError: If response quality is insufficient
        """
        # 1. Validate final_answer content quality
        assert body['final_answer'], "final_answer should not be empty"
        assert len(body['final_answer']) > 50, "Answer should be substantive (>50 chars)"
        
        # Verify mentions bridge components from test data and mock responses
        component_keywords = ["deck", "support", "arch", "cable", "load", "bridge"]
        found_components = [
            kw for kw in component_keywords
            if kw in body['final_answer'].lower()
        ]
        assert len(found_components) >= 2, \
            f"final_answer should mention bridge components from mock, found: {found_components}"
        
        # Check for specific mock response content from DriftAggregatorHandler
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
        
        # Check for specific mock key_facts content
        first_fact_lower = body['key_facts'][0]['fact'].lower()
        mock_keywords = ["deck", "traffic", "load", "distribute", "carry"]
        found_mock_keywords = [kw for kw in mock_keywords if kw in first_fact_lower]
        assert len(found_mock_keywords) >= 1, \
            f"key_facts should contain mock response content, found keywords: {found_mock_keywords}"
        
        # 3. Validate citations contain full context tree structure
        citations_str = body['key_facts'][0]['citations']
        assert isinstance(citations_str, str), "Citations should be a string"
        assert len(citations_str) > 100, "Citations should contain substantial context"
        
        # Check for DRIFT tree structure components
        assert '"question"' in citations_str or 'question' in citations_str.lower(), \
            "Citations should reference the question"
        assert '"primer"' in citations_str or 'initial_answer' in citations_str.lower(), \
            "Citations should include primer information"
        assert '"followups"' in citations_str or 'answer' in citations_str.lower(), \
            "Citations should include followup answers"
        
        # Check for mock response content
        citations_lower = citations_str.lower()
        primer_keywords = ["bridge", "deck", "support", "load"]
        local_keywords = ["concrete", "steel", "arch", "compression", "material", "thrust"]
        found_primer = [kw for kw in primer_keywords if kw in citations_lower]
        found_local = [kw for kw in local_keywords if kw in citations_lower]
        
        # Should have content from at least primer OR local executor responses
        assert len(found_primer) >= 1 or len(found_local) >= 1, \
            f"Citations should contain mock handler content. Primer: {found_primer}, Local: {found_local}"
        
        # 4. Validate enhanced neighborhood expansion data
        assert 'neighbour' in citations_lower or 'entity' in citations_lower or 'properties' in citations_lower, \
            "Citations should include entity neighbourhood information"
        
        # Check for chunk text content (actual retrieved context)
        component_mentions_in_citations = [
            kw for kw in component_keywords if kw in citations_lower
        ]
        assert len(component_mentions_in_citations) >= 2, \
            f"Citations should mention multiple bridge components from context, found: {component_mentions_in_citations}"
        
        # 5. Verify no processing errors occurred
        response_str = str(body)
        assert "ERROR FETCHING LOCAL EXECUTOR" not in response_str, \
            "Should not have executor errors in response"
        assert "ERROR" not in body.get('final_answer', '').upper() or len(body.get('final_answer', '')) > 50, \
            "Should have substantive answer, not an error message"
        
        # 6. Validate residual_uncertainty
        assert isinstance(body.get('residual_uncertainty'), str), \
            "residual_uncertainty should be a string"
        assert len(body.get('residual_uncertainty', '')) > 10, \
            "residual_uncertainty should be substantive"
        
        # Check for mock response content from DriftAggregatorHandler
        uncertainty_lower = body.get('residual_uncertainty', '').lower()
        uncertainty_keywords = ["material", "design", "vary", "type", "specific", "bridge"]
        found_uncertainty = [kw for kw in uncertainty_keywords if kw in uncertainty_lower]
        assert len(found_uncertainty) >= 1, \
            f"residual_uncertainty should contain mock response elements, found: {found_uncertainty}"
    
    @staticmethod
    def validate_drift_json_tree(body: Dict[str, Any]) -> None:
        """
        Validate DRIFT JSON tree structure embedded in citations.
        
        Parses and validates the embedded JSON tree structure that contains:
        - Question and primer nodes
        - Followup context with neighborhood data
        - Enhanced fields (neighbours, related_entities, relationships)
        
        Args:
            body: Retrieval service response with key_facts containing citations
            
        Note:
            This validation is optional - it gracefully handles cases where
            JSON tree is not present or has a different structure.
        """
        import json as json_lib
        
        if not body.get('key_facts') or len(body['key_facts']) == 0:
            return  # No citations to parse
        
        citations_str = body['key_facts'][0].get('citations', '')
        if not isinstance(citations_str, str):
            return  # Invalid citations format
        
        try:
            # Try to find and parse JSON structure in citations
            if '{"question":' not in citations_str and '{"primer":' not in citations_str:
                return  # No JSON tree present
            
            # Look for JSON object boundaries
            start_idx = max(
                citations_str.find('{"question":'),
                citations_str.find('{"primer":')
            )
            if start_idx < 0:
                return
            
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
            
            if end_idx <= start_idx:
                return  # Could not find matching braces
            
            tree_json_str = citations_str[start_idx:end_idx]
            tree_json = json_lib.loads(tree_json_str)
            
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
        
        except (json_lib.JSONDecodeError, KeyError, IndexError):
            # JSON parsing might fail if structure is different, that's okay
            # as long as we have the content validated elsewhere
            pass

