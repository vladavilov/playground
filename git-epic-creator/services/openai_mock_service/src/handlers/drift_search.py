import json
import re
from typing import Any, Dict, List
import structlog
from handlers.base import BaseHandler

logger = structlog.get_logger(__name__)


class DriftHydeHandler(BaseHandler):
    """Handles DRIFT-search HyDE (Hypothetical Document Embeddings) requests.
    
    Expected model: gpt-4o-mini (fast model)
    Used by: neo4j-retrieval-service hyde_node
    """

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        return "hyde" in lower_text or "hypothetical answer paragraph" in lower_text

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        logger.info("drift_mock_hyde", model=model)
        return "Hypothetical paragraph about bridge components: deck, supports, arches, cables, and foundations."


class DriftPrimerHandler(BaseHandler):
    """Handles DRIFT-search primer requests.
    
    Expected model: gpt-4o-mini (fast model)
    Used by: neo4j-retrieval-service primer_node
    """

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        return "you are drift-search primer" in lower_text

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        logger.info("drift_mock_primer", model=model)
        primer_json = {
            "initial_answer": "Bridges consist of deck, supports, and load-bearing structures.",
            "followups": [
                {"question": "Clarify deck materials and structural role."},
                {"question": "Explain arch mechanics in load distribution."},
            ],
            "rationale": "Primer synthesized from community summaries and sampled chunks.",
        }
        return json.dumps(primer_json)


class DriftLocalExecutorHandler(BaseHandler):
    """Handles DRIFT-search local executor requests with branching logic.
    
    Expected model: gpt-4o-mini (fast model)
    Used by: neo4j-retrieval-service local_executor_node
    """

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        return "you are drift-search local executor" in lower_text

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        # Extract the follow-up question from the prompt
        followup_match = re.search(r"follow-up:\s*(.+?)\ntarget communities:", combined_text, re.IGNORECASE | re.DOTALL)
        followup_text = followup_match.group(1).strip().lower() if followup_match else ""
        
        logger.info("drift_mock_local_executor", model=model, followup_snippet=followup_text[:100] if followup_text else "not_found")
        
        # Branch 1: Clarify deck materials and structural role
        if "deck materials" in followup_text and "structural role" in followup_text:
            local_json = {
                "answer": (
                    "Bridge decks are commonly built from reinforced concrete, steel orthotropic panels, or composite systems. "
                    "The deck distributes vehicular loads to primary load-bearing elements (girders, arches, or cables) and provides a riding surface."
                ),
                "citations": [
                    {"span": "deck materials overview"},
                    {"span": "load path from deck to supports"},
                ],
                "new_followups": [
                    {
                        "question": "List common deck materials and how they influence load distribution."
                    }
                ],
                "confidence": 0.9,
                "should_continue": True,
            }
            logger.info("drift_mock_local_deck_materials")
            return json.dumps(local_json)

        # Branch 2: List common deck materials (second-level followup)
        if "list common deck materials" in followup_text and "load distribution" in followup_text:
            local_json = {
                "answer": (
                    "Reinforced concrete decks spread loads through slab action to supporting girders; steel orthotropic decks channel loads via stiffened plates; "
                    "composite steel–concrete decks combine slab and girder action, improving stiffness and distributing loads more evenly."
                ),
                "citations": [
                    {"span": "concrete slab load distribution"},
                    {"span": "orthotropic deck behavior"},
                ],
                "new_followups": [],
                "confidence": 0.92,
                "should_continue": False,
            }
            logger.info("drift_mock_local_deck_materials_followup")
            return json.dumps(local_json)

        # Branch 3: Explain arch mechanics
        if "arch mechanics" in followup_text or ("arch" in followup_text and "load distribution" in followup_text):
            local_json = {
                "answer": (
                    "Arches carry deck loads primarily in compression, transferring forces as thrust to abutments. "
                    "Load paths curve along the arch rib, minimizing bending and channeling forces into supports."
                ),
                "citations": [
                    {"span": "arch compression and thrust"},
                    {"span": "load path along arch rib"},
                ],
                "new_followups": [],
                "confidence": 0.9,
                "should_continue": False,
            }
            logger.info("drift_mock_local_arch_mechanics")
            return json.dumps(local_json)

        # Default fallback
        local_json = {
            "answer": "ERROR FETCHING LOCAL EXECUTOR",
            "citations": [{"span": "deck overview"}, {"span": "arch mechanics"}],
            "new_followups": [],
            "confidence": 0.9,
            "should_continue": False,
        }
        logger.warning("drift_mock_local_fallback", followup_text=followup_text[:200] if followup_text else "empty")
        return json.dumps(local_json)


class DriftAggregatorHandler(BaseHandler):
    """Handles DRIFT-search aggregator requests.
    
    Expected model: gpt-4o-mini (fast model)
    Used by: neo4j-retrieval-service aggregate_node
    """

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        return "you are drift-search aggregator" in lower_text

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        # Extract and parse the tree JSON from the user prompt
        tree_json = None
        try:
            tree_start_match = re.search(r"Q/A tree \(primer \+ follow-ups\):\s*\{", combined_text, re.IGNORECASE)
            if tree_start_match:
                start_pos = tree_start_match.end() - 1
                brace_count = 0
                in_string = False
                escape_next = False
                end_pos = start_pos
                
                for i in range(start_pos, len(combined_text)):
                    char = combined_text[i]
                    
                    if escape_next:
                        escape_next = False
                        continue
                    if char == '\\':
                        escape_next = True
                        continue
                    if char == '"':
                        in_string = not in_string
                        continue
                    
                    if not in_string:
                        if char == '{':
                            brace_count += 1
                        elif char == '}':
                            brace_count -= 1
                            if brace_count == 0:
                                end_pos = i + 1
                                break
                
                tree_str = combined_text[start_pos:end_pos]
                tree_json = json.loads(tree_str)
                logger.info("tree_parsed_successfully", tree_keys=list(tree_json.keys()) if tree_json else [])
            else:
                logger.warning("tree_start_not_found", prompt_snippet=combined_text[:500])
        except Exception as exc:
            logger.warning("failed_to_parse_tree", error=str(exc), exc_type=type(exc).__name__)
        
        # Build comprehensive citations structure from the tree
        citations_parts = []
        citations_data = {}
        
        if tree_json:
            citations_data["question"] = tree_json.get("question", "")
            
            primer = tree_json.get("primer", {})
            if primer:
                citations_data["primer"] = {
                    "initial_answer": primer.get("initial_answer", ""),
                    "followups": primer.get("followups", []),
                    "rationale": primer.get("rationale", ""),
                }
            
            followups_data = []
            for followup in tree_json.get("followups", []):
                followup_item = {
                    "answer": followup.get("answer", ""),
                    "citations": followup.get("citations", []),
                    "confidence": followup.get("confidence", 0),
                    "should_continue": followup.get("should_continue", False),
                }
                
                # Extract chunk texts with neighbours from context
                if "context" in followup:
                    context_list = followup["context"]
                    if isinstance(context_list, list):
                        for chunk_data in context_list:
                            if isinstance(chunk_data, dict):
                                chunk_text = chunk_data.get("text", "")
                                if chunk_text:
                                    citations_parts.append(chunk_text)
                                
                                neighbours = chunk_data.get("neighbours")
                                if neighbours:
                                    citations_parts.append(f"neighbours: {json.dumps(neighbours)}")
                    
                    followup_item["context"] = followup["context"]
                
                if "new_followups" in followup and followup.get("should_continue"):
                    followup_item["new_followups"] = followup["new_followups"]
                
                followups_data.append(followup_item)
            
            citations_data["followups"] = followups_data
        
        # Build final citations string: JSON tree + extracted chunk texts
        citations_str_parts = [json.dumps(citations_data) if citations_data else combined_text]
        if citations_parts:
            citations_str_parts.extend(citations_parts)
        citations_str = " ".join(citations_str_parts)
        
        agg_json = {
            "final_answer": "Bridges comprise the deck, supports, and load-bearing structures such as arches or cables.",
            "key_facts": [
                {"fact": "Deck carries traffic and distributes loads.", "citations": citations_str},
            ],
            "residual_uncertainty": "Specific materials and design vary by bridge type.",
        }
        logger.info("drift_mock_aggregator", model=model, tree_parsed=bool(tree_json), citations_parts_count=len(citations_parts))
        return json.dumps(agg_json)


class DriftSearchSystemHandler(BaseHandler):
    """Handles DRIFT_LOCAL_SYSTEM_PROMPT with response/score/follow_up_queries."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        return (
            "Format your response in JSON with the following keys" in combined_text
            and "Additionally provide a score between 0 and 100" in combined_text
            and "follow_up_queries" in combined_text
        )

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        result = {
            "response": (
                "### Smallpdf capabilities\n\n"
                "- Upload, organize, and share digital documents\n"
                "- Optional Storage for processed files\n"
                "- Cross‑device access with Mobile App sync\n"
                "- Right‑click actions: Convert, Compress, Modify\n"
                "- Collaboration: e‑signatures, large file sending, G Suite App\n"
            ),
            "score": 78,
            "follow_up_queries": [
                "What file types are supported for Convert and Modify?",
                "Are Storage retention and encryption configurable?",
                "What size limits apply to large file sending?",
            ],
        }
        return json.dumps(result)


class DriftReduceHandler(BaseHandler):
    """Handles DRIFT_REDUCE_PROMPT with markdown summary."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        system_content = "".join(
            str(m.get("content", ""))
            for m in messages
            if isinstance(m, dict) and m.get("role") == "system"
        )
        
        return (
            "---Role---" in system_content
            and "responding to questions about data in the reports provided" in system_content
            and "---Data Reports---" in combined_text
        )

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        return (
            "### Summary\n\n"
            "Smallpdf centralizes document work: upload, organize, and share. When Storage is enabled, processed files are retained and accessible across devices, with Mobile App sync to the portal. Right‑click actions (Convert, Compress, Modify) and collaboration features (e‑signatures, large file sending, G Suite App) streamline workflows.\n\n"
            "### Notes\n\n"
            "- Capabilities emphasize convenience and collaboration.\n"
            "- Storage behavior may warrant retention/security clarifications."
        )

