from typing import List, Dict
import json


def primer_messages(question: str, community_details: List[Dict], sample_chunks: List[Dict]) -> List[Dict[str, str]]:
    prompt = (
        "You are DRIFT-Search Primer.\n"
        "Input: user question + community summaries + sample chunks.\n"
        "Tasks:\n"
        "- Draft initial answer (note uncertainty if needed).\n"
        "- Generate 2–6 follow-up questions with target communities.\n"
        "Return JSON: { initial_answer, followups:[{question, target_communities:[...] }], rationale }\n\n"
        f"User question: {question}\n, community details: {json.dumps(community_details)}\n, sample chunks: {json.dumps(sample_chunks)}"
    )
    return [
        {"role": "system", "content": "You are DRIFT-Search Primer."},
        {"role": "user", "content": prompt},
    ]


def local_executor_messages(qtext: str, target_communities: List[int], chunks_preview: List[int]) -> List[Dict[str, str]]:
    prompt = (
        "You are DRIFT-Search Local Executor.\n"
        "Input: follow-up question + retrieved chunks + graph neighborhoods.\n"
        "Tasks:\n"
        "- Answer follow-up using ONLY provided context.\n"
        "- Cite chunk IDs where evidence comes from.\n"
        "- Propose 0–3 additional follow-ups (if needed).\n"
        "- Assign confidence [0..1] and whether to continue.\n"
        "Return JSON:\n"
        "{ answer, citations:[{chunk_id, span}], new_followups:[...], confidence, should_continue }\n\n"
        f"Follow-up: {qtext}\nTarget communities: {target_communities}\nScoped chunk ids: {chunks_preview}"
    )
    return [
        {"role": "system", "content": "You are DRIFT-Search Local Executor."},
        {"role": "user", "content": prompt},
    ]


def aggregator_messages(question: str, tree: Dict) -> List[Dict[str, str]]:
    prompt = (
        "You are DRIFT-Search Aggregator.\n"
        f"User question: {question}\n"
        f"Q/A tree (primer + follow-ups): {json.dumps(tree)}\n"
        "Tasks:\n"
        "1. Produce final concise answer.\n"
        "2. List key facts with citations (chunk IDs).\n"
        "3. Note any residual uncertainty.\n"
        "Return JSON:\n"
        "{ final_answer, key_facts:[{fact, citations:[...] }], residual_uncertainty }"
    )
    return [
        {"role": "system", "content": "You are DRIFT-Search Aggregator."},
        {"role": "user", "content": prompt},
    ]


