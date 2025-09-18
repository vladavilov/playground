from typing import List, Dict, Any
import json


def hyde_messages(question: str) -> List[Dict[str, str]]:
    return [
        {
            "role": "system",
            "content": (
                "You are assisting a retrieval system. Write a short, factual paragraph that would likely appear in an ideal answer to this user question."
            ),
        },
        {"role": "user", "content": f"Question: \"{question}\"\nHypothetical answer paragraph:"},
    ]


def build_hyde_embed_text(question: str, hyde_answer: str) -> str:
    # Required format: User question on first line, then the LLM answer used for HyDE
    return f"{question}\n{hyde_answer}"


def primer_messages(question: str, community_details: List[Dict], sample_chunks: List[Dict]) -> List[Dict[str, str]]:
    prompt = (
        "You are DRIFT-Search Primer.\n"
        "Input: user question + community summaries + sample chunks.\n"
        "Tasks:\n"
        "- Draft initial answer (note uncertainty if needed).\n"
        "- Generate 2–6 follow-up questions.\n"
        "Return JSON: { initial_answer, followups:[{question}], rationale }\n\n"
        f"User question: {question}\n, community details: {json.dumps(community_details)}\n, sample chunks: {json.dumps(sample_chunks)}"
    )
    return [
        {"role": "system", "content": "You are DRIFT-Search Primer."},
        {"role": "user", "content": prompt},
    ]


def local_executor_messages(qtext: str, target_communities: List[Dict[str, Any]], chunks_preview: List[Dict[str, Any]]) -> List[Dict[str, str]]:
    prompt = (
        "You are DRIFT-Search Local Executor.\n"
        "Input: follow-up question + retrieved chunks + graph neighborhoods.\n"
        "Tasks:\n"
        "- Answer follow-up using ONLY provided context.\n"
        "- Cite chunk spans where evidence comes from.\n"
        "- Propose 0–3 additional follow-ups (if needed).\n"
        "- Assign confidence [0..1] and whether to continue.\n"
        "Return JSON:\n"
        "{ answer, citations:[{span}], new_followups:[...], confidence, should_continue }\n\n"
        f"Follow-up: {qtext}\nTarget communities: {json.dumps(target_communities)}\nScoped chunk contexts: {json.dumps(chunks_preview)}"
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
        "2. List key facts with citations.\n"
        "3. Note any residual uncertainty.\n"
        "Return JSON:\n"
        "{ final_answer, key_facts:[{fact, citations:[...] }], residual_uncertainty }"
    )
    return [
        {"role": "system", "content": "You are DRIFT-Search Aggregator."},
        {"role": "user", "content": prompt},
    ]


