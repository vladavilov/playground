from langchain_core.prompts import ChatPromptTemplate


def hyde_prompt() -> ChatPromptTemplate:
    return ChatPromptTemplate.from_messages([
        ("system", "You are assisting a retrieval system. Write a short, factual paragraph that would likely appear in an ideal answer to this user question."),
        ("user", "Question: \"{question}\"\nHypothetical answer paragraph:"),
    ])


def build_hyde_embed_text(question: str, hyde_answer: str) -> str:
    return f"{question}\n{hyde_answer}"


def primer_prompt() -> ChatPromptTemplate:
    return ChatPromptTemplate.from_messages([
        ("system", "You are DRIFT-Search Primer."),
        ("user", (
            "You are DRIFT-Search Primer.\n"
            "Input: user question + community summaries + sample chunks.\n"
            "Tasks:\n"
            "- Draft initial answer (note uncertainty if needed).\n"
            "- Generate 2–6 follow-up questions.\n"
            "Return JSON: {{ initial_answer, followups:[{{question}}], rationale }}\n\n"
            "User question: {question}\n, community details: {community_details}\n, sample chunks: {sample_chunks}"
        )),
    ])


def local_executor_prompt() -> ChatPromptTemplate:
    return ChatPromptTemplate.from_messages([
        ("system", "You are DRIFT-Search Local Executor."),
        ("user", (
            "You are DRIFT-Search Local Executor.\n"
            "Input: follow-up question + retrieved chunks + graph neighborhoods.\n"
            "Tasks:\n"
            "- Answer follow-up using ONLY provided context.\n"
            "- Cite chunk spans where evidence comes from.\n"
            "- Propose 0–3 additional follow-ups (if needed).\n"
            "- Assign confidence [0..1] and whether to continue.\n\n"
            "Return JSON with strict schema:\n"
            '{{ "answer": "<string>", "citations": [{{"chunk_id": "<string>", "span": "<string>"}}], '
            '"new_followups": [{{"question": "<string>"}}], "confidence": <float>, "should_continue": <bool> }}\n\n'
            "IMPORTANT:\n"
            "- chunk_id must be a STRING (use the exact chunk_id from the provided context)\n"
            "- new_followups must be an array of objects with 'question' field, NOT plain strings.\n"
            'Example: {{"new_followups": [{{"question": "clarify X"}}, {{"question": "explain Y"}}]}}\n\n'
            "Follow-up: {qtext}\nTarget communities: {target_communities}\nScoped chunk contexts: {chunks_preview}"
        )),
    ])


def aggregator_prompt() -> ChatPromptTemplate:
    return ChatPromptTemplate.from_messages([
        ("system", "You are DRIFT-Search Aggregator."),
        ("user", (
            "You are DRIFT-Search Aggregator.\n"
            "User question: {question}\n"
            "Q/A tree (primer + follow-ups): {tree}\n"
            "Tasks:\n"
            "1. Produce final concise answer.\n"
            "2. List key facts with citations (chunk IDs as strings).\n"
            "3. Note any residual uncertainty.\n"
            "Return JSON:\n"
            '{{ "final_answer": "<string>", "key_facts": [{{"fact": "<string>", "citations": ["<chunk_id>", ...]}}], "residual_uncertainty": "<string>" }}\n'
            "IMPORTANT: citations must be an array of STRINGS (chunk IDs from the provided context)"
        )),
    ])


