from langchain_core.prompts import ChatPromptTemplate


def hyde_prompt() -> ChatPromptTemplate:
    return ChatPromptTemplate.from_messages([
        ("system", "You are assisting a retrieval system. Write a short, factual paragraph (2-3 sentences) that would likely appear in an ideal answer to this user question."),
        ("user", "Question: \"{question}\"\nHypothetical answer paragraph (2-3 sentences):"),
    ])


def build_hyde_embed_text(question: str, hyde_answer: str) -> str:
    return f"{question}\n{hyde_answer}"


def primer_prompt() -> ChatPromptTemplate:
    return ChatPromptTemplate.from_messages([
        ("system", "You are DRIFT-Search Primer."),
        ("user", (
            "Input: user question + community summaries.\n"
            "Tasks:\n"
            "- Draft initial answer (note uncertainty if needed).\n"
            "- Generate 2–6 follow-up questions.\n"
            "Return JSON: {{ initial_answer, followups:[{{question}}], rationale }}\n\n"
            "User question: {question}\n"
            "Community details: {community_details}"
        )),
    ])


def local_executor_prompt() -> ChatPromptTemplate:
    return ChatPromptTemplate.from_messages([
        ("system", "You are DRIFT-Search Local Executor."),
        ("user", (
            "Input: follow-up question + retrieved chunks + graph neighborhoods.\n"
            "Tasks:\n"
            "- Answer follow-up using ONLY provided context.\n"
            "- Cite chunk spans where evidence comes from.\n"
            "- Do not add chunk id and spans to the answer text.\n"
            "- Propose 0–3 additional follow-ups (if needed).\n"
            "- Assign confidence [0..1] and whether to continue.\n\n"
            "Return JSON with strict schema:\n"
            '{{ "answer": "<string>", "citations": [{{"chunk_id": "<string>", "span": "<string>"}}], '
            '"new_followups": [{{"question": "<string>"}}], "confidence": <float>, "should_continue": <bool> }}\n\n'
            "CITATION REQUIREMENTS (CRITICAL):\n"
            "- You MUST use chunk_id values from the list below\n"
            "- DO NOT invent or hallucinate chunk_ids\n"
            "- If citing evidence, use one of these EXACT chunk_id strings:\n"
            "  Valid chunk IDs: {valid_chunk_ids}\n"
            "- If you cannot find evidence in these chunks, return an empty citations array\n\n"
            "OTHER REQUIREMENTS:\n"
            "- new_followups must be an array of objects with 'question' field, NOT plain strings.\n"
            'Example: {{"new_followups": [{{"question": "clarify X"}}, {{"question": "explain Y"}}]}}\n\n'
            "Follow-up: {qtext}\nTarget communities: {target_communities}\nScoped chunk contexts: {chunks_preview}"
        )),
    ])


def aggregator_prompt() -> ChatPromptTemplate:
    return ChatPromptTemplate.from_messages([
        ("system", "You are DRIFT-Search Aggregator."),
        ("user", (
            "User question: {question}\n"
            "Q/A tree (primer + follow-ups): {tree}\n"
            "Tasks:\n"
            "1. Produce final comprehensive answer synthesizing all followup findings.\n"
            "2. List ALL distinct key facts from the followup answers, preserving important details. Each followup answer should typically produce at least one key fact.\n"
            "3. Organize key facts logically by topic or relevance.\n"
            "4. Note any residual uncertainty.\n"
            "Return JSON:\n"
            '{{ "final_answer": "<string>", "key_facts": [{{"fact": "<string>", "citations": ["<chunk_id>", "<chunk_id>", ...]}}], "residual_uncertainty": "<string>" }}\n\n'
            "CITATION REQUIREMENTS (CRITICAL):\n"
            "- Citations MUST be chunk_id strings ONLY (e.g., \"abc123\", \"def456\")\n"
            "- You MUST extract chunk_id from the valid citations list below\n"
            "- DO NOT return formatted strings like \"[document_name] \\\"text\\\"\"\n"
            "- DO NOT invent or hallucinate chunk_ids\n"
            "- Valid citations with chunk_ids:\n"
            "{valid_citations}\n"
            "- Extract chunk_id from each citation (format: ... (chunk_id: <id>))\n"
            "- If you cannot find evidence in these citations, return an empty citations array\n\n"
            "EXAMPLE:\n"
            "Input citation: \"[file.py] \\\"some text\\\" (chunk_id: abc123)\"\n"
            "Output in citations array: \"abc123\""
        )),
    ])


