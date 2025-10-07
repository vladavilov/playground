import json as _json
import re as _re


class _FakeMessage:
    def __init__(self, content: str) -> None:
        self.content = content


class _FakeResp:
    def __init__(self, content: str) -> None:
        self.choices = [type("_C", (), {"message": _FakeMessage(content)})()]


class _FakeCompletions:
    async def create(self, *, model, messages, temperature, response_format):  # noqa: D401, ARG002
        user_content = "".join(str(m.get("content", "")) for m in messages if isinstance(m, dict))
        system_content = "".join(str(m.get("content", "")) for m in messages if isinstance(m, dict) and m.get("role") == "system")
        # Auditor: severity/suggestions response via system prompt recognition
        if any(
            phrase in system_content
            for phrase in (
                "Return ONLY JSON with: {severity: number in [0,1], suggestions: string[]}",
                "senior requirements QA reviewer",
                "Critique the requirements",
            )
        ):
            return _FakeResp("{\"severity\": 0.0, \"suggestions\": []}")

        # Strategist: axis scores / questions
        if "axis_scores" in user_content or "questions" in user_content:
            payload = [{
                "id": "Q1",
                "text": "Clarify completeness",
                "axis": "completeness",
                "priority": 1,
                "expected_score_gain": 0.1,
                "targets": [],
                "options": None,
            }]
            return _FakeResp(_json.dumps(payload))
        # Auditor: plain dict with severity/suggestions (strict match on key 'requirements')
        if "'requirements':" in user_content or '"requirements":' in user_content:
            return _FakeResp("{\"severity\": 0.0, \"suggestions\": []}")
        # Engineer: schema for requirements OR system prompt mentions requirements engineer
        if ("'schema': {'business_requirements'" in user_content
            or '"business_requirements":' in user_content
            or "requirements engineer" in system_content.lower()
        ):
            payload = {
                "business_requirements": [{
                    "id": "BR-1",
                    "title": "Upload",
                    "description": "Allow users to upload files",
                    "acceptance_criteria": [
                        "Given a user, When they upload, Then the file is stored",
                    ],
                    "priority": "Must",
                }],
                "functional_requirements": [],
                "assumptions": ["Local storage available"],
                "risks": ["Storage limits"],
            }
            return _FakeResp(_json.dumps(payload))
        # Analyst: intents schema/instructions
        if "\"schema\": [\"string\"]" in user_content or "instructions" in user_content:
            return _FakeResp("[\"Upload\", \"Store\"]")
        # Default to engineer-style payload
        payload = {
            "business_requirements": [{
                "id": "BR-1",
                "title": "Upload",
                "description": "Allow users to upload files",
                "acceptance_criteria": [
                    "Given a user, When they upload, Then the file is stored",
                ],
                "priority": "Must",
            }],
            "functional_requirements": [],
            "assumptions": ["Local storage available"],
            "risks": ["Storage limits"],
        }
        return _FakeResp(_json.dumps(payload))


class _FakeChat:
    def __init__(self) -> None:
        self.completions = _FakeCompletions()


class _FakeOpenAI:
    def __init__(self, *args, **kwargs) -> None:  # noqa: D401, ARG002
        self.chat = _FakeChat()


class _FakeHTTPResp:
    def json(self):
        return {"citations": ["ctx:1"], "snippets": [], "provenance": []}


class _FakeHTTPClient:
    async def __aenter__(self):
        return self
    async def __aexit__(self, exc_type, exc, tb):  # noqa: ANN001
        return False
    async def post(self, url, json):  # noqa: A002 - shadow builtin ok in tests
        del url, json
        return _FakeHTTPResp()


def stub_valuation_axes(monkeypatch, value: float) -> None:
    # Preserve helper name but decouple tests from specific evaluation library
    async def _fake_axes(self, draft, user_prompt, context):  # noqa: ANN001, ARG002
        return {
            "faithfulness": value,
            "groundedness": value,
            "response_relevancy": value,
            "completeness": value,
        }

    from orchestrator.experts.consistency_auditor import ConsistencyAuditor
    monkeypatch.setattr(ConsistencyAuditor, "_evaluate_axes", _fake_axes, raising=False)


class _FakeLangChainLLM:
    def __init__(self, *, severity: float = 0.0):
        self._severity = float(severity)

    def with_structured_output(self, model_cls):
        class _Chain:
            def __init__(self, severity: float, model_cls):
                self._severity = severity
                self._model_cls = model_cls

            async def ainvoke(self, payload):  # noqa: ANN001
                # Try to infer by model fields
                field_names = set(getattr(self._model_cls, "model_fields", {}).keys())
                # PromptAnalyst intents
                if {"intents"}.issubset(field_names):
                    return self._model_cls(intents=["upload", "store"])  # type: ignore
                # RequirementsEngineer draft
                if {"business_requirements", "functional_requirements", "assumptions", "risks"}.issubset(field_names):
                    draft = {
                        "business_requirements": [{
                            "id": "BR-1",
                            "title": "Upload",
                            "description": "Allow users to upload files",
                            "acceptance_criteria": [
                                "Given a user, When they upload, Then the file is stored",
                            ],
                            "priority": "Must",
                        }],
                        "functional_requirements": [],
                        "assumptions": ["Local storage available"],
                        "risks": ["Storage limits"],
                    }
                    return self._model_cls(**draft)  # type: ignore
                # ConsistencyAuditor critique
                if {"severity", "suggestions"}.issubset(field_names):
                    return self._model_cls(severity=self._severity, suggestions=["Tighten ACs using Given/When/Then"])  # type: ignore
                # QuestionStrategist questions
                if {"questions"}.issubset(field_names):
                    axes = {}
                    weakest = None
                    # Accept either a mapping input or a ChatPromptValue from LangChain
                    if isinstance(payload, dict):
                        axes = (payload or {}).get("axes") or {}
                    else:
                        try:
                            messages = getattr(payload, "messages", [])
                            text = "".join(str(getattr(m, "content", "")) for m in messages)
                            # Parse axis_scores from the rendered prompt text
                            # Matches 'faithfulness': 0.3 or "faithfulness": 0.3
                            pairs = _re.findall(r"['\"](faithfulness|groundedness|response_relevancy|completeness)['\"]\s*:\s*([0-9]*\.?[0-9]+)", text)
                            for k, v in pairs:
                                try:
                                    axes[k] = float(v)
                                except Exception:
                                    continue
                        except Exception:
                            axes = {}
                    if isinstance(axes, dict) and axes:
                        weakest = sorted(axes.items(), key=lambda kv: kv[1])[0][0]
                    q = [{
                        "id": f"Q_{weakest or 'completeness'}",
                        "text": f"Improve {weakest or 'completeness'}?",
                        "axis": weakest or "completeness",
                        "priority": 1,
                        "expected_score_gain": 0.1,
                        "targets": [],
                        "options": None,
                    }]
                    return self._model_cls(questions=q)  # type: ignore
                # Default: return instance without fields
                return self._model_cls()  # type: ignore

            async def __call__(self, payload):  # noqa: ANN001
                # Make this chain callable so LangChain can coerce it to a Runnable
                return await self.ainvoke(payload)

        return _Chain(self._severity, model_cls)


def make_fake_llm(*, severity: float = 0.0) -> _FakeLangChainLLM:
    return _FakeLangChainLLM(severity=severity)
