"""
Text summarizer adapter backed by OpenAI-compatible client using shared LLM config.
"""

from openai import OpenAI
from configuration.llm_config import get_llm_config

class OpenAITextSummarizer:
	"""Simple adapter for an OpenAI-compatible chat completions client."""

	def __init__(self, client: object | None = None):
		self._settings = get_llm_config()
		self._client = client or self._build_client()

	def _build_client(self):
		return OpenAI(
			base_url=self._settings.OAI_BASE_URL,
			api_key=self._settings.OAI_KEY
		)

	def summarize(self, prompt: str, *, max_tokens: int = 200, temperature: float = 0.0) -> str:
		resp = self._client.chat.completions.create(
			model=self._settings.OAI_MODEL,
			messages=[
				{"role": "system", "content": "You are a precision summarizer.\n\nGoal: Summarize the user-provided content to match the user’s stated objective. Do not invent details.\n\nRules:\n- Use only facts from the provided content; no external knowledge.\n- Preserve exact names, identifiers, and paths using backticks.\n- Quantify where possible (counts, versions, sizes, dates).\n- Keep neutral, clear, and actionable; avoid fluff."},
				{"role": "user", "content": prompt},
			],
			max_tokens=max_tokens,
			temperature=temperature,
		)
		return (resp.choices[0].message.content or "").strip()

