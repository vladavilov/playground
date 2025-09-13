"""
Thin Neo4j repository abstraction to centralize Cypher execution with
explicit transaction control and causal bookmark propagation across stages.
"""

from typing import Any, Dict, List, Optional
from threading import Lock

from neo4j import Driver, READ_ACCESS, WRITE_ACCESS


class Neo4jRepository:
	"""Repository over a neo4j.Driver providing read/write helpers with bookmarks."""

	# Shared bookmarks across repository instances to ensure causal chaining between stages
	_bookmarks: List[str] = []
	_lock: Lock = Lock()

	def __init__(self, driver: Driver):
		self.driver = driver

	@classmethod
	def _get_bookmarks(cls) -> List[str]:
		with cls._lock:
			return list(cls._bookmarks)

	@classmethod
	def _set_bookmarks(cls, bookmarks: List[str]) -> None:
		with cls._lock:
			# Keep only unique bookmarks to avoid growth
			seen = set(bookmarks)
			cls._bookmarks = [b for b in bookmarks if b in seen]

	def _update_bookmarks_from_session(self, session) -> None:
		try:
			bms = getattr(session, "last_bookmarks", None)
			if callable(bms):
				new_bms = bms() or []
			else:
				new_bms = []
			existing = self._get_bookmarks()
			self._set_bookmarks(list({*existing, *new_bms}))
		except Exception:
			pass

	def run(self, query: str, parameters: Optional[Dict[str, Any]] = None) -> List[Dict[str, Any]]:
		"""Execute a read query using implicit transaction (auto-commit)."""
		with self.driver.session(default_access_mode=READ_ACCESS, bookmarks=self._get_bookmarks()) as session:
			result = session.run(query, parameters or {})
			records = [record.data() for record in result]
			self._update_bookmarks_from_session(session)
			return records

	def run_write(self, query: str, parameters: Optional[Dict[str, Any]] = None) -> List[Dict[str, Any]]:
		"""Execute a write query inside an explicit write transaction."""
		with self.driver.session(default_access_mode=WRITE_ACCESS, bookmarks=self._get_bookmarks()) as session:
			def _work(tx):
				res = tx.run(query, parameters or {})
				return [r.data() for r in res]
			records = session.execute_write(_work)
			self._update_bookmarks_from_session(session)
			return records

	def single(self, query: str, parameters: Optional[Dict[str, Any]] = None) -> Optional[Dict[str, Any]]:
		with self.driver.session(default_access_mode=READ_ACCESS, bookmarks=self._get_bookmarks()) as session:
			rec = session.run(query, parameters or {}).single()
			self._update_bookmarks_from_session(session)
			return rec.data() if rec else None

	def barrier(self) -> None:
		"""Open and commit a trivial write tx to advance and propagate bookmarks."""
		with self.driver.session(default_access_mode=WRITE_ACCESS, bookmarks=self._get_bookmarks()) as session:
			def _work(tx):
				# Trivial write via an idempotent noop pattern
				tx.run("RETURN 1 AS ok")
				return None
			session.execute_write(_work)
			self._update_bookmarks_from_session(session)


