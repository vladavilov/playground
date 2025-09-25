"""
Reusable workflow fixtures and assertion helpers for end-to-end tests.

Keep tests focused on flow; move common assertions/utilities here.
"""

from __future__ import annotations

import io
import time
from typing import Any, Dict, Tuple, Iterable, List

import requests

from config import TestConstants
from services.redis_test_monitor import RedisTestMonitor
from shared_utils import ProjectTestUtils
from services.workflow_models import WorkflowTestFixtures
from contextlib import contextmanager
from pathlib import Path

from services.cypher_loader import execute_cypher_script

class WorkflowAssertions:
    """High-cohesion helper for asserting workflow behavior across services."""

    # ----- Core flow helpers -----
    @contextmanager
    def ui_monitoring(self, project_id: str, redis_monitor: RedisTestMonitor):
        """Context manager to ensure UI monitoring is active only during critical section."""
        redis_monitor.start_ui_progress_monitoring(project_id)
        try:
            yield
        finally:
            redis_monitor.stop_ui_progress_monitoring()

    def upload_document(self, project_id: str, fixtures: WorkflowTestFixtures, task_monitor: RedisTestMonitor) -> Dict[str, Any]:
        initial_task_count = task_monitor.get_current_task_count()
        initial_queue_count = task_monitor.get_current_task_count()

        files = {
            'files': (
                fixtures.unique_test_filename,
                io.BytesIO(fixtures.test_pdf_content),
                'application/pdf'
            )
        }

        upload_url = ProjectTestUtils.build_upload_url(
            fixtures.service_urls['project_management'], project_id
        )

        upload_response = requests.post(
            upload_url,
            files=files,
            headers=fixtures.auth_headers,
            timeout=30
        )

        assert upload_response.status_code == TestConstants.HTTP_OK, (
            f"Failed to upload document: {upload_response.text}"
        )

        time.sleep(2)

        verification_results = task_monitor.verify_task_published_comprehensive(
            project_id=project_id,
            initial_stream_count=initial_queue_count,
            timeout=TestConstants.DEFAULT_TIMEOUT
        )

        verification_passed = (
            verification_results['verification_successful'] or
            verification_results.get('queue_increased') or
            task_monitor.get_current_task_count() > initial_task_count
        )

        assert verification_passed, (
            f"Expected task publishing verification to pass after document upload. "
            f"Verification results: {verification_results}. "
            f"Queue length: {initial_task_count} -> {task_monitor.get_current_task_count()}"
        )

        return upload_response.json()

    def wait_for_api_status(
        self,
        project_id: str,
        fixtures: WorkflowTestFixtures,
        expected_status: str,
        *,
        timeout: int = TestConstants.DOCUMENT_PROCESSING_TIMEOUT,
        interval: float = 1.0,
    ) -> Dict[str, Any]:
        """Poll the Project Management API until project reaches expected status."""
        deadline = time.time() + timeout
        last: Dict[str, Any] = {}
        while time.time() < deadline:
            try:
                project_url = ProjectTestUtils.build_project_url(
                    fixtures.service_urls['project_management'], project_id
                )
                resp = requests.get(
                    project_url,
                    headers=fixtures.auth_headers,
                    timeout=TestConstants.DEFAULT_TIMEOUT,
                )
                if resp.status_code == TestConstants.HTTP_OK:
                    last = resp.json()
                    if last.get("status") == expected_status:
                        return last
            except requests.RequestException:
                pass
            time.sleep(interval)
        assert last.get("status") == expected_status, (
            f"API status did not reach '{expected_status}', last='{last.get('status')}'"
        )
        return last

    def verify_db_status(
        self,
        project_id: str,
        fixtures: WorkflowTestFixtures,
        expected_status: str,
        *,
        min_pct: float | None = None,
    ) -> None:
        cursor = fixtures.postgres_connection.cursor()
        if min_pct is None:
            cursor.execute(
                "SELECT status FROM projects WHERE id = %s",
                (project_id,)
            )
            row = cursor.fetchone()
            cursor.close()
            assert row is not None, "Project not found in database"
            assert row[0] == expected_status, (
                f"Expected DB status '{expected_status}', got '{row[0]}'"
            )
        else:
            cursor.execute(
                "SELECT status, processed_pct FROM projects WHERE id = %s",
                (project_id,)
            )
            row = cursor.fetchone()
            cursor.close()
            assert row is not None, "Project not found in database"
            assert row[0] == expected_status, (
                f"Expected DB status '{expected_status}', got '{row[0]}'"
            )
            assert float(row[1] or 0) >= float(min_pct), (
                f"Expected processed_pct >= {min_pct}, got {row[1]}"
            )

    def verify_neo4j_project_and_documents(self, project_id: str, fixtures: WorkflowTestFixtures, *, min_docs: int = 1) -> None:
        with fixtures.neo4j_driver.session() as session:
            # Verify GraphRAG-ingested nodes exist according to runner.py logic
            # 1) Documents
            doc_count_record = session.run(
                """
                MATCH (d:__Document__)
                RETURN count(d) AS c
                """
            ).single()
            assert doc_count_record is not None, "Failed to query __Document__ nodes"
            assert int(doc_count_record["c"] or 0) >= min_docs, (
                f"Expected at least {min_docs} __Document__ node(s), found {doc_count_record['c']}"
            )

            # 2) Document -> Chunk edges created during import
            chunk_edge_record = session.run(
                """
                MATCH (:__Document__)-[:HAS_CHUNK]->(c:__Chunk__)
                RETURN count(DISTINCT c) AS c
                """
            ).single()
            assert chunk_edge_record is not None, "Failed to query HAS_CHUNK edges to __Chunk__"
            if min_docs > 0:
                assert int(chunk_edge_record["c"] or 0) > 0, "Expected HAS_CHUNK edges from __Document__ to __Chunk__"

            # 3) Optional entity links (not guaranteed for all inputs but should exist post-ingestion)
            entity_link_record = session.run(
                """
                MATCH (:__Chunk__)-[:HAS_ENTITY]->(e:__Entity__)
                RETURN count(e) AS c
                """
            ).single()
            assert entity_link_record is not None, "Failed to query HAS_ENTITY links from __Chunk__ to __Entity__"

    def verify_neo4j_constraints_minimal(self, fixtures: WorkflowTestFixtures) -> None:
        with fixtures.neo4j_driver.session() as session:
            constraints = list(session.run(
                """
                SHOW CONSTRAINTS
                YIELD name, entityType, labelsOrTypes, properties, type
                RETURN name, entityType, labelsOrTypes, properties, type
                """
            ))
            def has_node_unique(label: str) -> bool:
                for rec in constraints:
                    if (rec.get("entityType") == "NODE"
                        and label in (rec.get("labelsOrTypes") or [])
                        and "id" in (rec.get("properties") or [])
                        and "UNIQUE" in (rec.get("type") or "")):
                        return True
                return False
            assert has_node_unique("__Entity__"), "Missing unique constraint on __Entity__(id)"

    def verify_neo4j_vector_index(self, fixtures: WorkflowTestFixtures) -> None:
        """Verify vector indexes exist and are ONLINE with correct settings.

        Community uses property 'summary_embedding'; chunks use 'embedding'.
        """
        expected = [
            {"label": "__Chunk__", "property": "embedding"},
            {"label": "__Community__", "property": "summary_embedding"},
        ]
        expected_dims = 1536
        expected_sim = "cosine"

        with fixtures.neo4j_driver.session() as session:
            records = list(session.run(
                """
                SHOW INDEXES
                YIELD name, state, type, entityType, labelsOrTypes, properties, options
                WHERE type = 'VECTOR'
                RETURN name, state, type, entityType, labelsOrTypes, properties, options
                """
            ))

            def find_vector_index(label: str, prop: str):
                for rec in records:
                    labels = rec.get("labelsOrTypes") or []
                    props = rec.get("properties") or []
                    if label in labels and prop in props:
                        return rec
                return None

            for cfg in expected:
                idx = find_vector_index(cfg["label"], cfg["property"])
                assert idx is not None, f"Missing vector index for {cfg['label']}({cfg['property']})"
                assert idx.get("state") == "ONLINE", f"Vector index for {cfg['label']} is not ONLINE"
                index_cfg = (idx.get("options") or {}).get("indexConfig", {}) or {}
                dims = index_cfg.get("vector.dimensions")
                sim = index_cfg.get("vector.similarity_function")
                if isinstance(sim, str):
                    sim = sim.lower()
                assert dims == expected_dims, (
                    f"Vector index dimension must be {expected_dims} for {cfg['label']}"
                )
                assert sim == expected_sim, (
                    f"Vector similarity must be {expected_sim} for {cfg['label']}"
                )

    # ----- Neo4j admin/test helpers reused across e2e modules -----
    def reset_neo4j_database(
        self,
        driver: Any,
        database_name: str
    ) -> None:
        """Remove all nodes/relationships and drop provided indexes if exist.

        Keeps behaviour identical to existing drift-search setup.
        """
        with driver.session(database=database_name) as session:
            session.run("MATCH (n) DETACH DELETE n").consume()
            session.run("CALL apoc.schema.assert({}, {})").consume()

    def load_cypher_script(self, driver: Any, database_name: str, script_path: Path) -> None:
        execute_cypher_script(driver, database_name, script_path)

    def verify_required_index_names(
        self,
        driver: Any,
        *,
        required_vector_names: Iterable[str] = (),
        required_fulltext_names: Iterable[str] = (),
    ) -> None:
        with driver.session() as session:
            result = session.run(
                """
                SHOW INDEXES YIELD name, type, entityType, labelsOrTypes, properties, options
                RETURN name, type, entityType, labelsOrTypes, properties, options
                """
            )
            indexes: List[Dict[str, Any]] = [dict(r) for r in result]
            names = {idx.get("name") for idx in indexes}

            # Presence checks by family
            missing_vec = set(required_vector_names) - names
            missing_fts = set(required_fulltext_names) - names
            assert not missing_vec and not missing_fts, (
                f"Missing indexes: vector={missing_vec} fulltext={missing_fts}. Present: {names}"
            )

            # Additional sanity: types for provided names
            idx_by_name = {idx.get("name"): idx for idx in indexes}
            for nm in required_vector_names:
                if nm in idx_by_name:
                    assert str(idx_by_name[nm].get("type", "")).upper() == "VECTOR"
            for nm in required_fulltext_names:
                if nm in idx_by_name:
                    assert str(idx_by_name[nm].get("type", "")).upper() == "FULLTEXT"

    def poll_active_queries(self, driver: Any, duration_seconds: float = 2.0) -> list[str]:
        import time as _time
        deadline = _time.time() + duration_seconds
        seen: list[str] = []
        while _time.time() < deadline:
            try:
                with driver.session() as session:
                    rows = list(session.run("SHOW TRANSACTIONS YIELD currentQuery RETURN currentQuery"))
                    for r in rows:
                        q = r.get("currentQuery") or ""
                        if q and q not in seen:
                            seen.append(q)
            except Exception:
                pass
            _time.sleep(0.05)
        return seen

    # ----- Assertions -----
    def expected_ui_sequence(self) -> list:
        """Standard expected UI status/progress messages for a full run."""
        return [
            TestConstants.PROJECT_STATUS_PROCESSING,
            TestConstants.PROJECT_STATUS_ACTIVE,
            TestConstants.PROJECT_STATUS_RAG_PROCESSING,
            (TestConstants.PROJECT_STATUS_RAG_PROCESSING, "Pipeline started"),
            (TestConstants.PROJECT_STATUS_RAG_PROCESSING, "Pipeline finished"),
            TestConstants.PROJECT_STATUS_RAG_READY,
        ]

    def verify_upload_response(self, project_id: str, upload_result: Dict[str, Any], fixtures: WorkflowTestFixtures) -> None:
        assert upload_result["project_id"] == project_id
        assert upload_result["total_files"] == 1
        assert upload_result["successful_uploads"] == 1
        assert upload_result["failed_uploads"] == 0
        assert upload_result["processing_initiated"] is True
        assert fixtures.unique_test_filename in upload_result["uploaded_files"]

    def verify_project_in_database(self, project_id: str, fixtures: WorkflowTestFixtures) -> None:
        cursor = fixtures.postgres_connection.cursor()
        cursor.execute("SELECT id, name, status FROM projects WHERE id = %s", (project_id,))
        db_project = cursor.fetchone()
        cursor.close()
        assert db_project is not None, "Project not found in database"
        assert str(db_project[0]) == project_id
        assert db_project[1] == fixtures.test_project_data["name"]
        assert db_project[2] == fixtures.test_project_data["status"]

    # ----- Additional helpers reused by other tests -----
    def prepare_multiple_files(self, project_id: str, test_pdf_content: bytes) -> Tuple[list, list]:
        files = []
        expected_filenames = []
        for i in range(3):
            filename = f"test_document_{i}_{project_id[:8]}.pdf"
            expected_filenames.append(filename)
            files.append(('files', (filename, io.BytesIO(test_pdf_content), 'application/pdf')))
        return files, expected_filenames

    def verify_multiple_upload_response(self, upload_result: Dict[str, Any], project_id: str, expected_filenames: list) -> None:
        assert upload_result["project_id"] == project_id
        assert upload_result["total_files"] == 3
        assert upload_result["successful_uploads"] == 3
        assert upload_result["failed_uploads"] == 0
        assert upload_result["processing_initiated"] is True
        for filename in expected_filenames:
            assert filename in upload_result["uploaded_files"]

    def assert_upload_without_files(self, project_id: str, service_urls: Dict[str, str], auth_headers: Dict[str, str]) -> None:
        upload_url = ProjectTestUtils.build_upload_url(service_urls['project_management'], project_id)
        upload_response = requests.post(
            upload_url,
            headers=auth_headers,
            timeout=TestConstants.DEFAULT_TIMEOUT
        )
        assert upload_response.status_code in [400, 422], (
            "Should return error for upload without files"
        )

    def assert_upload_to_nonexistent_project(self, service_urls: Dict[str, str], auth_headers: Dict[str, str]) -> None:
        fake_project_id = "ffffffff-ffff-ffff-ffff-ffffffffffff"
        files = {
            'files': ("test.pdf", io.BytesIO(b"fake pdf content"), 'application/pdf')
        }
        upload_url = ProjectTestUtils.build_upload_url(service_urls['project_management'], fake_project_id)
        upload_response = requests.post(
            upload_url,
            files=files,
            headers=auth_headers,
            timeout=TestConstants.DEFAULT_TIMEOUT
        )
        assert upload_response.status_code == TestConstants.HTTP_NOT_FOUND, (
            f"Should return 404 for non-existent project, got {upload_response.status_code}. "
            f"Response: {upload_response.text}"
        )
