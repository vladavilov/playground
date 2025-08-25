"""
Reusable workflow fixtures and assertion helpers for end-to-end tests.

Keep tests focused on flow; move common assertions/utilities here.
"""

from __future__ import annotations

import io
import time
from typing import Any, Dict, Tuple

import requests

from config import TestConstants
from services.redis_test_monitor import RedisTestMonitor
from shared_utils import ProjectTestUtils
from services.workflow_models import WorkflowTestFixtures
from contextlib import contextmanager

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
            # Verify project node presence by property
            project_record = session.run(
                """
                MATCH (p)
                WHERE (p.project_id IS NOT NULL AND p.project_id = $project_id)
                   OR (p.id IS NOT NULL AND toString(p.id) = $project_id)
                RETURN p
                LIMIT 1
                """,
                project_id=project_id,
            ).single()
            assert project_record is not None, f"No project node found for project {project_id}"

            # Verify HAS_DOCUMENT relationships from the anchor
            rel_record = session.run(
                """
                MATCH (p)
                WHERE (p.project_id IS NOT NULL AND p.project_id = $project_id)
                   OR (p.id IS NOT NULL AND toString(p.id) = $project_id)
                OPTIONAL MATCH (p)-[:HAS_DOCUMENT]->(d)
                RETURN collect(d) AS documents
                """,
                project_id=project_id,
            ).single()
            assert rel_record is not None, f"Project found but failed to retrieve document links for {project_id}"
            documents = rel_record["documents"]
            valid_docs = [d for d in (documents or []) if d is not None]
            assert len(valid_docs) >= min_docs, (
                f"Expected at least {min_docs} HAS_DOCUMENT relation(s), found {len(valid_docs)}"
            )

            # Verify stored document records exist for this project (as produced by GraphRAG pipeline)
            doc_count_record = session.run(
                """
                MATCH (n)
                WHERE n.project_id = $project_id AND (n.file_name IS NOT NULL OR n.title IS NOT NULL)
                RETURN count(n) AS c
                """,
                project_id=project_id,
            ).single()
            assert doc_count_record is not None and int(doc_count_record["c"] or 0) >= min_docs, (
                f"Expected at least {min_docs} stored document record(s) for project {project_id}"
            )

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
        with fixtures.neo4j_driver.session() as session:
            records = list(session.run(
                """
                SHOW INDEXES
                YIELD name, state, type, entityType, labelsOrTypes, properties, options
                WHERE type = 'VECTOR'
                RETURN name, state, type, entityType, labelsOrTypes, properties, options
                """
            ))
            def get_entity_vector_index():
                for rec in records:
                    labels = rec.get("labelsOrTypes") or []
                    props = rec.get("properties") or []
                    # Align to ingestion pipeline which indexes Chunk.embedding
                    if "Chunk" in labels and "embedding" in props:
                        return rec
                return None
            entity_vec_idx = get_entity_vector_index()
            assert entity_vec_idx is not None, "Missing vector index for Chunk(embedding)"
            assert entity_vec_idx.get("state") == "ONLINE", "Entity vector index is not ONLINE"
            index_cfg = (entity_vec_idx.get("options") or {}).get("indexConfig", {}) or {}
            dims = index_cfg.get("vector.dimensions")
            sim = index_cfg.get("vector.similarity_function")
            if isinstance(sim, str):
                sim = sim.lower()
            assert dims == 1536, "Vector index dimension must be 1536"
            assert sim == "cosine", "Vector similarity must be cosine"

    # ----- Assertions -----
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
