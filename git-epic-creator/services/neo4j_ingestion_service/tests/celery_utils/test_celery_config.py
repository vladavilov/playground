import re


def test_celery_worker_configuration_and_task_registration():
    from worker.celery_app import get_task_validation_status, celery_app

    status = get_task_validation_status()

    assert "tasks.neo4j_ingestion.run_graphrag_job" in status["expected_tasks"]
    assert status["all_tasks_registered"] is True

    assert celery_app.conf.worker_prefetch_multiplier == 1

    assert getattr(celery_app.conf, "task_acks_late", False) is True

    routes = celery_app.conf.task_routes or {}
    route_keys = list(routes.keys()) if isinstance(routes, dict) else []
    assert any(
        k.startswith("tasks.neo4j_ingestion") for k in route_keys
    ) or any(
        re.match(r"tasks\\.neo4j_ingestion(\\..*)?", k or "") for k in route_keys
    ), "Task routes should route tasks.neo4j_ingestion.* to neo4j_ingestion queue"


