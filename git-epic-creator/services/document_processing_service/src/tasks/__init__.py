# Tasks package
# Import task functions and app getter
from .document_tasks import process_project_documents_task, get_celery_app_with_tasks

__all__ = ['process_project_documents_task', 'get_celery_app_with_tasks']