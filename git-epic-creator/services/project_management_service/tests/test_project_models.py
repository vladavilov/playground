import pytest
from pydantic import ValidationError

from models.project_rest import ProjectSet


def test_project_set_accepts_multiple_repo_urls():
    v = ProjectSet(
        name="P",
        gitlab_repository_urls=[
            "https://gitlab.example.com/group/repo.git",
            "git@gitlab.example.com:group/repo.git",
        ],
    )
    assert v.gitlab_repository_urls and len(v.gitlab_repository_urls) == 2


def test_project_set_rejects_duplicate_repo_urls():
    with pytest.raises(ValidationError):
        ProjectSet(
            name="P",
            gitlab_repository_urls=[
                "https://gitlab.example.com/group/repo.git",
                "https://gitlab.example.com/group/repo.git",
            ],
        )


