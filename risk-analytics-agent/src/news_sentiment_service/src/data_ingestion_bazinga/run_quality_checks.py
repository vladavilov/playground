"""
Usage::

    python run_quality_checks.py
"""

from __future__ import annotations

import subprocess
from pathlib import Path

ROOT = Path(__file__).parent
ARTEFACT_FILE = ROOT / "artefact_quality_report.txt"
COV_HTML_DIR = ROOT / "coverage_html"


def _run(cmd: list[str], desc: str) -> str:
    """Execute *cmd* and return its stdout+stderr text."""
    result = subprocess.run(cmd, capture_output=True, text=True)
    header = f"\n{'='*80}\n{desc}\n{'='*80}\n"
    return header + result.stdout + result.stderr


def main() -> None:  # pragma: no cover
    sections: list[str] = []

    sections.append(
        _run(["black", "--check", "--diff", str(ROOT)], "Black (formatting check)")
    )
    sections.append(_run(["ruff", str(ROOT)], "Ruff (lint + static analysis)"))

    if COV_HTML_DIR.exists():
        import shutil

        shutil.rmtree(COV_HTML_DIR)

    cov_cmd = [
        "pytest",
        "--cov",
        "news_sentiment_service.src.data_ingestion_bazinga",
        "--cov-report",
        "term-missing",
        "--cov-report",
        f"html:{COV_HTML_DIR}",
    ]
    sections.append(_run(cov_cmd, "Pytest (unit tests + coverage)"))

    ARTEFACT_FILE.write_text("".join(sections))
    print(f"Quality report written to {ARTEFACT_FILE.relative_to(ROOT)}")
    print(f"HTML coverage generated at {COV_HTML_DIR.relative_to(ROOT)}")


if __name__ == "__main__":
    main()
