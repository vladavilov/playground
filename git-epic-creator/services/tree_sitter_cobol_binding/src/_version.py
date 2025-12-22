from __future__ import annotations

from importlib.metadata import PackageNotFoundError, version

from _meta import DEFAULT_DIALECT_KEY, GRAMMAR_REVISIONS

__grammar_version__ = GRAMMAR_REVISIONS[DEFAULT_DIALECT_KEY]

try:
    __version__ = version("tree-sitter-cobol")
except PackageNotFoundError:  # pragma: no cover
    __version__ = "0.0.0"
