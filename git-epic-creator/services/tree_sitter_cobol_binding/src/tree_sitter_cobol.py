from __future__ import annotations

import _binding

from _meta import DEFAULT_DIALECT_KEY, DIALECT_KEYS, GRAMMAR_REVISIONS
from _version import __grammar_version__, __version__


def language() -> object:
    """Return a capsule/handle compatible with ``tree_sitter.Language(...)``."""

    return _binding.language()


def languages() -> dict[str, object]:
    """Return available dialect capsules.

    Today all dialect keys map to the same baseline grammar capsule.
    """

    cap = language()
    return {k: cap for k in DIALECT_KEYS}


def language_ibm() -> object:
    """IBM Enterprise COBOL (z/OS) dialect capsule (currently baseline alias)."""

    return language()


def language_micro_focus() -> object:
    """Micro Focus COBOL dialect capsule (currently baseline alias)."""

    return language()


def language_gnucobol() -> object:
    """GnuCOBOL dialect capsule (currently baseline alias)."""

    return language()


def grammar_revisions() -> dict[str, str]:
    """Return pinned grammar commit SHA per dialect key."""

    return dict(GRAMMAR_REVISIONS)


__all__ = [
    "DEFAULT_DIALECT_KEY",
    "DIALECT_KEYS",
    "GRAMMAR_REVISIONS",
    "__grammar_version__",
    "__version__",
    "grammar_revisions",
    "language",
    "language_gnucobol",
    "language_ibm",
    "language_micro_focus",
    "languages",
]
