from __future__ import annotations

from setuptools import Extension, setup

ext_modules = [
    Extension(
        name="_binding",
        sources=[
            "src/_binding.c",
            "src/libs/tree-sitter-cobol/src/parser.c",
            "src/libs/tree-sitter-cobol/src/scanner.c",
        ],
        include_dirs=["src/libs/tree-sitter-cobol/src"],
        language="c",
    )
]

setup(ext_modules=ext_modules)
