"""Shared utilities for AI services.

This package provides reusable utilities for:
- LLM client creation and management
- Token counting and cost estimation
- JSON parsing and validation
- Citation processing and validation
- Chunk manipulation and compression
- Embedding generation with caching
- Retry policies and error handling
"""

# Intentionally no eager imports here.
#
# Many submodules have optional heavy dependencies (LLM/embedding providers, etc).
# Keeping this `__init__` import-free ensures `import utils.<something>` doesn't
# fail just because an unrelated optional dependency isn't installed.
