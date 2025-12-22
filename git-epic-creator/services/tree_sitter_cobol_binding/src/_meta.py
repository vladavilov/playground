# Golden, deterministic metadata for the wheel.

DEFAULT_DIALECT_KEY = "cobol"

# Dialect keys we expose in the public API.
# For now, they all map to the same baseline grammar capsule. This keeps the
# wheel deterministic while the consuming pipeline does the heavy lifting
# (fixed/free normalization, directives, COPY/REPLACE, EXEC blocks).
DIALECT_KEYS: tuple[str, ...] = (
    "cobol",
    "ibm",
    "micro_focus",
    "gnucobol",
)

# Pinned grammar revision(s) embedded into this wheel.
# Update only with corpus-backed tests.
GRAMMAR_REVISIONS: dict[str, str] = {
    "cobol": "8ba6692cc3c2bded0693d198936c6e26e6501230",
    "ibm": "8ba6692cc3c2bded0693d198936c6e26e6501230",
    "micro_focus": "8ba6692cc3c2bded0693d198936c6e26e6501230",
    "gnucobol": "8ba6692cc3c2bded0693d198936c6e26e6501230",
}
