from __future__ import annotations

import os
import re
from pathlib import Path

root = Path(r"c:\Users\vlada\cobol-migration-poc\risk-analytics\playground\git-epic-creator\services\neo4j_repository_service")
queries_dir = root / "queries"
src_dir = root / "src"

# Collect all cypher keys.
all_keys: dict[str, Path] = {}
for p in queries_dir.rglob("*.cypher"):
    rel = p.relative_to(queries_dir)
    key = rel.with_suffix("").as_posix()  # stable `/`
    all_keys[key] = p

# Collect referenced keys from Rust source string literals.
used_keys: set[str] = set()
string_pat = re.compile(r'"((?:code_graph|requirements_graph|schema)/[^\"]+)"')
for p in src_dir.rglob("*.rs"):
    txt = p.read_text(encoding="utf-8")
    used_keys.update(string_pat.findall(txt))

# init_schema uses all schema/*
schema_keys = {k for k in all_keys if k.startswith("schema/")}
used_keys |= schema_keys

unused = sorted(set(all_keys) - used_keys)
print(f"Total cypher files: {len(all_keys)}")
print(f"Used keys (incl schema/*): {len(used_keys)}")
print(f"Unused cypher files by Rust code: {len(unused)}")
for k in unused:
    print(k)

# For safety, scan other services for references to these keys or file paths.
services_root = root.parent

def is_referenced_elsewhere(key: str, path: Path) -> bool:
    # key string or filename (without extension) or relative posix path with extension
    rel_with_ext = path.relative_to(root).as_posix()
    filename = path.name
    stem = path.stem

    needles = [key, rel_with_ext, filename, stem]

    for p in services_root.rglob("*"):
        if not p.is_file():
            continue
        # skip large binaries and target dirs
        if "\\target\\" in str(p) or "/target/" in p.as_posix():
            continue
        if p.suffix.lower() in {".exe", ".dll", ".pdb", ".png", ".jpg", ".jpeg", ".gif", ".zip"}:
            continue
        try:
            data = p.read_text(encoding="utf-8", errors="ignore")
        except Exception:
            continue
        for n in needles:
            if n and n in data:
                return True
    return False

still_referenced = []
for k in unused:
    p = all_keys[k]
    if is_referenced_elsewhere(k, p):
        still_referenced.append(k)

print("\nUnused-but-referenced-elsewhere:")
for k in still_referenced:
    print(k)

safe_to_delete = [k for k in unused if k not in set(still_referenced)]
print(f"\nSafe to delete (no refs in services/*): {len(safe_to_delete)}")
for k in safe_to_delete:
    print(k)
