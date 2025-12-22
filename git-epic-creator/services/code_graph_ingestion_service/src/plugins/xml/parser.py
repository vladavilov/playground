"""Minimal XML extraction for wiring."""

from __future__ import annotations

import re
from dataclasses import dataclass


_RE_CLASS_ATTR = re.compile(r'\bclass\s*=\s*"([^"]+)"', re.IGNORECASE)


@dataclass(frozen=True)
class XmlWiring:
    class_names: list[str]


def extract_class_attributes(xml_text: str) -> XmlWiring:
    names = [m.group(1).strip() for m in _RE_CLASS_ATTR.finditer(xml_text)]
    # Deterministic de-dup
    out: list[str] = []
    seen = set()
    for n in names:
        if n in seen:
            continue
        seen.add(n)
        out.append(n)
    return XmlWiring(class_names=out)



