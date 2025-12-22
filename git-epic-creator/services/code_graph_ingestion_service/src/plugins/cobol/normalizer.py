"""COBOL preprocessing.

Produces:
- evidence stream: verbatim physical lines
- parse stream: normalized logical lines suitable for Tree-sitter parsing
- mapping: logical line -> physical line span
"""

from __future__ import annotations

import re
from dataclasses import dataclass


_RE_FREE = re.compile(r">>\s*SOURCE\s+FORMAT\s+FREE", re.IGNORECASE)
_RE_FIXED = re.compile(r">>\s*SOURCE\s+FORMAT\s+FIXED", re.IGNORECASE)


@dataclass(frozen=True)
class PreprocessResult:
    physical_lines: list[str]
    logical_lines: list[str]
    logical_spans: list[tuple[int, int]]  # (start_physical_line, end_physical_line)
    parse_bytes: bytes


def preprocess_cobol_bytes(raw: bytes) -> PreprocessResult:
    """Preprocess COBOL bytes into evidence + parse streams.

    NOTE: Encoding detection/EBCDIC decode is out-of-scope for now; callers must
    pass decoded bytes (typically UTF-8).
    """

    # Evidence: preserve physical lines verbatim (but normalize newlines for internal processing).
    text = raw.decode("utf-8", errors="replace")
    text = text.replace("\r\n", "\n").replace("\r", "\n")
    physical_lines = text.split("\n")
    # If the file ended with a newline, split() leaves a trailing empty line; drop it for stability.
    if physical_lines and physical_lines[-1] == "":
        physical_lines = physical_lines[:-1]

    mode = "FIXED"
    logical_lines: list[str] = []
    logical_spans: list[tuple[int, int]] = []

    cur_parts: list[str] = []
    cur_span: tuple[int, int] | None = None

    def flush_current() -> None:
        nonlocal cur_parts, cur_span
        if not cur_parts or cur_span is None:
            cur_parts = []
            cur_span = None
            return
        logical_lines.append("".join(cur_parts))
        logical_spans.append(cur_span)
        cur_parts = []
        cur_span = None

    i = 0
    while i < len(physical_lines):
        line_no = i + 1
        line = physical_lines[i]

        # Mode switches apply to subsequent lines.
        if _RE_FREE.search(line):
            mode_next = "FREE"
        elif _RE_FIXED.search(line):
            mode_next = "FIXED"
        else:
            mode_next = mode

        kind, content, is_cont = _classify_and_extract(mode, line)

        if kind == "comment":
            flush_current()
            # Keep a placeholder line in the parse stream so spans are stable.
            logical_lines.append("")
            logical_spans.append((line_no, line_no))
            mode = mode_next
            i += 1
            continue

        # Normalize inline `*>` comments for FREE mode (common enterprise behavior).
        content = _strip_inline_comment_to_eol(content)

        if not cur_parts:
            cur_parts = [content]
            cur_span = (line_no, line_no)
        elif is_cont:
            cur_parts.append(content)
            cur_span = (cur_span[0], line_no) if cur_span else (line_no, line_no)
        else:
            flush_current()
            cur_parts = [content]
            cur_span = (line_no, line_no)

        mode = mode_next
        i += 1

    flush_current()

    # Collapse EXEC ... END-EXEC blocks into a single opaque logical line.
    logical_lines, logical_spans = _collapse_exec_blocks(logical_lines, logical_spans)

    parse_stream = "\n".join(logical_lines) + "\n"
    return PreprocessResult(
        physical_lines=physical_lines,
        logical_lines=logical_lines,
        logical_spans=logical_spans,
        parse_bytes=parse_stream.encode("utf-8", errors="replace"),
    )


def _strip_inline_comment_to_eol(s: str) -> str:
    idx = s.find("*>")
    if idx >= 0:
        return s[:idx]
    return s


def _classify_and_extract(mode: str, line: str) -> tuple[str, str, bool]:
    """Return (kind, content, is_continuation)."""

    if mode == "FIXED":
        padded = line + (" " * (80 - len(line))) if len(line) < 80 else line
        indicator = padded[6] if len(padded) > 6 else " "
        if indicator in ("*", "/"):
            return "comment", "", False
        if indicator.upper() == "D":
            return "comment", "", False
        content = padded[7:72] if len(padded) >= 72 else padded[7:]
        is_cont = indicator == "-"
        return "code", content.rstrip("\n"), is_cont

    # FREE
    stripped = line.lstrip()
    if stripped.startswith("*"):
        return "comment", "", False
    # Free-format continuation is dialect-specific; keep minimal deterministic behavior:
    # treat a line starting with '-' (after whitespace) as continuation.
    if stripped.startswith("-"):
        return "code", stripped[1:], True
    return "code", line, False


def _collapse_exec_blocks(
    logical_lines: list[str], logical_spans: list[tuple[int, int]]
) -> tuple[list[str], list[tuple[int, int]]]:
    out_lines: list[str] = []
    out_spans: list[tuple[int, int]] = []

    i = 0
    while i < len(logical_lines):
        line = logical_lines[i]
        span = logical_spans[i]

        if line.lstrip().upper().startswith("EXEC "):
            start_span = span[0]
            end_span = span[1]
            j = i + 1
            while j < len(logical_lines):
                end_span = logical_spans[j][1]
                if "END-EXEC" in logical_lines[j].upper():
                    j += 1
                    break
                j += 1
            out_lines.append("EXEC_BLOCK.")
            out_spans.append((start_span, end_span))
            i = j
            continue

        out_lines.append(line)
        out_spans.append(span)
        i += 1

    return out_lines, out_spans


