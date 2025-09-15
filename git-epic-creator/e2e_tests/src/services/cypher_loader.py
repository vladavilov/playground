from __future__ import annotations

from pathlib import Path
from typing import List

from neo4j import Driver


def execute_cypher_script(driver: Driver, db_name: str, script_path: Path) -> None:
    """
    Execute a Cypher script file that may include :begin / :commit markers and semicolon-delimited statements.

    - Lines starting with ':begin' or '::begin' open a transaction
    - Lines starting with ':commit' or '::commit' commit the current transaction
    - Statements are terminated by ';' and may span multiple lines
    """
    lines = script_path.read_text(encoding="utf-8").splitlines()

    def is_begin(line: str) -> bool:
        l = line.strip().lower()
        return l.startswith(":begin") or l.startswith("::begin")

    def is_commit(line: str) -> bool:
        l = line.strip().lower()
        return l.startswith(":commit") or l.startswith("::commit")

    with driver.session(database=db_name) as session:
        tx = None
        buffer: List[str] = []

        def flush_statement() -> None:
            nonlocal buffer, tx
            if not buffer:
                return
            stmt = "\n".join(buffer).strip()
            buffer = []
            if not stmt:
                return
            # Ensure a trailing semicolon isn't required by Neo4j
            if stmt.endswith(";"):
                stmt = stmt[:-1]
            if tx is None:
                session.run(stmt).consume()
            else:
                tx.run(stmt)

        for raw_line in lines:
            line = raw_line.rstrip()
            if not line:
                continue
            if is_begin(line):
                # Flush any buffered statement before starting tx
                flush_statement()
                if tx is None:
                    tx = session.begin_transaction()
                continue
            if is_commit(line):
                # Flush any buffered statement before commit
                flush_statement()
                if tx is not None:
                    tx.commit()
                    tx = None
                continue

            buffer.append(line)
            if line.strip().endswith(";"):
                flush_statement()

        # Final flush
        flush_statement()
        if tx is not None:
            tx.commit()
            tx = None
