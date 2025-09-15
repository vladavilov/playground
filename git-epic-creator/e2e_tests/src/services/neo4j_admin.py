from __future__ import annotations

from typing import List, Dict

from neo4j import GraphDatabase, Driver
from neo4j.exceptions import ClientError


UNSUPPORTED_ADMIN_CODE = "Neo.ClientError.Statement.UnsupportedAdministrationCommand"


def supports_multi_db(driver: Driver) -> bool:
    """Return True if SHOW DATABASES is supported (multi-DB environments)."""
    try:
        with driver.session(database="system") as session:
            session.run("SHOW DATABASES").consume()
        return True
    except ClientError as e:
        # Community edition or restricted environments
        if getattr(e, "code", "") == UNSUPPORTED_ADMIN_CODE:
            return False
        raise


def list_databases(driver: Driver) -> List[Dict[str, str]]:
    """Return rows from SHOW DATABASES using the system database.

    Returns empty list if unsupported.
    """
    try:
        with driver.session(database="system") as session:
            result = session.run("SHOW DATABASES")
            return [dict(record) for record in result]
    except ClientError as e:
        if getattr(e, "code", "") == UNSUPPORTED_ADMIN_CODE:
            return []
        raise


def database_exists(driver: Driver, name: str) -> bool:
    rows = list_databases(driver)
    for row in rows:
        if row.get("name") == name:
            return True
    return False


def ensure_database(driver: Driver, name: str, wait_seconds: int = 30) -> None:
    """Create the database if supported; otherwise no-op (use default DB)."""
    try:
        with driver.session(database="system") as session:
            session.run(f"CREATE DATABASE {name} IF NOT EXISTS;")
            session.run(f"WAIT FOR DATABASE {name} WAIT {wait_seconds} SECONDS;")
    except ClientError as e:
        if getattr(e, "code", "") == UNSUPPORTED_ADMIN_CODE:
            # Single DB environment; caller should use default database
            return
        raise


def drop_database(driver: Driver, name: str) -> None:
    try:
        with driver.session(database="system") as session:
            session.run(f"DROP DATABASE {name} IF EXISTS;")
    except ClientError as e:
        if getattr(e, "code", "") == UNSUPPORTED_ADMIN_CODE:
            return
        raise
