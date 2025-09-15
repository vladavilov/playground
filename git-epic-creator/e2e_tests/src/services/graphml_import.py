from __future__ import annotations

from pathlib import Path
from typing import Optional

from neo4j import Driver
from neo4j.exceptions import ClientError


APOC_FS_ERROR_HINT = (
    "APOC file import failed. Ensure apoc.import.file.enabled=true and apoc.import.file.use_neo4j_config=true,\n"
    "and that the path is under the Neo4j import directory."
)


def import_graphml_from_path(driver: Driver, db_name: str, graphml_path: Path, read_labels: bool = True) -> None:
    """
    Import a GraphML file into the specified database.

    Preferred strategy:
    - Compress text in-query: WITH apoc.util.compress($text,{compression:'DEFLATE'}) AS xmlCompressed
      CALL apoc.import.graphml(xmlCompressed,{compression:'DEFLATE',readLabels:true})

    Fallbacks (if APOC version lacks this signature):
    - Streaming import (passing text with {stream:true})
    - File-URI import
    """
    graphml_text = graphml_path.read_text(encoding="utf-8")

    # Preferred: compressed text import (avoids apoc.import.file.enabled)
    try:
        with driver.session(database=db_name) as session:
            session.run(
                (
                    "WITH apoc.util.compress($text, {compression:'DEFLATE'}) AS xmlCompressed "
                    "CALL apoc.import.graphml(xmlCompressed, {compression:'DEFLATE', readLabels:$readLabels}) "
                    "YIELD source, format, nodes, relationships, properties "
                    "RETURN source, format, nodes, relationships, properties"
                ),
                text=graphml_text,
                readLabels=bool(read_labels),
            ).consume()
            return
    except ClientError:
        pass

    # Fallback: stream-based import
    try:
        with driver.session(database=db_name) as session:
            session.run(
                "CALL apoc.import.graphml($data, $config)",
                data=graphml_text,
                config={"stream": True, "readLabels": read_labels},
            ).consume()
            return
    except ClientError:
        pass

    # Fallback: file-URI import
    file_uri = graphml_path.resolve().as_uri()
    with driver.session(database=db_name) as session:
        session.run(
            "CALL apoc.import.graphml($path, $config)",
            path=file_uri,
            config={"readLabels": read_labels},
        ).consume()


def import_graphml_from_string(driver: Driver, db_name: str, graphml_xml: str, read_labels: bool = True) -> None:
    """
    Fallback import using streaming XML content, if filesystem import is not available.
    """
    with driver.session(database=db_name) as session:
        # apoc.import.graphml requires filesystem; as a fallback, use apoc.load.xml to parse and custom load.
        # For this project, prefer filesystem import. This function is a placeholder for potential extension.
        raise RuntimeError(
            "Streaming GraphML import not implemented. Place the file in Neo4j import dir and use import_graphml_from_path."
        )
