// Transactional "bundle" ingestion for GraphRAG parquet outputs.
// Neo4j 5.26 note: importing variables into CALL subqueries via `WITH` is deprecated;
// we use the variable-scope clause: `CALL (p) { ... }`.

MERGE (p:__Project__ {id: $project_id})
WITH p

CALL (p) {
  WITH p, $documents AS docs
  WITH p, docs WHERE size(docs) = 0
  RETURN 0 AS documents_created
  UNION
  WITH p, $documents AS docs
  WITH p, docs WHERE size(docs) > 0
  UNWIND docs AS value
  MERGE (d:__Document__ {project_id: $project_id, id: value.id})
  SET d += value, d.project_id = $project_id
  // Ensure title is populated: fallback to metadata.file_name if title is empty
  WITH p, d, value
  SET d.title = CASE
    WHEN d.title IS NULL OR d.title = ''
    THEN coalesce(
      CASE WHEN value.metadata.file_name IS NOT NULL AND value.metadata.file_name <> ''
           THEN value.metadata.file_name
           ELSE NULL END,
      d.id
    )
    ELSE d.title
  END
  MERGE (d)-[:IN_PROJECT]->(p)
  RETURN count(DISTINCT d) AS documents_created
}

WITH p, documents_created
CALL (p) {
  WITH p, $chunks AS chunks
  WITH p, chunks WHERE size(chunks) = 0
  RETURN 0 AS chunks_created
  UNION
  WITH p, $chunks AS chunks
  WITH p, chunks WHERE size(chunks) > 0
  UNWIND chunks AS v
  MERGE (c:__Chunk__ {project_id: $project_id, id: v.id})
  SET c += v,
      c.project_id = $project_id,
      c.text_hash = CASE
        WHEN v.text IS NOT NULL AND v.text <> ''
        THEN substring(apoc.util.sha256([v.text]), 0, 16)
        ELSE NULL
      END
  MERGE (c)-[:IN_PROJECT]->(p)
  // Create HAS_CHUNK relationships scoped to the project
  WITH c, p, coalesce(v.document_ids,[]) AS dids
  UNWIND dids AS did
  WITH c, p, did WHERE did IS NOT NULL
  OPTIONAL MATCH (d:__Document__)-[:IN_PROJECT]->(p)
  WHERE d.id = did
  FOREACH (_ IN CASE WHEN d IS NOT NULL THEN [1] ELSE [] END |
    MERGE (d)-[:HAS_CHUNK]->(c)
  )
  RETURN count(DISTINCT c) AS chunks_created
}

WITH p, documents_created, chunks_created
CALL (p) {
  WITH p, $entities AS ents
  WITH p, ents WHERE size(ents) = 0
  RETURN 0 AS entities_created
  UNION
  WITH p, $entities AS ents
  WITH p, ents WHERE size(ents) > 0
  UNWIND ents AS value
  WITH
    p,
    value,
    toUpper(trim(coalesce(value.norm_title, value.title))) AS norm_title,
    trim(value.description) AS description

  // Deterministic entity matching by priority: id > norm_title > description.
  OPTIONAL MATCH (e_id:__Entity__ {project_id: $project_id, id: value.id})
  OPTIONAL MATCH (e_nt:__Entity__ {project_id: $project_id, norm_title: norm_title})-[:IN_PROJECT]->(p)
  WHERE e_id IS NULL AND norm_title IS NOT NULL AND norm_title <> ''
  WITH p, value, norm_title, description, e_id, e_nt
  ORDER BY e_nt.id
  WITH p, value, norm_title, description, e_id, head(collect(e_nt)) AS e_nt

  OPTIONAL MATCH (e_desc:__Entity__ {project_id: $project_id, description: description})-[:IN_PROJECT]->(p)
  WHERE e_id IS NULL AND e_nt IS NULL AND description IS NOT NULL AND description <> ''
  WITH p, value, norm_title, description, e_id, e_nt, e_desc
  ORDER BY e_desc.id
  WITH p, value, norm_title, description, e_id, e_nt, head(collect(e_desc)) AS e_desc

  WITH p, value, norm_title, description, coalesce(e_id, e_nt, e_desc) AS matched_entity
  WITH p, value, norm_title, coalesce(matched_entity.id, value.id) AS target_id
  MERGE (entity:__Entity__ {project_id: $project_id, id: target_id})
  ON CREATE SET
    entity = value,
    entity.project_id = $project_id,
    entity.norm_title = norm_title,
    entity.merged_ids = [value.id]
  ON MATCH SET
    entity += value,
    entity.project_id = $project_id,
    entity.id = target_id,
    entity.norm_title = coalesce(norm_title, entity.norm_title),
    entity.merged_ids =
      CASE
        WHEN value.id IN coalesce(entity.merged_ids, [target_id])
        THEN entity.merged_ids
        ELSE coalesce(entity.merged_ids, [target_id]) + [value.id]
      END

  MERGE (entity)-[:IN_PROJECT]->(p)

  // Create HAS_ENTITY relationships from chunks scoped to project
  WITH entity, p, value
  UNWIND coalesce(value.text_unit_ids, []) AS chunk_id
  OPTIONAL MATCH (c:__Chunk__ {project_id: $project_id, id: chunk_id})-[:IN_PROJECT]->(p)
  FOREACH (_ IN CASE WHEN c IS NOT NULL THEN [1] ELSE [] END |
    MERGE (c)-[:HAS_ENTITY]->(entity)
  )

  RETURN count(DISTINCT entity) AS entities_created
}

WITH p, documents_created, chunks_created, entities_created
CALL (p) {
  WITH p, $relationships AS rels
  WITH p, rels WHERE size(rels) = 0
  RETURN 0 AS relationships_processed
  UNION
  WITH p, $relationships AS rels
  WITH p, rels WHERE size(rels) > 0
  UNWIND rels AS value
  WITH p, value, toUpper(coalesce(value.source, '')) AS source_key, toUpper(coalesce(value.target, '')) AS target_key
  WHERE source_key <> '' AND target_key <> ''

  // Find source entity with project scoping
  CALL (source_key, p) {
    OPTIONAL MATCH (s:__Entity__)-[:IN_PROJECT]->(p)
    WHERE toUpper(coalesce(s.norm_title, s.title, '')) = source_key
    WITH s
    ORDER BY s.id
    LIMIT 1
    RETURN s
  }
  // Find target entity with project scoping
  CALL (target_key, p) {
    OPTIONAL MATCH (t:__Entity__)-[:IN_PROJECT]->(p)
    WHERE toUpper(coalesce(t.norm_title, t.title, '')) = target_key
    WITH t
    ORDER BY t.id
    LIMIT 1
    RETURN t
  }

  WITH p, value, s, t
  WHERE s IS NOT NULL AND t IS NOT NULL
  WITH value, s, t, coalesce(value.text_unit_ids, []) AS new_text_units
  MERGE (s)-[rel:RELATED]->(t)
  ON CREATE SET
    rel.project_id = $project_id,
    rel.id = coalesce(value.id, $project_id + '|' + s.id + '|' + t.id),
    rel.source = value.source,
    rel.target = value.target,
    rel.source_id = s.id,
    rel.target_id = t.id,
    rel.description = value.description,
    rel.weight = value.weight,
    rel.combined_degree = value.combined_degree,
    rel.text_unit_ids = new_text_units
  ON MATCH SET
    rel.project_id = $project_id,
    rel.description = coalesce(value.description, rel.description),
    rel.source = coalesce(value.source, rel.source),
    rel.target = coalesce(value.target, rel.target),
    rel.source_id = s.id,
    rel.target_id = t.id,
    rel.weight = coalesce(value.weight, rel.weight),
    rel.combined_degree = coalesce(value.combined_degree, rel.combined_degree)

  // Always count merged relationships; only update text_unit_ids when we have ids.
  // (Previous version filtered out rows with empty ids and undercounted.)
  WITH rel, [id IN (coalesce(rel.text_unit_ids, []) + new_text_units) WHERE id IS NOT NULL] AS combined_ids
  SET rel.text_unit_ids =
    CASE
      WHEN size(combined_ids) = 0 THEN coalesce(rel.text_unit_ids, [])
      ELSE apoc.coll.toSet(combined_ids)
    END
  RETURN count(DISTINCT rel) AS relationships_processed
}

WITH p, documents_created, chunks_created, entities_created, relationships_processed
CALL (p) {
  WITH p, $community_reports AS reps
  WITH p, reps WHERE size(reps) = 0
  RETURN 0 AS community_reports_created
  UNION
  WITH p, $community_reports AS reps
  WITH p, reps WHERE size(reps) > 0
  UNWIND reps AS value
  MERGE (c:__Community__ {community:value.community, project_id: $project_id})
  SET c.id = toString(value.community) + '_' + $project_id,
      c.level = value.level,
      c.title = value.title,
      c.rank = value.rank,
      c.rating_explanation = value.rating_explanation,
      c.full_content = value.full_content,
      c.summary = value.summary,
      c.full_content_json = value.full_content_json
  MERGE (c)-[:IN_PROJECT]->(p)
  RETURN count(DISTINCT c) AS community_reports_created
}

WITH p, documents_created, chunks_created, entities_created, relationships_processed, community_reports_created
CALL (p) {
  WITH p, $communities AS comms
  WITH p, comms WHERE size(comms) = 0
  RETURN 0 AS communities_created
  UNION
  WITH p, $communities AS comms
  WITH p, comms WHERE size(comms) > 0
  UNWIND comms AS value
  MERGE (c:__Community__ {community: value.community, project_id: $project_id})
  SET c.id = toString(value.community) + '_' + $project_id, c += value
  MERGE (c)-[:IN_PROJECT]->(p)

  // Match entities by entity_ids (supports merged_ids lookup and norm_title fallback)
  WITH c, p, value
  WITH c, p, value, [eid IN coalesce(value.entity_ids, []) WHERE eid IS NOT NULL] AS raw_eids
  WITH c, p, value, [eid IN raw_eids | toUpper(trim(toString(eid)))] AS eids
  CALL (p, eids) {
    WITH p, eids
    WITH p, eids WHERE size(eids) > 0

    CALL (p, eids) {
      WITH p, eids
      UNWIND eids AS eid
      OPTIONAL MATCH (e:__Entity__ {project_id: p.id, id: eid})-[:IN_PROJECT]->(p)
      RETURN collect(DISTINCT e) AS direct
    }

    CALL (p, eids) {
      WITH p, eids
      UNWIND eids AS eid
      OPTIONAL MATCH (e:__Entity__ {project_id: p.id, norm_title: eid})-[:IN_PROJECT]->(p)
      RETURN collect(DISTINCT e) AS by_norm_title
    }

    CALL (p, eids) {
      WITH p, eids
      UNWIND eids AS eid
      OPTIONAL MATCH (e:__Entity__ {project_id: p.id})-[:IN_PROJECT]->(p)
      WHERE eid IN coalesce(e.merged_ids, [])
      RETURN collect(DISTINCT e) AS by_merged_id
    }

    WITH apoc.coll.toSet(coalesce(direct, []) + coalesce(by_norm_title, []) + coalesce(by_merged_id, [])) AS merged
    RETURN [e IN merged WHERE e IS NOT NULL] AS entities_from_ids
    UNION
    WITH p, eids WHERE size(eids) = 0
    RETURN [] AS entities_from_ids
  }
  WITH c, p, value, entities_from_ids

  // Match chunks and their entities from text_unit_ids
  WITH c, p, value, entities_from_ids
  OPTIONAL MATCH (ch:__Chunk__)-[:IN_PROJECT]->(p)
  WHERE size(coalesce(value.text_unit_ids, [])) > 0 AND ch.id IN value.text_unit_ids
  OPTIONAL MATCH (ch)-[:HAS_ENTITY]->(e_ch:__Entity__)-[:IN_PROJECT]->(p)
  WITH c, p, entities_from_ids,
       collect(DISTINCT ch) AS related_chunks,
       collect(DISTINCT e_ch) AS entities_from_chunks
  WITH c, p,
       CASE WHEN size(entities_from_ids) > 0 THEN entities_from_ids
            WHEN size(entities_from_chunks) > 0 THEN entities_from_chunks
            ELSE [] END AS effective_entities,
       related_chunks

  // Update entity_ids property with canonical IDs when entities found
  FOREACH (_ IN CASE WHEN size(effective_entities) > 0 THEN [1] ELSE [] END |
    SET c.entity_ids = [e IN effective_entities | e.id]
  )

  // Create entity->community relationships
  FOREACH (e IN effective_entities |
    MERGE (e)-[:IN_COMMUNITY]->(c)
  )

  // Link chunks that contain these entities
  WITH c, p, effective_entities, related_chunks
  UNWIND CASE WHEN effective_entities = [] THEN [null] ELSE effective_entities END AS e
  WITH c, p, e, related_chunks WHERE e IS NOT NULL
  OPTIONAL MATCH (ch2:__Chunk__)-[:HAS_ENTITY]->(e)
  WHERE (ch2)-[:IN_PROJECT]->(p)
  FOREACH (_ IN CASE WHEN ch2 IS NOT NULL THEN [1] ELSE [] END |
    MERGE (ch2)-[:IN_COMMUNITY]->(c)
  )

  // Link chunks from text_unit_ids directly
  WITH DISTINCT c, p, related_chunks
  FOREACH (ch3 IN related_chunks |
    MERGE (ch3)-[:IN_COMMUNITY]->(c)
  )

  RETURN count(DISTINCT c) AS communities_created
}

RETURN
  documents_created,
  chunks_created,
  entities_created,
  relationships_processed,
  community_reports_created,
  communities_created


