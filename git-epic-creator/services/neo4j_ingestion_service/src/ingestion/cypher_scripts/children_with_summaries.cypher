MATCH (p:__Community__ {id: $pid})
OPTIONAL MATCH (p)<-[:IN_COMMUNITY]-(child:__Community__)
WITH p, collect(child) AS cs
WITH p, [c IN cs WHERE c IS NOT NULL | {id: c.id, summary: coalesce(c.summary, ''), numberOfChildren: size((c)<-[:IN_COMMUNITY]-(:__Community__))}] AS communities
RETURN p.id AS communityId, communities


