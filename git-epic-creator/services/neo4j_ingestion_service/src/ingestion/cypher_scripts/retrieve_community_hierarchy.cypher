MATCH (p:`__Community__`)
WITH p,
  [ (p)<-[:IN_COMMUNITY]-(child:`__Community__`) |
    {
      id: child.id,
      summary: coalesce(child.summary, ""),
      numberOfChildren: size( (child)<-[:IN_COMMUNITY]-(:`__Community__`) )
    }
  ] AS communities
RETURN p.id AS communityId, communities
ORDER BY p.level ASC, communityId ASC
