# Neo4j Community Edition with Pre-baked APOC and GDS Plugins
# Base image: Latest Neo4j Community Edition
FROM neo4j:5.26.8-community

# Copy pre-downloaded plugin JAR files into the plugins directory
# Plugins must be manually placed in ./plugins/ before building
# Path is relative to build context (./services from docker-compose.yml)
COPY --chown=neo4j:neo4j --chmod=0644 neo4j_baked/plugins/*.jar /var/lib/neo4j/plugins/

# Environment variables for plugin configuration
# These enable unrestricted and allowlisted procedures
ENV NEO4J_dbms_security_procedures_unrestricted=apoc.*,gds.* \
    NEO4J_dbms_security_procedures_allowlist=apoc.*,gds.* \
    NEO4J_ACCEPT_LICENSE_AGREEMENT=yes

# Expose standard Neo4j ports
# 7474: HTTP
# 7687: Bolt
EXPOSE 7474 7687

# Health check to verify Neo4j is responding
HEALTHCHECK --interval=30s --timeout=10s --retries=5 --start-period=40s \
  CMD cypher-shell -u neo4j -p ${NEO4J_AUTH##*/} "RETURN 1;" || exit 1

# Use the default Neo4j entrypoint
# No need to override CMD/ENTRYPOINT - inherit from base image

