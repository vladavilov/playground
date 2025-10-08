# Neo4j with Pre-baked APOC and GDS Plugins

This directory contains a custom Neo4j Docker image with pre-installed APOC and Graph Data Science (GDS) plugins.

## Directory Structure

```
neo4j_baked/
├── Dockerfile          # Custom Neo4j image definition
├── plugins/            # Directory for plugin JAR files (manual download required)
│   ├── apoc-5.26.0-core.jar                    # APOC plugin (download manually)
│   └── neo4j-graph-data-science-2.8.1.jar      # GDS plugin (download manually)
└── README.md           # This file
```

**Important:** The Docker build context is set to `./services` in docker-compose.yml, so the Dockerfile references plugins as `neo4j_baked/plugins/*.jar`.

## Neo4j Version

**Current Base Image:** `neo4j:5.26.0-community`

## Required Plugins

### 1. APOC (Awesome Procedures on Cypher)

**Version:** 5.26.0 (must match Neo4j version)

**Download URL:**
```
https://github.com/neo4j/apoc/releases/download/5.26.0/apoc-5.26.0-core.jar
```

**Direct Download Command:**
```bash
curl -L -o plugins/apoc-5.26.0-core.jar \
  https://github.com/neo4j/apoc/releases/download/5.26.0/apoc-5.26.0-core.jar
```

**Alternative:** Visit [APOC Releases](https://github.com/neo4j/apoc/releases) and download the core JAR file matching Neo4j 5.26.0.

### 2. Neo4j Graph Data Science (GDS)

**Version:** 2.8.1 (compatible with Neo4j 5.21+)

**Download URL:**
```
https://graphdatascience.ninja/neo4j-graph-data-science-2.8.1.jar
```

**Direct Download Command:**
```bash
curl -L -o plugins/neo4j-graph-data-science-2.8.1.jar \
  https://graphdatascience.ninja/neo4j-graph-data-science-2.8.1.jar
```

**Alternative:** Visit [Neo4j Download Center - GDS](https://neo4j.com/download-center/#graph-data-science) and download the appropriate version.

## Installation Instructions

### Step 1: Download Plugins

Navigate to this directory and run:

```bash
cd playground/git-epic-creator/services/neo4j_baked
```

Download APOC:
```bash
curl -L -o plugins/apoc-5.26.0-core.jar \
  https://github.com/neo4j/apoc/releases/download/5.26.0/apoc-5.26.0-core.jar
```

Download GDS:
```bash
curl -L -o plugins/neo4j-graph-data-science-2.8.1.jar \
  https://graphdatascience.ninja/neo4j-graph-data-science-2.8.1.jar
```

### Step 2: Verify Plugin Files

Ensure both JAR files are present in the `plugins/` directory:

```bash
ls -lh plugins/
```

Expected output:
```
apoc-5.26.0-core.jar                    (~10-15 MB)
neo4j-graph-data-science-2.8.1.jar      (~70-80 MB)
```

### Step 3: Build the Docker Image

From the project root directory:

```bash
cd ../..  # Back to playground/git-epic-creator
docker-compose build neo4j
```

### Step 4: Start the Service

```bash
docker-compose up -d neo4j
```

### Step 5: Verify Plugins are Loaded

Check that plugins are available:

```bash
docker exec -it neo4j_db cypher-shell -u neo4j -p neo4j123 \
  "CALL dbms.procedures() YIELD name WHERE name STARTS WITH 'apoc' OR name STARTS WITH 'gds' RETURN count(name) as plugin_count;"
```

You should see a non-zero count of procedures.

List APOC procedures:
```bash
docker exec -it neo4j_db cypher-shell -u neo4j -p neo4j123 \
  "CALL dbms.procedures() YIELD name WHERE name STARTS WITH 'apoc' RETURN name LIMIT 10;"
```

List GDS procedures:
```bash
docker exec -it neo4j_db cypher-shell -u neo4j -p neo4j123 \
  "CALL dbms.procedures() YIELD name WHERE name STARTS WITH 'gds' RETURN name LIMIT 10;"
```

## Benefits

- **Faster Startup**: No runtime plugin downloads required
- **Air-gapped Deployment**: Works without internet connectivity
- **Version Control**: Exact plugin versions are guaranteed
- **Reproducibility**: Consistent builds across environments

## Updating Neo4j or Plugins

### To Update Neo4j Version:

1. Update `FROM neo4j:X.Y.Z-community` in `Dockerfile`
2. Download matching APOC version (X.Y.Z)
3. Verify GDS compatibility with new Neo4j version
4. Update this README with new version numbers
5. Rebuild the image

### To Update Plugin Versions:

1. Download new plugin JAR files to `plugins/` directory
2. Remove old JAR files
3. Update this README with new version numbers
4. Rebuild the image

## Troubleshooting

### Plugins Not Loading

If plugins don't appear in Neo4j:

1. Check Docker build logs:
   ```bash
   docker-compose build neo4j
   ```

2. Verify plugin files exist in the container:
   ```bash
   docker exec -it neo4j_db ls -lh /var/lib/neo4j/plugins/
   ```

3. Check Neo4j logs:
   ```bash
   docker logs neo4j_db
   ```

4. Ensure security settings allow the procedures:
   ```bash
   docker exec -it neo4j_db env | grep -E "NEO4J.*procedures"
   ```

### Build Fails with "No such file or directory"

This means plugin JAR files are missing from the `plugins/` directory. Download them as per Step 1 above.

## Additional Resources

- [Neo4j Docker Documentation](https://neo4j.com/docs/operations-manual/current/docker/)
- [APOC Documentation](https://neo4j.com/labs/apoc/)
- [GDS Documentation](https://neo4j.com/docs/graph-data-science/current/)
- [Neo4j Configuration Reference](https://neo4j.com/docs/operations-manual/current/configuration/)

