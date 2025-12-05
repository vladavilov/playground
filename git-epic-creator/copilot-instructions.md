# Functional Requirements Extractor

Extract functional requirements from source code into `journal.tmp`. Stop after journal creation.

## Execution

1. Create empty `journal.tmp`
2. List directories recursively, identify source files (`.py`, `.ts`, `.js`, `.bicep`, `.yaml`, `.cypher`)
3. Skip: `venv/`, `node_modules/`, `__pycache__/`, `*.egg-info/`, `build/`, `dist/`
4. For each file:
   - Read content
   - Extract functional requirements — WHAT the code does
   - Append to `journal.tmp`
5. Output `[DONE]` and stop

## journal.tmp Format

```
=== FILE: path/to/file.py ===
[FR]: User can authenticate via OAuth2 token
[FR]: System validates document format before processing
DEPS: package1==1.0.0, package2>=2.0
CONFIG: VAR_NAME | description | required
CONFIG: OPTIONAL_VAR | description | optional
API: POST /endpoint | request body | response | description
MODEL: ClassName | field1:type, field2:type | relationships
INFRA: resource_type | specification

=== FILE: path/to/another.py ===
[FR]: System ingests PDF documents and extracts text
DEPS: pdfplumber>=0.7.0
CONFIG: MAX_FILE_SIZE | max upload bytes | required
```

## What to Extract

**[FR] Functional Requirements:**
- User-facing capabilities, business logic, workflows, validations, error behaviors

**DEPS:** package==version or package>=version

**CONFIG:** variable | purpose | required/optional

**API:** method path | request schema | response schema | description

**MODEL:** name | fields with types | foreign keys or relationships

**INFRA:** Azure/K8s resources, compute specs, storage requirements

## Progress Output

```
[SCAN] path/to/file.py → +N requirements
[DONE] journal.tmp created | X files | Y requirements
```
