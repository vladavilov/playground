#!/usr/bin/env bash
set -euo pipefail

# Developer-only script. Runtime installs must NOT regenerate.

TREE_SITTER_CLI_VERSION="0.20.7"
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
vendor="$root/src/libs/tree-sitter-cobol"

command -v node >/dev/null 2>&1 || { echo "node is required to run tree-sitter-cli (developer-only)"; exit 1; }

cd "$vendor"
echo "Regenerating parser sources using tree-sitter-cli@$TREE_SITTER_CLI_VERSION"

npx "tree-sitter-cli@$TREE_SITTER_CLI_VERSION" generate

# Keep the vendored grammar capsule lean for the Python wheel: the wheel build
# only needs parser.c/scanner.c and tree_sitter/parser.h.
rm -f "$vendor/Cargo.toml" "$vendor/binding.gyp"
rm -rf "$vendor/bindings"
rm -f "$vendor/src/grammar.json" "$vendor/src/node-types.json"

echo "Done. Review changes under src/libs/.../src and commit outputs."

