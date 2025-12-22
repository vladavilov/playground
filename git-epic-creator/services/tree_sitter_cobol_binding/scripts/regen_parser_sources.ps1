$ErrorActionPreference = "Stop"

# Developer-only script. Runtime installs must NOT regenerate.

$TREE_SITTER_CLI_VERSION = "0.20.7"

$here = Split-Path -Parent $MyInvocation.MyCommand.Path
$root = Resolve-Path (Join-Path $here "..")
$vendor = Join-Path $root "src\libs\tree-sitter-cobol"

Push-Location $vendor
try {
  if (-not (Get-Command node -ErrorAction SilentlyContinue)) {
    throw "node is required to run tree-sitter-cli (developer-only)"
  }

  Write-Host "Regenerating parser sources using tree-sitter-cli@$TREE_SITTER_CLI_VERSION" -ForegroundColor Cyan
  npx "tree-sitter-cli@$TREE_SITTER_CLI_VERSION" generate

  # Keep the vendored grammar capsule lean for the Python wheel: the wheel build
  # only needs parser.c/scanner.c and tree_sitter/parser.h.
  Remove-Item -ErrorAction SilentlyContinue -Force (Join-Path $vendor "Cargo.toml")
  Remove-Item -ErrorAction SilentlyContinue -Force (Join-Path $vendor "binding.gyp")
  Remove-Item -ErrorAction SilentlyContinue -Recurse -Force (Join-Path $vendor "bindings")
  Remove-Item -ErrorAction SilentlyContinue -Force (Join-Path $vendor "src\grammar.json")
  Remove-Item -ErrorAction SilentlyContinue -Force (Join-Path $vendor "src\node-types.json")

  Write-Host "Done. Review changes under src/libs/.../src and commit outputs." -ForegroundColor Green
}
finally {
  Pop-Location
}

