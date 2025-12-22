$ErrorActionPreference = "Stop"

$here = Split-Path -Parent $MyInvocation.MyCommand.Path
$root = Resolve-Path (Join-Path $here "..")

Write-Host "Building sdist+wheel (local platform)" -ForegroundColor Cyan
python -m pip install -U pip
python -m pip install "build==1.2.2.post1" "cibuildwheel==2.22.0" "pytest==8.3.4" "tree-sitter==0.25.1"

Push-Location $root
try {
  python -m build
  Write-Host "Building wheels via cibuildwheel" -ForegroundColor Cyan
  $env:CIBW_BUILD = "cp312-*"
  python -m cibuildwheel --output-dir wheelhouse
}
finally {
  Pop-Location
}

