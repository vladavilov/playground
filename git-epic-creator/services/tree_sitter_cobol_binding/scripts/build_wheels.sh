#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

python -m pip install -U pip
python -m pip install "build==1.2.2.post1" "cibuildwheel==2.22.0" "pytest==8.3.4" "tree-sitter==0.25.1"

cd "$root"
python -m build

export CIBW_BUILD="cp312-*"
python -m cibuildwheel --output-dir wheelhouse

