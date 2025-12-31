#!/usr/bin/env bash
set -euo pipefail

# Verify formatting using Ormolu (does not modify files).

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

find src test -name "*.hs" -print0 | xargs -0 stack exec --package ormolu -- ormolu -m check
