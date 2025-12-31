#!/usr/bin/env bash
set -euo pipefail

# Lint Haskell sources using HLint.
# Uses Stack to provide hlint without adding it as a project dependency.

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

stack exec --package hlint -- hlint src test
