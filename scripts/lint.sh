#!/usr/bin/env bash
set -euo pipefail

# Lint Haskell sources using HLint.
# Uses Stack to provide hlint without adding it as a project dependency.

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

echo "Linting with HLint"
stack exec --package hlint -- hlint src test

# Check for compiler warnings (unused imports, etc.) by treating them as errors.
# This ensures the codebase stays clean of unused code.
echo "Checking compiler warnings"
stack build --test --no-run-tests --ghc-options="-Werror"

echo "No linting errors or compiler warnings found"
