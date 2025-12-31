#!/usr/bin/env bash
set -euo pipefail

# Format Haskell sources using Ormolu.
find src test -name "*.hs" -print0 | xargs -0 stack exec --package ormolu -- ormolu -m inplace