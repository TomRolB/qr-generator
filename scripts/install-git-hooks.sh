#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

git config core.hooksPath .githooks

# Best-effort: make scripts executable on Unixy filesystems.
chmod +x "$repo_root/.githooks/pre-commit" "$repo_root/scripts/format.sh" 2>/dev/null || true

echo "Git hooks installed (core.hooksPath=.githooks)."
