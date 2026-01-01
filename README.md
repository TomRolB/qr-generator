# qr-generator

## Prereqs
- GHC/Stack: install Stack from https://docs.haskellstack.org/

## Build / test
- `stack build`
- `stack test`

## Formatting
Formatting is done with Ormolu via Stack:
- `./scripts/format.sh`

To verify formatting without modifying files (useful for CI):
- `./scripts/format-check.sh`

## Linting
Linting is done with HLint via Stack, and also checks for compiler warnings (unused imports, etc):
- `./scripts/lint.sh`

## Git hooks (recommended)
This repo uses a repo-local hooks path so hooks are easy to install and consistent across machines.

Install hooks (one-time per clone):
- Windows (PowerShell): `./scripts/install-git-hooks.ps1`
- macOS/Linux (bash): `./scripts/install-git-hooks.sh`

What it does:
- Configures `core.hooksPath` to `.githooks`
- Runs formatting on `pre-commit` and stages any formatting changes in `src/` + `test/`
