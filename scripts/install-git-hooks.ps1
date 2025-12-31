$ErrorActionPreference = 'Stop'

$repoRoot = Resolve-Path (Join-Path $PSScriptRoot '..')
Set-Location $repoRoot

if (-not (Get-Command git -ErrorAction SilentlyContinue)) {
  throw 'git is not available on PATH. Install Git for Windows and retry.'
}

git config core.hooksPath .githooks
Write-Host 'Git hooks installed (core.hooksPath=.githooks).'
Write-Host 'Tip: commit from Git Bash / VS Code (Git for Windows) so the bash hook runs.'
