# Hindley-Milner dev tasks
# Run any recipe with:  just <recipe>
# List all recipes:     just

set shell := ["bash", "-euo", "pipefail", "-c"]

SRC_DIRS := "src test"

# List available recipes (default)
[private]
default:
    @just --list --unsorted

# ── Build ────────────────────────────────────────────────────────────────────

# Compile everything
build:
    cabal build all

# Compile and run the test suite
test:
    cabal test all --test-show-details=always

# Build + test in one shot (CI-friendly)
check: build test

# ── Interactive development ──────────────────────────────────────────────────

# Start a GHCi REPL (loads the library; try  :m Infer  then  inferExpr ...)
repl:
    cabal repl hindley-milner

# Live-reloading watcher: recompiles on every save and reruns tests
watch:
    ghcid \
      --command "cabal repl hindley-milner" \
      --test ":! cabal test all --test-show-details=always"

# ── Code quality ─────────────────────────────────────────────────────────────

# Run hlint on all source files
lint:
    hlint {{ SRC_DIRS }}

# Format all .hs files in-place with ormolu
fmt:
    find {{ SRC_DIRS }} -name "*.hs" -print0 | xargs -0 ormolu --mode inplace

# Check formatting without modifying files (useful in CI)
fmt-check:
    find {{ SRC_DIRS }} -name "*.hs" -print0 | xargs -0 ormolu --mode check

# ── Cleanup ──────────────────────────────────────────────────────────────────

# Remove cabal build artifacts
clean:
    cabal clean
