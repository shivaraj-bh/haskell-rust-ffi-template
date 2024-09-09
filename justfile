default:
    @just --list

# Auto-format the source tree
fmt:
    treefmt

# Update package index from Hackage and run 'cabal run'
# Required in CI to avoid:
# Error: [Cabal-7160] The package list for 'hackage.haskell.org' does not exist. Run 'cabal update' to download it.
_update-and-run *ARGS:
    cd haskell && cabal update && cabal run {{ARGS}}

# Run 'cabal run' on the project
run *ARGS:
    cd haskell && cabal run {{ARGS}}

# Run 'ghcid -c cabal repl' to hot-reload
watch *ARGS:
    cd haskell && ghcid -c cabal repl {{ARGS}}