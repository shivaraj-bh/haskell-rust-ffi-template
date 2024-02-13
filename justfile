default:
    @just --list

# Auto-format the source tree
fmt:
    treefmt

# Run 'cabal run' on the project
run *ARGS:
    cd haskell && cabal run {{ARGS}}

# Run 'ghcid -c cabal repl' to hot-reload
watch *ARGS:
    cd haskell && ghcid -c cabal repl {{ARGS}}