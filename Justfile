# List available recipes
default:
    @just --list

# Build the ssg executable
build:
    cabal build

# Generate the site into _site/
site *args: build
    cabal run ssg -- {{args}}

# Remove generated site output and Shake's build database
clean:
    rm -rf _site _build

# clean, plus cabal's own build artifacts
distclean: clean
    rm -rf dist-newstyle

# Format Haskell sources
fmt:
    ormolu --mode inplace app/Main.hs

# Run tests
test:
    cabal test
    cabal repl --with-compiler=doctest

# Start a GHCi REPL
repl:
    cabal repl

alias ghci := repl
