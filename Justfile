# List available recipes
default:
    @just --list

# Build the ssg executable
build:
    cabal build

# Generate the site
site *args: build
    cabal run ssg -- {{args}}

# Remove generated site output and Shake's build database
clean:
    rm -rf _site _build html.tgz

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

# Generate haddock documentation
doc:
    cabal haddock --haddock-executables

# Start a GHCi REPL
repl:
    cabal repl

# Archive the generated site.
tar: site
    tar zcf html.tgz -C _site/html .


alias ghci := repl
