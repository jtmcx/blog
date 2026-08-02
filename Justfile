# List available recipes
default:
    @just --list

# Build the site executable
build:
    cabal build

# Generate the site
site *args: build
    cabal run site -- {{args}}

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

# Disable MacOS extended attributes in tarballs.
export COPYFILE_DISABLE := "1"

# Generate site tarball
tar:
    tar zcf html.tgz -C _site/html .

# Deploy the static site
push: tar
    scp html.tgz jtm.cx:
    ssh jtm.cx "./deploy-site html.tgz"

alias ghci := repl
