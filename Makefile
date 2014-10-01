
all: build

debug: clean
	cabal configure --disable-library-profiling --disable-optimization --enable-executable-dynamic --enable-tests --enable-library-coverage --ghc-options="-Wall"

release: clean
	cabal configure

haddock:
	cabal haddock --hyperlink-sources

build:
	cabal build -j

clean:
	cabal clean

test:
	cabal test

.PHONY: all debug release haddock build clean test
