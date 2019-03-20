all: build doc test
clean:
	cabal v2-clean
watch +ARGS="":
	watchexec -cre cabal,hs,y -- just {{ARGS}}

build:
	cabal v2-build
doc:
	cabal v2-haddock --enable-documentation
run +ARGS="":
	cabal v2-run stahl -- {{ARGS}}
test:
	cabal v2-build test:tests
	cabal v2-run test:tests
