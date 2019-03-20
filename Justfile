all: build doc
watch +ARGS="":
	watchexec -cre cabal,hs,y -- just {{ARGS}}

build:
	cabal new-build
doc:
	cabal new-haddock --enable-documentation
run +ARGS="":
	cabal new-run stahl -- {{ARGS}}
