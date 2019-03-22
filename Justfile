all: build doc test
clean:
	cabal v2-clean
watch +ARGS="":
	watchexec -cre cabal,hs,md,tex,y -- just {{ARGS}}

doc: haddock mdbook latex

build:
	cabal v2-build
haddock:
	cabal v2-haddock --enable-documentation
run +ARGS="":
	cabal v2-run stahl -- {{ARGS}}
test:
	cabal v2-build test:tests
	cabal v2-run test:tests

mdbook:
	mdbook build docs
latex:
	cd docs/latex && tectonic semantics.tex
