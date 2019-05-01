# Builds, documents, and tests the compiler.
all: build doc test
# Removes build artifacts.
clean:
	cabal v2-clean
	test ! -d docs/book || rm -r docs/book
	test ! -f docs/semantics/semantics.pdf || rm docs/semantics/semantics.pdf
# Watches the given target.
watch +TARGETS="all":
	watchexec -cre cabal,hs,md,y -- just {{TARGETS}}

# Builds all docs.
doc: haddock mdbook latex

# Builds the compiler.
build:
	cabal v2-build
# Builds the compiler API docs.
haddock:
	cabal v2-haddock --enable-documentation
# Runs the compiler with the given arguments.
run +ARGS="":
	cabal v2-run stahl -- {{ARGS}}
# Runs the tests.
test:
	cabal v2-build test:tests
	cabal v2-run test:tests

# Builds the online docs.
mdbook:
	mdbook build docs
# Builds the semantics paper.
latex:
	cd docs/semantics && tectonic semantics.tex
