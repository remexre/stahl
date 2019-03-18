all:
	cabal new-build
watch:
	watchexec -cre cabal,hs,y $(MAKE)
.PHONY: all watch
