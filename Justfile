all:
	cabal new-build
watch +ARGS="":
	watchexec -cre cabal,hs,y -- just {{ARGS}}

run +ARGS="":
	cabal new-run stahl -- {{ARGS}}
