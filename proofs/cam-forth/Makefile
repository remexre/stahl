all:
	leanpkg build
clean:
	find -name '*.olean' | xargs rm
run:
	leanpkg build -- --run src/main.lean
watch:
	watchexec -cre lean $(MAKE)
.PHONY: all clean run watch
