all: out/stahl.fth
ci:
	docker build -t remexre/stahl-builder .travis
	docker run -v "$(shell pwd):/code" --rm remexre/stahl-builder make ci-inner
ci-inner:
	sh -c "trap 'chown -R $(shell stat -c "%u:%g" Makefile) .' EXIT; $(MAKE) clean all"
clean:
	rm -rf tmp out
watch:
	watchexec -cre stahl $(MAKE)
.PHONY: all build ci ci-inner clean watch

out/stahl.fth: tmp/stahl-bootstrap $(SRCS)
	@mkdir -p $(dir $@)
	tmp/stahl-bootstrap $(SRCS) -o $@
.PHONY: out/stahl

tmp/stahl-bootstrap:
	@mkdir -p $(dir $@)
	scripts/get-stahl-bootstrap.sh
