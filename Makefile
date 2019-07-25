all: tmp/stahl-bootstrap.fth
ci:
	docker build -t remexre/stahl-builder .travis
	docker run -v "$(shell pwd):/code" --rm remexre/stahl-builder make ci-inner
ci-inner:
	sh -c "trap 'chown -R $(shell stat -c "%u:%g" Makefile) .' EXIT; $(MAKE) clean all"
clean:
	rm -rf out tmp
watch:
	watchexec -cre asd,lisp,stahl $(MAKE)
.PHONY: all build ci ci-inner clean watch

bootstrap-repl:
.PHONY: bootstrap-repl

BOOTSTRAP_SRCS := $(shell find bootstrap -name '*.lisp')
SRCS := $(shell find -name '*.stahl')

tmp/stahl-bootstrap.fth: bootstrap.lisp $(BOOTSTRAP_SRCS) $(SRCS)
	@mkdir -p $(dir $@)
	ecl --load bootstrap.lisp
