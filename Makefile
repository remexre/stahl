LISP := sbcl
STRIP := strip

all: out/stahl-bootstrap.fth proofs
clean:
	rm -rf out tmp
	$(MAKE) -C proofs clean
proofs:
	$(MAKE) -C proofs
watch:
	watchexec -cre asd,asm,lean,lisp,stahl $(MAKE)
.PHONY: all clean proofs watch

ci:
	docker build -t remexre/stahl-builder .travis
	docker run -v "$(shell pwd):/code" --rm remexre/stahl-builder make ci-inner
ci-cron:
	docker build -t remexre/stahl-builder .travis
	docker run -v "$(shell pwd):/code" --rm remexre/stahl-builder make ci-cron-inner
.PHONY: ci ci-cron

ci-cron-inner: ci-inner
ci-inner:
	sh -c "trap 'chown -R $(shell stat -c "%u:%g" Makefile) .' EXIT; $(MAKE) clean all"
.PHONY: ci-cron-inner ci-inner

bootstrap-repl:
	$(LISP) --load bootstrap/entrypoints/repl.lisp --eval '(in-package #:bootstrap)'
bootstrap-swank:
	$(LISP) --load bootstrap/entrypoints/swank.lisp
.PHONY: bootstrap-repl bootstrap-swank

BOOTSTRAP_SRCS := $(shell find bootstrap -name '*.lisp')
SRCS := $(shell find -name '*.stahl')

tmp/bootstrap: bootstrap/bootstrap.asd $(BOOTSTRAP_SRCS)
	@mkdir -p $(dir $@)
	$(LISP) --load bootstrap/entrypoints/compile.lisp

out/stahl-bootstrap.fth: tmp/bootstrap $(SRCS)
	@mkdir -p $(dir $@)
	tmp/bootstrap -o $@ $(SRCS)
