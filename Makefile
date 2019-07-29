LISP := sbcl
STRIP := strip
SWANK := $(HOME)/.local/share/nvim/plugged/slimv/slime/start-swank.lisp

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
	$(LISP) --load bootstrap/bootstrap.asd --eval '(ql:quickload :bootstrap)' --eval '(in-package :bootstrap)'
bootstrap-swank:
	$(LISP) --load bootstrap/bootstrap.asd \
		--eval '(ql:quickload :bootstrap)' \
		--load $(SWANK)
.PHONY: bootstrap-repl bootstrap-swank

BOOTSTRAP_SRCS := $(shell find bootstrap -name '*.lisp')
SRCS := $(shell find -name '*.stahl')

tmp/bootstrap: bootstrap/bootstrap.asd $(BOOTSTRAP_SRCS)
	@mkdir -p $(dir $@)
	$(LISP) --load bootstrap/bootstrap.asd \
		--eval '(ql:quickload :bootstrap)' \
		--eval "(sb-ext:save-lisp-and-die #p\"$@\" :toplevel #'bootstrap:cli-main :executable t)"

tmp/stahl-bootstrap.fth: tmp/bootstrap $(SRCS)
	@mkdir -p $(dir $@)
	tmp/bootstrap -o $@ $(SRCS)
