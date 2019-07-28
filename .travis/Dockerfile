FROM alpine:edge
RUN apk add --no-cache curl make && \
	apk add --no-cache -X http://dl-cdn.alpinelinux.org/alpine/edge/testing sbcl && \
	curl https://beta.quicklisp.org/quicklisp.lisp -o /tmp/quicklisp.lisp && \
	yes "" | sbcl --load /tmp/quicklisp.lisp \
	             --eval '(quicklisp-quickstart:install)' \
	             --eval '(ql:add-to-init-file)' \
	             --eval '(quit)'
RUN sbcl \
	--eval '(ql:quickload :trivia)' \
	--eval '(ql:quickload :unix-opts)' \
	--eval '(quit)'
VOLUME /code
WORKDIR /code
