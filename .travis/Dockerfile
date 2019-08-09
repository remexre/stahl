FROM alpine:edge
RUN apk add --no-cache bash curl make && \
	apk add --no-cache -X http://dl-cdn.alpinelinux.org/alpine/edge/testing sbcl && \
	curl https://beta.quicklisp.org/quicklisp.lisp -o /tmp/quicklisp.lisp && \
	yes "" | sbcl --load /tmp/quicklisp.lisp \
	             --eval '(quicklisp-quickstart:install)' \
	             --eval '(ql:add-to-init-file)' \
	             --eval '(quit)' && \
	curl -L https://github.com/leanprover/lean/releases/download/v3.4.2/lean-3.4.2-linux.tar.gz | tar xzC /opt
ENV PATH="/opt/lean-3.4.2-linux/bin:$PATH"
RUN sbcl \
	--eval '(ql:quickload :trivia)' \
	--eval '(ql:quickload :unix-opts)' \
	--eval '(quit)'
VOLUME /code
WORKDIR /code
