FROM alpine:edge
RUN apk add --no-cache ecl make && \
	curl https://beta.quicklisp.org/quicklisp.lisp -o /tmp/quicklisp.lisp && \
	ecl --load /tmp/quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(quit)' && \
	echo '(load "~/quicklisp/setup.lisp")' > ~/.eclrc
VOLUME /code
WORKDIR /code
