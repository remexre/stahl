FROM alpine:edge
RUN apk add --no-cache curl ecl ecl-dev gcc gc-dev g++ gmp-dev libc-dev libffi-dev make && \
	curl https://beta.quicklisp.org/quicklisp.lisp -o /tmp/quicklisp.lisp && \
	yes "" | ecl --load /tmp/quicklisp.lisp \
	             --eval '(quicklisp-quickstart:install)' \
	             --eval '(ql:add-to-init-file)' \
	             --eval '(quit)'
VOLUME /code
WORKDIR /code
