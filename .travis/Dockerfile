FROM alpine:edge
RUN apk add --no-cache make && \
	apk add --no-cache -X http://dl-cdn.alpinelinux.org/alpine/edge/testing racket
RUN raco pkg install --auto threading-lib
VOLUME /code
WORKDIR /code
