FROM alpine:latest

MAINTAINER anon

RUN apk --no-cache add \
        htop\
        tini

ENTRYPOINT ["/sbin/tini", "--"]
