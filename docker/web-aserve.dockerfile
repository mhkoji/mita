FROM ubuntu:18.04

RUN apt update && apt install -y \
    wget \
    sbcl \
    ## for locale-gen
    locales

RUN mkdir \
    /app \
    /app-output \
    /build

RUN cd /build && \
    wget https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --noinform \
         --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load /build/quicklisp.lisp \
         --eval "(quicklisp-quickstart:install)"

## uax-15 needs the followings
RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

COPY . /app
RUN cd /root/quicklisp/local-projects && \
    ln -s /app/ && \
    sbcl --noinform \
         --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load "/root/quicklisp/setup.lisp" \
         --load "/app/docker/web.lisp" \
         --eval "(sb-ext:save-lisp-and-die \
                  \"/app-output/web-aserve.bin\" \
                  :executable t \
                  :toplevel #'mita.docker.web:start-aserve)"

ENTRYPOINT ["/app-output/web-aserve.bin"]
