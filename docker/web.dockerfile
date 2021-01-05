FROM node:12.18.3-stretch AS static_builder

RUN mkdir /static

WORKDIR /build

COPY mita-web/frontend .

RUN npm install && npm run build

################################################

FROM ubuntu:18.04 AS bin_builder

RUN apt update && apt install -y \
    wget \
    sbcl \
    imagemagick \
    ### for locale-gen
    locales && \
    ##
    mkdir \
    /app \
    /build && \
    ##
    cd /build && \
    wget https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --noinform \
         --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load /build/quicklisp.lisp \
         --eval "(quicklisp-quickstart:install)" && \
    ## uax-15 needs the followings
    locale-gen en_US.UTF-8

ENV LANG=en_US.UTF-8 \
    LANGUAGE=en_US:en \
    LC_ALL=en_US.UTF-8

COPY ./docker/requirements.lisp /build
RUN sbcl --noinform \
         --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load "/root/quicklisp/setup.lisp" \
         --load "/build/requirements.lisp"

COPY --from=static_builder /static /app/static

COPY . /root/quicklisp/local-projects/mita
RUN sbcl --noinform \
         --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load "/root/quicklisp/setup.lisp" \
         --load "/root/quicklisp/local-projects/mita/docker/web.lisp" \
         --eval "(sb-ext:save-lisp-and-die \
                  \"/app/web\" \
                  :executable t \
                  :toplevel #'mita.docker.web:main)"

ENTRYPOINT ["/app/web", "clack"]
