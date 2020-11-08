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
    ## for locale-gen
    locales

RUN mkdir \
    /app \
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

COPY third-party /root/quicklisp/local-projects
RUN sbcl --noinform \
         --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load "/root/quicklisp/setup.lisp" \
         --eval "(ql:quickload '(:uax-15 :postmodern :ironclad :cl-bcrypt))"

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
                  :toplevel #'mita.docker.web:start)"  && \
    sbcl --noinform \
         --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load "/root/quicklisp/setup.lisp" \
         --load "/root/quicklisp/local-projects/mita/docker/web.lisp" \
         --eval "(sb-ext:save-lisp-and-die \
                  \"/app/web-init\" \
                  :executable t \
                  :toplevel #'mita.docker.web:init)"

ENTRYPOINT ["/app/web"]
