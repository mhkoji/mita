FROM ubuntu:18.04 AS bin_builder

RUN apt update && apt install -y \
    wget \
    sbcl \
    imagemagick \
    libmysqlclient-dev \
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

COPY ./delivery-gui/docker/requirements.lisp /build
RUN sbcl --noinform \
         --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load "/root/quicklisp/setup.lisp" \
         --load "/build/requirements.lisp"

COPY . /root/quicklisp/local-projects/mita
RUN sbcl --noinform \
         --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load "/root/quicklisp/setup.lisp" \
         --load "/root/quicklisp/local-projects/mita/delivery-gui/docker/config.lisp" \
         --load "/root/quicklisp/local-projects/mita/delivery-gui/docker/gui.lisp" \
         --eval "(sb-ext:save-lisp-and-die \
                  \"/app/gui\" \
                  :executable t \
                  :toplevel #'mita.docker.gui:main)"

ENTRYPOINT ["/app/gui", "clack"]
