FROM node:18-buster AS static-builder

WORKDIR /build

COPY mita-web/frontend/package.json mita-web/frontend/package-lock.json ./

RUN npm install

COPY mita-web/frontend .

RUN mkdir -p /backend/static/gen/ && npm run build

##########################################

FROM ubuntu:23.10 AS bin-builder

RUN apt update && apt install -y \
      sbcl \
      ## mita
      cl-csv \
      cl-local-time \
      cl-uuid \
      ## mita-web
      cl-bordeaux-threads \
      cl-who \
      cl-split-sequence \
      cl-yason \
      ## mita-web-server-hunchentoot
      cl-hunchentoot \
      cl-quri \
      ## myway
      curl \
      unzip \
      cl-utilities && \
    curl -L https://github.com/fukamachi/myway/archive/refs/heads/master.zip \
         -o /tmp/myway.zip && \
    curl -L https://github.com/stylewarning/map-set/archive/refs/heads/master.zip \
         -o /tmp/map-set.zip && \
    unzip /tmp/myway.zip -d /usr/share/common-lisp/source/ && \
    unzip /tmp/map-set.zip -d /usr/share/common-lisp/source/ && \
    cd /usr/share/common-lisp/systems/ && \
      ln -s ../source/myway-master/myway.asd && \
      ln -s ../source/map-set-master/map-set.asd

## cache
RUN sbcl \
      --noinform \
      --no-userinit \
      --no-sysinit \
      --non-interactive \
      --eval '(require :asdf)' \
      --eval '(asdf:load-system :cl-csv)' \
      --eval '(asdf:load-system :local-time)' \
      --eval '(asdf:load-system :uuid)' \
      --eval '(asdf:load-system :bordeaux-threads)' \
      --eval '(asdf:load-system :cl-who)' \
      --eval '(asdf:load-system :split-sequence)' \
      --eval '(asdf:load-system :yason)' \
      --eval '(asdf:load-system :hunchentoot)' \
      --eval '(asdf:load-system :myway)' \
      --eval '(asdf:load-system :quri)' && \
    mkdir \
      /build

COPY mita \
     /build/mita/
COPY docker/mita-web/main.lisp mita-web/backend/ \
     /build/mita-web-backend/

RUN sbcl \
      --noinform \
      --no-userinit \
      --no-sysinit \
      --non-interactive \
      --eval '(require :asdf)' \
      --eval '(push "/build/mita/" asdf:*central-registry*)' \
      --eval '(push "/build/mita-web-backend/" asdf:*central-registry*)' \
      --eval '(asdf:load-system :mita-web-server-hunchentoot)' \
      --load "/build/mita-web-backend/main.lisp" \
      --eval "(sb-ext:save-lisp-and-die \"/mita\" :toplevel #'mita.docker.mita-web:main :executable t)"

##########################################

FROM ubuntu:23.10

RUN apt update && apt install -y \
      ## cl+ssl
      libssl3

COPY --from=static-builder /backend /mita-www
COPY --from=bin-builder /mita /mita

ENV LANG ja_JP.UTF-8

CMD ["/mita"]
