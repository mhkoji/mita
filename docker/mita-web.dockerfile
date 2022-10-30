FROM node:18-buster AS static-builder

WORKDIR /build

COPY mita-web/frontend/package.json mita-web/frontend/package-lock.json ./

RUN npm install

COPY mita-web/frontend .

RUN mkdir -p /backend/static/gen/ && npm run build

##########################################

FROM ubuntu:20.04

RUN apt update && apt install -y \
    wget \
    sbcl

RUN mkdir \
    /app \
    /app-build \
    /output

RUN wget \
      https://beta.quicklisp.org/quicklisp.lisp \
      --directory-prefix /app-build && \
    sbcl \
      --noinform \
      --no-userinit \
      --no-sysinit \
      --non-interactive \
      --load /app-build/quicklisp.lisp \
      --eval "(quicklisp-quickstart:install)"

COPY mita             /app/mita
COPY mita-web/backend /app/mita-web-backend

RUN sbcl \
      --noinform \
      --no-userinit \
      --no-sysinit \
      --non-interactive \
      --load "/root/quicklisp/setup.lisp" \
      --eval '(push "/app/mita/" ql:*local-project-directories*)' \
      --eval '(push "/app/mita-web-backend/" ql:*local-project-directories*)' \
      --eval '(ql:quickload :mita-web-server-hunchentoot)' \
      --load "/app/mita-web-backend/bin/docker.lisp" \
      --eval "(sb-ext:save-lisp-and-die \"/mita\" :toplevel #'mita.web.bin.docker:main :executable t)"

COPY --from=static-builder /backend /mita-static

CMD ["/mita"]
