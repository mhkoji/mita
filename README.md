# mita
An application written in Common Lisp

## Setup

```shell
git submodule update --init

docker-compose up
```

## Start the Server

```
CL-USER> (ql:quickload :mita-auth)
CL-USER> (mita.web.server:init-db)
CL-USER> (mita.web.server:start)
CL-USER> (mita.auth.server:start)
```
