# mita
An application written in Common Lisp

## Setup

```shell
## Postmodern of the lastest version is required.
git submodule update --init

docker pull postgres
docker run --rm -p 5432:5432 -e POSTGRES_DB=mita -e POSTGRES_HOST_AUTH_METHOD=trust postgres 
```

## Start the Server

```
CL-USER> *default-pathname-defaults*
#P"/path to /mita/mita-web/"
CL-USER> (mita.web.server:start :init-db t)
```
