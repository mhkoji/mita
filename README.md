# mita
An application written in Common Lisp

## Start the Server

```shell
git submodule update --init

docker-compose build

## Start the Server
docker-compose up

## init
docker run --net=host --entrypoint "" -it mita_web /bin/bash
root@xxxx:/# ./app-output/web-init
```
