# mita
An application written in Common Lisp

## Start the Server

```shell
git submodule update --init

docker-compose build

## Start the Server
docker-compose up

## Init
docker run --network=mita_default --entrypoint "" -it mita_web /app/web-init
```
