# mita
An application written in Common Lisp

## Start the Server

```shell
docker-compose build

## Start the Server
docker-compose up

## Init
docker run -v $PWD/data:/data --network=mita_default --entrypoint "" -it mita_web /app/web init
```
