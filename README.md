# mita
An application written in Common Lisp

## Start the Server

```shell
cd delivery-web

docker-compose build

## Start the Server
docker-compose up

## Init
docker run -v $PWD/../data:/data --network=delivery-web_default --entrypoint "" -it delivery-web_web /app/web init
```
