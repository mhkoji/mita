#!/bin/bash

CURR_DIR=$(cd $(dirname $0); pwd)

docker build $CURR_DIR/../../ -t mita-web -f $CURR_DIR/Dockerfile
