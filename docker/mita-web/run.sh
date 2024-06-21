#!/bin/bash

if [[ "$#" -ne 1 ]]; then
  echo "Usage: $0 FILE_ROOT_PATH"
  exit 1
fi

file_root_path=$1

docker run \
       --network=host \
       --rm \
       -v "$file_root_path:/file-root-path/" \
       -t mita-web
