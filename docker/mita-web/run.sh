#!/bin/bash

if [[ "$#" -ne 3 ]]; then
  echo "Usage: $0 FILE_ROOT_PATH TAG_PATH PORT"
  exit 1
fi

file_root_path=$1
tag_path=$2
port=$3

docker run \
       --network=host \
       --rm \
       -v "$file_root_path:/file-root-path/" \
       -v "$tag_path:/tag-store/" \
       -t mita-web "$port"
