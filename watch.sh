#!/usr/bin/env bash

set -e

make clean all
parallel -j4 <<EOF
  live-server --port=8080 target
  fswatch -o . --exclude target | xargs -n1 -I{} make all
EOF
