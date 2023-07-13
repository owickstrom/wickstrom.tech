#!/usr/bin/env bash

set -e

make clean all
parallel -j4 <<EOF
  python3 -m http.server --directory target
  fswatch -o . --exclude target | xargs -n1 -I{} make all
EOF